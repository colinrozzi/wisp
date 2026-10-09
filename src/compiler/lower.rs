use super::*;

// Lowering: macro-expanded generics/traits/derive -> monomorphic forms.
#[derive(Debug, Clone)]
pub(crate) struct GenericFnDef {
    name: String,
    tparams: Vec<String>,               // type parameters (declaration order)
    constraints: Vec<(String, String)>, // (trait, type parameter it constrains)
    func_params: Vec<usize>,            // argument positions of function-typed parameters
    params: SExpr,                      // ((name type) ...) with type params still symbolic
    ret: SExpr,                         // return type expr with type params still symbolic
    body: SExpr,
}

/// A generic variant template: `(variant (Name T ...) (case payload...) ...)`.
/// Monomorphized on demand into a concrete nominal variant with per-instantiation
/// case names, mirroring how generic functions are specialized.
#[derive(Clone)]
pub(crate) struct GenericTypeDef {
    tparams: Vec<String>,
    cases: Vec<SExpr>, // each `(case-name payload-type...)`, type params symbolic
    span: Span,
}

/// A generic record template: `(record (Name T ...) (field T) ...)`. Monomorphized
/// by name into a concrete nominal record, the same way generic variants are.
#[derive(Clone)]
pub(crate) struct GenericRecordDef {
    tparams: Vec<String>,
    fields: Vec<SExpr>, // each `(field-name field-type)`, type params symbolic
    span: Span,
}

/// One monomorphization request: a template specialized at concrete types (one binding
/// per type parameter, in declaration order) and function-name arguments.
#[derive(Debug, Clone)]
pub(crate) struct SpecKey {
    name: String,
    bindings: Vec<(String, String)>, // (type parameter, concrete type)
    func_args: Vec<String>,
}

/// True when a parameter's type expr is a function type `(-> arg... ret)`.
pub(crate) fn is_func_type(ty: &SExpr) -> bool {
    matches!(ty, SExpr::List(items, _) if head_sym(items) == Some("->"))
}

/// Argument positions of the function-typed parameters in a param list.
pub(crate) fn func_param_indices(params: &SExpr) -> Vec<usize> {
    let mut out = Vec::new();
    if let SExpr::List(items, _) = params {
        for (i, p) in items.iter().enumerate() {
            if let Some((_, ty)) = param_name_and_type(p)
                && is_func_type(ty)
            {
                out.push(i);
            }
        }
    }
    out
}

/// The parameter name at a given position in a param list.
pub(crate) fn param_name_at(params: &SExpr, idx: usize) -> Option<String> {
    if let SExpr::List(items, _) = params {
        param_name_and_type(items.get(idx)?).map(|(n, _)| n.to_string())
    } else {
        None
    }
}

/// The mangled name of a specialized template: base, then each function argument,
/// then the concrete type (if any). e.g. `map--double--s32`, `apply-twice--inc`.
pub(crate) fn template_fn_name(base: &str, func_args: &[String], concretes: &[String]) -> String {
    let mut s = base.to_string();
    for fa in func_args {
        s.push_str("--");
        s.push_str(&sanitize_method(fa));
    }
    for c in concretes {
        s.push_str("--");
        s.push_str(c);
    }
    s
}

/// A trait method's declared signature (the type parameter is still symbolic).
#[derive(Debug, Clone)]
pub(crate) struct TraitMethodSig {
    name: String,
    params: SExpr,
    ret: SExpr,
}

/// A declared trait: its type parameters and its method signatures.
#[derive(Debug, Clone)]
pub(crate) struct TraitDef {
    tparams: Vec<String>,
    methods: Vec<TraitMethodSig>,
}

/// Parse a bodyless trait method signature: `(fn name params [:] ret)`.
pub(crate) fn parse_method_sig(mi: &[SExpr]) -> Option<TraitMethodSig> {
    let name = match mi.get(1)? {
        SExpr::Sym(s, _) => s.clone(),
        _ => return None,
    };
    let params = mi.get(2)?.clone();
    let mut idx = 3;
    if mi.get(idx).is_some_and(is_colon) {
        idx += 1;
    }
    let ret = mi.get(idx)?.clone();
    if idx != mi.len() - 1 {
        return None; // a trait method has no body
    }
    Some(TraitMethodSig { name, params, ret })
}

/// Structural equality of two type expressions, ignoring spans.
pub(crate) fn type_expr_eq(a: &SExpr, b: &SExpr) -> bool {
    match (a, b) {
        (SExpr::Sym(x, _), SExpr::Sym(y, _)) => x == y,
        (SExpr::List(xs, _), SExpr::List(ys, _)) => {
            xs.len() == ys.len() && xs.iter().zip(ys).all(|(p, q)| type_expr_eq(p, q))
        }
        _ => false,
    }
}

/// The type expr of each parameter in a param list (colon or bare form).
pub(crate) fn param_type_exprs(params: &SExpr) -> Vec<&SExpr> {
    let mut out = Vec::new();
    if let SExpr::List(items, _) = params {
        for p in items {
            if let Some((_, ty)) = param_name_and_type(p) {
                out.push(ty);
            }
        }
    }
    out
}

/// Context for resolving trait-method calls inside a specialized body.
pub(crate) struct MethodCtx {
    constraints: Vec<(String, String)>, // (trait, type parameter it constrains)
    bindings: Vec<(String, String)>,    // (type parameter, concrete type)
}

pub(crate) struct Lowering<'a> {
    ctx: &'a CompileContext,
    generics: HashMap<String, GenericFnDef>,
    traits: HashMap<String, TraitDef>, // trait name -> declaration (type params + sigs)
    method_to_trait: HashMap<String, String>,
    monofn_returns: HashMap<String, String>, // fn name -> scalar return type string
    fn_params: HashMap<String, Vec<Option<String>>>, // concrete fn name -> per-param type strings
    instances: HashMap<(String, String), HashMap<String, String>>, // (trait, type-key)->method->fn
    instance_defs: HashMap<String, SExpr>,   // instance fn name -> its (renamed) fn form
    used_instances: HashSet<String>,         // instance fn names actually referenced
    instance_worklist: Vec<String>,          // instance fns awaiting emission
    worklist: Vec<SpecKey>,                  // template specializations awaiting emission
    emitted: HashSet<String>,                // mangled names already emitted
    generic_variants: HashMap<String, GenericTypeDef>, // generic variant templates
    case_to_generic: HashMap<String, String>, // variant case name -> generic variant name
    type_instances: HashMap<String, (String, Vec<String>)>, // mangled -> (generic, concretes)
    type_worklist: Vec<(String, Vec<String>)>, // (generic, concretes) awaiting emission
    generic_records: HashMap<String, GenericRecordDef>, // generic record templates
    record_instances: HashMap<String, (String, Vec<String>)>, // mangled -> (generic, concretes)
    record_worklist: Vec<(String, Vec<String>)>, // (generic, concretes) awaiting emission
}

/// The scalar type name for a type-expr that is a bare symbol (e.g. `s32`).
pub(crate) fn type_expr_string(e: &SExpr) -> Option<String> {
    match e {
        SExpr::Sym(s, _) => Some(s.clone()),
        _ => None,
    }
}

/// Map a literal's attached `Type` to its surface name.
pub(crate) fn scalar_type_name(ty: &Type) -> Option<String> {
    Some(
        match ty {
            Type::S32 => "s32",
            Type::S64 => "s64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::U8 => "u8",
            Type::Bool => "bool",
            Type::U16 => "u16",
            Type::U32 => "u32",
            Type::U64 => "u64",
            _ => return None,
        }
        .to_string(),
    )
}

/// A canonical string for a concrete type expr, supporting nesting:
/// `s32`, `(list s32)`, `(option (list s32))`, `(tuple s32 f64)`, ...
pub(crate) fn canonical_type(e: &SExpr) -> Option<String> {
    match e {
        SExpr::Sym(s, _) => Some(s.clone()),
        SExpr::List(items, _) => {
            let parts = items
                .iter()
                .map(canonical_type)
                .collect::<Option<Vec<_>>>()?;
            Some(format!("({})", parts.join(" ")))
        }
        _ => None,
    }
}

/// Parse a canonical type string (e.g. `(list s32)`) back into a type expr.
pub(crate) fn type_str_to_expr(s: &str) -> Option<SExpr> {
    let toks = tokenize(s);
    if toks.is_empty() {
        return None;
    }
    let (e, _) = parse_sexpr(&toks, 0);
    Some(e)
}

/// The element type of a canonical list type string, e.g. `(list s32)` -> `s32`.
pub(crate) fn list_elem_type(s: &str) -> Option<String> {
    match type_str_to_expr(s)? {
        SExpr::List(items, _) if items.len() == 2 && head_sym(&items) == Some("list") => {
            canonical_type(&items[1])
        }
        _ => None,
    }
}

/// Structurally unify a parameter type pattern against a concrete type expr,
/// accumulating a binding for each of `tparams` it can determine (first wins).
/// e.g. pattern `(-> T U)` vs concrete `(-> s32 f64)` binds `T=s32`, `U=f64`.
pub(crate) fn unify_types(
    pat: &SExpr,
    concrete: &SExpr,
    tparams: &[String],
    out: &mut Vec<(String, String)>,
) {
    match (pat, concrete) {
        (SExpr::Sym(s, _), _) if tparams.iter().any(|t| t == s) => {
            if !out.iter().any(|(k, _)| k == s)
                && let Some(c) = canonical_type(concrete)
            {
                out.push((s.clone(), c));
            }
        }
        (SExpr::List(ps, _), SExpr::List(cs, _)) if ps.len() == cs.len() => {
            for (p, c) in ps.iter().zip(cs) {
                unify_types(p, c, tparams, out);
            }
        }
        _ => {}
    }
}

/// Apply every `(type parameter, concrete)` substitution to a type expr.
pub(crate) fn subst_types(e: &SExpr, bindings: &[(String, String)]) -> SExpr {
    let mut out = e.clone();
    for (tp, concrete) in bindings {
        out = subst_type(&out, tp, concrete);
    }
    out
}

/// Substitute a type parameter symbol with a concrete type throughout a type expr.
/// The concrete type may itself be compound (e.g. `(list s32)`).
pub(crate) fn subst_type(e: &SExpr, tparam: &str, concrete: &str) -> SExpr {
    match e {
        SExpr::Sym(s, span) if s == tparam => {
            if concrete.contains('(') {
                type_str_to_expr(concrete)
                    .unwrap_or_else(|| SExpr::Sym(concrete.to_string(), span.clone()))
            } else {
                SExpr::Sym(concrete.to_string(), span.clone())
            }
        }
        SExpr::List(items, span) => SExpr::List(
            items
                .iter()
                .map(|i| subst_type(i, tparam, concrete))
                .collect(),
            span.clone(),
        ),
        other => other.clone(),
    }
}

/// Extract the name symbol and type expr from one param, for either
/// `(name type)` or `(name : type)`.
pub(crate) fn param_name_and_type(p: &SExpr) -> Option<(&str, &SExpr)> {
    if let SExpr::List(pp, _) = p {
        let name = match pp.first() {
            Some(SExpr::Sym(n, _)) => n.as_str(),
            _ => return None,
        };
        let ty = match pp.len() {
            2 => &pp[1],
            3 if matches!(&pp[1], SExpr::Sym(s, _) if s == ":") => &pp[2],
            _ => return None,
        };
        return Some((name, ty));
    }
    None
}

/// Strip a `(lin T)` / `(aff T)` multiplicity qualifier, returning the inner type
/// expr. The lowering pass reasons about types structurally and ignores
/// multiplicity (the linearity checker handles it later), so its type lookups
/// see `T`, not `(lin T)`.
pub(crate) fn unwrap_mult(e: &SExpr) -> &SExpr {
    if let SExpr::List(items, _) = e
        && items.len() == 2
        && matches!(head_sym(items), Some("lin") | Some("aff"))
    {
        &items[1]
    } else {
        e
    }
}

/// Build a name->canonical-type environment from a param list SExpr. Compound
/// types (e.g. `(list s32)`) are included, so structural inference can use them.
pub(crate) fn param_env(params: &SExpr) -> Vec<(String, String)> {
    let mut env = Vec::new();
    if let SExpr::List(items, _) = params {
        for p in items {
            if let Some((n, ty)) = param_name_and_type(p)
                && let Some(ts) = canonical_type(unwrap_mult(ty))
            {
                env.push((n.to_string(), ts));
            }
        }
    }
    env
}

/// Per-parameter declared canonical type strings (None where absent).
pub(crate) fn param_type_strings(params: &SExpr) -> Vec<Option<String>> {
    let mut out = Vec::new();
    if let SExpr::List(items, _) = params {
        for p in items {
            match param_name_and_type(p) {
                Some((_, ty)) => out.push(canonical_type(unwrap_mult(ty))),
                None => out.push(None),
            }
        }
    }
    out
}

pub(crate) fn head_sym(items: &[SExpr]) -> Option<&str> {
    match items.first() {
        Some(SExpr::Sym(s, _)) => Some(s.as_str()),
        _ => None,
    }
}

/// True when an SExpr is the bare `:` symbol used for type annotations.
pub(crate) fn is_colon(e: &SExpr) -> bool {
    matches!(e, SExpr::Sym(s, _) if s == ":")
}

/// True for the built-in scalar type names.
pub(crate) fn is_scalar_name(s: &str) -> bool {
    matches!(s, "s32" | "s64" | "f32" | "f64" | "u8")
}

/// Structural view of a `fn` form. Tolerates an optional `:` before the return
/// type and an optional `(where ...)` clause:
///   (fn name (params) [:] ret [(where ...)] body)
pub(crate) struct FnShape<'a> {
    pub(crate) name: &'a SExpr,
    pub(crate) params: &'a SExpr,
    pub(crate) ret: &'a SExpr,
    pub(crate) where_clause: Option<&'a SExpr>,
    pub(crate) body: &'a SExpr,
}

pub(crate) fn fn_shape(items: &[SExpr]) -> Option<FnShape<'_>> {
    // items[0] == "fn" is checked by the caller.
    let name = items.get(1)?;
    let params = items.get(2)?;
    let mut idx = 3;
    if items.get(idx).is_some_and(is_colon) {
        idx += 1;
    }
    let ret = items.get(idx)?;
    idx += 1;
    let where_clause = if idx + 1 < items.len()
        && matches!(items.get(idx), Some(SExpr::List(w, _)) if head_sym(w) == Some("where"))
    {
        let w = items.get(idx);
        idx += 1;
        w
    } else {
        None
    };
    let body = items.get(idx)?;
    if idx != items.len() - 1 {
        return None; // trailing junk after the body
    }
    Some(FnShape {
        name,
        params,
        ret,
        where_clause,
        body,
    })
}

pub(crate) fn sanitize_method(m: &str) -> String {
    m.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '-' {
                c.to_string()
            } else {
                format!("c{}", c as u32)
            }
        })
        .collect()
}

pub(crate) fn instance_fn_name(trait_name: &str, types: &[String], method: &str) -> String {
    format!(
        "{}--{}--{}",
        trait_name,
        sanitize_method(method),
        types.join("--")
    )
}

/// The internal map key for an instance: the trait's type arguments, joined.
pub(crate) fn instance_key(types: &[String]) -> String {
    types.join(",")
}

impl<'a> Lowering<'a> {
    /// Infer the surface type name of an expression (literals, params, known calls).
    fn infer_type(&self, e: &SExpr, env: &[(String, String)]) -> Option<String> {
        match e {
            SExpr::Int { ty, .. } => scalar_type_name(ty),
            SExpr::Float { ty, .. } => scalar_type_name(ty),
            SExpr::Sym(name, _) => env.iter().find(|(n, _)| n == name).map(|(_, t)| t.clone()),
            SExpr::List(items, _) => {
                let head = head_sym(items)?;
                if let Some(info) = lookup_wasm_instr(head) {
                    return scalar_type_name(&info.result);
                }
                // Built-in list/string operations with a known result type.
                match head {
                    "list-new" => {
                        return Some(format!("(list {})", canonical_type(items.get(1)?)?));
                    }
                    "list-len" | "string-len" | "string-ref" => return Some("s32".to_string()),
                    "list-push" => return self.infer_type(items.get(1)?, env),
                    "list-get" => {
                        let lt = self.infer_type(items.get(1)?, env)?;
                        return list_elem_type(&lt);
                    }
                    _ => {}
                }
                // A call to a template (generic and/or higher-order): its result type
                // is the return type with the type parameters replaced by the inferred
                // type arguments. Function arguments do not affect the return.
                if let Some(genfn) = self.generics.get(head) {
                    let bindings = self.infer_bindings(genfn, &items[1..], env);
                    let ret = subst_types(&genfn.ret, &bindings);
                    return canonical_type(&ret);
                }
                // A generic record construction's type is its mangled instantiation.
                if self.generic_records.contains_key(head) {
                    return self.generic_record_instance(head, &items[1..], env);
                }
                self.monofn_returns.get(head).cloned()
            }
            _ => None,
        }
    }

    /// The signature of a known monomorphic function as a `(-> arg... ret)` type expr,
    /// used to unify a function parameter's declared type against the actual function.
    fn func_signature_expr(&self, fname: &str) -> Option<SExpr> {
        let params = self.fn_params.get(fname)?;
        let ret = self.monofn_returns.get(fname)?;
        let mut parts = vec!["->".to_string()];
        for p in params {
            parts.push(p.clone()?);
        }
        parts.push(ret.clone());
        type_str_to_expr(&format!("({})", parts.join(" ")))
    }

    /// Infer a concrete binding for each type parameter of a template call. A value
    /// parameter's type pattern is unified against the argument's inferred type; a
    /// function parameter's `(-> ...)` is unified against the function argument's
    /// signature. Bindings are returned in type-parameter declaration order.
    fn infer_bindings(
        &self,
        genfn: &GenericFnDef,
        args: &[SExpr],
        env: &[(String, String)],
    ) -> Vec<(String, String)> {
        let mut found: Vec<(String, String)> = Vec::new();
        let ptypes = param_type_exprs(&genfn.params);
        for (i, pexpr) in ptypes.iter().enumerate() {
            let Some(a) = args.get(i) else { continue };
            if genfn.func_params.contains(&i) {
                if let SExpr::Sym(fname, _) = a
                    && let Some(sig) = self.func_signature_expr(fname)
                {
                    unify_types(pexpr, &sig, &genfn.tparams, &mut found);
                }
            } else if let Some(cstr) = self.infer_type(a, env)
                && let Some(cexpr) = type_str_to_expr(&cstr)
            {
                unify_types(pexpr, &cexpr, &genfn.tparams, &mut found);
            }
        }
        // Return in declaration order, keeping only parameters that got a binding.
        genfn
            .tparams
            .iter()
            .filter_map(|tp| {
                found
                    .iter()
                    .find(|(k, _)| k == tp)
                    .map(|(_, c)| (tp.clone(), c.clone()))
            })
            .collect()
    }

    /// Resolve which concrete types a trait-method call uses, one per trait type
    /// parameter (in declaration order). Bindings come from the argument types, the
    /// expected type (return-position dispatch), and the enclosing generic's bindings.
    /// Returns None if any type parameter is left unbound.
    fn resolve_trait_types(
        &self,
        trait_name: &str,
        method: &str,
        args: &[SExpr],
        env: &[(String, String)],
        expected: Option<&str>,
        mrc: Option<&MethodCtx>,
    ) -> Option<Vec<String>> {
        let tdef = self.traits.get(trait_name)?;
        let sig = tdef.methods.iter().find(|m| m.name == method)?;
        let mut bindings: Vec<(String, String)> = Vec::new();

        // From the argument types.
        for (i, pexpr) in param_type_exprs(&sig.params).iter().enumerate() {
            if let Some(a) = args.get(i)
                && let Some(cs) = self.infer_type(a, env)
                && let Some(ce) = type_str_to_expr(&cs)
            {
                unify_types(pexpr, &ce, &tdef.tparams, &mut bindings);
            }
        }
        // From the expected type, against the return type (return-position dispatch).
        if let Some(exp) = expected
            && let Some(ee) = type_str_to_expr(exp)
        {
            unify_types(&sig.ret, &ee, &tdef.tparams, &mut bindings);
        }
        // From the enclosing generic body: a `(where (Trait G) ...)` constraint maps
        // the generic's type parameter `G` to this (single-parameter) trait's own type
        // parameter, so we can use `G`'s binding when arguments don't fix it.
        if let Some(mc) = mrc
            && tdef.tparams.len() == 1
            && !bindings.iter().any(|(k, _)| k == &tdef.tparams[0])
        {
            for (tn, gtp) in &mc.constraints {
                if tn == trait_name
                    && let Some((_, gc)) = mc.bindings.iter().find(|(k, _)| k == gtp)
                {
                    bindings.push((tdef.tparams[0].clone(), gc.clone()));
                    break;
                }
            }
        }

        let types: Vec<String> = tdef
            .tparams
            .iter()
            .filter_map(|tp| {
                bindings
                    .iter()
                    .find(|(k, _)| k == tp)
                    .map(|(_, c)| c.clone())
            })
            .collect();
        (types.len() == tdef.tparams.len()).then_some(types)
    }

    /// Rewrite a body: resolve trait methods (when `mrc` is set) and generic calls.
    ///
    /// `expected` is the type this expression is used at, flowing *down* from
    /// context (a return annotation, an ascription, an `if`/`let` tail, or a
    /// sibling argument). It supplies the missing type for return-type dispatch
    /// (methods like `zero : T` whose type parameter is only in the return).
    fn walk(
        &mut self,
        e: &SExpr,
        env: &[(String, String)],
        mrc: Option<&MethodCtx>,
        expected: Option<String>,
    ) -> Result<SExpr> {
        match e {
            SExpr::List(items, span) => {
                // Colon ascription `(expr : type)`: rewrite a generic ADT annotation
                // to its concrete name and steer the expected type into `expr`.
                if items.len() == 3 && is_colon(&items[1]) {
                    let rewritten_ty = self.rewrite_type_expr(&items[2]);
                    let exp = canonical_type(&rewritten_ty);
                    let inner = self.walk(&items[0], env, mrc, exp)?;
                    return Ok(SExpr::List(
                        vec![inner, items[1].clone(), rewritten_ty],
                        span.clone(),
                    ));
                }

                if let Some(head) = head_sym(items) {
                    // Trait-method call: resolve to a concrete instance function.
                    // Works anywhere, not only inside a generic body — the dispatch
                    // type comes from the arguments, the expected type, or (inside a
                    // generic) the type-parameter binding.
                    if let Some(trait_name) = self.method_to_trait.get(head).cloned() {
                        // Bind the trait's type parameters from the argument types, the
                        // expected type, and (inside a generic) the enclosing bindings.
                        let types = self
                            .resolve_trait_types(
                                &trait_name,
                                head,
                                &items[1..],
                                env,
                                expected.as_deref(),
                                mrc,
                            )
                            .ok_or_else(|| {
                                self.ctx.error(
                                    format!(
                                        "cannot resolve trait method '{}': no instance matches the argument types or the expected type",
                                        head
                                    ),
                                    span,
                                )
                            })?;
                        let fname = self
                            .instances
                            .get(&(trait_name.clone(), instance_key(&types)))
                            .and_then(|m| m.get(head))
                            .cloned()
                            .ok_or_else(|| {
                                self.ctx.error(
                                    format!(
                                        "no instance of trait '{}' for {:?} (method '{}')",
                                        trait_name, types, head
                                    ),
                                    span,
                                )
                            })?;
                        // Mark this instance for emission (instances are emitted on
                        // demand, so an unused stdlib costs nothing).
                        self.mark_instance(&fname);
                        // Each argument is expected at the instance method's param type.
                        let arg_exp = self.fn_params.get(&fname).cloned().unwrap_or_default();
                        let mut new_items = Vec::with_capacity(items.len());
                        new_items.push(SExpr::Sym(fname, items[0].span().clone()));
                        for (i, a) in items[1..].iter().enumerate() {
                            let exp = arg_exp.get(i).cloned().flatten();
                            new_items.push(self.walk(a, env, mrc, exp)?);
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // Template call (generic and/or higher-order)?
                    if let Some(genfn) = self.generics.get(head).cloned() {
                        let args = &items[1..];
                        // Collect the function-name argument at each function parameter.
                        let mut func_args: Vec<String> = Vec::new();
                        for &fi in &genfn.func_params {
                            match args.get(fi) {
                                Some(SExpr::Sym(fname, _)) => func_args.push(fname.clone()),
                                _ => {
                                    return Err(self.ctx.error(
                                        format!(
                                            "higher-order argument {} to '{}' must be a function name",
                                            fi + 1,
                                            genfn.name
                                        ),
                                        span,
                                    ));
                                }
                            }
                        }
                        // Infer a binding for each type parameter from the arguments.
                        let mut bindings = self.infer_bindings(&genfn, args, env);
                        // Any parameter still unbound (e.g. one appearing only in the
                        // return type) is taken from the expected type, by unifying the
                        // return type against it.
                        if bindings.len() < genfn.tparams.len()
                            && let Some(exp) = expected.as_deref()
                            && let Some(ee) = type_str_to_expr(exp)
                        {
                            unify_types(&genfn.ret, &ee, &genfn.tparams, &mut bindings);
                            bindings = genfn
                                .tparams
                                .iter()
                                .filter_map(|tp| {
                                    bindings
                                        .iter()
                                        .find(|(k, _)| k == tp)
                                        .map(|(_, c)| (tp.clone(), c.clone()))
                                })
                                .collect();
                        }
                        if bindings.len() < genfn.tparams.len() {
                            return Err(self.ctx.error(
                                format!(
                                    "cannot infer type argument(s) for generic '{}'",
                                    genfn.name
                                ),
                                span,
                            ));
                        }
                        let concretes: Vec<String> =
                            bindings.iter().map(|(_, c)| c.clone()).collect();
                        self.worklist.push(SpecKey {
                            name: genfn.name.clone(),
                            bindings: bindings.clone(),
                            func_args: func_args.clone(),
                        });
                        let mname = template_fn_name(&genfn.name, &func_args, &concretes);
                        let gptypes = param_type_strings(&genfn.params);
                        let mut new_items = Vec::with_capacity(items.len());
                        new_items.push(SExpr::Sym(mname, items[0].span().clone()));
                        for (i, a) in args.iter().enumerate() {
                            // Function arguments are compile-time; drop them from the call.
                            if genfn.func_params.contains(&i) {
                                continue;
                            }
                            // A parameter typed as a bare type parameter is expected at
                            // that parameter's concrete binding.
                            let exp = gptypes.get(i).cloned().flatten().map(|s| {
                                bindings
                                    .iter()
                                    .find(|(tp, _)| *tp == s)
                                    .map(|(_, c)| c.clone())
                                    .unwrap_or(s)
                            });
                            new_items.push(self.walk(a, env, mrc, exp)?);
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // Generic ADT constructor: a case of a generic variant. The
                    // instantiation is inferred from the argument types, falling back
                    // to the expected type for nullary / under-determined cases.
                    if let Some(gen_name) = self.case_to_generic.get(head).cloned() {
                        let gdef = self.generic_variants.get(&gen_name).cloned().unwrap();
                        let args = &items[1..];
                        let payload_pats: Vec<SExpr> = gdef
                            .cases
                            .iter()
                            .find_map(|c| match c {
                                SExpr::List(ci, _) if head_sym(ci) == Some(head) => {
                                    Some(ci[1..].to_vec())
                                }
                                _ => None,
                            })
                            .unwrap_or_default();
                        if args.len() != payload_pats.len() {
                            return Err(self.ctx.error(
                                format!(
                                    "variant case '{}' expects {} argument(s), got {}",
                                    head,
                                    payload_pats.len(),
                                    args.len()
                                ),
                                span,
                            ));
                        }
                        // Infer type-parameter bindings from the argument types.
                        let mut found: Vec<(String, String)> = Vec::new();
                        for (pat, a) in payload_pats.iter().zip(args) {
                            if let Some(cs) = self.infer_type(a, env)
                                && let Some(ce) = type_str_to_expr(&cs)
                            {
                                unify_types(pat, &ce, &gdef.tparams, &mut found);
                            }
                        }
                        // Fallback: an expected type naming this generic supplies any
                        // still-unbound parameter (e.g. `((none) : (opt s32))`).
                        if found.len() < gdef.tparams.len()
                            && let Some(exp) = expected.as_deref()
                            && let Some((g, concretes)) = self.type_instances.get(exp).cloned()
                            && g == gen_name
                        {
                            for (tp, c) in gdef.tparams.iter().zip(concretes) {
                                if !found.iter().any(|(k, _)| k == tp) {
                                    found.push((tp.clone(), c));
                                }
                            }
                        }
                        let mut concretes = Vec::new();
                        for tp in &gdef.tparams {
                            match found.iter().find(|(k, _)| k == tp) {
                                Some((_, c)) => concretes.push(c.clone()),
                                None => {
                                    return Err(self.ctx.error(
                                        format!(
                                            "cannot infer type argument '{}' for constructor '{}'; add a type annotation",
                                            tp, head
                                        ),
                                        span,
                                    ));
                                }
                            }
                        }
                        let mangled_type = self.queue_variant(&gen_name, &concretes);
                        let mangled_case = Self::mangle_case_name(head, &mangled_type);
                        let bindings: Vec<(String, String)> = gdef
                            .tparams
                            .iter()
                            .cloned()
                            .zip(concretes.iter().cloned())
                            .collect();
                        let mut new_items = Vec::with_capacity(items.len());
                        new_items.push(SExpr::Sym(mangled_case, items[0].span().clone()));
                        for (pat, a) in payload_pats.iter().zip(args) {
                            let exp = canonical_type(&subst_types(pat, &bindings));
                            new_items.push(self.walk(a, env, mrc, exp)?);
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // `match` on a generic-variant value: rewrite each arm's case name
                    // to the scrutinee's concrete instantiation and bind the arm
                    // variables at their substituted payload types.
                    if head == "match"
                        && items.len() >= 2
                        && let Some(sty) = self.infer_type(&items[1], env)
                        && let Some((gen_name, concretes)) = self.type_instances.get(&sty).cloned()
                    {
                        let gdef = self.generic_variants.get(&gen_name).cloned().unwrap();
                        let bindings: Vec<(String, String)> = gdef
                            .tparams
                            .iter()
                            .cloned()
                            .zip(concretes.iter().cloned())
                            .collect();
                        let new_scrut = self.walk(&items[1], env, mrc, None)?;
                        let mut new_items = vec![items[0].clone(), new_scrut];
                        for arm in &items[2..] {
                            if let SExpr::List(ai, aspan) = arm
                                && ai.len() == 2
                                && let SExpr::List(pat, pspan) = &ai[0]
                                && let Some(SExpr::Sym(case, cspan)) = pat.first()
                                && self.case_to_generic.get(case) == Some(&gen_name)
                            {
                                let payload_pats: Vec<SExpr> = gdef
                                    .cases
                                    .iter()
                                    .find_map(|c| match c {
                                        SExpr::List(ci, _)
                                            if head_sym(ci) == Some(case.as_str()) =>
                                        {
                                            Some(ci[1..].to_vec())
                                        }
                                        _ => None,
                                    })
                                    .unwrap_or_default();
                                let mut arm_env = env.to_vec();
                                for (bnd, pty) in pat[1..].iter().zip(&payload_pats) {
                                    if let SExpr::Sym(bn, _) = bnd
                                        && let Some(ts) =
                                            canonical_type(&subst_types(pty, &bindings))
                                    {
                                        arm_env.push((bn.clone(), ts));
                                    }
                                }
                                let new_body =
                                    self.walk(&ai[1], &arm_env, mrc, expected.clone())?;
                                let mut new_pat = pat.clone();
                                new_pat[0] =
                                    SExpr::Sym(Self::mangle_case_name(case, &sty), cspan.clone());
                                new_items.push(SExpr::List(
                                    vec![SExpr::List(new_pat, pspan.clone()), new_body],
                                    aspan.clone(),
                                ));
                            } else if let SExpr::List(ai, aspan) = arm
                                && ai.len() == 2
                            {
                                // A non-generic arm (e.g. the `_` wildcard): keep the
                                // pattern, walk only the body.
                                let new_body = self.walk(&ai[1], env, mrc, expected.clone())?;
                                new_items.push(SExpr::List(
                                    vec![ai[0].clone(), new_body],
                                    aspan.clone(),
                                ));
                            } else {
                                new_items.push(self.walk(arm, env, mrc, None)?);
                            }
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // Generic record field access `(Name.field expr)`: rewrite the
                    // accessor to the record expression's concrete instantiation.
                    if let Some((rec, field)) = head.split_once('.')
                        && self.generic_records.contains_key(rec)
                        && items.len() == 2
                    {
                        let expr_ty = self.infer_type(&items[1], env);
                        let new_expr = self.walk(&items[1], env, mrc, None)?;
                        let new_head = match expr_ty {
                            Some(t) if self.record_instances.contains_key(&t) => {
                                format!("{}.{}", t, field)
                            }
                            // Unresolved: leave the generic accessor; parse_program
                            // reports the unknown record type with a source span.
                            _ => head.to_string(),
                        };
                        return Ok(SExpr::List(
                            vec![SExpr::Sym(new_head, items[0].span().clone()), new_expr],
                            span.clone(),
                        ));
                    }

                    // Generic record construction `(Name field...)`: infer the
                    // instantiation from the field argument types (expected-type
                    // fallback for under-determined parameters), then rewrite to the
                    // concrete record constructor.
                    if let Some(gdef) = self.generic_records.get(head).cloned() {
                        let args = &items[1..];
                        let field_pats: Vec<SExpr> = gdef
                            .fields
                            .iter()
                            .filter_map(|f| match f {
                                SExpr::List(fd, _) if fd.len() >= 2 => Some(fd[1].clone()),
                                _ => None,
                            })
                            .collect();
                        if args.len() != field_pats.len() {
                            return Err(self.ctx.error(
                                format!(
                                    "record '{}' expects {} field(s), got {}",
                                    head,
                                    field_pats.len(),
                                    args.len()
                                ),
                                span,
                            ));
                        }
                        let mut found: Vec<(String, String)> = Vec::new();
                        for (pat, a) in field_pats.iter().zip(args) {
                            if let Some(cs) = self.infer_type(a, env)
                                && let Some(ce) = type_str_to_expr(&cs)
                            {
                                unify_types(pat, &ce, &gdef.tparams, &mut found);
                            }
                        }
                        if found.len() < gdef.tparams.len()
                            && let Some(exp) = expected.as_deref()
                            && let Some((g, concretes)) = self.record_instances.get(exp).cloned()
                            && g == *head
                        {
                            for (tp, c) in gdef.tparams.iter().zip(concretes) {
                                if !found.iter().any(|(k, _)| k == tp) {
                                    found.push((tp.clone(), c));
                                }
                            }
                        }
                        let mut concretes = Vec::new();
                        for tp in &gdef.tparams {
                            match found.iter().find(|(k, _)| k == tp) {
                                Some((_, c)) => concretes.push(c.clone()),
                                None => {
                                    return Err(self.ctx.error(
                                        format!(
                                            "cannot infer type argument '{}' for record '{}'; add a type annotation",
                                            tp, head
                                        ),
                                        span,
                                    ));
                                }
                            }
                        }
                        let mangled = self.queue_record(head, &concretes);
                        let bindings: Vec<(String, String)> = gdef
                            .tparams
                            .iter()
                            .cloned()
                            .zip(concretes.iter().cloned())
                            .collect();
                        let mut new_items = Vec::with_capacity(items.len());
                        new_items.push(SExpr::Sym(mangled, items[0].span().clone()));
                        for (pat, a) in field_pats.iter().zip(args) {
                            let exp = canonical_type(&subst_types(pat, &bindings));
                            new_items.push(self.walk(a, env, mrc, exp)?);
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // Forms that carry the expected type into their tail positions.
                    match head {
                        "if" if items.len() == 4 => {
                            let c = self.walk(&items[1], env, mrc, None)?;
                            let t = self.walk(&items[2], env, mrc, expected.clone())?;
                            let f = self.walk(&items[3], env, mrc, expected)?;
                            return Ok(SExpr::List(vec![items[0].clone(), c, t, f], span.clone()));
                        }
                        "let" if items.len() == 3 => {
                            // (let (name value) body) or (let (name : type value) body)
                            if let SExpr::List(bind, bspan) = &items[1] {
                                let (val_idx, val_exp) = if bind.len() == 4 && is_colon(&bind[1]) {
                                    (3, type_expr_string(&bind[2]))
                                } else if bind.len() == 2 {
                                    (1, None)
                                } else {
                                    (usize::MAX, None)
                                };
                                if val_idx != usize::MAX {
                                    let bound_ty = val_exp
                                        .clone()
                                        .or_else(|| self.infer_type(&bind[val_idx], env));
                                    let new_val = self.walk(&bind[val_idx], env, mrc, val_exp)?;
                                    let mut new_bind = bind.clone();
                                    new_bind[val_idx] = new_val;
                                    // The body may reference the bound name; record its type.
                                    let mut body_env = env.to_vec();
                                    if let (Some(SExpr::Sym(n, _)), Some(bt)) =
                                        (bind.first(), bound_ty)
                                    {
                                        body_env.push((n.clone(), bt));
                                    }
                                    let body = self.walk(&items[2], &body_env, mrc, expected)?;
                                    return Ok(SExpr::List(
                                        vec![
                                            items[0].clone(),
                                            SExpr::List(new_bind, bspan.clone()),
                                            body,
                                        ],
                                        span.clone(),
                                    ));
                                }
                            }
                        }
                        s if is_scalar_name(s) && items.len() == 2 => {
                            // scalar cast / ascription `(type expr)`
                            let x = self.walk(&items[1], env, mrc, Some(s.to_string()))?;
                            return Ok(SExpr::List(vec![items[0].clone(), x], span.clone()));
                        }
                        s if lookup_wasm_instr(s).is_some() => {
                            // A raw wasm instruction expects each argument at its
                            // operand type (e.g. `i32.add` wants two `s32`s).
                            let ptypes: Vec<Option<String>> = lookup_wasm_instr(s)
                                .unwrap()
                                .params
                                .iter()
                                .map(scalar_type_name)
                                .collect();
                            let mut new_items = Vec::with_capacity(items.len());
                            new_items.push(items[0].clone());
                            for (i, a) in items[1..].iter().enumerate() {
                                let exp = ptypes.get(i).cloned().flatten();
                                new_items.push(self.walk(a, env, mrc, exp)?);
                            }
                            return Ok(SExpr::List(new_items, span.clone()));
                        }
                        _ => {}
                    }
                }
                // Plain list: recurse into every element (no expected type).
                let mut new_items = Vec::with_capacity(items.len());
                for i in items {
                    new_items.push(self.walk(i, env, mrc, None)?);
                }
                Ok(SExpr::List(new_items, span.clone()))
            }
            // A default integer literal (there is no `s32` suffix, so `ty == S32`
            // is always a default) adopts the expected type: it widens to `s64`, or
            // promotes to a float. An explicit suffix (`s64`, `f32`, `f64`) is left
            // untouched, as is a literal with no expected type.
            SExpr::Int {
                value,
                ty: Type::S32,
                span,
            } if expected.is_some() => Ok(match expected.as_deref().unwrap() {
                "s64" => SExpr::Int {
                    value: *value,
                    ty: Type::S64,
                    span: span.clone(),
                },
                "f32" => SExpr::Float {
                    value: *value as f64,
                    ty: Type::F32,
                    span: span.clone(),
                },
                "f64" => SExpr::Float {
                    value: *value as f64,
                    ty: Type::F64,
                    span: span.clone(),
                },
                _ => e.clone(),
            }),
            other => Ok(other.clone()),
        }
    }

    /// Record that an instance method is used, queuing it for emission once.
    fn mark_instance(&mut self, fname: &str) {
        if self.used_instances.insert(fname.to_string()) {
            self.instance_worklist.push(fname.to_string());
        }
    }

    /// Mangle a generic type instantiation into a concrete nominal name:
    /// `box` @ `[s32]` -> `box$s32`; non-symbol characters in a nested argument
    /// (e.g. the parens of `(list s32)`) become `_`.
    fn mangle_type_name(base: &str, concretes: &[String]) -> String {
        let mut s = base.to_string();
        for c in concretes {
            s.push('$');
            for ch in c.chars() {
                if ch.is_alphanumeric() || ch == '-' {
                    s.push(ch);
                } else {
                    s.push('_');
                }
            }
        }
        s
    }

    /// The per-instantiation case name: `wrap` in `box$s32` -> `wrap$box$s32`.
    /// Unique per instantiation, so the existing by-name constructor/match
    /// resolution needs no change once a generic type is monomorphized.
    fn mangle_case_name(case: &str, type_name: &str) -> String {
        format!("{}${}", case, type_name)
    }

    /// Queue a generic variant instantiation for emission (once) and return its
    /// mangled concrete type name.
    fn queue_variant(&mut self, name: &str, concretes: &[String]) -> String {
        let mangled = Self::mangle_type_name(name, concretes);
        if !self.type_instances.contains_key(&mangled) {
            self.type_instances
                .insert(mangled.clone(), (name.to_string(), concretes.to_vec()));
            self.type_worklist
                .push((name.to_string(), concretes.to_vec()));
        }
        mangled
    }

    /// Queue a generic record instantiation for emission (once) and return its
    /// mangled concrete type name.
    fn queue_record(&mut self, name: &str, concretes: &[String]) -> String {
        let mangled = Self::mangle_type_name(name, concretes);
        if !self.record_instances.contains_key(&mangled) {
            self.record_instances
                .insert(mangled.clone(), (name.to_string(), concretes.to_vec()));
            self.record_worklist
                .push((name.to_string(), concretes.to_vec()));
        }
        mangled
    }

    /// The mangled concrete name of a generic record construction, inferring each
    /// type parameter from the field argument types. Read-only (no queueing) so
    /// `infer_type` can use it. None if `name` is not a generic record or a type
    /// parameter can't be determined.
    fn generic_record_instance(
        &self,
        name: &str,
        args: &[SExpr],
        env: &[(String, String)],
    ) -> Option<String> {
        let gdef = self.generic_records.get(name)?;
        let field_pats: Vec<&SExpr> = gdef
            .fields
            .iter()
            .filter_map(|f| match f {
                SExpr::List(fd, _) if fd.len() >= 2 => Some(&fd[1]),
                _ => None,
            })
            .collect();
        if args.len() != field_pats.len() {
            return None;
        }
        let mut found: Vec<(String, String)> = Vec::new();
        for (pat, a) in field_pats.iter().zip(args) {
            if let Some(cs) = self.infer_type(a, env)
                && let Some(ce) = type_str_to_expr(&cs)
            {
                unify_types(pat, &ce, &gdef.tparams, &mut found);
            }
        }
        let mut concretes = Vec::new();
        for tp in &gdef.tparams {
            concretes.push(
                found
                    .iter()
                    .find(|(k, _)| k == tp)
                    .map(|(_, c)| c.clone())?,
            );
        }
        Some(Self::mangle_type_name(name, &concretes))
    }

    /// Rewrite type-expression occurrences of generic ADTs into their mangled
    /// concrete names, queueing each instantiation. Recurses into builtin
    /// parameterized types (list/option/result/tuple/->) so nested generics like
    /// `(list (box s32))` resolve. Non-generic type exprs pass through unchanged.
    fn rewrite_type_expr(&mut self, e: &SExpr) -> SExpr {
        match e {
            SExpr::List(items, span) if !items.is_empty() => {
                let rewritten: Vec<SExpr> =
                    items.iter().map(|i| self.rewrite_type_expr(i)).collect();
                if let Some(head) = head_sym(items) {
                    let concretes: Vec<String> =
                        rewritten[1..].iter().filter_map(canonical_type).collect();
                    let arity = concretes.len();
                    if let Some(gdef) = self.generic_variants.get(head)
                        && arity == gdef.tparams.len()
                    {
                        let mangled = self.queue_variant(head, &concretes);
                        return SExpr::Sym(mangled, span.clone());
                    }
                    if let Some(gdef) = self.generic_records.get(head)
                        && arity == gdef.tparams.len()
                    {
                        let mangled = self.queue_record(head, &concretes);
                        return SExpr::Sym(mangled, span.clone());
                    }
                }
                SExpr::List(rewritten, span.clone())
            }
            other => other.clone(),
        }
    }

    /// Rewrite the type positions of one parameter (`(name type)` or `(name : type)`).
    fn rewrite_param(&mut self, p: &SExpr) -> SExpr {
        if let SExpr::List(pp, pspan) = p {
            if pp.len() == 2 {
                return SExpr::List(
                    vec![pp[0].clone(), self.rewrite_type_expr(&pp[1])],
                    pspan.clone(),
                );
            } else if pp.len() == 3 && is_colon(&pp[1]) {
                return SExpr::List(
                    vec![pp[0].clone(), pp[1].clone(), self.rewrite_type_expr(&pp[2])],
                    pspan.clone(),
                );
            }
        }
        p.clone()
    }

    /// Rewrite generic ADT references in a function signature (parameter types and
    /// return type), leaving the body for `walk`. Returns the new `fn` items.
    fn rewrite_fn_signature(&mut self, items: &[SExpr]) -> Vec<SExpr> {
        let mut out = items.to_vec();
        if let Some(SExpr::List(ps, pspan)) = out.get(2).cloned() {
            let new_ps: Vec<SExpr> = ps.iter().map(|p| self.rewrite_param(p)).collect();
            out[2] = SExpr::List(new_ps, pspan);
        }
        // The return type is at index 3, or 4 when a `:` precedes it.
        let ret_idx = if out.get(3).is_some_and(is_colon) {
            4
        } else {
            3
        };
        if let Some(r) = out.get(ret_idx).cloned() {
            out[ret_idx] = self.rewrite_type_expr(&r);
        }
        out
    }

    /// Emit a concrete variant form for one generic instantiation: substitute the
    /// type parameters, mangle each case name, and rewrite nested generic payloads
    /// (queueing them in turn).
    fn emit_variant_instance(&mut self, gen_name: &str, concretes: &[String]) -> SExpr {
        let gdef = self.generic_variants.get(gen_name).cloned().unwrap();
        let mangled = Self::mangle_type_name(gen_name, concretes);
        let span = gdef.span.clone();
        let bindings: Vec<(String, String)> = gdef
            .tparams
            .iter()
            .cloned()
            .zip(concretes.iter().cloned())
            .collect();
        let mut out = vec![
            SExpr::Sym("variant".to_string(), span.clone()),
            SExpr::Sym(mangled.clone(), span.clone()),
        ];
        for case in &gdef.cases {
            if let SExpr::List(ci, cspan) = case
                && let Some(SExpr::Sym(cn, cnspan)) = ci.first()
            {
                let mut new_ci = vec![SExpr::Sym(
                    Self::mangle_case_name(cn, &mangled),
                    cnspan.clone(),
                )];
                for pty in &ci[1..] {
                    let subst = subst_types(pty, &bindings);
                    new_ci.push(self.rewrite_type_expr(&subst));
                }
                out.push(SExpr::List(new_ci, cspan.clone()));
            }
        }
        SExpr::List(out, span)
    }

    /// Emit a concrete record form for one generic instantiation: substitute the
    /// type parameters into each field type (rewriting nested generics in turn).
    /// Field names are kept; the record name carries the instantiation.
    fn emit_record_instance(&mut self, gen_name: &str, concretes: &[String]) -> SExpr {
        let gdef = self.generic_records.get(gen_name).cloned().unwrap();
        let mangled = Self::mangle_type_name(gen_name, concretes);
        let span = gdef.span.clone();
        let bindings: Vec<(String, String)> = gdef
            .tparams
            .iter()
            .cloned()
            .zip(concretes.iter().cloned())
            .collect();
        let mut out = vec![
            SExpr::Sym("record".to_string(), span.clone()),
            SExpr::Sym(mangled, span.clone()),
        ];
        for field in &gdef.fields {
            if let SExpr::List(fd, fspan) = field
                && fd.len() >= 2
                && let SExpr::Sym(..) = &fd[0]
            {
                let subst = subst_types(&fd[1], &bindings);
                out.push(SExpr::List(
                    vec![fd[0].clone(), self.rewrite_type_expr(&subst)],
                    fspan.clone(),
                ));
            }
        }
        SExpr::List(out, span)
    }

    /// Produce the specialized `fn` form for one monomorphization request:
    /// the type parameter is substituted, each function parameter's name is replaced
    /// by its function argument (and the parameter is dropped from the signature),
    /// and generic/trait calls in the body are then resolved.
    fn specialize(&mut self, key: &SpecKey) -> Result<SExpr> {
        let genfn = self
            .generics
            .get(&key.name)
            .cloned()
            .expect("generic exists");
        let concretes: Vec<String> = key.bindings.iter().map(|(_, c)| c.clone()).collect();
        let mname = template_fn_name(&key.name, &key.func_args, &concretes);

        // 1. Substitute the type parameters throughout params, return, and body. Type
        //    positions like `(list-new T)` become concrete; safe because a type
        //    parameter is uppercase by convention and never names a value.
        let mut params = subst_types(&genfn.params, &key.bindings);
        let ret = subst_types(&genfn.ret, &key.bindings);
        let mut body = subst_types(&genfn.body, &key.bindings);

        // 2. Substitute each function parameter's name with its function argument, so
        //    `(f x)` becomes `(double x)`. Reuses `subst_type` (a bare symbol swap).
        for (fp_idx, fname) in genfn.func_params.iter().zip(&key.func_args) {
            if let Some(pname) = param_name_at(&genfn.params, *fp_idx) {
                body = subst_type(&body, &pname, fname);
            }
        }

        // 3. Drop the function parameters from the signature — they are compile-time.
        if !genfn.func_params.is_empty()
            && let SExpr::List(pitems, pspan) = &params
        {
            let kept: Vec<SExpr> = pitems
                .iter()
                .enumerate()
                .filter(|(i, _)| !genfn.func_params.contains(i))
                .map(|(_, p)| p.clone())
                .collect();
            params = SExpr::List(kept, pspan.clone());
        }

        let env = param_env(&params);
        self.fn_params
            .insert(mname.clone(), param_type_strings(&params));
        let mc = MethodCtx {
            constraints: genfn.constraints.clone(),
            bindings: key.bindings.clone(),
        };
        // The body is in return position, so it is expected at the return type.
        let ret_exp = canonical_type(&ret);
        let body = self.walk(&body, &env, Some(&mc), ret_exp)?;
        let span = genfn.body.span().clone();
        Ok(SExpr::List(
            vec![
                SExpr::Sym("fn".to_string(), span.clone()),
                SExpr::Sym(mname, span.clone()),
                params,
                ret,
                body,
            ],
            span,
        ))
    }

    /// Rewrite the body of a retained `fn` form (generic calls -> specialized names).
    /// The body is always the last element, whatever the annotation shape.
    fn process_fn_form(&mut self, items: &[SExpr], span: &Span) -> Result<SExpr> {
        // Rewrite generic ADT references in the signature first, so the body's env
        // and expected return type see the mangled concrete names.
        let items = self.rewrite_fn_signature(items);
        let shape = fn_shape(&items)
            .ok_or_else(|| self.ctx.error("malformed function definition", span))?;
        let env = param_env(shape.params);
        // The body is in return position, so it is expected at the return type
        // (ignoring any multiplicity qualifier, which the checker handles).
        let ret_exp = canonical_type(unwrap_mult(shape.ret));
        let new_body = self.walk(shape.body, &env, None, ret_exp)?;
        let mut new_items = items.clone();
        if let Some(last) = new_items.last_mut() {
            *last = new_body;
        }
        Ok(SExpr::List(new_items, span.clone()))
    }

    /// Rewrite any `fn` bodies reachable from a retained top-level form.
    fn process_form(&mut self, form: &SExpr) -> Result<SExpr> {
        if let SExpr::List(items, span) = form {
            match head_sym(items) {
                Some("fn") => return self.process_fn_form(items, span),
                Some("export") => {
                    // Rewrite an inner (fn ...) if present, leaving the wrapper shape intact.
                    let mut new_items = Vec::with_capacity(items.len());
                    for it in items {
                        match it {
                            SExpr::List(inner, ispan) if head_sym(inner) == Some("fn") => {
                                new_items.push(self.process_fn_form(inner, ispan)?);
                            }
                            other => new_items.push(other.clone()),
                        }
                    }
                    return Ok(SExpr::List(new_items, span.clone()));
                }
                _ => {}
            }
        }
        Ok(form.clone())
    }
}

/// Lower traits/instances/generics to plain monomorphic forms.
/// The primitive equality instruction for a scalar type, or None if not scalar.
pub(crate) fn scalar_eq_instr(ty: &str) -> Option<&'static str> {
    Some(match ty {
        "s32" | "u8" => "i32.eq",
        "s64" => "i64.eq",
        "f32" => "f32.eq",
        "f64" => "f64.eq",
        _ => return None,
    })
}

/// Generate an `(instance (Eq Type) ...)` that compares a record field by field.
pub(crate) fn derive_eq_record(
    trait_name: &str,
    type_name: &str,
    fields: &[(String, String)],
    span: &Span,
    ctx: &CompileContext,
) -> Result<SExpr> {
    let sym = |s: &str| SExpr::Sym(s.to_string(), span.clone());
    let list = |v: Vec<SExpr>| SExpr::List(v, span.clone());

    // One comparison per field: (<eq> (Type.field a) (Type.field b)).
    let mut cmps = Vec::new();
    for (fname, fty) in fields {
        let eq = scalar_eq_instr(fty).ok_or_else(|| {
            ctx.error(
                format!(
                    "cannot derive Eq for '{}': field '{}' has non-scalar type '{}'",
                    type_name, fname, fty
                ),
                span,
            )
        })?;
        let accessor = format!("{}.{}", type_name, fname);
        cmps.push(list(vec![
            sym(eq),
            list(vec![sym(&accessor), sym("a")]),
            list(vec![sym(&accessor), sym("b")]),
        ]));
    }
    // Combine with i32.and (an empty record is always equal).
    let body = cmps
        .into_iter()
        .reduce(|acc, c| list(vec![sym("i32.and"), acc, c]))
        .unwrap_or_else(|| SExpr::Int {
            value: 1,
            ty: Type::S32,
            span: span.clone(),
        });

    let param = |n: &str| list(vec![sym(n), sym(":"), sym(type_name)]);
    let method = list(vec![
        sym("fn"),
        sym("="),
        list(vec![param("a"), param("b")]),
        sym(":"),
        sym("s32"),
        body,
    ]);
    Ok(list(vec![
        sym("instance"),
        list(vec![sym(trait_name), sym(type_name)]),
        method,
    ]))
}

/// Generate an `(instance (Eq Variant) ...)` that compares two variant values:
/// equal only when they share a case and that case's (scalar) payloads are equal.
/// Built as a nested match — outer on `a`, inner on `b` with a `_` fallback to 0.
pub(crate) fn derive_eq_variant(
    trait_name: &str,
    type_name: &str,
    cases: &[(String, Vec<String>)],
    span: &Span,
    ctx: &CompileContext,
) -> Result<SExpr> {
    let sym = |s: &str| SExpr::Sym(s.to_string(), span.clone());
    let list = |v: Vec<SExpr>| SExpr::List(v, span.clone());
    let int = |v: i64| SExpr::Int {
        value: v,
        ty: Type::S32,
        span: span.clone(),
    };

    let mut outer_arms = Vec::new();
    for (case, payloads) in cases {
        let a_binds: Vec<String> = (0..payloads.len()).map(|i| format!("a_{i}")).collect();
        let b_binds: Vec<String> = (0..payloads.len()).map(|i| format!("b_{i}")).collect();
        // Compare payloads pairwise (scalar equality, matching the record path).
        let mut cmps = Vec::new();
        for (i, pty) in payloads.iter().enumerate() {
            let eq = scalar_eq_instr(pty).ok_or_else(|| {
                ctx.error(
                    format!(
                        "cannot derive Eq for '{}': case '{}' has non-scalar payload '{}'",
                        type_name, case, pty
                    ),
                    span,
                )
            })?;
            cmps.push(list(vec![sym(eq), sym(&a_binds[i]), sym(&b_binds[i])]));
        }
        let eq_body = cmps
            .into_iter()
            .reduce(|acc, c| list(vec![sym("i32.and"), acc, c]))
            .unwrap_or_else(|| int(1));
        // Inner match on b: the same case compares payloads; anything else is 0.
        let inner_pat = {
            let mut p = vec![sym(case)];
            p.extend(b_binds.iter().map(|n| sym(n)));
            list(p)
        };
        let inner = list(vec![
            sym("match"),
            sym("b"),
            list(vec![inner_pat, eq_body]),
            list(vec![list(vec![sym("_")]), int(0)]),
        ]);
        let outer_pat = {
            let mut p = vec![sym(case)];
            p.extend(a_binds.iter().map(|n| sym(n)));
            list(p)
        };
        outer_arms.push(list(vec![outer_pat, inner]));
    }
    let mut match_expr = vec![sym("match"), sym("a")];
    match_expr.extend(outer_arms);
    let body = list(match_expr);

    let param = |n: &str| list(vec![sym(n), sym(":"), sym(type_name)]);
    let method = list(vec![
        sym("fn"),
        sym("="),
        list(vec![param("a"), param("b")]),
        sym(":"),
        sym("s32"),
        body,
    ]);
    Ok(list(vec![
        sym("instance"),
        list(vec![sym(trait_name), sym(type_name)]),
        method,
    ]))
}

/// Compile-time deriving: `(derive Trait Type)` inspects Type's definition and emits a
/// trait instance. Runs after macro expansion and before the generics pre-pass, so the
/// generated instance flows through the normal trait pipeline. This is the first
/// "type-aware macro": it reflects on a type's structure to generate code.
pub(crate) fn expand_derives(forms: Vec<SExpr>, ctx: &CompileContext) -> Result<Vec<SExpr>> {
    // Collect record shapes: name -> [(field, canonical type)].
    let mut records: HashMap<String, Vec<(String, String)>> = HashMap::new();
    for form in &forms {
        if let SExpr::List(items, _) = form
            && head_sym(items) == Some("record")
            && let Some(SExpr::Sym(name, _)) = items.get(1)
        {
            let mut fields = Vec::new();
            for f in &items[2..] {
                if let SExpr::List(fd, _) = f
                    && let Some(SExpr::Sym(fname, _)) = fd.first()
                    && let Some(fty) = fd.get(1).and_then(canonical_type)
                {
                    fields.push((fname.clone(), fty));
                }
            }
            records.insert(name.clone(), fields);
        }
    }

    // Collect (bare-named) variant shapes: name -> [(case, [payload canonical types])].
    let mut variants: HashMap<String, Vec<(String, Vec<String>)>> = HashMap::new();
    for form in &forms {
        if let SExpr::List(items, _) = form
            && head_sym(items) == Some("variant")
            && let Some(SExpr::Sym(name, _)) = items.get(1)
        {
            let mut cases = Vec::new();
            for c in &items[2..] {
                if let SExpr::List(ci, _) = c
                    && let Some(SExpr::Sym(cname, _)) = ci.first()
                {
                    let payloads: Vec<String> = ci[1..].iter().filter_map(canonical_type).collect();
                    cases.push((cname.clone(), payloads));
                }
            }
            variants.insert(name.clone(), cases);
        }
    }

    let mut out = Vec::new();
    for form in forms {
        let is_derive = matches!(&form, SExpr::List(items, _) if head_sym(items) == Some("derive"));
        if !is_derive {
            out.push(form);
            continue;
        }
        let (items, span) = match &form {
            SExpr::List(i, s) => (i, s),
            _ => unreachable!(),
        };
        let trait_name = match items.get(1) {
            Some(SExpr::Sym(s, _)) => s.clone(),
            _ => return Err(ctx.error("derive expects (derive Trait Type)", span)),
        };
        let type_name = match items.get(2) {
            Some(SExpr::Sym(s, _)) => s.clone(),
            _ => return Err(ctx.error("derive expects (derive Trait Type)", span)),
        };
        match trait_name.as_str() {
            "Eq" => {
                if let Some(fields) = records.get(&type_name) {
                    out.push(derive_eq_record(
                        &trait_name,
                        &type_name,
                        fields,
                        span,
                        ctx,
                    )?);
                } else if let Some(cases) = variants.get(&type_name) {
                    out.push(derive_eq_variant(
                        &trait_name,
                        &type_name,
                        cases,
                        span,
                        ctx,
                    )?);
                } else {
                    return Err(ctx.error(
                        format!(
                            "cannot derive Eq for '{}': not a record or variant",
                            type_name
                        ),
                        span,
                    ));
                }
            }
            other => {
                return Err(ctx.error(format!("cannot derive '{}' (supported: Eq)", other), span));
            }
        }
    }
    Ok(out)
}

pub(crate) fn expand_generics(forms: Vec<SExpr>, ctx: &CompileContext) -> Result<Vec<SExpr>> {
    let mut low = Lowering {
        ctx,
        generics: HashMap::new(),
        traits: HashMap::new(),
        method_to_trait: HashMap::new(),
        monofn_returns: HashMap::new(),
        fn_params: HashMap::new(),
        instances: HashMap::new(),
        instance_defs: HashMap::new(),
        used_instances: HashSet::new(),
        instance_worklist: Vec::new(),
        worklist: Vec::new(),
        emitted: HashSet::new(),
        generic_variants: HashMap::new(),
        case_to_generic: HashMap::new(),
        type_instances: HashMap::new(),
        type_worklist: Vec::new(),
        generic_records: HashMap::new(),
        record_instances: HashMap::new(),
        record_worklist: Vec::new(),
    };

    let mut retained: Vec<SExpr> = Vec::new();

    // Pass 0: collect trait declarations (name, type parameter, method signatures)
    // so instances and `where` clauses can be checked regardless of source order.
    let mut traits: HashMap<String, TraitDef> = HashMap::new();
    let mut seen_instances: HashSet<(String, String)> = HashSet::new();
    for form in &forms {
        let (items, span) = match form {
            SExpr::List(items, span) if !items.is_empty() => (items, span),
            _ => continue,
        };
        if head_sym(items) != Some("trait") {
            continue;
        }
        // (trait (Name T U ...) methods...)
        let (name, tparams) = match items.get(1) {
            Some(SExpr::List(head, _)) if head.len() >= 2 => {
                let name = match &head[0] {
                    SExpr::Sym(n, _) => n.clone(),
                    _ => return Err(ctx.error("trait name must be a symbol", span)),
                };
                let mut tparams = Vec::new();
                for t in &head[1..] {
                    match t {
                        SExpr::Sym(tp, _) => tparams.push(tp.clone()),
                        _ => return Err(ctx.error("trait type parameter must be a symbol", span)),
                    }
                }
                (name, tparams)
            }
            _ => return Err(ctx.error("trait expects (trait (Name T ...) methods...)", span)),
        };
        let mut methods = Vec::new();
        for m in &items[2..] {
            let mi = match m {
                SExpr::List(mi, _) if head_sym(mi) == Some("fn") => mi,
                _ => return Err(ctx.error("trait method must be (fn name params : ret)", span)),
            };
            let sig = parse_method_sig(mi)
                .ok_or_else(|| ctx.error("malformed trait method signature", span))?;
            low.method_to_trait.insert(sig.name.clone(), name.clone());
            methods.push(sig);
        }
        if traits
            .insert(name.clone(), TraitDef { tparams, methods })
            .is_some()
        {
            return Err(ctx.error(format!("duplicate trait '{}'", name), span));
        }
    }
    // Make the trait declarations available during body rewriting (Pass 2/3).
    low.traits = traits.clone();

    // Pass 0.5: collect generic variant templates `(variant (Name T ...) ...)`, so
    // their constructors/matches resolve regardless of source order (Pass 1/2).
    for form in &forms {
        let (items, span) = match form {
            SExpr::List(items, span) if !items.is_empty() => (items, span),
            _ => continue,
        };
        if head_sym(items) != Some("variant") {
            continue;
        }
        let head = match items.get(1) {
            Some(SExpr::List(head, _)) if !head.is_empty() => head,
            _ => continue, // concrete variant (bare-symbol name): handled in Pass 1
        };
        let name = match &head[0] {
            SExpr::Sym(n, _) => n.clone(),
            _ => return Err(ctx.error("variant name must be a symbol", span)),
        };
        let mut tparams = Vec::new();
        for t in &head[1..] {
            match t {
                SExpr::Sym(tp, _) => tparams.push(tp.clone()),
                _ => return Err(ctx.error("variant type parameter must be a symbol", span)),
            }
        }
        if tparams.is_empty() {
            return Err(ctx.error("generic variant needs at least one type parameter", span));
        }
        let cases: Vec<SExpr> = items[2..].to_vec();
        for c in &cases {
            if let SExpr::List(ci, _) = c
                && let Some(SExpr::Sym(cn, _)) = ci.first()
                && let Some(prev) = low.case_to_generic.insert(cn.clone(), name.clone())
                && prev != name
            {
                return Err(ctx.error(
                    format!(
                        "variant case '{}' is declared by both '{}' and '{}'",
                        cn, prev, name
                    ),
                    span,
                ));
            }
        }
        if low
            .generic_variants
            .insert(
                name.clone(),
                GenericTypeDef {
                    tparams,
                    cases,
                    span: span.clone(),
                },
            )
            .is_some()
        {
            return Err(ctx.error(format!("duplicate generic variant '{}'", name), span));
        }
    }

    // Pass 0.6: collect generic record templates `(record (Name T ...) ...)`.
    for form in &forms {
        let (items, span) = match form {
            SExpr::List(items, span) if !items.is_empty() => (items, span),
            _ => continue,
        };
        if head_sym(items) != Some("record") {
            continue;
        }
        let head = match items.get(1) {
            Some(SExpr::List(head, _)) if !head.is_empty() => head,
            _ => continue, // concrete record (bare-symbol name): handled in Pass 1
        };
        let name = match &head[0] {
            SExpr::Sym(n, _) => n.clone(),
            _ => return Err(ctx.error("record name must be a symbol", span)),
        };
        let mut tparams = Vec::new();
        for t in &head[1..] {
            match t {
                SExpr::Sym(tp, _) => tparams.push(tp.clone()),
                _ => return Err(ctx.error("record type parameter must be a symbol", span)),
            }
        }
        if tparams.is_empty() {
            return Err(ctx.error("generic record needs at least one type parameter", span));
        }
        let fields: Vec<SExpr> = items[2..].to_vec();
        if low
            .generic_records
            .insert(
                name.clone(),
                GenericRecordDef {
                    tparams,
                    fields,
                    span: span.clone(),
                },
            )
            .is_some()
        {
            return Err(ctx.error(format!("duplicate generic record '{}'", name), span));
        }
    }

    // Pass 1: classify every top-level form.
    for form in &forms {
        let (items, span) = match form {
            SExpr::List(items, span) if !items.is_empty() => (items, span),
            _ => {
                retained.push(form.clone());
                continue;
            }
        };
        match head_sym(items) {
            Some("trait") => {} // collected in Pass 0
            Some("instance") => {
                // (instance (Trait Type ...) (fn method params [:] ret body) ...)
                let (trait_name, types) = match items.get(1) {
                    Some(SExpr::List(head, _)) if head.len() >= 2 => {
                        let tn = match &head[0] {
                            SExpr::Sym(s, _) => s.clone(),
                            _ => return Err(ctx.error("instance trait must be a symbol", span)),
                        };
                        let mut types = Vec::new();
                        for t in &head[1..] {
                            types.push(canonical_type(t).ok_or_else(|| {
                                ctx.error("instance type must be a type name", span)
                            })?);
                        }
                        (tn, types)
                    }
                    _ => {
                        return Err(
                            ctx.error("instance expects (instance (Trait Type ...) ...)", span)
                        );
                    }
                };
                let trait_def = traits.get(&trait_name).ok_or_else(|| {
                    ctx.error(format!("unknown trait '{}' in instance", trait_name), span)
                })?;
                if trait_def.tparams.len() != types.len() {
                    return Err(ctx.error(
                        format!(
                            "instance of '{}' has {} type argument(s) but the trait declares {}",
                            trait_name,
                            types.len(),
                            trait_def.tparams.len()
                        ),
                        span,
                    ));
                }
                // (type parameter -> instance type) bindings for substitution.
                let tbindings: Vec<(String, String)> = trait_def
                    .tparams
                    .iter()
                    .cloned()
                    .zip(types.iter().cloned())
                    .collect();
                if !seen_instances.insert((trait_name.clone(), instance_key(&types))) {
                    return Err(ctx.error(
                        format!(
                            "duplicate instance for ({} {})",
                            trait_name,
                            types.join(" ")
                        ),
                        span,
                    ));
                }
                let mut provided: HashSet<String> = HashSet::new();
                for m in &items[2..] {
                    let mi = match m {
                        SExpr::List(mi, _) if head_sym(mi) == Some("fn") => mi,
                        _ => return Err(ctx.error("instance method must be (fn ...)", span)),
                    };
                    let shape =
                        fn_shape(mi).ok_or_else(|| ctx.error("malformed instance method", span))?;
                    let method = match shape.name {
                        SExpr::Sym(s, _) => s.clone(),
                        _ => return Err(ctx.error("method name must be a symbol", span)),
                    };
                    // The method must be declared by the trait.
                    let tsig = trait_def
                        .methods
                        .iter()
                        .find(|s| s.name == method)
                        .ok_or_else(|| {
                            ctx.error(
                                format!("trait '{}' has no method '{}'", trait_name, method),
                                span,
                            )
                        })?;
                    // Its signature must match the trait's, with the type parameters
                    // substituted by this instance's types.
                    let want_params = subst_types(&tsig.params, &tbindings);
                    let want_ret = subst_types(&tsig.ret, &tbindings);
                    let want_pt = param_type_exprs(&want_params);
                    let got_pt = param_type_exprs(shape.params);
                    if want_pt.len() != got_pt.len()
                        || !want_pt.iter().zip(&got_pt).all(|(a, b)| type_expr_eq(a, b))
                    {
                        return Err(ctx.error(
                            format!(
                                "instance ({} {}) method '{}' has parameter types that do not match trait '{}'",
                                trait_name, types.join(" "), method, trait_name
                            ),
                            span,
                        ));
                    }
                    if !type_expr_eq(&want_ret, shape.ret) {
                        return Err(ctx.error(
                            format!(
                                "instance ({} {}) method '{}' has a return type that does not match trait '{}'",
                                trait_name, types.join(" "), method, trait_name
                            ),
                            span,
                        ));
                    }
                    if !provided.insert(method.clone()) {
                        return Err(ctx.error(
                            format!(
                                "instance ({} {}) defines method '{}' more than once",
                                trait_name,
                                types.join(" "),
                                method
                            ),
                            span,
                        ));
                    }
                    let fname = instance_fn_name(&trait_name, &types, &method);
                    // Emit the instance method as a plain concrete fn (same shape, renamed).
                    if let Some(rt) = canonical_type(shape.ret) {
                        low.monofn_returns.insert(fname.clone(), rt);
                    }
                    low.fn_params
                        .insert(fname.clone(), param_type_strings(shape.params));
                    let mut new_mi = mi.clone();
                    new_mi[1] = SExpr::Sym(fname.clone(), mi[1].span().clone());
                    // Store the instance fn; it is emitted only if it is referenced.
                    low.instance_defs
                        .insert(fname.clone(), SExpr::List(new_mi, span.clone()));
                    low.instances
                        .entry((trait_name.clone(), instance_key(&types)))
                        .or_default()
                        .insert(method, fname);
                }
                // Every trait method must be implemented.
                for s in &trait_def.methods {
                    if !provided.contains(&s.name) {
                        return Err(ctx.error(
                            format!(
                                "instance ({} {}) is missing method '{}'",
                                trait_name,
                                types.join(" "),
                                s.name
                            ),
                            span,
                        ));
                    }
                }
            }
            Some("fn") => {
                let shape = fn_shape(items)
                    .ok_or_else(|| ctx.error("malformed function definition", span))?;
                let name = match shape.name {
                    SExpr::Sym(s, _) => s.clone(),
                    _ => return Err(ctx.error("function name must be a symbol", span)),
                };
                // A function is a template (specialized on demand) if it has a `where`
                // clause (a type parameter) and/or a function-typed parameter.
                let func_params = func_param_indices(shape.params);
                let is_template = shape.where_clause.is_some() || !func_params.is_empty();
                if is_template {
                    let mut constraints: Vec<(String, String)> = Vec::new();
                    let mut tparams: Vec<String> = Vec::new();
                    let note_tparam = |tp: &str, list: &mut Vec<String>| {
                        if !list.iter().any(|t| t == tp) {
                            list.push(tp.to_string());
                        }
                    };
                    if let Some(SExpr::List(where_items, _)) = shape.where_clause {
                        for c in &where_items[1..] {
                            // A bare type parameter with no constraint: (where T).
                            if let SExpr::Sym(tp, _) = c {
                                note_tparam(tp, &mut tparams);
                            } else if let SExpr::List(cc, _) = c
                                && cc.len() >= 2
                                && let SExpr::Sym(tn, _) = &cc[0]
                                && cc[1..].iter().all(|x| matches!(x, SExpr::Sym(..)))
                            {
                                // A trait bound: (Trait T ...). All named type
                                // parameters are declared; the constraint is recorded
                                // against the first (used for single-parameter dispatch).
                                for x in &cc[1..] {
                                    if let SExpr::Sym(tp, _) = x {
                                        note_tparam(tp, &mut tparams);
                                    }
                                }
                                if let SExpr::Sym(tp, _) = &cc[1] {
                                    constraints.push((tn.clone(), tp.clone()));
                                }
                            } else {
                                return Err(ctx.error(
                                    "where clause entry must be a type parameter or (Trait TypeParam ...)",
                                    span,
                                ));
                            }
                        }
                        if tparams.is_empty() {
                            return Err(ctx.error(
                                "generic fn needs a type parameter in its where clause",
                                span,
                            ));
                        }
                    }
                    for (tn, _) in &constraints {
                        if !traits.contains_key(tn) {
                            return Err(
                                ctx.error(format!("unknown trait '{}' in where clause", tn), span)
                            );
                        }
                    }
                    low.generics.insert(
                        name.clone(),
                        GenericFnDef {
                            name,
                            tparams,
                            constraints,
                            func_params,
                            params: shape.params.clone(),
                            ret: shape.ret.clone(),
                            body: shape.body.clone(),
                        },
                    );
                } else {
                    low.fn_params
                        .insert(name.clone(), param_type_strings(shape.params));
                    if let Some(rt) = canonical_type(shape.ret) {
                        low.monofn_returns.insert(name, rt);
                    }
                    retained.push(form.clone());
                }
            }
            Some("export") => {
                // Record the signature of an inner (fn ...) so calls to it propagate
                // types, then retain the export unchanged for Pass 2.
                for it in &items[1..] {
                    if let SExpr::List(inner, _) = it
                        && head_sym(inner) == Some("fn")
                        && let Some(shape) = fn_shape(inner)
                        && let SExpr::Sym(n, _) = shape.name
                    {
                        low.fn_params
                            .insert(n.clone(), param_type_strings(shape.params));
                        if let Some(rt) = canonical_type(shape.ret) {
                            low.monofn_returns.insert(n.clone(), rt);
                        }
                    }
                }
                retained.push(form.clone());
            }
            Some("variant") => {
                // A generic variant template (list name) is collected in Pass 0.5 and
                // emitted per instantiation; a concrete variant is retained as-is.
                if !matches!(items.get(1), Some(SExpr::List(..))) {
                    retained.push(form.clone());
                }
            }
            Some("record") => {
                // Likewise: a generic record template is emitted per instantiation; a
                // concrete record is retained as-is.
                if !matches!(items.get(1), Some(SExpr::List(..))) {
                    retained.push(form.clone());
                }
            }
            _ => retained.push(form.clone()),
        }
    }

    // Pass 2: rewrite retained forms. This marks the instances they use and seeds
    // the generic worklist.
    let mut output: Vec<SExpr> = Vec::new();
    for form in &retained {
        let rewritten = low.process_form(form)?;
        output.push(rewritten);
    }

    // Pass 3: drain both worklists, emitting one copy per specialized generic and
    // one copy per referenced instance. Each emitted body may reference further
    // generics or instances, so we loop until both are empty. Unused instances
    // (e.g. most of an included stdlib) are never emitted.
    loop {
        if let Some(key) = low.worklist.pop() {
            let concretes: Vec<String> = key.bindings.iter().map(|(_, c)| c.clone()).collect();
            let mname = template_fn_name(&key.name, &key.func_args, &concretes);
            if low.emitted.insert(mname) {
                let fn_form = low.specialize(&key)?;
                output.push(fn_form);
            }
        } else if let Some(fname) = low.instance_worklist.pop() {
            if let Some(SExpr::List(items, span)) = low.instance_defs.get(&fname).cloned() {
                let rewritten = low.process_fn_form(&items, &span)?;
                output.push(rewritten);
            }
        } else if let Some((gen_name, concretes)) = low.type_worklist.pop() {
            // Emit a concrete variant per generic instantiation. Emission may rewrite
            // nested generic payloads, queueing further instantiations.
            let form = low.emit_variant_instance(&gen_name, &concretes);
            output.push(form);
        } else if let Some((gen_name, concretes)) = low.record_worklist.pop() {
            // Emit a concrete record per generic instantiation.
            let form = low.emit_record_instance(&gen_name, &concretes);
            output.push(form);
        } else {
            break;
        }
    }

    Ok(output)
}
