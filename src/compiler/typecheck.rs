use super::*;

// Type checking + the substructural (linearity) checker.
pub(crate) fn type_check(
    prog: &Program,
    signatures: &HashMap<String, Signature>,
    _ctx: &CompileContext,
) -> Result<()> {
    // Build global type map
    let mut globals_map = HashMap::new();
    for global in &prog.globals {
        globals_map.insert(global.name.clone(), (global.ty.clone(), global.mutable));
    }

    // Build records and variants maps for type checking
    let records_map: HashMap<String, RecordDef> = prog
        .records
        .iter()
        .map(|r| (r.name.clone(), r.clone()))
        .collect();
    let variants_map: HashMap<String, VariantDef> = prog
        .variants
        .iter()
        .map(|v| (v.name.clone(), v.clone()))
        .collect();

    // A variant is linear (or affine) by its type if any payload slot is — so a
    // value holding a linear resource cannot be copied and re-matched. `Lin`
    // dominates `Aff` dominates `Un`.
    let variant_mult: HashMap<String, Multiplicity> = prog
        .variants
        .iter()
        .map(|v| (v.name.clone(), variant_multiplicity(v)))
        .collect();

    for func in &prog.functions {
        let mut env = HashMap::new();
        for param in &func.params {
            env.insert(param.name.clone(), param.ty.clone());
        }
        let body_ty = check_expr(
            &func.body,
            &env,
            signatures,
            &globals_map,
            &records_map,
            &variants_map,
        )?;
        if body_ty != func.return_type {
            bail!(
                "function '{}' returns {:?} but body has type {:?}",
                func.name,
                func.return_type,
                body_ty
            );
        }
        // A borrow is transient and tied to its source; it must not escape by being
        // returned (that would outlive the borrowed capability).
        if matches!(func.return_type, Type::Borrow(_)) {
            bail!("function '{}' cannot return a borrow", func.name);
        }
        check_fn_linearity(func, &prog.capabilities, &variant_mult)?;
    }
    Ok(())
}

/// The direct sub-expressions of an expression. Exhaustive so that adding a new
/// `Expr` variant forces a decision here (the linearity use-count must see every
/// value position, or it would undercount and admit a double-use).
pub(crate) fn expr_children(e: &Expr) -> Vec<&Expr> {
    match e {
        Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::StringLiteral(_)
        | Expr::Var(_)
        | Expr::GlobalGet { .. }
        | Expr::None { .. }
        | Expr::ListNew { .. } => vec![],
        Expr::Ascribe { expr, .. }
        | Expr::RecordAccess { expr, .. }
        | Expr::ListLen { list: expr }
        | Expr::StringLen { string: expr }
        | Expr::StringFromBytes { bytes: expr }
        | Expr::StringToBytes { string: expr }
        | Expr::AnyFromS32 { value: expr }
        | Expr::AnyToS32 { value: expr }
        | Expr::AnyFromString { value: expr }
        | Expr::AnyToString { value: expr }
        | Expr::HeapAlloc { size: expr }
        | Expr::AnyAddr { value: expr }
        | Expr::AnyFromAddr { value: expr }
        | Expr::StringAddr { value: expr }
        | Expr::StringFromAddr { value: expr }
        | Expr::RawInvoke { value: expr, .. }
        | Expr::GlobalSet { value: expr, .. }
        | Expr::Some { value: expr, .. }
        | Expr::Ok { value: expr, .. }
        | Expr::ReleaseCap { value: expr }
        | Expr::Err { value: expr, .. } => vec![expr],
        Expr::Call { args, .. }
        | Expr::WasmInstr { args, .. }
        | Expr::VariantConstruct { payload: args, .. }
        | Expr::RecordConstruct { fields: args, .. }
        | Expr::Begin { exprs: args }
        | Expr::TupleConstruct { values: args } => args.iter().collect(),
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => vec![cond, then_branch, else_branch],
        Expr::Let { value, body, .. } => vec![value, body],
        Expr::WithCap { body, .. } => vec![body],
        // A borrow references a name, not a sub-expression; it never counts as a
        // consuming use (that is the whole point), so it has no children here.
        Expr::BorrowCap { .. } => vec![],
        Expr::Match { expr, cases } => {
            let mut v = vec![expr.as_ref()];
            v.extend(cases.iter().map(|a| &a.body));
            v
        }
        Expr::ListPush { list, value } => vec![list, value],
        Expr::ListGet { list, index } => vec![list, index],
        Expr::StringRef { string, index } => vec![string, index],
        Expr::Substring { string, start, end } => vec![string, start, end],
        Expr::StringAppend { left, right } | Expr::StringEq { left, right } => vec![left, right],
    }
}

/// Count how many times each tracked (linear or affine) binding is referenced in
/// `e`. `if`/`match` branches are combined per multiplicity: a *linear* value must
/// be used identically on every path (its total is otherwise undefined), while an
/// *affine* value may be used on some paths and dropped on others (per-path max).
/// Only names in `tracked` are counted (`match`/`let`-bound names are unrestricted).
pub(crate) fn linear_uses(
    e: &Expr,
    tracked: &HashMap<String, Multiplicity>,
) -> Result<HashMap<String, usize>> {
    fn merge_sum(
        mut a: HashMap<String, usize>,
        b: &HashMap<String, usize>,
    ) -> HashMap<String, usize> {
        for (k, v) in b {
            *a.entry(k.clone()).or_insert(0) += v;
        }
        a
    }
    fn merge_branches(
        a: &HashMap<String, usize>,
        b: &HashMap<String, usize>,
        tracked: &HashMap<String, Multiplicity>,
    ) -> Result<HashMap<String, usize>> {
        let mut out = HashMap::new();
        for (name, mult) in tracked {
            let ca = a.get(name).copied().unwrap_or(0);
            let cb = b.get(name).copied().unwrap_or(0);
            let merged = match mult {
                Multiplicity::Lin => {
                    if ca != cb {
                        bail!(
                            "linear value '{}' is used {} time(s) on one branch but {} on another; it must be consumed the same way on every path",
                            name,
                            ca,
                            cb
                        );
                    }
                    ca
                }
                // Affine (and unrestricted) take the worst-case path.
                Multiplicity::Aff | Multiplicity::Un => ca.max(cb),
            };
            if merged > 0 {
                out.insert(name.clone(), merged);
            }
        }
        Ok(out)
    }
    match e {
        Expr::Var(name) => {
            let mut m = HashMap::new();
            if tracked.contains_key(name) {
                m.insert(name.clone(), 1);
            }
            Ok(m)
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let c = linear_uses(cond, tracked)?;
            let t = linear_uses(then_branch, tracked)?;
            let f = linear_uses(else_branch, tracked)?;
            Ok(merge_sum(c, &merge_branches(&t, &f, tracked)?))
        }
        Expr::Match { expr, cases } => {
            // The scrutinee is consumed here (a linear scrutinee counts as one use).
            let acc = linear_uses(expr, tracked)?;
            // Each arm's linear/affine payload bindings are local obligations: they
            // are tracked within the arm, enforced (exactly/at-most once), and then
            // removed before the arm's outer-binding uses are branch-merged. This is
            // the type-state narrowing — a linear resource extracted by the match
            // cannot be dropped or duplicated in the arm.
            let mut merged: Option<HashMap<String, usize>> = None;
            for arm in cases {
                let mut inner = tracked.clone();
                for (b, m) in arm.bindings.iter().zip(&arm.binding_mult) {
                    if *m != Multiplicity::Un {
                        inner.insert(b.clone(), *m);
                    }
                }
                let mut u = linear_uses(&arm.body, &inner)?;
                for (b, m) in arm.bindings.iter().zip(&arm.binding_mult) {
                    let c = u.get(b).copied().unwrap_or(0);
                    match m {
                        Multiplicity::Lin if c != 1 => bail!(
                            "match binding '{}' is linear and must be used exactly once (used {} time(s))",
                            b,
                            c
                        ),
                        Multiplicity::Aff if c > 1 => bail!(
                            "match binding '{}' is affine and must be used at most once (used {} time(s))",
                            b,
                            c
                        ),
                        _ => {}
                    }
                    u.remove(b);
                }
                merged = Some(match merged {
                    None => u,
                    Some(prev) => merge_branches(&prev, &u, tracked)?,
                });
            }
            Ok(merge_sum(acc, &merged.unwrap_or_default()))
        }
        Expr::WithCap { name, body, .. } => {
            // RAII: the scope owns the capability and releases it at the end, so the
            // body may only *borrow* it (via `(& c)`, which does not count) — never
            // consume it by value. A by-value use would be a second release and, worse,
            // releasing early then borrowing would be use-after-release; zero by-value
            // uses keeps the scope's release strictly last, which is sound. The cap
            // binding is linear; outer bindings still count toward their obligations.
            let mut inner = tracked.clone();
            inner.insert(name.clone(), Multiplicity::Lin);
            let mut uses = linear_uses(body, &inner)?;
            let by_value = uses.get(name).copied().unwrap_or(0);
            if by_value > 0 {
                bail!(
                    "capability '{}' may only be borrowed (& {}) inside with-cap, not consumed; the scope releases it (used by value {} time(s))",
                    name,
                    name,
                    by_value
                );
            }
            uses.remove(name);
            Ok(uses)
        }
        Expr::Let {
            name,
            value,
            body,
            mult,
        } => {
            // The value may itself consume outer linear bindings. A linear/affine
            // binding is a local obligation: tracked in the body, enforced, then
            // removed before returning the body's outer-binding uses.
            let v = linear_uses(value, tracked)?;
            let mut inner = tracked.clone();
            if *mult != Multiplicity::Un {
                inner.insert(name.clone(), *mult);
            }
            let mut b = linear_uses(body, &inner)?;
            if *mult != Multiplicity::Un {
                let c = b.get(name).copied().unwrap_or(0);
                match mult {
                    Multiplicity::Lin if c != 1 => bail!(
                        "linear binding '{}' must be used exactly once (used {} time(s))",
                        name,
                        c
                    ),
                    Multiplicity::Aff if c > 1 => bail!(
                        "affine binding '{}' must be used at most once (used {} time(s))",
                        name,
                        c
                    ),
                    _ => {}
                }
                b.remove(name);
            }
            Ok(merge_sum(v, &b))
        }
        _ => {
            let mut acc = HashMap::new();
            for child in expr_children(e) {
                acc = merge_sum(acc, &linear_uses(child, tracked)?);
            }
            Ok(acc)
        }
    }
}

/// Confirm every `with-cap` names a declared capability. Walks the whole body.
pub(crate) fn validate_cap_names(e: &Expr, capabilities: &HashSet<String>) -> Result<()> {
    if let Expr::WithCap { cap, .. } = e
        && !capabilities.contains(cap)
    {
        bail!(
            "unknown capability '{}'; declare it with (capability {})",
            cap,
            cap
        );
    }
    for child in expr_children(e) {
        validate_cap_names(child, capabilities)?;
    }
    Ok(())
}

/// Enforce the substructural contract of a function: each `(lin T)` parameter (and
/// every capability parameter, and every `with-cap` binding) must be consumed
/// exactly once, and each `(aff T)` parameter at most once, counting `if`/`match`
/// branches per multiplicity.
pub(crate) fn check_fn_linearity(
    func: &Function,
    capabilities: &HashSet<String>,
    variant_mult: &HashMap<String, Multiplicity>,
) -> Result<()> {
    validate_cap_names(&func.body, capabilities)?;
    // A capability-typed parameter is linear by its type; a variant holding a
    // linear/affine payload is linear/affine by its type (infectious linearity, so
    // it can't be copied and re-matched to extract the resource twice); otherwise
    // the declared `(lin/aff T)` multiplicity applies.
    let is_cap =
        |p: &Parameter| matches!(&p.ty, Type::Resource(name) if capabilities.contains(name));
    let mult_of = |p: &Parameter| -> Multiplicity {
        if is_cap(p) {
            return Multiplicity::Lin;
        }
        if let Type::Variant(name) = &p.ty
            && let Some(m) = variant_mult.get(name)
            && *m != Multiplicity::Un
        {
            // The declared qualifier can only strengthen the inherent one.
            return if p.mult == Multiplicity::Lin {
                Multiplicity::Lin
            } else {
                *m
            };
        }
        p.mult
    };
    let tracked: HashMap<String, Multiplicity> = func
        .params
        .iter()
        .map(|p| (p.name.clone(), mult_of(p)))
        .filter(|(_, m)| *m != Multiplicity::Un)
        .collect();
    // Always walk the body: it may contain `with-cap` bindings even when no
    // parameter is tracked (linear_uses enforces their obligations too).
    let uses = linear_uses(&func.body, &tracked)?;
    for p in &func.params {
        let count = uses.get(&p.name).copied().unwrap_or(0);
        match mult_of(p) {
            Multiplicity::Un => {}
            Multiplicity::Lin => {
                let noun = if is_cap(p) {
                    "capability parameter"
                } else {
                    "linear parameter"
                };
                match count {
                    1 => {}
                    0 => bail!(
                        "{} '{}' of function '{}' is never used; it must be consumed exactly once",
                        noun,
                        p.name,
                        func.name
                    ),
                    n => bail!(
                        "{} '{}' of function '{}' is used {} times but must be used exactly once",
                        noun,
                        p.name,
                        func.name,
                        n
                    ),
                }
            }
            Multiplicity::Aff => {
                if count > 1 {
                    bail!(
                        "affine parameter '{}' of function '{}' is used {} times but must be used at most once",
                        p.name,
                        func.name,
                        count
                    );
                }
            }
        }
    }
    Ok(())
}

pub(crate) fn collect_signatures(prog: &Program) -> Result<HashMap<String, Signature>> {
    let mut signatures = HashMap::new();
    for func in &prog.functions {
        let params = func.params.iter().map(|p| p.ty.clone()).collect();
        let sig = Signature {
            params,
            result: func.return_type.clone(),
        };
        if signatures.insert(func.name.clone(), sig).is_some() {
            bail!("Duplicate function '{}'", func.name);
        }
    }
    for import in &prog.imports {
        let params = import.params.iter().map(|p| p.ty.clone()).collect();
        let sig = Signature {
            params,
            result: import.return_type.clone(),
        };
        // Imports are keyed by bare name only for typed-call resolution, which
        // raw-invoked imports never use. Same-named functions in different
        // interfaces (store.exists / filesystem.exists) are allowed; keep the
        // first signature (its typed wrapper is the one that survives dedup).
        signatures.entry(import.name.clone()).or_insert(sig);
    }
    Ok(signatures)
}

pub(crate) fn check_expr(
    expr: &Expr,
    env: &HashMap<String, Type>,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) -> Result<Type> {
    match expr {
        Expr::Int { ty, .. } => Ok(ty.clone()),
        Expr::Float { ty, .. } => Ok(ty.clone()),
        Expr::StringLiteral(_) => Ok(Type::Str),
        Expr::Ascribe { expr, ty } => {
            let inner_ty = check_expr(expr, env, signatures, globals, records, variants)?;
            // A same-type ascription is an identity annotation (any type is fine);
            // a differing ascription is a numeric cast (both sides must be numeric).
            if inner_ty != *ty {
                ensure_numeric(&inner_ty, "cast requires scalar types")?;
                ensure_numeric(ty, "cast requires scalar types")?;
                if conversion_instr(&inner_ty, ty).is_none() {
                    bail!("unsupported cast from {:?} to {:?}", inner_ty, ty);
                }
            }
            Ok(ty.clone())
        }
        Expr::Var(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| anyhow!("unknown variable '{}'", name)),
        Expr::Call { name, args } => {
            let sig = signatures
                .get(name)
                .ok_or_else(|| anyhow!("call to unknown function '{}'", name))?;
            if sig.params.len() != args.len() {
                bail!(
                    "function '{}' expects {} arguments but {} were provided",
                    name,
                    sig.params.len(),
                    args.len()
                );
            }
            for (arg, expected_ty) in args.iter().zip(sig.params.iter()) {
                let ty = check_expr(arg, env, signatures, globals, records, variants)?;
                if ty != *expected_ty {
                    bail!(
                        "argument type mismatch calling '{}': expected {:?}, got {:?}",
                        name,
                        expected_ty,
                        ty
                    );
                }
            }
            Ok(sig.result.clone())
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = check_expr(cond, env, signatures, globals, records, variants)?;
            if cond_ty != Type::S32 {
                bail!("if condition must be s32 (0/1), got {:?}", cond_ty);
            }
            let then_ty = check_expr(then_branch, env, signatures, globals, records, variants)?;
            let else_ty = check_expr(else_branch, env, signatures, globals, records, variants)?;
            if then_ty != else_ty {
                bail!(
                    "if branches must return the same type, got {:?} and {:?}",
                    then_ty,
                    else_ty
                );
            }
            Ok(then_ty)
        }
        Expr::Let {
            name, value, body, ..
        } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            let mut next_env = env.clone();
            next_env.insert(name.clone(), value_ty);
            check_expr(body, &next_env, signatures, globals, records, variants)
        }
        Expr::Begin { exprs } => {
            let mut last_ty = Type::S32;
            for expr in exprs {
                last_ty = check_expr(expr, env, signatures, globals, records, variants)?;
            }
            Ok(last_ty)
        }
        Expr::WasmInstr { name, args } => {
            let instr_info = lookup_wasm_instr(name)
                .ok_or_else(|| anyhow!("unknown WASM instruction '{}'", name))?;

            // Special handling for const instructions - they define the type, not check it
            if name.ends_with(".const") {
                if args.len() != 1 {
                    bail!("{} expects exactly 1 argument", name);
                }
                // Just verify it's a literal, don't type check it
                match &args[0] {
                    Expr::Int { .. } | Expr::Float { .. } => {}
                    _ => bail!("{} requires a literal value", name),
                }
                return Ok(instr_info.result);
            }

            if instr_info.params.len() != args.len() {
                bail!(
                    "WASM instruction '{}' expects {} arguments but {} were provided",
                    name,
                    instr_info.params.len(),
                    args.len()
                );
            }
            for (arg, expected_ty) in args.iter().zip(instr_info.params.iter()) {
                let ty = check_expr(arg, env, signatures, globals, records, variants)?;
                // Allow u8 -> s32 coercion since u8 is stored as i32
                let types_compatible =
                    ty == *expected_ty || (*expected_ty == Type::S32 && ty == Type::U8);
                if !types_compatible {
                    bail!(
                        "argument type mismatch in '{}': expected {:?}, got {:?}",
                        name,
                        expected_ty,
                        ty
                    );
                }
            }
            Ok(instr_info.result)
        }
        Expr::GlobalGet { name } => {
            let (ty, _mutable) = globals
                .get(name)
                .ok_or_else(|| anyhow!("unknown global '{}'", name))?;
            Ok(ty.clone())
        }
        Expr::GlobalSet { name, value } => {
            let (expected_ty, mutable) = globals
                .get(name)
                .ok_or_else(|| anyhow!("unknown global '{}'", name))?;
            if !mutable {
                bail!("cannot set immutable global '{}'", name);
            }
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *expected_ty {
                bail!(
                    "type mismatch setting global '{}': expected {:?}, got {:?}",
                    name,
                    expected_ty,
                    value_ty
                );
            }
            Ok(value_ty)
        }
        Expr::RecordConstruct {
            record_name,
            fields,
        } => {
            let record_def = records
                .get(record_name)
                .ok_or_else(|| anyhow!("unknown record type '{}'", record_name))?;
            if record_def.fields.len() != fields.len() {
                bail!(
                    "record '{}' expects {} fields but {} were provided",
                    record_name,
                    record_def.fields.len(),
                    fields.len()
                );
            }
            for (field_expr, field_def) in fields.iter().zip(record_def.fields.iter()) {
                let ty = check_expr(field_expr, env, signatures, globals, records, variants)?;
                if ty != field_def.ty {
                    bail!(
                        "field '{}' of record '{}': expected {:?}, got {:?}",
                        field_def.name,
                        record_name,
                        field_def.ty,
                        ty
                    );
                }
            }
            Ok(Type::Record(record_name.clone()))
        }
        Expr::RecordAccess {
            record_name,
            field_name,
            expr,
        } => {
            let record_def = records
                .get(record_name)
                .ok_or_else(|| anyhow!("unknown record type '{}'", record_name))?;
            let field = record_def
                .fields
                .iter()
                .find(|f| f.name == *field_name)
                .ok_or_else(|| anyhow!("record '{}' has no field '{}'", record_name, field_name))?;
            let expr_ty = check_expr(expr, env, signatures, globals, records, variants)?;
            if expr_ty != Type::Record(record_name.clone()) {
                bail!(
                    "field access expects record '{}', got {:?}",
                    record_name,
                    expr_ty
                );
            }
            Ok(field.ty.clone())
        }
        Expr::VariantConstruct {
            variant_name,
            case_name,
            payload,
        } => {
            let variant_def = variants
                .get(variant_name)
                .ok_or_else(|| anyhow!("unknown variant type '{}'", variant_name))?;
            let (_, case) = variant_def
                .find_case(case_name)
                .ok_or_else(|| anyhow!("variant '{}' has no case '{}'", variant_name, case_name))?;
            if case.payload.len() != payload.len() {
                bail!(
                    "variant case '{}::{}' expects {} payload values but {} were provided",
                    variant_name,
                    case_name,
                    case.payload.len(),
                    payload.len()
                );
            }
            for (payload_expr, expected_ty) in payload.iter().zip(case.payload.iter()) {
                let ty = check_expr(payload_expr, env, signatures, globals, records, variants)?;
                if ty != *expected_ty {
                    bail!(
                        "payload type mismatch in '{}::{}': expected {:?}, got {:?}",
                        variant_name,
                        case_name,
                        expected_ty,
                        ty
                    );
                }
            }
            Ok(Type::Variant(variant_name.clone()))
        }
        Expr::Match { expr, cases } => {
            let expr_ty = check_expr(expr, env, signatures, globals, records, variants)?;
            // Desugar a `_` wildcard into explicit arms before checking coverage.
            let effective = expand_match_wildcard(cases, &expr_ty, variants);
            let cases = &effective;

            // Handle Option and Result types specially
            match &expr_ty {
                Type::Option(inner_ty) => {
                    // Option can match on 'some' and 'none'
                    let mut result_ty: Option<Type> = None;
                    for arm in cases {
                        // Validate case names and bindings for option
                        let expected_bindings = match arm.case_name.as_str() {
                            "some" => 1,
                            "none" => 0,
                            other => bail!(
                                "option match: unknown case '{}', expected 'some' or 'none'",
                                other
                            ),
                        };
                        if arm.bindings.len() != expected_bindings {
                            bail!(
                                "match arm for '{}' expects {} bindings but {} were provided",
                                arm.case_name,
                                expected_bindings,
                                arm.bindings.len()
                            );
                        }

                        // Extend environment with bound variables
                        let mut arm_env = env.clone();
                        if arm.case_name == "some" && !arm.bindings.is_empty() {
                            arm_env.insert(arm.bindings[0].clone(), (**inner_ty).clone());
                        }

                        let arm_ty = check_expr(
                            &arm.body, &arm_env, signatures, globals, records, variants,
                        )?;
                        match &result_ty {
                            None => result_ty = Some(arm_ty),
                            Some(expected) => {
                                if arm_ty != *expected {
                                    bail!(
                                        "match arms must return the same type, got {:?} and {:?}",
                                        expected,
                                        arm_ty
                                    );
                                }
                            }
                        }
                    }
                    // Exhaustiveness: both 'some' and 'none' must be covered.
                    for req in ["some", "none"] {
                        if !cases.iter().any(|a| a.case_name == req) {
                            bail!("non-exhaustive match on option: missing '{}' case", req);
                        }
                    }
                    result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"))
                }
                Type::Result(ok_ty, err_ty) => {
                    // Result can match on 'ok' and 'err'
                    let mut result_ty: Option<Type> = None;
                    for arm in cases {
                        // Validate case names and bindings for result
                        let (expected_bindings, payload_ty) = match arm.case_name.as_str() {
                            "ok" => (1, (**ok_ty).clone()),
                            "err" => (1, (**err_ty).clone()),
                            other => bail!(
                                "result match: unknown case '{}', expected 'ok' or 'err'",
                                other
                            ),
                        };
                        if arm.bindings.len() != expected_bindings {
                            bail!(
                                "match arm for '{}' expects {} bindings but {} were provided",
                                arm.case_name,
                                expected_bindings,
                                arm.bindings.len()
                            );
                        }

                        // Extend environment with bound variables
                        let mut arm_env = env.clone();
                        if !arm.bindings.is_empty() {
                            arm_env.insert(arm.bindings[0].clone(), payload_ty);
                        }

                        let arm_ty = check_expr(
                            &arm.body, &arm_env, signatures, globals, records, variants,
                        )?;
                        match &result_ty {
                            None => result_ty = Some(arm_ty),
                            Some(expected) => {
                                if arm_ty != *expected {
                                    bail!(
                                        "match arms must return the same type, got {:?} and {:?}",
                                        expected,
                                        arm_ty
                                    );
                                }
                            }
                        }
                    }
                    // Exhaustiveness: both 'ok' and 'err' must be covered.
                    for req in ["ok", "err"] {
                        if !cases.iter().any(|a| a.case_name == req) {
                            bail!("non-exhaustive match on result: missing '{}' case", req);
                        }
                    }
                    result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"))
                }
                Type::Variant(variant_name) => {
                    // User-defined variant
                    let variant_def = variants
                        .get(variant_name)
                        .ok_or_else(|| anyhow!("unknown variant type '{}'", variant_name))?;

                    // Check that all cases exist and have correct bindings
                    let mut result_ty: Option<Type> = None;
                    for arm in cases {
                        let (_, case) = variant_def.find_case(&arm.case_name).ok_or_else(|| {
                            anyhow!("variant '{}' has no case '{}'", variant_name, arm.case_name)
                        })?;
                        if arm.bindings.len() != case.payload.len() {
                            bail!(
                                "match arm for '{}' expects {} bindings but {} were provided",
                                arm.case_name,
                                case.payload.len(),
                                arm.bindings.len()
                            );
                        }

                        // Extend environment with bound variables
                        let mut arm_env = env.clone();
                        for (binding_name, ty) in arm.bindings.iter().zip(case.payload.iter()) {
                            arm_env.insert(binding_name.clone(), ty.clone());
                        }

                        let arm_ty = check_expr(
                            &arm.body, &arm_env, signatures, globals, records, variants,
                        )?;
                        match &result_ty {
                            None => result_ty = Some(arm_ty),
                            Some(expected) => {
                                if arm_ty != *expected {
                                    bail!(
                                        "match arms must return the same type, got {:?} and {:?}",
                                        expected,
                                        arm_ty
                                    );
                                }
                            }
                        }
                    }
                    // Exhaustiveness: every case of the variant must be covered.
                    let missing: Vec<&str> = variant_def
                        .cases
                        .iter()
                        .map(|c| c.name.as_str())
                        .filter(|n| !cases.iter().any(|a| a.case_name == *n))
                        .collect();
                    if !missing.is_empty() {
                        bail!(
                            "non-exhaustive match on variant '{}': missing case(s): {}",
                            variant_name,
                            missing.join(", ")
                        );
                    }
                    result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"))
                }
                _ => bail!(
                    "match expression must be a variant, option, or result type, got {:?}",
                    expr_ty
                ),
            }
        }
        // Option constructors
        Expr::Some { inner_type, value } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *inner_type {
                bail!(
                    "some value type mismatch: expected {:?}, got {:?}",
                    inner_type,
                    value_ty
                );
            }
            Ok(Type::Option(Box::new(inner_type.clone())))
        }
        Expr::None { inner_type } => Ok(Type::Option(Box::new(inner_type.clone()))),
        // Result constructors
        Expr::Ok {
            ok_type,
            err_type,
            value,
        } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *ok_type {
                bail!(
                    "ok value type mismatch: expected {:?}, got {:?}",
                    ok_type,
                    value_ty
                );
            }
            Ok(Type::Result(
                Box::new(ok_type.clone()),
                Box::new(err_type.clone()),
            ))
        }
        Expr::Err {
            ok_type,
            err_type,
            value,
        } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *err_type {
                bail!(
                    "err value type mismatch: expected {:?}, got {:?}",
                    err_type,
                    value_ty
                );
            }
            Ok(Type::Result(
                Box::new(ok_type.clone()),
                Box::new(err_type.clone()),
            ))
        }
        // Tuple constructor
        Expr::TupleConstruct { values } => {
            let elem_types: Vec<Type> = values
                .iter()
                .map(|v| check_expr(v, env, signatures, globals, records, variants))
                .collect::<Result<Vec<_>>>()?;
            Ok(Type::Tuple(elem_types))
        }
        Expr::WithCap { name, cap, body } => {
            // Bind the capability token (a resource-represented handle) and check
            // the body. The borrow-only obligation is enforced by check_fn_linearity.
            let mut next_env = env.clone();
            next_env.insert(name.clone(), Type::Resource(cap.clone()));
            let body_ty = check_expr(body, &next_env, signatures, globals, records, variants)?;
            // A borrow must not outlive the capability it borrows; the scope releases
            // the cap at its end, so the body may not yield a borrow of it.
            if matches!(body_ty, Type::Borrow(_)) {
                bail!("a borrow cannot escape its with-cap scope");
            }
            Ok(body_ty)
        }
        Expr::ReleaseCap { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            match ty {
                Type::Resource(_) => Ok(Type::S32),
                other => bail!("release-cap expects a capability, got {:?}", other),
            }
        }
        Expr::BorrowCap { name } => {
            let ty = env
                .get(name)
                .cloned()
                .ok_or_else(|| anyhow!("unknown variable '{}'", name))?;
            match ty {
                Type::Resource(_) => Ok(Type::Borrow(Box::new(ty))),
                other => bail!("(& {}) expects a capability, got {:?}", name, other),
            }
        }
        // List operations
        Expr::ListNew { elem_type } => Ok(Type::List(Box::new(elem_type.clone()))),
        Expr::ListPush { list, value } => {
            let list_ty = check_expr(list, env, signatures, globals, records, variants)?;
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => bail!("list-push expects a list, got {:?}", list_ty),
            };
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            // Allow s32 -> u8 coercion since u8 is stored as i32 internally
            let types_compatible =
                value_ty == elem_type || (elem_type == Type::U8 && value_ty == Type::S32);
            if !types_compatible {
                bail!(
                    "list-push value type mismatch: expected {:?}, got {:?}",
                    elem_type,
                    value_ty
                );
            }
            Ok(list_ty)
        }
        Expr::ListGet { list, index } => {
            let list_ty = check_expr(list, env, signatures, globals, records, variants)?;
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => bail!("list-get expects a list, got {:?}", list_ty),
            };
            let index_ty = check_expr(index, env, signatures, globals, records, variants)?;
            if index_ty != Type::S32 {
                bail!("list-get index must be s32, got {:?}", index_ty);
            }
            Ok(elem_type)
        }
        Expr::ListLen { list } => {
            let list_ty = check_expr(list, env, signatures, globals, records, variants)?;
            match &list_ty {
                Type::List(_) => Ok(Type::S32),
                _ => bail!("list-len expects a list, got {:?}", list_ty),
            }
        }
        Expr::StringLen { string } => {
            let str_ty = check_expr(string, env, signatures, globals, records, variants)?;
            match &str_ty {
                Type::Str => Ok(Type::S32),
                _ => bail!("string-len expects a string, got {:?}", str_ty),
            }
        }
        Expr::StringRef { string, index } => {
            let str_ty = check_expr(string, env, signatures, globals, records, variants)?;
            if str_ty != Type::Str {
                bail!("string-ref expects a string, got {:?}", str_ty);
            }
            let idx_ty = check_expr(index, env, signatures, globals, records, variants)?;
            if idx_ty != Type::S32 {
                bail!("string-ref index must be s32, got {:?}", idx_ty);
            }
            Ok(Type::S32)
        }
        Expr::Substring { string, start, end } => {
            let str_ty = check_expr(string, env, signatures, globals, records, variants)?;
            if str_ty != Type::Str {
                bail!("substring expects a string, got {:?}", str_ty);
            }
            let start_ty = check_expr(start, env, signatures, globals, records, variants)?;
            if start_ty != Type::S32 {
                bail!("substring start index must be s32, got {:?}", start_ty);
            }
            let end_ty = check_expr(end, env, signatures, globals, records, variants)?;
            if end_ty != Type::S32 {
                bail!("substring end index must be s32, got {:?}", end_ty);
            }
            Ok(Type::Str)
        }
        Expr::AnyFromS32 { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::S32 {
                bail!("any-s32 expects an s32, got {:?}", ty);
            }
            Ok(Type::Any)
        }
        Expr::AnyToS32 { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Any {
                bail!("any-as-s32 expects an any, got {:?}", ty);
            }
            Ok(Type::S32)
        }
        Expr::AnyFromString { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Str {
                bail!("any-string expects a string, got {:?}", ty);
            }
            Ok(Type::Any)
        }
        Expr::AnyToString { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Any {
                bail!("any-as-string expects an any, got {:?}", ty);
            }
            Ok(Type::Str)
        }
        Expr::HeapAlloc { size } => {
            let ty = check_expr(size, env, signatures, globals, records, variants)?;
            if ty != Type::S32 {
                bail!("heap-alloc expects an s32 size, got {:?}", ty);
            }
            Ok(Type::S32)
        }
        Expr::AnyAddr { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Any {
                bail!("any-addr expects an any, got {:?}", ty);
            }
            Ok(Type::S32)
        }
        Expr::AnyFromAddr { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::S32 {
                bail!("any-from-addr expects an s32 address, got {:?}", ty);
            }
            Ok(Type::Any)
        }
        Expr::StringAddr { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Str {
                bail!("string-addr expects a string, got {:?}", ty);
            }
            Ok(Type::S32)
        }
        Expr::StringFromAddr { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::S32 {
                bail!("string-from-addr expects an s32 address, got {:?}", ty);
            }
            Ok(Type::Str)
        }
        Expr::RawInvoke { value, .. } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Any {
                bail!(
                    "raw-invoke expects an any (encoded args tuple), got {:?}",
                    ty
                );
            }
            Ok(Type::Any)
        }
        Expr::StringAppend { left, right } => {
            let left_ty = check_expr(left, env, signatures, globals, records, variants)?;
            if left_ty != Type::Str {
                bail!("string-append expects strings, got {:?}", left_ty);
            }
            let right_ty = check_expr(right, env, signatures, globals, records, variants)?;
            if right_ty != Type::Str {
                bail!("string-append expects strings, got {:?}", right_ty);
            }
            Ok(Type::Str)
        }
        Expr::StringEq { left, right } => {
            let left_ty = check_expr(left, env, signatures, globals, records, variants)?;
            if left_ty != Type::Str {
                bail!("string=? expects strings, got {:?}", left_ty);
            }
            let right_ty = check_expr(right, env, signatures, globals, records, variants)?;
            if right_ty != Type::Str {
                bail!("string=? expects strings, got {:?}", right_ty);
            }
            Ok(Type::S32)
        }
        Expr::StringFromBytes { bytes } => {
            let bytes_ty = check_expr(bytes, env, signatures, globals, records, variants)?;
            match &bytes_ty {
                Type::List(elem) if **elem == Type::U8 => Ok(Type::Str),
                _ => bail!("string-from-bytes expects list<u8>, got {:?}", bytes_ty),
            }
        }
        Expr::StringToBytes { string } => {
            let str_ty = check_expr(string, env, signatures, globals, records, variants)?;
            if str_ty != Type::Str {
                bail!("string-to-bytes expects string, got {:?}", str_ty);
            }
            Ok(Type::List(Box::new(Type::U8)))
        }
    }
}

pub(crate) fn ensure_numeric(ty: &Type, msg: &str) -> Result<()> {
    match ty {
        // Every scalar is castable: the four native WASM numerics plus the
        // i32-backed integer refinements (bool/u8/u16/u32) and u64.
        Type::S32
        | Type::S64
        | Type::F32
        | Type::F64
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::Bool => Ok(()),
        Type::Record(name) => bail!("{}: expected scalar type, got record '{}'", msg, name),
        Type::Variant(name) => bail!("{}: expected scalar type, got variant '{}'", msg, name),
        Type::Option(_) => bail!("{}: expected scalar type, got option", msg),
        Type::Result(_, _) => bail!("{}: expected scalar type, got result", msg),
        Type::List(_) => bail!("{}: expected scalar type, got list", msg),
        Type::Str => bail!("{}: expected scalar type, got string", msg),
        Type::Tuple(_) => bail!("{}: expected scalar type, got tuple", msg),
        Type::Any => bail!("{}: expected scalar type, got any", msg),
        Type::Resource(name) => bail!("{}: expected scalar type, got resource '{}'", msg, name),
        Type::Borrow(_) => bail!("{}: expected scalar type, got borrow", msg),
    }
}

pub fn parse_sexpr(tokens: &[Token], pos: usize) -> (SExpr, usize) {
    let token = tokens.get(pos);
    match token.map(|t| (&t.kind, &t.span)) {
        Some((TokenKind::LParen, start_span)) => {
            let mut elems = Vec::new();
            let mut i = pos + 1;
            loop {
                match tokens.get(i).map(|t| (&t.kind, &t.span)) {
                    Some((TokenKind::RParen, end_span)) => {
                        let span = start_span.merge(end_span);
                        return (SExpr::List(elems, span), i + 1);
                    }
                    Some(_) => {
                        let (sexpr, next) = parse_sexpr(tokens, i);
                        elems.push(sexpr);
                        i = next;
                    }
                    None => {
                        panic!(
                            "Unclosed parenthesis at line {}, column {}",
                            start_span.line, start_span.column
                        );
                    }
                }
            }
        }
        Some((TokenKind::RParen, span)) => {
            panic!(
                "Unexpected closing parenthesis at line {}, column {}",
                span.line, span.column
            );
        }
        Some((TokenKind::Symbol(s), span)) => (SExpr::Sym(s.clone(), span.clone()), pos + 1),
        Some((TokenKind::Number(NumericToken::Int { value, ty }), span)) => (
            SExpr::Int {
                value: *value,
                ty: ty.clone(),
                span: span.clone(),
            },
            pos + 1,
        ),
        Some((TokenKind::Number(NumericToken::Float { value, ty }), span)) => (
            SExpr::Float {
                value: *value,
                ty: ty.clone(),
                span: span.clone(),
            },
            pos + 1,
        ),
        Some((TokenKind::String(s), span)) => (SExpr::Str(s.clone(), span.clone()), pos + 1),
        Some((TokenKind::Quasiquote, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::Quasiquote(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::Unquote, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::Unquote(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::UnquoteSplice, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::UnquoteSplice(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::SyntaxQuote, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::SyntaxQuote(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::Quasisyntax, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::Quasisyntax(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::Unsyntax, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::Unsyntax(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::UnsyntaxSplice, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::UnsyntaxSplice(Box::new(inner), span.clone()), next)
        }
        None => panic!("Unexpected end of tokens"),
    }
}
