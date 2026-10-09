use super::*;

// Parser: s-expressions -> typed AST (Program, Expr, type exprs).
pub(crate) fn parse_program(forms: Vec<SExpr>, ctx: &CompileContext) -> Result<Program> {
    let mut pending = Vec::new();
    let mut defined = HashSet::new();
    let mut imports = Vec::new();
    let mut imported = HashSet::new();
    let mut exports = Vec::new();
    let mut export_set = HashSet::new();
    let mut globals = Vec::new();
    let mut global_names = HashSet::new();
    let mut records = Vec::new();
    let mut record_names: HashSet<String> = HashSet::new();
    let mut variants = Vec::new();
    let mut variant_names: HashSet<String> = HashSet::new();
    let mut resources = Vec::new();
    let mut resource_names: HashSet<String> = HashSet::new();
    // Capability type names. A capability is a linear resource: it shares the
    // resource representation (an i32 handle, so it parses as Type::Resource and
    // needs no new Type variant), but every binding of it carries a use-exactly-once
    // obligation and it has no public constructor (minted only by `with-cap`).
    let mut capability_names: HashSet<String> = HashSet::new();
    let mut world_config: Option<WorldConfig> = None;
    let mut data_segments = Vec::new();

    // First pass: collect type names (records, variants, resources) so we can distinguish them
    for form in &forms {
        if let SExpr::List(items, span) = form {
            if items.is_empty() {
                return Err(ctx.error("empty list is not a valid top-level form", span));
            }
            match &items[0] {
                SExpr::Sym(sym, _) if sym == "record" => {
                    if items.len() >= 2
                        && let SExpr::Sym(name, _) = &items[1]
                        && !record_names.insert(name.clone())
                    {
                        return Err(ctx.error(format!("duplicate record type '{}'", name), span));
                    }
                }
                SExpr::Sym(sym, _) if sym == "variant" => {
                    if items.len() >= 2
                        && let SExpr::Sym(name, _) = &items[1]
                        && !variant_names.insert(name.clone())
                    {
                        return Err(ctx.error(format!("duplicate variant type '{}'", name), span));
                    }
                }
                SExpr::Sym(sym, _) if sym == "resource" => {
                    if items.len() >= 2
                        && let SExpr::Sym(name, _) = &items[1]
                        && !resource_names.insert(name.clone())
                    {
                        return Err(ctx.error(format!("duplicate resource type '{}'", name), span));
                    }
                }
                SExpr::Sym(sym, _) if sym == "capability" => {
                    if items.len() >= 2
                        && let SExpr::Sym(name, _) = &items[1]
                    {
                        // Register as a resource name (so it parses as Type::Resource)
                        // and as a capability (so the checker treats it as linear).
                        if !resource_names.insert(name.clone()) {
                            return Err(ctx.error(format!("duplicate type name '{}'", name), span));
                        }
                        capability_names.insert(name.clone());
                    }
                }
                _ => {}
            }
        }
    }

    // Second pass: parse everything with type names available
    for form in forms {
        match form {
            SExpr::List(items, span) => {
                if items.is_empty() {
                    return Err(ctx.error("empty list is not a valid top-level form", &span));
                }
                match &items[0] {
                    SExpr::Sym(sym, _) if sym == "fn" => {
                        let func = parse_fn_form(
                            SExpr::List(items, span.clone()),
                            &variant_names,
                            &resource_names,
                            ctx,
                        )?;
                        if !defined.insert(func.name.clone()) {
                            return Err(
                                ctx.error(format!("duplicate function '{}'", func.name), &span)
                            );
                        }
                        pending.push(func);
                    }
                    SExpr::Sym(sym, _) if sym == "export" => {
                        if items.len() == 3 {
                            // (export "alias" (fn ...)) - aliased export
                            match (&items[1], &items[2]) {
                                (SExpr::Str(alias, _), SExpr::List(_, inner_span)) => {
                                    let func = parse_fn_form(
                                        items[2].clone(),
                                        &variant_names,
                                        &resource_names,
                                        ctx,
                                    )?;
                                    if !defined.insert(func.name.clone()) {
                                        return Err(ctx.error(
                                            format!("duplicate function '{}'", func.name),
                                            inner_span,
                                        ));
                                    }
                                    if export_set.insert(func.name.clone()) {
                                        exports.push(ExportDef::aliased(
                                            alias.clone(),
                                            func.name.clone(),
                                        ));
                                    }
                                    pending.push(func);
                                }
                                _ => {
                                    return Err(ctx.error(
                                        "aliased export expects (export \"name\" (fn ...))",
                                        &span,
                                    ));
                                }
                            }
                        } else if items.len() == 2 {
                            match &items[1] {
                                SExpr::Sym(name, _) => {
                                    if export_set.insert(name.clone()) {
                                        exports.push(ExportDef::simple(name.clone()));
                                    }
                                }
                                SExpr::List(_, inner_span) => {
                                    let func = parse_fn_form(
                                        items[1].clone(),
                                        &variant_names,
                                        &resource_names,
                                        ctx,
                                    )?;
                                    if !defined.insert(func.name.clone()) {
                                        return Err(ctx.error(
                                            format!("duplicate function '{}'", func.name),
                                            inner_span,
                                        ));
                                    }
                                    if export_set.insert(func.name.clone()) {
                                        exports.push(ExportDef::simple(func.name.clone()));
                                    }
                                    pending.push(func);
                                }
                                other => {
                                    return Err(ctx.error(
                                        "export argument must be a symbol or (fn ...)",
                                        other.span(),
                                    ));
                                }
                            }
                        } else {
                            return Err(ctx.error("export expects 1 or 2 arguments", &span));
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "import" => {
                        let import =
                            parse_import_form(&items, &variant_names, &resource_names, ctx)?;
                        if defined.contains(&import.name) {
                            return Err(ctx.error(
                                format!(
                                    "function '{}' is already defined and cannot be imported",
                                    import.name
                                ),
                                &span,
                            ));
                        }
                        if !imported.insert((import.module.clone(), import.name.clone())) {
                            return Err(ctx.error(
                                format!("duplicate import '{}/{}'", import.module, import.name),
                                &span,
                            ));
                        }
                        imports.push(import);
                    }
                    SExpr::Sym(sym, _) if sym == "global" => {
                        let global =
                            parse_global_form(&items, &variant_names, &resource_names, ctx)?;
                        if !global_names.insert(global.name.clone()) {
                            return Err(
                                ctx.error(format!("duplicate global '{}'", global.name), &span)
                            );
                        }
                        globals.push(global);
                    }
                    SExpr::Sym(sym, _) if sym == "record" => {
                        let record =
                            parse_record_form(&items, &variant_names, &resource_names, ctx)?;
                        // Already checked for duplicates in first pass
                        records.push(record);
                    }
                    SExpr::Sym(sym, _) if sym == "variant" => {
                        let variant =
                            parse_variant_form(&items, &variant_names, &resource_names, ctx)?;
                        // Already checked for duplicates in first pass
                        variants.push(variant);
                    }
                    SExpr::Sym(sym, _) if sym == "resource" => {
                        let resource = parse_resource_form(&items, ctx)?;
                        // Already checked for duplicates in first pass
                        resources.push(resource);
                    }
                    SExpr::Sym(sym, _) if sym == "capability" => {
                        // A capability is a pure type declaration (no fields, no
                        // codegen, unforgeable). Validate shape; it was registered in
                        // the first pass. `(capability Name)`.
                        if items.len() != 2 || !matches!(&items[1], SExpr::Sym(..)) {
                            return Err(ctx.error_with_note(
                                "invalid capability declaration",
                                &span,
                                "expected: (capability Name)",
                            ));
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "world" => {
                        if world_config.is_some() {
                            return Err(
                                ctx.error("only one (world ...) declaration is allowed", &span)
                            );
                        }
                        world_config = Some(parse_world_form(&items, ctx)?);
                    }
                    SExpr::Sym(sym, _) if sym == "data" => {
                        if items.len() != 3 {
                            return Err(ctx.error("data expects (data <offset> <string>)", &span));
                        }
                        let offset = match &items[1] {
                            SExpr::Int { value, .. } => *value as i32,
                            _ => {
                                return Err(
                                    ctx.error("data offset must be an integer", items[1].span())
                                );
                            }
                        };
                        let bytes = match &items[2] {
                            SExpr::Str(s, _) => s.bytes().collect::<Vec<u8>>(),
                            _ => {
                                return Err(
                                    ctx.error("data content must be a string", items[2].span())
                                );
                            }
                        };
                        data_segments.push(DataSegment { offset, bytes });
                    }
                    other => {
                        return Err(ctx.error_with_note(
                            "unknown top-level form",
                            other.span(),
                            "expected 'fn', 'export', 'import', 'global', 'record', 'variant', 'resource', 'capability', 'world', or 'data'"
                        ));
                    }
                }
            }
            other => {
                return Err(ctx.error("top-level forms must be lists", other.span()));
            }
        }
    }

    let mut signatures = HashMap::new();
    for func in &pending {
        let params = func.params.iter().map(|p| p.ty.clone()).collect();
        let sig = Signature {
            params,
            result: func.return_type.clone(),
        };
        if signatures.insert(func.name.clone(), sig).is_some() {
            return Err(ctx.error(format!("duplicate function '{}'", func.name), &func.span));
        }
    }

    for import in &imports {
        let params = import.params.iter().map(|p| p.ty.clone()).collect();
        let sig = Signature {
            params,
            result: import.return_type.clone(),
        };
        // Same-named imports across interfaces are allowed (keyed by bare name
        // only for typed-call resolution, which raw-invoked imports don't use);
        // keep the first, matching the typed-wrapper dedup.
        signatures.entry(import.name.clone()).or_insert(sig);
    }

    for export in exports.iter() {
        if !signatures.contains_key(&export.func_name) {
            return Err(ctx.error(
                format!("cannot export undefined function '{}'", export.func_name),
                &Span::dummy(),
            ));
        }
        if imported.iter().any(|(_, name)| name == &export.func_name) {
            return Err(ctx.error(
                format!("cannot export imported function '{}'", export.func_name),
                &Span::dummy(),
            ));
        }
    }

    // Build records and variants maps for parse_expr
    let records_map: HashMap<String, RecordDef> = records
        .iter()
        .map(|r| (r.name.clone(), r.clone()))
        .collect();
    let variants_map: HashMap<String, VariantDef> = variants
        .iter()
        .map(|v| (v.name.clone(), v.clone()))
        .collect();

    let mut functions = Vec::new();
    for func in pending {
        // Create bindings with scopes from parameters for hygienic variable resolution
        let param_bindings = func
            .params
            .iter()
            .map(|p| Binding::new(p.name.clone(), p.scopes.clone()))
            .collect::<Vec<_>>();
        let body_expr = parse_expr(
            &func.body,
            &param_bindings,
            &signatures,
            &records_map,
            &variants_map,
            ctx,
        )?;
        functions.push(Function {
            name: func.name,
            params: func.params,
            return_type: func.return_type,
            body: body_expr,
        });
    }

    Ok(Program {
        functions,
        imports,
        exports,
        globals,
        records,
        variants,
        resources,
        capabilities: capability_names,
        world_config,
        data_segments,
    })
}

/// Parse (world name (wit-deps "path") (import pkg/iface) ... (export pkg/iface) ...)
pub(crate) fn parse_world_form(items: &[SExpr], ctx: &CompileContext) -> Result<WorldConfig> {
    let span = items[0].span().clone();
    if items.len() < 2 {
        return Err(ctx.error_with_note(
            "invalid world declaration",
            &span,
            "expected: (world name (wit-deps \"path\") (import pkg/iface) ... (export pkg/iface) ...)",
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("world name must be a symbol", other.span())),
    };

    let mut wit_deps = None;
    let mut external_imports = Vec::new();
    let mut external_exports = Vec::new();

    for item in &items[2..] {
        match item {
            SExpr::List(sub_items, sub_span) => {
                if sub_items.is_empty() {
                    return Err(ctx.error("empty world clause", sub_span));
                }
                match &sub_items[0] {
                    SExpr::Sym(sym, _) if sym == "wit-deps" => {
                        if sub_items.len() != 2 {
                            return Err(ctx.error("wit-deps expects a path string", sub_span));
                        }
                        match &sub_items[1] {
                            SExpr::Sym(path, _) => {
                                // Allow unquoted path for convenience
                                wit_deps = Some(PathBuf::from(path));
                            }
                            other => {
                                // Try to extract string literal if parser supports it
                                return Err(ctx.error(
                                    "wit-deps path must be a string or symbol",
                                    other.span(),
                                ));
                            }
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "import" => {
                        if sub_items.len() != 2 {
                            return Err(
                                ctx.error("import expects an interface reference", sub_span)
                            );
                        }
                        match &sub_items[1] {
                            SExpr::Sym(iface_ref, _) => match ExternalInterface::parse(iface_ref) {
                                Some(ext) => external_imports.push(ext),
                                None => return Err(ctx.error(
                                    format!(
                                        "invalid interface reference '{}', expected 'pkg:ns/iface'",
                                        iface_ref
                                    ),
                                    sub_span,
                                )),
                            },
                            other => {
                                return Err(ctx
                                    .error("import expects an interface reference", other.span()));
                            }
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "export" => {
                        if sub_items.len() != 2 {
                            return Err(
                                ctx.error("export expects an interface reference", sub_span)
                            );
                        }
                        match &sub_items[1] {
                            SExpr::Sym(iface_ref, _) => match ExternalInterface::parse(iface_ref) {
                                Some(ext) => external_exports.push(ext),
                                None => return Err(ctx.error(
                                    format!(
                                        "invalid interface reference '{}', expected 'pkg:ns/iface'",
                                        iface_ref
                                    ),
                                    sub_span,
                                )),
                            },
                            other => {
                                return Err(ctx
                                    .error("export expects an interface reference", other.span()));
                            }
                        }
                    }
                    other => {
                        return Err(ctx.error_with_note(
                            "unknown world clause",
                            other.span(),
                            "expected 'wit-deps', 'import', or 'export'",
                        ));
                    }
                }
            }
            other => return Err(ctx.error("world clause must be a list", other.span())),
        }
    }

    Ok(WorldConfig {
        name,
        wit_deps,
        external_imports,
        external_exports,
    })
}

pub(crate) fn parse_fn_form(
    form: SExpr,
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<PendingFunction> {
    let (items, span) = match form {
        SExpr::List(items, span) => (items, span),
        other => return Err(ctx.error("function definition must be a list", other.span())),
    };
    match items.first() {
        Some(SExpr::Sym(s, _)) if s == "fn" => {}
        _ => return Err(ctx.error("function definition must start with 'fn'", &span)),
    }
    let shape = fn_shape(&items).ok_or_else(|| {
        ctx.error_with_note(
            "invalid function definition",
            &span,
            "expected: (fn name ((param : type) ...) [:] return-type body)",
        )
    })?;
    if shape.where_clause.is_some() {
        return Err(ctx.error(
            "generic functions (with a `where` clause) are not valid here",
            &span,
        ));
    }
    let name = match shape.name {
        SExpr::Sym(name, _) => name.clone(),
        other => return Err(ctx.error("function name must be a symbol", other.span())),
    };
    let params = parse_typed_params(shape.params, variant_names, resource_names, ctx)?;
    let return_type = parse_type_expr(shape.ret, variant_names, resource_names, ctx)?;
    let body = shape.body.clone();
    Ok(PendingFunction {
        name,
        params,
        return_type,
        body,
        span,
    })
}

pub(crate) fn parse_import_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Import> {
    let span = items[0].span().clone();
    // (import module name (params) [:] return-type)
    let ret_idx = match items.len() {
        5 => 4,
        6 if is_colon(&items[4]) => 5,
        _ => {
            return Err(ctx.error_with_note(
                "invalid import declaration",
                &span,
                "expected: (import module name ((param : type) ...) [:] return-type)",
            ));
        }
    };

    let module = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("import module must be a symbol", other.span())),
    };
    let name = match &items[2] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("import name must be a symbol", other.span())),
    };
    let params = parse_typed_params(&items[3], variant_names, resource_names, ctx)?;
    let return_type = parse_type_expr(&items[ret_idx], variant_names, resource_names, ctx)?;

    Ok(Import {
        module,
        name,
        params,
        return_type,
    })
}

pub(crate) fn parse_global_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Global> {
    let span = items[0].span().clone();
    // (global $name [:] type mutability init-value)
    let type_idx = match items.len() {
        5 => 2,
        6 if is_colon(&items[2]) => 3,
        _ => {
            return Err(ctx.error_with_note(
                "invalid global declaration",
                &span,
                "expected: (global $name [:] type mutability init-value)",
            ));
        }
    };
    let mut_idx = type_idx + 1;
    let init_idx = type_idx + 2;

    let name = match &items[1] {
        SExpr::Sym(s, sym_span) => {
            if !s.starts_with('$') {
                return Err(ctx.error_with_note(
                    "global name must start with '$'",
                    sym_span,
                    "e.g., $heap-ptr, $counter",
                ));
            }
            s.clone()
        }
        other => {
            return Err(ctx.error("global name must be a symbol starting with $", other.span()));
        }
    };

    let ty = parse_type_expr(&items[type_idx], variant_names, resource_names, ctx)?;

    let mutable = match &items[mut_idx] {
        SExpr::Sym(s, sym_span) => match s.as_str() {
            "mut" => true,
            "const" => false,
            _ => {
                return Err(ctx.error_with_note(
                    "invalid mutability specifier",
                    sym_span,
                    "expected 'mut' or 'const'",
                ));
            }
        },
        other => return Err(ctx.error("mutability must be 'mut' or 'const'", other.span())),
    };

    let init_value = match &items[init_idx] {
        SExpr::Int { value, .. } => *value,
        other => {
            return Err(ctx.error(
                "global init value must be an integer constant",
                other.span(),
            ));
        }
    };

    Ok(Global {
        name,
        ty,
        mutable,
        init_value,
    })
}

/// Parse a record definition: (record name (field1 type1) (field2 type2) ...)
pub(crate) fn parse_record_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<RecordDef> {
    let span = items[0].span().clone();

    if items.len() < 2 {
        return Err(ctx.error_with_note(
            "invalid record declaration",
            &span,
            "expected: (record name (field type) ...)",
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("record name must be a symbol", other.span())),
    };

    let mut fields = Vec::new();
    for item in &items[2..] {
        match item {
            SExpr::List(parts, field_span) => {
                if parts.len() != 2 {
                    return Err(ctx.error_with_note(
                        "invalid field declaration",
                        field_span,
                        "expected: (field-name type)",
                    ));
                }
                let field_name = match &parts[0] {
                    SExpr::Sym(s, _) => s.clone(),
                    other => return Err(ctx.error("field name must be a symbol", other.span())),
                };
                let field_ty = parse_type_expr(&parts[1], variant_names, resource_names, ctx)?;
                fields.push(RecordField {
                    name: field_name,
                    ty: field_ty,
                });
            }
            other => return Err(ctx.error("field must be a list (name type)", other.span())),
        }
    }

    if fields.is_empty() {
        return Err(ctx.error_with_note(
            "record must have at least one field",
            &span,
            "add fields like: (record point (x s32) (y s32))",
        ));
    }

    Ok(RecordDef { name, fields })
}

pub(crate) fn parse_variant_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<VariantDef> {
    let span = items[0].span().clone();

    if items.len() < 2 {
        return Err(ctx.error_with_note(
            "invalid variant declaration",
            &span,
            "expected: (variant name (case payload...) ...)",
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("variant name must be a symbol", other.span())),
    };

    let mut cases = Vec::new();
    for item in &items[2..] {
        match item {
            SExpr::List(parts, case_span) => {
                if parts.is_empty() {
                    return Err(ctx.error("variant case must have a name", case_span));
                }
                let case_name = match &parts[0] {
                    SExpr::Sym(s, _) => s.clone(),
                    other => return Err(ctx.error("case name must be a symbol", other.span())),
                };
                let mut payload = Vec::new();
                let mut payload_mult = Vec::new();
                for ty_expr in &parts[1..] {
                    // A `(lin T)` / `(aff T)` payload makes this slot linear/affine;
                    // the qualifier is erased at the Type level.
                    let mult = match ty_expr {
                        SExpr::List(q, _) if head_sym(q) == Some("lin") => Multiplicity::Lin,
                        SExpr::List(q, _) if head_sym(q) == Some("aff") => Multiplicity::Aff,
                        _ => Multiplicity::Un,
                    };
                    payload.push(parse_type_expr(
                        ty_expr,
                        variant_names,
                        resource_names,
                        ctx,
                    )?);
                    payload_mult.push(mult);
                }
                cases.push(VariantCase {
                    name: case_name,
                    payload,
                    payload_mult,
                });
            }
            other => {
                return Err(ctx.error(
                    "variant case must be a list (case-name type...)",
                    other.span(),
                ));
            }
        }
    }

    if cases.is_empty() {
        return Err(ctx.error_with_note(
            "variant must have at least one case",
            &span,
            "add cases like: (variant shape (circle s32) (rectangle s32 s32) (point))",
        ));
    }

    Ok(VariantDef { name, cases })
}

pub(crate) fn parse_resource_form(items: &[SExpr], ctx: &CompileContext) -> Result<ResourceDef> {
    let span = items[0].span().clone();

    if items.len() != 2 {
        return Err(ctx.error_with_note(
            "invalid resource declaration",
            &span,
            "expected: (resource name)",
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("resource name must be a symbol", other.span())),
    };

    Ok(ResourceDef { name })
}

pub(crate) fn parse_typed_params(
    expr: &SExpr,
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Vec<Parameter>> {
    match expr {
        SExpr::List(params, _) => {
            let mut result = Vec::new();
            for p in params {
                match p {
                    SExpr::List(parts, param_span) => {
                        // Accept both `(name type)` and `(name : type)`.
                        let type_expr = match parts.len() {
                            2 => &parts[1],
                            3 if matches!(&parts[1], SExpr::Sym(s, _) if s == ":") => &parts[2],
                            _ => {
                                return Err(ctx.error_with_note(
                                    "invalid parameter",
                                    param_span,
                                    "expected: (name type) or (name : type)",
                                ));
                            }
                        };
                        let (name, scopes) = match &parts[0] {
                            SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                            other => {
                                return Err(
                                    ctx.error("parameter name must be a symbol", other.span())
                                );
                            }
                        };
                        // A `(lin T)` / `(aff T)` parameter type sets its multiplicity.
                        // The qualifier is erased at the Type level; `parse_type_expr`
                        // yields the underlying `T`.
                        let mult = match type_expr {
                            SExpr::List(q, _) if head_sym(q) == Some("lin") => Multiplicity::Lin,
                            SExpr::List(q, _) if head_sym(q) == Some("aff") => Multiplicity::Aff,
                            _ => Multiplicity::Un,
                        };
                        let ty = parse_type_expr(type_expr, variant_names, resource_names, ctx)?;
                        result.push(Parameter {
                            name,
                            ty,
                            scopes,
                            mult,
                        });
                    }
                    other => {
                        return Err(ctx.error_with_note(
                            "invalid parameter",
                            other.span(),
                            "expected: (name type)",
                        ));
                    }
                }
            }
            Ok(result)
        }
        other => Err(ctx.error("expected parameter list", other.span())),
    }
}

pub(crate) fn parse_type_expr(
    expr: &SExpr,
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Type> {
    match expr {
        SExpr::Sym(s, span) => parse_type_symbol(s, variant_names, resource_names, span, ctx),
        SExpr::List(items, span) => {
            if items.is_empty() {
                return Err(ctx.error("empty type expression", span));
            }
            match &items[0] {
                SExpr::Sym(s, _) if s == "option" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid option type",
                            span,
                            "expected: (option T)",
                        ));
                    }
                    let inner = parse_type_expr(&items[1], variant_names, resource_names, ctx)?;
                    Ok(Type::Option(Box::new(inner)))
                }
                SExpr::Sym(s, _) if s == "result" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid result type",
                            span,
                            "expected: (result T E)",
                        ));
                    }
                    let ok_ty = parse_type_expr(&items[1], variant_names, resource_names, ctx)?;
                    let err_ty = parse_type_expr(&items[2], variant_names, resource_names, ctx)?;
                    Ok(Type::Result(Box::new(ok_ty), Box::new(err_ty)))
                }
                SExpr::Sym(s, _) if s == "list" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid list type",
                            span,
                            "expected: (list T)",
                        ));
                    }
                    let inner = parse_type_expr(&items[1], variant_names, resource_names, ctx)?;
                    Ok(Type::List(Box::new(inner)))
                }
                SExpr::Sym(s, _) if s == "borrow" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid borrow type",
                            span,
                            "expected: (borrow T)",
                        ));
                    }
                    let inner = parse_type_expr(&items[1], variant_names, resource_names, ctx)?;
                    Ok(Type::Borrow(Box::new(inner)))
                }
                SExpr::Sym(s, _) if s == "tuple" => {
                    if items.len() < 2 {
                        return Err(ctx.error_with_note(
                            "invalid tuple type",
                            span,
                            "expected: (tuple T1 T2 ...)",
                        ));
                    }
                    let elems: Vec<Type> = items[1..]
                        .iter()
                        .map(|item| parse_type_expr(item, variant_names, resource_names, ctx))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(Type::Tuple(elems))
                }
                // Multiplicity qualifier: `(lin T)` is `T` with a use-exactly-once
                // obligation enforced by the linearity checker; the qualifier is
                // erased at the Type level (same runtime representation as `T`).
                // Multiplicity qualifiers `(lin T)` / `(aff T)`: erased at the Type
                // level (same representation as `T`); the obligation is enforced by
                // the linearity checker from the binding's recorded multiplicity.
                SExpr::Sym(s, _) if s == "lin" || s == "aff" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid multiplicity-qualified type",
                            span,
                            "expected: (lin T) or (aff T)",
                        ));
                    }
                    parse_type_expr(&items[1], variant_names, resource_names, ctx)
                }
                _ => Err(ctx.error("unknown parameterized type", span)),
            }
        }
        other => Err(ctx.error("type must be a symbol or parameterized type", other.span())),
    }
}

pub(crate) fn parse_type_symbol(
    sym: &str,
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    _span: &Span,
    _ctx: &CompileContext,
) -> Result<Type> {
    match sym {
        "s32" => Ok(Type::S32),
        "s64" => Ok(Type::S64),
        "f32" => Ok(Type::F32),
        "f64" => Ok(Type::F64),
        "u8" => Ok(Type::U8),
        "bool" => Ok(Type::Bool),
        "u16" => Ok(Type::U16),
        "u32" => Ok(Type::U32),
        "u64" => Ok(Type::U64),
        "string" => Ok(Type::Str),
        "any" => Ok(Type::Any),            // Pack dynamic `value`
        "unit" => Ok(Type::Tuple(vec![])), // unit type is empty tuple
        // Check if this is a variant type name
        other if variant_names.contains(other) => Ok(Type::Variant(other.to_string())),
        // Check if this is a resource type name
        other if resource_names.contains(other) => Ok(Type::Resource(other.to_string())),
        // Otherwise treat as a record type name
        // We'll validate that the record actually exists during type checking
        other => Ok(Type::Record(other.to_string())),
    }
}

pub(crate) fn is_type_symbol(sym: &str) -> bool {
    matches!(
        sym,
        "s32"
            | "s64"
            | "f32"
            | "f64"
            | "u8"
            | "bool"
            | "u16"
            | "u32"
            | "u64"
            | "string"
            | "any"
            | "unit"
    )
}

pub(crate) fn parse_expr(
    sexpr: &SExpr,
    vars: &[Binding],
    functions: &HashMap<String, Signature>,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
    ctx: &CompileContext,
) -> Result<Expr> {
    match sexpr {
        SExpr::Int { value, ty, .. } => Ok(Expr::Int {
            value: *value,
            ty: ty.clone(),
        }),
        SExpr::Float { value, ty, .. } => Ok(Expr::Float {
            value: *value,
            ty: ty.clone(),
        }),
        SExpr::Str(s, _) => Ok(Expr::StringLiteral(s.clone())),
        SExpr::Sym(s, span) => {
            // Hygienic variable resolution: find bindings with matching name
            // where the binding's scopes are a subset of the reference's scopes
            let ref_scopes = &span.scopes;

            let matching_bindings: Vec<_> = vars
                .iter()
                .filter(|b| b.name == *s && b.is_visible_from(ref_scopes))
                .collect();

            match matching_bindings.len() {
                0 => Err(ctx.error(format!("unknown variable '{}'", s), span)),
                1 => {
                    // Use mangled name to preserve scope distinction in codegen
                    Ok(Expr::Var(matching_bindings[0].mangled_name()))
                }
                _ => {
                    // Multiple matching bindings - find the most specific one
                    // (the one with the most scopes that is still a subset)
                    let best = matching_bindings
                        .iter()
                        .max_by_key(|b| b.scopes.scopes.len())
                        .unwrap();
                    // Use mangled name to preserve scope distinction in codegen
                    Ok(Expr::Var(best.mangled_name()))
                }
            }
        }
        SExpr::List(items, list_span) => {
            if items.is_empty() {
                return Err(ctx.error("empty list is not a valid expression", list_span));
            }
            // Ascription in colon form: (expr : type), mirroring the head form.
            // Accepts any type (scalar OR compound): a same-type ascription is an
            // identity annotation; a differing scalar ascription is a cast.
            if items.len() == 3 && is_colon(&items[1]) {
                let variant_names: HashSet<String> = variants.keys().cloned().collect();
                let ty = parse_type_expr(&items[2], &variant_names, &HashSet::new(), ctx)?;
                let inner = parse_expr(&items[0], vars, functions, records, variants, ctx)?;
                return Ok(Expr::Ascribe {
                    expr: Box::new(inner),
                    ty,
                });
            }
            // Create variant name set for type parsing
            let variant_names: HashSet<String> = variants.keys().cloned().collect();
            let op = &items[0];
            match op {
                SExpr::Sym(sym, sym_span) if is_type_symbol(sym) && items.len() == 2 => {
                    // Numeric casts emit conversion instructions.
                    let numeric = match sym.as_str() {
                        "s32" => Some(Type::S32),
                        "s64" => Some(Type::S64),
                        "f32" => Some(Type::F32),
                        "f64" => Some(Type::F64),
                        _ => None,
                    };
                    if let Some(ty) = numeric {
                        let inner = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                        return Ok(Expr::Ascribe {
                            expr: Box::new(inner),
                            ty,
                        });
                    }
                    // The first-class integer/bool scalars have no literal syntax of
                    // their own; `(bool 1)` / `(u32 100)` retypes an integer literal.
                    let scalar = match sym.as_str() {
                        "u8" => Some(Type::U8),
                        "u16" => Some(Type::U16),
                        "u32" => Some(Type::U32),
                        "u64" => Some(Type::U64),
                        "bool" => Some(Type::Bool),
                        _ => None,
                    };
                    if let Some(ty) = scalar {
                        // An integer literal retypes in place; a runtime value
                        // casts via Ascribe (masking/extension as needed).
                        if let SExpr::Int { value, .. } = &items[1] {
                            return Ok(Expr::Int { value: *value, ty });
                        }
                        let inner = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                        return Ok(Expr::Ascribe {
                            expr: Box::new(inner),
                            ty,
                        });
                    }
                    Err(ctx.error_with_note(
                        format!("cannot cast to '{sym}'"),
                        sym_span,
                        "casts apply to scalar types (s32/s64/f32/f64/u8/u16/u32/u64/bool)",
                    ))
                }
                SExpr::Sym(sym, sym_span) if sym == "if" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'if' expression",
                            list_span,
                            "expected: (if condition then-expr else-expr)",
                        ));
                    }
                    let cond = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let then_branch =
                        parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    let else_branch =
                        parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::If {
                        cond: Box::new(cond),
                        then_branch: Box::new(then_branch),
                        else_branch: Box::new(else_branch),
                    })
                }
                SExpr::Sym(sym, sym_span) if sym == "global.get" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'global.get' expression",
                            list_span,
                            "expected: (global.get $name)",
                        ));
                    }
                    let name = match &items[1] {
                        SExpr::Sym(s, s_span) => {
                            if !s.starts_with('$') {
                                return Err(ctx.error("global name must start with '$'", s_span));
                            }
                            s.clone()
                        }
                        other => {
                            return Err(ctx.error(
                                "global.get argument must be a global name starting with $",
                                other.span(),
                            ));
                        }
                    };
                    Ok(Expr::GlobalGet { name })
                }
                SExpr::Sym(sym, sym_span) if sym == "global.set" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'global.set' expression",
                            list_span,
                            "expected: (global.set $name value)",
                        ));
                    }
                    let name = match &items[1] {
                        SExpr::Sym(s, s_span) => {
                            if !s.starts_with('$') {
                                return Err(ctx.error("global name must start with '$'", s_span));
                            }
                            s.clone()
                        }
                        other => {
                            return Err(ctx.error(
                                "global.set first argument must be a global name starting with $",
                                other.span(),
                            ));
                        }
                    };
                    let value = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::GlobalSet {
                        name,
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, sym_span) if sym == "let" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'let' expression",
                            list_span,
                            "expected: (let (name value) body)",
                        ));
                    }
                    let binding = match &items[1] {
                        SExpr::List(parts, _) => parts,
                        other => {
                            return Err(ctx.error_with_note(
                                "let binding must be a list",
                                other.span(),
                                "expected: (name value) or (name : type value)",
                            ));
                        }
                    };
                    // Accept `(name value)`, `(name : scalar value)` (a cast), and
                    // `(name : (lin T) value)` / `(name : (aff T) value)` (a linear/
                    // affine binding — the qualifier marks multiplicity, no cast).
                    let (name_sexpr, value_sexpr, annotation, explicit_mult) = match binding.len() {
                        2 => (&binding[0], &binding[1], None, None),
                        4 if is_colon(&binding[1]) => match &binding[2] {
                            SExpr::List(q, _) if head_sym(q) == Some("lin") => {
                                (&binding[0], &binding[3], None, Some(Multiplicity::Lin))
                            }
                            SExpr::List(q, _) if head_sym(q) == Some("aff") => {
                                (&binding[0], &binding[3], None, Some(Multiplicity::Aff))
                            }
                            SExpr::Sym(t, _) if is_type_symbol(t) => {
                                let ty = match t.as_str() {
                                    "s32" => Type::S32,
                                    "s64" => Type::S64,
                                    "f32" => Type::F32,
                                    "f64" => Type::F64,
                                    _ => {
                                        return Err(ctx.error_with_note(
                                                "let scalar cast supports s32/s64/f32/f64",
                                                binding[2].span(),
                                                "for other types use (name : (lin T) value) or no annotation",
                                            ));
                                    }
                                };
                                (&binding[0], &binding[3], Some(ty), None)
                            }
                            other => {
                                return Err(ctx.error_with_note(
                                    "let type annotation must be a scalar type or (lin T)/(aff T)",
                                    other.span(),
                                    "e.g. (name : s32 value) or (name : (lin T) value)",
                                ));
                            }
                        },
                        _ => {
                            return Err(ctx.error_with_note(
                                "invalid let binding",
                                items[1].span(),
                                "expected: (name value) or (name : type value)",
                            ));
                        }
                    };
                    let (name, name_scopes) = match name_sexpr {
                        SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                        other => {
                            return Err(
                                ctx.error("let binding name must be a symbol", other.span())
                            );
                        }
                    };
                    let mut value_expr =
                        parse_expr(value_sexpr, vars, functions, records, variants, ctx)?;
                    // A colon annotation ascribes the value to the declared type.
                    if let Some(ty) = annotation {
                        value_expr = Expr::Ascribe {
                            expr: Box::new(value_expr),
                            ty,
                        };
                    }
                    // Multiplicity: an explicit `(lin/aff T)` wins; otherwise infer it
                    // from the value (a linear-variant constructor or a function
                    // returning one makes the binding linear — infectious linearity).
                    let mult = explicit_mult
                        .unwrap_or_else(|| value_multiplicity(value_sexpr, functions, variants));
                    // Create a new binding with the name and its scopes for hygienic resolution
                    let new_binding = Binding::new(name, name_scopes);
                    let mangled_name = new_binding.mangled_name();
                    let mut next_vars = vars.to_vec();
                    next_vars.push(new_binding);
                    let body_expr =
                        parse_expr(&items[2], &next_vars, functions, records, variants, ctx)?;
                    Ok(Expr::Let {
                        name: mangled_name, // Use mangled name for codegen
                        value: Box::new(value_expr),
                        body: Box::new(body_expr),
                        mult,
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "with-cap" => {
                    // (with-cap (c Cap) body): bind c to a fresh linear capability
                    // token of type Cap for the scope of body.
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'with-cap' expression",
                            list_span,
                            "expected: (with-cap (name Cap) body)",
                        ));
                    }
                    let binding = match &items[1] {
                        SExpr::List(parts, _) if parts.len() == 2 => parts,
                        other => {
                            return Err(ctx.error_with_note(
                                "with-cap binding must be (name Cap)",
                                other.span(),
                                "expected: (with-cap (name Cap) body)",
                            ));
                        }
                    };
                    let (name, name_scopes) = match &binding[0] {
                        SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                        other => {
                            return Err(ctx.error("capability name must be a symbol", other.span()));
                        }
                    };
                    let cap = match &binding[1] {
                        SExpr::Sym(s, _) => s.clone(),
                        other => {
                            return Err(ctx.error("capability type must be a symbol", other.span()));
                        }
                    };
                    let new_binding = Binding::new(name, name_scopes);
                    let mangled_name = new_binding.mangled_name();
                    let mut next_vars = vars.to_vec();
                    next_vars.push(new_binding);
                    let body_expr =
                        parse_expr(&items[2], &next_vars, functions, records, variants, ctx)?;
                    Ok(Expr::WithCap {
                        name: mangled_name,
                        cap,
                        body: Box::new(body_expr),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "release-cap" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'release-cap' expression",
                            list_span,
                            "expected: (release-cap capability)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::ReleaseCap {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, sym_span) if sym == "&" => {
                    // (& c): borrow the capability bound to c without consuming it.
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid borrow expression",
                            list_span,
                            "expected: (& capability-variable)",
                        ));
                    }
                    let inner = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    match inner {
                        Expr::Var(name) => Ok(Expr::BorrowCap { name }),
                        _ => Err(ctx.error_with_note(
                            "borrow expects a capability variable",
                            sym_span,
                            "expected: (& c) where c is a capability bound by with-cap",
                        )),
                    }
                }
                SExpr::Sym(sym, _sym_span) if sym == "begin" => {
                    if items.len() < 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'begin' expression",
                            list_span,
                            "expected: (begin expr1 expr2 ...)",
                        ));
                    }
                    let mut exprs = Vec::new();
                    for item in &items[1..] {
                        exprs.push(parse_expr(item, vars, functions, records, variants, ctx)?);
                    }
                    Ok(Expr::Begin { exprs })
                }
                SExpr::Sym(sym, _sym_span) if sym == "match" => {
                    // (match expr ((case var1 var2) body) ...)
                    if items.len() < 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'match' expression",
                            list_span,
                            "expected: (match expr ((case-name bindings...) body) ...)",
                        ));
                    }
                    let match_expr =
                        parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let mut arms = Vec::new();

                    for case_item in &items[2..] {
                        let case_parts = match case_item {
                            SExpr::List(parts, _) => parts,
                            other => {
                                return Err(ctx.error("match arm must be a list", other.span()));
                            }
                        };

                        if case_parts.len() != 2 {
                            return Err(ctx.error_with_note(
                                "invalid match arm",
                                case_item.span(),
                                "expected: ((case-name bindings...) body)",
                            ));
                        }

                        // Parse pattern: (case-name binding1 binding2 ...)
                        let pattern = match &case_parts[0] {
                            SExpr::List(pat_parts, _) => pat_parts,
                            other => {
                                return Err(ctx.error("match pattern must be a list", other.span()));
                            }
                        };

                        if pattern.is_empty() {
                            return Err(
                                ctx.error("match pattern cannot be empty", case_parts[0].span())
                            );
                        }

                        let case_name = match &pattern[0] {
                            SExpr::Sym(s, _) => s.clone(),
                            other => {
                                return Err(ctx.error("case name must be a symbol", other.span()));
                            }
                        };

                        // Collect bindings
                        let mut bindings = Vec::new();
                        let mut next_vars = vars.to_vec();
                        for binding in &pattern[1..] {
                            let (name, name_scopes) = match binding {
                                SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                                other => {
                                    return Err(ctx.error("binding must be a symbol", other.span()));
                                }
                            };
                            let new_binding = Binding::new(name, name_scopes);
                            let mangled_name = new_binding.mangled_name();
                            bindings.push(mangled_name);
                            next_vars.push(new_binding);
                        }

                        // Parse body with extended bindings
                        let body = parse_expr(
                            &case_parts[1],
                            &next_vars,
                            functions,
                            records,
                            variants,
                            ctx,
                        )?;

                        // The bindings' multiplicities come from the matched case's
                        // payload (a `(lin T)` payload binds a linear value). Unknown
                        // cases (`_`, option/result) carry unrestricted bindings.
                        let binding_mult = find_variant_by_case(&case_name, variants)
                            .and_then(|vd| {
                                vd.find_case(&case_name)
                                    .map(|(_, c)| c.payload_mult.clone())
                            })
                            .filter(|m| m.len() == bindings.len())
                            .unwrap_or_else(|| vec![Multiplicity::Un; bindings.len()]);
                        arms.push(MatchArm {
                            case_name,
                            bindings,
                            binding_mult,
                            body,
                        });
                    }

                    Ok(Expr::Match {
                        expr: Box::new(match_expr),
                        cases: arms,
                    })
                }
                // Option constructors: (some T value) and (none T)
                SExpr::Sym(sym, _sym_span) if sym == "some" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'some' expression",
                            list_span,
                            "expected: (some inner-type value)",
                        ));
                    }
                    let inner_type =
                        parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    let value = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Some {
                        inner_type,
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "none" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'none' expression",
                            list_span,
                            "expected: (none inner-type)",
                        ));
                    }
                    let inner_type =
                        parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    Ok(Expr::None { inner_type })
                }
                // Result constructors: (ok T E value) and (err T E value)
                SExpr::Sym(sym, _sym_span) if sym == "ok" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'ok' expression",
                            list_span,
                            "expected: (ok ok-type err-type value)",
                        ));
                    }
                    let ok_type = parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    let err_type =
                        parse_type_expr(&items[2], &variant_names, &HashSet::new(), ctx)?;
                    let value = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Ok {
                        ok_type,
                        err_type,
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "err" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'err' expression",
                            list_span,
                            "expected: (err ok-type err-type value)",
                        ));
                    }
                    let ok_type = parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    let err_type =
                        parse_type_expr(&items[2], &variant_names, &HashSet::new(), ctx)?;
                    let value = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Err {
                        ok_type,
                        err_type,
                        value: Box::new(value),
                    })
                }
                // Tuple constructor: (tuple expr1 expr2 ...)
                SExpr::Sym(sym, _sym_span) if sym == "tuple" => {
                    if items.len() < 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'tuple' expression",
                            list_span,
                            "expected: (tuple value1 value2 ...)",
                        ));
                    }
                    let values: Vec<Expr> = items[1..]
                        .iter()
                        .map(|item| parse_expr(item, vars, functions, records, variants, ctx))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(Expr::TupleConstruct { values })
                }
                // List operations
                SExpr::Sym(sym, _sym_span) if sym == "list-new" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-new' expression",
                            list_span,
                            "expected: (list-new elem-type)",
                        ));
                    }
                    let elem_type =
                        parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    Ok(Expr::ListNew { elem_type })
                }
                SExpr::Sym(sym, _sym_span) if sym == "list-push" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-push' expression",
                            list_span,
                            "expected: (list-push list value)",
                        ));
                    }
                    let list = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let value = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::ListPush {
                        list: Box::new(list),
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "list-get" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-get' expression",
                            list_span,
                            "expected: (list-get list index)",
                        ));
                    }
                    let list = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let index = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::ListGet {
                        list: Box::new(list),
                        index: Box::new(index),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "list-len" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-len' expression",
                            list_span,
                            "expected: (list-len list)",
                        ));
                    }
                    let list = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::ListLen {
                        list: Box::new(list),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-len" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-len' expression",
                            list_span,
                            "expected: (string-len string)",
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringLen {
                        string: Box::new(string),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-ref" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-ref' expression",
                            list_span,
                            "expected: (string-ref string index)",
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let index = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringRef {
                        string: Box::new(string),
                        index: Box::new(index),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "substring" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'substring' expression",
                            list_span,
                            "expected: (substring string start end)",
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let start = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    let end = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Substring {
                        string: Box::new(string),
                        start: Box::new(start),
                        end: Box::new(end),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-s32" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-s32' expression",
                            list_span,
                            "expected: (any-s32 s32-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyFromS32 {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-as-s32" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-as-s32' expression",
                            list_span,
                            "expected: (any-as-s32 any-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyToS32 {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-string" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-string' expression",
                            list_span,
                            "expected: (any-string string-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyFromString {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-as-string" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-as-string' expression",
                            list_span,
                            "expected: (any-as-string any-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyToString {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "heap-alloc" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'heap-alloc' expression",
                            list_span,
                            "expected: (heap-alloc size-expr)",
                        ));
                    }
                    let size = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::HeapAlloc {
                        size: Box::new(size),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-addr" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-addr' expression",
                            list_span,
                            "expected: (any-addr any-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyAddr {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-from-addr" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-from-addr' expression",
                            list_span,
                            "expected: (any-from-addr addr-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyFromAddr {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-addr" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-addr' expression",
                            list_span,
                            "expected: (string-addr string-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringAddr {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-from-addr" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-from-addr' expression",
                            list_span,
                            "expected: (string-from-addr addr-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringFromAddr {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "call-raw" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'call-raw' expression",
                            list_span,
                            "expected: (call-raw args-any)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::RawInvoke {
                        module: "theater:simple/rpc".to_string(),
                        import: "call".to_string(),
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "raw-invoke" => {
                    // (raw-invoke "iface" "name" args-any) -> any. Generic raw-CGRF
                    // call to any declared host import; args-any is a marshalled blob.
                    // The interface qualifies the raw symbol (so same-named functions
                    // in different interfaces don't collide).
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'raw-invoke' expression",
                            list_span,
                            "expected: (raw-invoke \"iface\" \"name\" args-any)",
                        ));
                    }
                    let (SExpr::Str(module, _), SExpr::Str(import, _)) = (&items[1], &items[2])
                    else {
                        return Err(ctx.error_with_note(
                            "raw-invoke interface and name must be string literals",
                            list_span,
                            "expected: (raw-invoke \"iface\" \"name\" args-any)",
                        ));
                    };
                    let value = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::RawInvoke {
                        module: module.clone(),
                        import: import.clone(),
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-append" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-append' expression",
                            list_span,
                            "expected: (string-append string1 string2)",
                        ));
                    }
                    let left = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let right = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringAppend {
                        left: Box::new(left),
                        right: Box::new(right),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string=?" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'string=?' expression",
                            list_span,
                            "expected: (string=? string1 string2)",
                        ));
                    }
                    let left = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let right = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringEq {
                        left: Box::new(left),
                        right: Box::new(right),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-from-bytes" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-from-bytes' expression",
                            list_span,
                            "expected: (string-from-bytes bytes)",
                        ));
                    }
                    let bytes = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringFromBytes {
                        bytes: Box::new(bytes),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-to-bytes" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-to-bytes' expression",
                            list_span,
                            "expected: (string-to-bytes string)",
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringToBytes {
                        string: Box::new(string),
                    })
                }
                _ => {
                    if let SExpr::Sym(sym, sym_span) = op {
                        // Check if this is a WASM instruction
                        if lookup_wasm_instr(sym).is_some() {
                            let mut args = Vec::new();
                            for arg in &items[1..] {
                                args.push(parse_expr(
                                    arg, vars, functions, records, variants, ctx,
                                )?);
                            }
                            Ok(Expr::WasmInstr {
                                name: sym.clone(),
                                args,
                            })
                        } else if let Some(expected) = functions.get(sym) {
                            // Function call
                            if items.len() - 1 != expected.params.len() {
                                return Err(ctx.error(
                                    format!(
                                        "function '{}' expects {} arguments, got {}",
                                        sym,
                                        expected.params.len(),
                                        items.len() - 1
                                    ),
                                    list_span,
                                ));
                            }
                            let mut args = Vec::new();
                            for arg in &items[1..] {
                                args.push(parse_expr(
                                    arg, vars, functions, records, variants, ctx,
                                )?);
                            }
                            Ok(Expr::Call {
                                name: sym.clone(),
                                args,
                            })
                        } else if let Some(record_def) = records.get(sym) {
                            // Record construction: (point 10 20)
                            if items.len() - 1 != record_def.fields.len() {
                                return Err(ctx.error(
                                    format!(
                                        "record '{}' expects {} fields, got {}",
                                        sym,
                                        record_def.fields.len(),
                                        items.len() - 1
                                    ),
                                    list_span,
                                ));
                            }
                            let mut fields = Vec::new();
                            for arg in &items[1..] {
                                fields.push(parse_expr(
                                    arg, vars, functions, records, variants, ctx,
                                )?);
                            }
                            Ok(Expr::RecordConstruct {
                                record_name: sym.clone(),
                                fields,
                            })
                        } else if let Some(variant_def) = find_variant_by_case(sym, variants) {
                            // Variant case construction: (circle 5) or (point)
                            let (_, case) = variant_def.find_case(sym).unwrap();
                            if items.len() - 1 != case.payload.len() {
                                return Err(ctx.error(
                                    format!(
                                        "variant case '{}' expects {} payload values, got {}",
                                        sym,
                                        case.payload.len(),
                                        items.len() - 1
                                    ),
                                    list_span,
                                ));
                            }
                            let mut payload = Vec::new();
                            for arg in &items[1..] {
                                payload.push(parse_expr(
                                    arg, vars, functions, records, variants, ctx,
                                )?);
                            }
                            Ok(Expr::VariantConstruct {
                                variant_name: variant_def.name.clone(),
                                case_name: sym.clone(),
                                payload,
                            })
                        } else if sym.contains('.') {
                            // Check for record field access: (point.x expr)
                            let parts: Vec<&str> = sym.splitn(2, '.').collect();
                            if parts.len() == 2 {
                                let record_name = parts[0];
                                let field_name = parts[1];
                                if let Some(_record_def) = records.get(record_name) {
                                    if items.len() != 2 {
                                        return Err(ctx.error_with_note(
                                            "invalid field access",
                                            list_span,
                                            format!(
                                                "expected: ({}.{} record-expr)",
                                                record_name, field_name
                                            ),
                                        ));
                                    }
                                    let expr = parse_expr(
                                        &items[1], vars, functions, records, variants, ctx,
                                    )?;
                                    Ok(Expr::RecordAccess {
                                        record_name: record_name.to_string(),
                                        field_name: field_name.to_string(),
                                        expr: Box::new(expr),
                                    })
                                } else {
                                    Err(ctx.error(
                                        format!("unknown record type '{}'", record_name),
                                        sym_span,
                                    ))
                                }
                            } else {
                                Err(ctx.error(
                                    format!("unknown function or operator '{}'", sym),
                                    sym_span,
                                ))
                            }
                        } else {
                            Err(ctx
                                .error(format!("unknown function or operator '{}'", sym), sym_span))
                        }
                    } else {
                        Err(ctx.error("expression must start with a symbol", op.span()))
                    }
                }
            }
        }
        SExpr::Quasiquote(_, span) | SExpr::Unquote(_, span) | SExpr::UnquoteSplice(_, span) => {
            Err(ctx.error(
                "quasiquote/unquote should have been expanded before parsing",
                span,
            ))
        }
        SExpr::SyntaxQuote(_, span)
        | SExpr::Quasisyntax(_, span)
        | SExpr::Unsyntax(_, span)
        | SExpr::UnsyntaxSplice(_, span) => Err(ctx.error(
            "syntax forms (#', #`, #,, #,@) should have been expanded before parsing",
            span,
        )),
    }
}
