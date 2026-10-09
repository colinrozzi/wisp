use super::*;

// Code generation: typed AST -> WAT, the WIT/Pact interface views, and the
// CGRF/Pack encode+decode glue. Downstream of parse/lower/type-check.
pub(crate) fn generate_wat(prog: &Program, signatures: &HashMap<String, Signature>) -> String {
    let mut out = String::new();
    out.push_str("(module\n");

    // Imports must come first in WAT
    for import in &prog.imports {
        out.push_str(&format!(
            "  (import \"{}\" \"{}\" (func ${} ",
            import.module, import.name, import.name
        ));
        for param in &import.params {
            out.push_str(&format!("(param ${} {}) ", param.name, wat_type(&param.ty)));
        }
        let result_clause = emit_wat_result(&import.return_type);
        if result_clause.is_empty() {
            out.push_str("))\n");
        } else {
            out.push_str(&format!("{})))\n", result_clause));
        }
    }

    // Declare memory (500 pages = 32MB, allow growth up to 1000 pages = 64MB)
    // Larger initial size needed for programs that do heavy string/list allocation
    out.push_str("  (memory 4000 4000)\n");

    // Emit data segments
    for seg in &prog.data_segments {
        out.push_str(&format!("  (data (i32.const {}) \"", seg.offset));
        for byte in &seg.bytes {
            out.push_str(&format!("\\{:02x}", byte));
        }
        out.push_str("\")\n");
    }

    // Build global type map for codegen
    let mut globals_map = HashMap::new();
    for global in &prog.globals {
        globals_map.insert(global.name.clone(), (global.ty.clone(), global.mutable));
    }

    // Build records and variants maps for codegen
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

    // Add heap pointer global for bump allocation (if we have records, variants, or parameterized types)
    let needs_heap = !prog.records.is_empty()
        || !prog.variants.is_empty()
        || prog.functions.iter().any(|f| {
            type_needs_heap(&f.return_type)
                || f.params.iter().any(|p| type_needs_heap(&p.ty))
                || expr_uses_heap(&f.body)
        });
    if needs_heap {
        // Start heap at byte 0 (first page of memory)
        out.push_str("  (global $__heap_ptr (mut i32) (i32.const 0))\n");
    }

    // Declare user globals
    for global in &prog.globals {
        let mutability = if global.mutable { "(mut " } else { "" };
        let close = if global.mutable { ")" } else { "" };
        out.push_str(&format!(
            "  (global {} {}{}{} ({}.const {}))\n",
            global.name,
            mutability,
            wat_type(&global.ty),
            close,
            wat_type(&global.ty),
            global.init_value
        ));
    }

    // Generate functions
    for func in &prog.functions {
        let is_exported = prog.exports.iter().any(|e| e.func_name == func.name);
        let needs_wrapper = is_exported && function_needs_abi_wrapper(func);

        // If this function is exported and needs a wrapper, name the internal function differently
        let internal_name = if needs_wrapper {
            format!("{}__internal", func.name)
        } else {
            func.name.clone()
        };

        let mut body = String::new();
        let mut env = CodegenEnv::new(&func.params);
        gen_expr(
            &func.body,
            &mut body,
            4,
            &mut env,
            signatures,
            &globals_map,
            &records_map,
            &variants_map,
            true, // Function body is in tail position
        );

        out.push_str(&format!("  (func ${} ", internal_name));
        for param in &func.params {
            out.push_str(&format!("(param ${} {}) ", param.name, wat_type(&param.ty)));
        }
        let result_clause = emit_wat_result(&func.return_type);
        if result_clause.is_empty() {
            out.push('\n');
        } else {
            out.push_str(&format!("{}\n", result_clause));
        }
        for local in &env.locals {
            out.push_str(&format!("    (local {})\n", wat_type(local)));
        }
        out.push_str(&body);
        out.push_str("  )\n");

        // Generate ABI wrapper if needed
        if needs_wrapper {
            generate_abi_wrapper(&mut out, func, &records_map, &variants_map);
        }
    }
    // Generate cabi_realloc for component model (required for strings/lists crossing component boundary)
    // cabi_realloc(old_ptr: i32, old_size: i32, align: i32, new_size: i32) -> i32
    if needs_heap {
        out.push_str("  (func $cabi_realloc (param $old_ptr i32) (param $old_size i32) (param $align i32) (param $new_size i32) (result i32)\n");
        out.push_str("    (local $ptr i32)\n");
        // Simple bump allocation - ignore old_ptr/old_size (no reuse), just allocate new_size bytes
        // Align the heap pointer to the requested alignment
        out.push_str("    global.get $__heap_ptr\n");
        out.push_str("    local.get $align\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.sub\n");
        out.push_str("    local.get $align\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.sub\n");
        out.push_str("    i32.const -1\n");
        out.push_str("    i32.xor\n");
        out.push_str("    i32.and\n");
        out.push_str("    local.set $ptr\n");
        // Bump the heap pointer
        out.push_str("    local.get $ptr\n");
        out.push_str("    local.get $new_size\n");
        out.push_str("    i32.add\n");
        out.push_str("    global.set $__heap_ptr\n");
        // Return the pointer
        out.push_str("    local.get $ptr\n");
        out.push_str("  )\n");
    }

    for export in &prog.exports {
        out.push_str(&format!(
            "  (export \"{}\" (func ${}))\n",
            export.export_name, export.func_name
        ));
    }

    // Export memory for component model
    out.push_str("  (export \"memory\" (memory 0))\n");

    // Export cabi_realloc for component model
    if needs_heap {
        out.push_str("  (export \"cabi_realloc\" (func $cabi_realloc))\n");
    }

    out.push_str(")\n");
    out
}

// Expression emission threads distinct lexical and type environments recursively.
#[allow(clippy::too_many_arguments)]
pub(crate) fn gen_expr(
    expr: &Expr,
    out: &mut String,
    indent: usize,
    env: &mut CodegenEnv,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
    is_tail: bool, // True if this expression is in tail position
) -> Type {
    let pad = " ".repeat(indent);
    match expr {
        Expr::Int { value, ty } => {
            let instr = match ty {
                // i32-backed scalars (Bool/U8/U16/U32 live in an i32 in Wisp memory).
                Type::S32 | Type::Bool | Type::U8 | Type::U16 | Type::U32 => "i32.const",
                Type::S64 | Type::U64 => "i64.const",
                _ => panic!("integer literal not supported for {:?}", ty),
            };
            out.push_str(&format!("{}{} {}\n", pad, instr, *value));
            ty.clone()
        }
        Expr::Float { value, ty } => {
            match ty {
                Type::F32 => out.push_str(&format!("{}f32.const {}\n", pad, *value as f32)),
                Type::F64 => out.push_str(&format!("{}f64.const {}\n", pad, *value)),
                _ => panic!("float literal not supported for {:?}", ty),
            }
            ty.clone()
        }
        Expr::StringLiteral(s) => {
            // String layout in memory: 4 bytes length + UTF-8 bytes
            let bytes = s.as_bytes();
            let len = bytes.len();
            let total_size = 4 + len; // 4 bytes for length + string data

            // Allocate space for the string
            let ptr_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, total_size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store length at ptr
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const {}\n", pad, len));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store each byte of the string
            for (i, byte) in bytes.iter().enumerate() {
                out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
                out.push_str(&format!("{}i32.const {}\n", pad, 4 + i)); // offset past length
                out.push_str(&format!("{}i32.add\n", pad));
                out.push_str(&format!("{}i32.const {}\n", pad, *byte));
                out.push_str(&format!("{}i32.store8\n", pad));
            }

            // Return pointer to string
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Str
        }
        Expr::Ascribe { expr, ty } => {
            let from_ty = gen_expr(
                expr, out, indent, env, signatures, globals, records, variants, false,
            );
            if from_ty == *ty {
                return from_ty;
            }
            let instrs = conversion_instr(&from_ty, ty)
                .unwrap_or_else(|| panic!("unsupported conversion {:?} -> {:?}", from_ty, ty));
            for instr in instrs {
                out.push_str(&format!("{}{}\n", pad, instr));
            }
            ty.clone()
        }
        Expr::Var(name) => {
            let (idx, ty) = env.lookup(name);
            out.push_str(&format!("{}local.get {}\n", pad, idx));
            ty
        }
        Expr::Call { name, args } => {
            let sig = signatures
                .get(name)
                .unwrap_or_else(|| panic!("Missing signature for {}", name));
            for arg in args {
                gen_expr(
                    arg, out, indent, env, signatures, globals, records, variants, false,
                );
            }
            // Use return_call for tail position to enable tail call optimization
            if is_tail {
                out.push_str(&format!("{}return_call ${}\n", pad, name));
            } else {
                out.push_str(&format!("{}call ${}\n", pad, name));
            }
            sig.result.clone()
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = gen_expr(
                cond, out, indent, env, signatures, globals, records, variants, false,
            );
            if cond_ty != Type::S32 {
                panic!("if condition must be s32");
            }
            let result_ty = expr_type(then_branch, env, signatures, globals, records, variants);
            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(&result_ty)));
            out.push_str(&format!("{}  (then\n", pad));
            gen_expr(
                then_branch,
                out,
                indent + 4,
                env,
                signatures,
                globals,
                records,
                variants,
                is_tail, // Both branches inherit tail position
            );
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{}  (else\n", pad));
            let else_ty = gen_expr(
                else_branch,
                out,
                indent + 4,
                env,
                signatures,
                globals,
                records,
                variants,
                is_tail, // Both branches inherit tail position
            );
            if else_ty != result_ty {
                panic!(
                    "if branches must match types: {:?} vs {:?}",
                    result_ty, else_ty
                );
            }
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{})\n", pad));
            result_ty
        }
        Expr::Let {
            name, value, body, ..
        } => {
            let value_ty = gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            // Only save to local if value type is not unit (unit leaves nothing on stack)
            let has_binding = !is_unit_type(&value_ty);
            if has_binding {
                let idx = env.declare_local(value_ty);
                out.push_str(&format!("{}local.set {}\n", pad, idx));
                env.push_binding(name.clone(), idx);
            }
            let body_ty = gen_expr(
                body, out, indent, env, signatures, globals, records, variants, is_tail,
            );
            if has_binding {
                env.pop_binding();
            }
            body_ty
        }
        Expr::Begin { exprs } => {
            let mut last_ty = Type::S32;
            for (i, expr) in exprs.iter().enumerate() {
                let is_last = i == exprs.len() - 1;
                last_ty = gen_expr(
                    expr,
                    out,
                    indent,
                    env,
                    signatures,
                    globals,
                    records,
                    variants,
                    is_last && is_tail,
                );
                if !is_last && !is_unit_type(&last_ty) {
                    // Drop the value of non-final expressions (only if they return a value)
                    out.push_str(&format!("{}drop\n", pad));
                }
            }
            last_ty
        }
        Expr::WasmInstr { name, args } => {
            let instr_info = lookup_wasm_instr(name)
                .unwrap_or_else(|| panic!("Missing WASM instruction info for {}", name));

            // Special handling for const instructions - they take immediates, not stack values
            if name.ends_with(".const") {
                if args.len() != 1 {
                    panic!("{} expects exactly 1 argument", name);
                }
                match &args[0] {
                    Expr::Int { value, .. } => {
                        out.push_str(&format!("{}{} {}\n", pad, name, value));
                    }
                    Expr::Float { value, .. } => {
                        out.push_str(&format!("{}{} {}\n", pad, name, value));
                    }
                    _ => panic!("{} requires a literal value", name),
                }
            } else if name.ends_with(".store")
                || name == "i32.store8"
                || name == "i32.store16"
                || name == "i64.store8"
                || name == "i64.store16"
                || name == "i64.store32"
            {
                // Store instructions: emit address, then value, then store
                // Note: In WASM stores don't return values, but we make them return the stored value
                // We save the value in a local, emit the store, then restore it
                if args.len() != 2 {
                    panic!("{} expects exactly 2 arguments (address, value)", name);
                }

                // Emit and save the value first
                let value_ty = gen_expr(
                    &args[1], out, indent, env, signatures, globals, records, variants, false,
                );
                let value_local = env.declare_local(value_ty);
                out.push_str(&format!("{}local.set {}\n", pad, value_local));

                // Emit the address
                gen_expr(
                    &args[0], out, indent, env, signatures, globals, records, variants, false,
                );

                // Get the value back
                out.push_str(&format!("{}local.get {}\n", pad, value_local));

                // Emit the store
                out.push_str(&format!("{}{}\n", pad, name));

                // Put the value back on the stack as the "return value"
                out.push_str(&format!("{}local.get {}\n", pad, value_local));
            } else {
                // Normal instructions - emit args then instruction
                for arg in args {
                    gen_expr(
                        arg, out, indent, env, signatures, globals, records, variants, false,
                    );
                }
                out.push_str(&format!("{}{}\n", pad, name));
            }
            instr_info.result
        }
        Expr::GlobalGet { name } => {
            out.push_str(&format!("{}global.get {}\n", pad, name));
            let (ty, _) = globals.get(name).expect("global should exist");
            ty.clone()
        }
        Expr::GlobalSet { name, value } => {
            // Global.set consumes the value, so we save it to a local first
            // and restore it after to return the value for composability
            let value_ty = gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let value_local = env.declare_local(value_ty.clone());
            out.push_str(&format!("{}local.set {}\n", pad, value_local));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            out.push_str(&format!("{}global.set {}\n", pad, name));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            value_ty
        }
        Expr::RecordConstruct {
            record_name,
            fields,
        } => {
            let record_def = records.get(record_name).expect("record should exist");
            let size = record_def.size();

            // Bump allocate: get current heap_ptr, advance it by record size
            // Save the base pointer to a local
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store each field at the appropriate offset
            for (i, (field_expr, field_def)) in
                fields.iter().zip(record_def.fields.iter()).enumerate()
            {
                let offset = record_def.field_offset(i);

                // Compute address: ptr + offset
                out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
                if offset > 0 {
                    out.push_str(&format!("{}i32.const {}\n", pad, offset));
                    out.push_str(&format!("{}i32.add\n", pad));
                }

                // Evaluate the field expression
                gen_expr(
                    field_expr, out, indent, env, signatures, globals, records, variants, false,
                );

                // Store based on field type
                let store_instr = match &field_def.ty {
                    Type::S32 => "i32.store",
                    Type::S64 => "i64.store",
                    Type::U64 => "i64.store",
                    Type::Bool => "i32.store",
                    Type::U16 | Type::U32 => "i32.store",
                    Type::F32 => "f32.store",
                    Type::F64 => "f64.store",
                    // All compound types are pointers, resources are i32 handles
                    Type::Record(_)
                    | Type::Variant(_)
                    | Type::Option(_)
                    | Type::Result(_, _)
                    | Type::List(_)
                    | Type::Str
                    | Type::Tuple(_)
                    | Type::U8
                    | Type::Resource(_)
                    | Type::Borrow(_)
                    | Type::Any => "i32.store",
                };
                out.push_str(&format!("{}{}\n", pad, store_instr));
            }

            // Return the pointer to the record
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Record(record_name.clone())
        }
        Expr::RecordAccess {
            record_name,
            field_name,
            expr,
        } => {
            let record_def = records.get(record_name).expect("record should exist");

            // Find the field and its offset
            let (field_idx, field_def) = record_def
                .fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.name == *field_name)
                .expect("field should exist");
            let offset = record_def.field_offset(field_idx);

            // Evaluate the record expression (gives us the pointer)
            gen_expr(
                expr, out, indent, env, signatures, globals, records, variants, false,
            );

            // Add offset if non-zero
            if offset > 0 {
                out.push_str(&format!("{}i32.const {}\n", pad, offset));
                out.push_str(&format!("{}i32.add\n", pad));
            }

            // Load based on field type
            let load_instr = match &field_def.ty {
                Type::S32 => "i32.load",
                Type::S64 => "i64.load",
                Type::U64 => "i64.load",
                Type::Bool => "i32.load",
                Type::U16 | Type::U32 => "i32.load",
                Type::F32 => "f32.load",
                Type::F64 => "f64.load",
                // All compound types are pointers, resources are i32 handles
                Type::Record(_)
                | Type::Variant(_)
                | Type::Option(_)
                | Type::Result(_, _)
                | Type::List(_)
                | Type::Str
                | Type::Tuple(_)
                | Type::U8
                | Type::Resource(_)
                | Type::Borrow(_)
                | Type::Any => "i32.load",
            };
            out.push_str(&format!("{}{}\n", pad, load_instr));
            field_def.ty.clone()
        }
        Expr::VariantConstruct {
            variant_name,
            case_name,
            payload,
        } => {
            let variant_def = variants.get(variant_name).expect("variant should exist");
            let (case_idx, case) = variant_def.find_case(case_name).expect("case should exist");
            let size = variant_def.size();

            // Bump allocate: get current heap_ptr, advance it by variant size
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant (case index) at offset 0
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const {}\n", pad, case_idx));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store payload values starting at offset 4 (after discriminant)
            let mut payload_offset = 4;
            for (payload_expr, payload_ty) in payload.iter().zip(case.payload.iter()) {
                // Compute address: ptr + payload_offset
                out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
                out.push_str(&format!("{}i32.const {}\n", pad, payload_offset));
                out.push_str(&format!("{}i32.add\n", pad));

                // Evaluate the payload expression
                gen_expr(
                    payload_expr,
                    out,
                    indent,
                    env,
                    signatures,
                    globals,
                    records,
                    variants,
                    false,
                );

                // Store based on payload type
                let store_instr = match payload_ty {
                    Type::S32 => "i32.store",
                    Type::S64 => "i64.store",
                    Type::U64 => "i64.store",
                    Type::Bool => "i32.store",
                    Type::U16 | Type::U32 => "i32.store",
                    Type::F32 => "f32.store",
                    Type::F64 => "f64.store",
                    Type::Record(_)
                    | Type::Variant(_)
                    | Type::Option(_)
                    | Type::Result(_, _)
                    | Type::List(_)
                    | Type::Str
                    | Type::Tuple(_)
                    | Type::U8
                    | Type::Resource(_)
                    | Type::Borrow(_)
                    | Type::Any => "i32.store",
                };
                out.push_str(&format!("{}{}\n", pad, store_instr));
                payload_offset += type_size(payload_ty);
            }

            // Return the pointer to the variant
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Variant(variant_name.clone())
        }
        Expr::Match {
            expr: scrutinee,
            cases,
        } => {
            // Keep the checked language type, including pointer-backed values.
            let result_ty = expr_type(expr, env, signatures, globals, records, variants);
            // Evaluate the expression to get the pointer
            let expr_ty = gen_expr(
                scrutinee, out, indent, env, signatures, globals, records, variants, false,
            );

            // Save the pointer to a local
            let value_ptr = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, value_ptr));

            // Desugar a `_` wildcard the same way the checker did.
            let effective = expand_match_wildcard(cases, &expr_ty, variants);
            let cases = &effective;

            // Handle Option, Result, and Variant types
            match &expr_ty {
                Type::Option(inner_ty) => {
                    // Option: discriminant 0 = none, 1 = some
                    // Load discriminant
                    out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                    out.push_str(&format!("{}i32.load\n", pad));

                    let num_cases = cases.len();
                    for (i, arm) in cases.iter().enumerate() {
                        let case_idx = match arm.case_name.as_str() {
                            "none" => 0,
                            "some" => 1,
                            _ => panic!("invalid option case"),
                        };

                        // Compare discriminant with case index
                        if i > 0 {
                            out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}i32.load\n", pad));
                        }
                        out.push_str(&format!("{}i32.const {}\n", pad, case_idx));
                        out.push_str(&format!("{}i32.eq\n", pad));

                        let is_last = i == num_cases - 1;
                        if !is_last || num_cases > 1 {
                            out.push_str(&format!(
                                "{}(if (result {})\n",
                                pad,
                                wat_type(&result_ty)
                            ));
                            out.push_str(&format!("{}  (then\n", pad));
                        }

                        // Load payload for 'some' case
                        let saved_binding_count = env.bindings.len();
                        if arm.case_name == "some" && !arm.bindings.is_empty() {
                            out.push_str(&format!("{}    local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}    i32.const 4\n", pad));
                            out.push_str(&format!("{}    i32.add\n", pad));

                            let load_instr = match **inner_ty {
                                Type::S32 => "i32.load",
                                Type::S64 => "i64.load",
                                Type::U64 => "i64.load",
                                Type::Bool => "i32.load",
                                Type::U16 | Type::U32 => "i32.load",
                                Type::F32 => "f32.load",
                                Type::F64 => "f64.load",
                                Type::Record(_)
                                | Type::Variant(_)
                                | Type::Option(_)
                                | Type::Result(_, _)
                                | Type::List(_)
                                | Type::Str
                                | Type::Tuple(_)
                                | Type::U8
                                | Type::Resource(_)
                                | Type::Borrow(_)
                                | Type::Any => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            let idx = env.declare_local((**inner_ty).clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(arm.bindings[0].clone(), idx);
                        }

                        // Generate arm body - inherits tail position
                        gen_expr(
                            &arm.body,
                            out,
                            indent + 4,
                            env,
                            signatures,
                            globals,
                            records,
                            variants,
                            is_tail,
                        );

                        // Pop bindings
                        while env.bindings.len() > saved_binding_count {
                            env.pop_binding();
                        }

                        if !is_last {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                        } else if num_cases > 1 {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                            out.push_str(&format!("{}    unreachable\n", pad));
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{})\n", pad));
                        }
                    }

                    // Close all if-else blocks
                    for _ in 0..num_cases.saturating_sub(1) {
                        out.push_str(&format!("{}  )\n", pad));
                        out.push_str(&format!("{})\n", pad));
                    }

                    result_ty
                }
                Type::Result(ok_ty, err_ty) => {
                    // Result: discriminant 0 = ok, 1 = err
                    // Load discriminant
                    out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                    out.push_str(&format!("{}i32.load\n", pad));

                    let num_cases = cases.len();
                    for (i, arm) in cases.iter().enumerate() {
                        let (case_idx, payload_ty) = match arm.case_name.as_str() {
                            "ok" => (0, (**ok_ty).clone()),
                            "err" => (1, (**err_ty).clone()),
                            _ => panic!("invalid result case"),
                        };

                        // Compare discriminant with case index
                        if i > 0 {
                            out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}i32.load\n", pad));
                        }
                        out.push_str(&format!("{}i32.const {}\n", pad, case_idx));
                        out.push_str(&format!("{}i32.eq\n", pad));

                        let is_last = i == num_cases - 1;
                        if !is_last || num_cases > 1 {
                            out.push_str(&format!(
                                "{}(if (result {})\n",
                                pad,
                                wat_type(&result_ty)
                            ));
                            out.push_str(&format!("{}  (then\n", pad));
                        }

                        // Load payload
                        let saved_binding_count = env.bindings.len();
                        if !arm.bindings.is_empty() {
                            out.push_str(&format!("{}    local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}    i32.const 4\n", pad));
                            out.push_str(&format!("{}    i32.add\n", pad));

                            let load_instr = match payload_ty {
                                Type::S32 => "i32.load",
                                Type::S64 => "i64.load",
                                Type::U64 => "i64.load",
                                Type::Bool => "i32.load",
                                Type::U16 | Type::U32 => "i32.load",
                                Type::F32 => "f32.load",
                                Type::F64 => "f64.load",
                                Type::Record(_)
                                | Type::Variant(_)
                                | Type::Option(_)
                                | Type::Result(_, _)
                                | Type::List(_)
                                | Type::Str
                                | Type::Tuple(_)
                                | Type::U8
                                | Type::Resource(_)
                                | Type::Borrow(_)
                                | Type::Any => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            let idx = env.declare_local(payload_ty.clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(arm.bindings[0].clone(), idx);
                        }

                        // Generate arm body - inherits tail position
                        gen_expr(
                            &arm.body,
                            out,
                            indent + 4,
                            env,
                            signatures,
                            globals,
                            records,
                            variants,
                            is_tail,
                        );

                        // Pop bindings
                        while env.bindings.len() > saved_binding_count {
                            env.pop_binding();
                        }

                        if !is_last {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                        } else if num_cases > 1 {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                            out.push_str(&format!("{}    unreachable\n", pad));
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{})\n", pad));
                        }
                    }

                    // Close all if-else blocks
                    for _ in 0..num_cases.saturating_sub(1) {
                        out.push_str(&format!("{}  )\n", pad));
                        out.push_str(&format!("{})\n", pad));
                    }

                    result_ty
                }
                Type::Variant(variant_name) => {
                    let variant_def = variants.get(variant_name).expect("variant should exist");

                    // For simplicity, use nested if-else for now
                    let num_cases = cases.len();

                    // A single-case variant always matches, so no discriminant test is
                    // emitted (emitting one would leave the comparison on the stack with
                    // no branch to consume it). Multi-case matches load it to compare.
                    if num_cases > 1 {
                        out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                        out.push_str(&format!("{}i32.load\n", pad));
                    }

                    for (i, arm) in cases.iter().enumerate() {
                        let (case_idx, case) = variant_def
                            .find_case(&arm.case_name)
                            .expect("case should exist");

                        let is_last = i == num_cases - 1;
                        if num_cases > 1 {
                            // Compare discriminant with case index.
                            if i > 0 {
                                out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                                out.push_str(&format!("{}i32.load\n", pad));
                            }
                            out.push_str(&format!("{}i32.const {}\n", pad, case_idx));
                            out.push_str(&format!("{}i32.eq\n", pad));
                            out.push_str(&format!(
                                "{}(if (result {})\n",
                                pad,
                                wat_type(&result_ty)
                            ));
                            out.push_str(&format!("{}  (then\n", pad));
                        }

                        // Load payload values into locals and bind them
                        let saved_binding_count = env.bindings.len();
                        let mut payload_offset = 4;
                        for (binding, payload_ty) in arm.bindings.iter().zip(case.payload.iter()) {
                            // Load payload value
                            out.push_str(&format!("{}    local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}    i32.const {}\n", pad, payload_offset));
                            out.push_str(&format!("{}    i32.add\n", pad));

                            let load_instr = match payload_ty {
                                Type::S32 => "i32.load",
                                Type::S64 => "i64.load",
                                Type::U64 => "i64.load",
                                Type::Bool => "i32.load",
                                Type::U16 | Type::U32 => "i32.load",
                                Type::F32 => "f32.load",
                                Type::F64 => "f64.load",
                                Type::Record(_)
                                | Type::Variant(_)
                                | Type::Option(_)
                                | Type::Result(_, _)
                                | Type::List(_)
                                | Type::Str
                                | Type::Tuple(_)
                                | Type::U8
                                | Type::Resource(_)
                                | Type::Borrow(_)
                                | Type::Any => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            // Save to local
                            let idx = env.declare_local(payload_ty.clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(binding.clone(), idx);

                            payload_offset += type_size(payload_ty);
                        }

                        // Generate arm body - inherits tail position
                        gen_expr(
                            &arm.body,
                            out,
                            indent + 4,
                            env,
                            signatures,
                            globals,
                            records,
                            variants,
                            is_tail,
                        );

                        // Pop bindings
                        while env.bindings.len() > saved_binding_count {
                            env.pop_binding();
                        }

                        if !is_last {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                        } else if num_cases > 1 {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                            out.push_str(&format!("{}    unreachable\n", pad));
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{})\n", pad));
                        }
                    }

                    // Close all the if-else blocks
                    for _ in 0..num_cases.saturating_sub(1) {
                        out.push_str(&format!("{}  )\n", pad));
                        out.push_str(&format!("{})\n", pad));
                    }

                    result_ty
                }
                _ => panic!("match expression must be variant, option, or result"),
            }
        }
        // Option: some - allocate, store discriminant 1, store value
        Expr::Some { inner_type, value } => {
            let size = 4 + type_size(inner_type); // discriminant + payload
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant = 1 (some)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 1\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store value at offset 4
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let store_instr = match inner_type {
                Type::S32 => "i32.store",
                Type::S64 => "i64.store",
                Type::F32 => "f32.store",
                Type::F64 => "f64.store",
                _ => "i32.store",
            };
            out.push_str(&format!("{}{}\n", pad, store_instr));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Option(Box::new(inner_type.clone()))
        }
        // Option: none - allocate, store discriminant 0
        Expr::None { inner_type } => {
            let size = 4 + type_size(inner_type); // discriminant + payload space
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant = 0 (none)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Option(Box::new(inner_type.clone()))
        }
        // Result: ok - allocate, store discriminant 0, store value
        Expr::Ok {
            ok_type,
            err_type,
            value,
        } => {
            let max_payload = std::cmp::max(type_size(ok_type), type_size(err_type));
            let size = 4 + max_payload; // discriminant + max payload
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant = 0 (ok)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store ok value at offset 4
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let store_instr = match ok_type {
                Type::S32 => "i32.store",
                Type::S64 => "i64.store",
                Type::F32 => "f32.store",
                Type::F64 => "f64.store",
                _ => "i32.store",
            };
            out.push_str(&format!("{}{}\n", pad, store_instr));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone()))
        }
        // Result: err - allocate, store discriminant 1, store value
        Expr::Err {
            ok_type,
            err_type,
            value,
        } => {
            let max_payload = std::cmp::max(type_size(ok_type), type_size(err_type));
            let size = 4 + max_payload; // discriminant + max payload
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant = 1 (err)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 1\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store err value at offset 4
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let store_instr = match err_type {
                Type::S32 => "i32.store",
                Type::S64 => "i64.store",
                Type::F32 => "f32.store",
                Type::F64 => "f64.store",
                _ => "i32.store",
            };
            out.push_str(&format!("{}{}\n", pad, store_instr));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone()))
        }
        // Tuple: evaluate values, allocate, store fields contiguously
        Expr::TupleConstruct { values } => {
            // First evaluate each value into a temp local, collecting types
            let mut value_types = Vec::new();
            let mut value_locals = Vec::new();
            for val in values {
                let val_ty = gen_expr(
                    val, out, indent, env, signatures, globals, records, variants, false,
                );
                let tmp_local = env.declare_local(val_ty.clone());
                out.push_str(&format!("{}local.set {}\n", pad, tmp_local));
                value_locals.push(tmp_local);
                value_types.push(val_ty);
            }

            // Allocate tuple on heap
            let total_size: usize = value_types.iter().map(type_size).sum();
            let ptr_local = env.declare_local(Type::S32);

            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, total_size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store each field from temp locals
            let mut field_offset = 0;
            for (val_ty, val_local) in value_types.iter().zip(value_locals.iter()) {
                out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
                if field_offset > 0 {
                    out.push_str(&format!("{}i32.const {}\n", pad, field_offset));
                    out.push_str(&format!("{}i32.add\n", pad));
                }
                out.push_str(&format!("{}local.get {}\n", pad, val_local));
                let store_instr = match val_ty {
                    Type::S64 => "i64.store",
                    Type::F32 => "f32.store",
                    Type::F64 => "f64.store",
                    _ => "i32.store", // s32, u8, pointers
                };
                out.push_str(&format!("{}{}\n", pad, store_instr));
                field_offset += type_size(val_ty);
            }

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Tuple(value_types)
        }
        Expr::WithCap { name, cap, body } => {
            // The capability token is a zero-cost i32 witness: mint it as a constant,
            // bind it, and run the body. The linear discipline is purely static.
            let idx = env.declare_local(Type::Resource(cap.clone()));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, idx));
            env.push_binding(name.clone(), idx);
            let body_ty = gen_expr(
                body, out, indent, env, signatures, globals, records, variants, is_tail,
            );
            env.pop_binding();
            body_ty
        }
        Expr::ReleaseCap { value } => {
            // Evaluate the capability (an i32 witness), discard it, yield 0. Releasing
            // is the terminal consumer; at runtime a static witness needs no teardown.
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}drop\n", pad));
            out.push_str(&format!("{}i32.const 0\n", pad));
            Type::S32
        }
        Expr::BorrowCap { name } => {
            // A borrow is the same i32 witness as the capability; just load it.
            let (idx, ty) = env.lookup(name);
            out.push_str(&format!("{}local.get {}\n", pad, idx));
            Type::Borrow(Box::new(ty))
        }
        // List: new - allocate header (len=0, cap=0, data=null)
        Expr::ListNew { elem_type } => {
            let header_size = 12; // 4 bytes len + 4 bytes cap + 4 bytes data ptr
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += header_size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, header_size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store len = 0
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store cap = 0
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store data = 0 (null)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 8\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::List(Box::new(elem_type.clone()))
        }
        // List: push - simplified version that reallocates every time
        Expr::ListPush { list, value } => {
            let list_ty = gen_expr(
                list, out, indent, env, signatures, globals, records, variants, false,
            );
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => panic!("list-push expects a list"),
            };
            let elem_size = type_size(&elem_type);
            let list_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, list_local));

            // Evaluate both operands before reading or changing the list header.
            // The value expression can itself push into this same list.
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let value_local = env.declare_local(elem_type.clone());
            out.push_str(&format!("{}local.set {}\n", pad, value_local));

            // Get current len
            let len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, len_local));

            // Allocate new data array (simple approach: always reallocate)
            let new_data_local = env.declare_local(Type::S32);
            let new_size = env.declare_local(Type::S32);

            // new_size = (len + 1) * elem_size
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.const 1\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_size));

            // new_data = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_data_local));

            // heap_ptr += new_size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, new_size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Copy old data if len > 0
            // Get old data pointer from list+8
            let old_data_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}i32.const 8\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, old_data_local));

            // Copy len * elem_size bytes from old_data to new_data
            // memory.copy(dst, src, len)
            out.push_str(&format!("{}local.get {}\n", pad, new_data_local)); // dst
            out.push_str(&format!("{}local.get {}\n", pad, old_data_local)); // src
            out.push_str(&format!("{}local.get {}\n", pad, len_local)); // len (in elements)
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad)); // len * elem_size
            out.push_str(&format!("{}memory.copy\n", pad));

            // Store new value at new_data + len * elem_size
            out.push_str(&format!("{}local.get {}\n", pad, new_data_local));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            let store_instr = match &elem_type {
                Type::S32 => "i32.store",
                Type::S64 => "i64.store",
                Type::F32 => "f32.store",
                Type::F64 => "f64.store",
                _ => "i32.store",
            };
            out.push_str(&format!("{}{}\n", pad, store_instr));

            // Update list header: len = len + 1
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.const 1\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Update list header: data = new_data
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}i32.const 8\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, new_data_local));
            out.push_str(&format!("{}i32.store\n", pad));

            // Return the list pointer
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            list_ty
        }
        // List: get - load element at index
        Expr::ListGet { list, index } => {
            let list_ty = gen_expr(
                list, out, indent, env, signatures, globals, records, variants, false,
            );
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => panic!("list-get expects a list"),
            };
            let elem_size = type_size(&elem_type);
            let list_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, list_local));

            // Evaluate index
            gen_expr(
                index, out, indent, env, signatures, globals, records, variants, false,
            );
            let index_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, index_local));

            // Load data pointer
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}i32.const 8\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.load\n", pad));

            // Add index * elem_size
            out.push_str(&format!("{}local.get {}\n", pad, index_local));
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));

            // Load element
            let load_instr = match &elem_type {
                Type::S32 => "i32.load",
                Type::S64 => "i64.load",
                Type::F32 => "f32.load",
                Type::F64 => "f64.load",
                _ => "i32.load",
            };
            out.push_str(&format!("{}{}\n", pad, load_instr));
            elem_type
        }
        // List: len - return length
        Expr::ListLen { list } => {
            gen_expr(
                list, out, indent, env, signatures, globals, records, variants, false,
            );
            // Load len field at offset 0
            out.push_str(&format!("{}i32.load\n", pad));
            Type::S32
        }
        // String: len - return length
        Expr::StringLen { string } => {
            gen_expr(
                string, out, indent, env, signatures, globals, records, variants, false,
            );
            // Load len field at offset 0 (string layout: 4 bytes len + data)
            out.push_str(&format!("{}i32.load\n", pad));
            Type::S32
        }
        // String: ref - get byte at index
        Expr::StringRef { string, index } => {
            // String layout: 4 bytes len + data bytes
            // Result: byte at (string_ptr + 4 + index)
            gen_expr(
                string, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                index, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.load8_u\n", pad));
            Type::S32
        }
        // String: substring - extract portion of string
        Expr::Substring { string, start, end } => {
            // Evaluate string pointer
            let str_local = env.declare_local(Type::S32);
            gen_expr(
                string, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, str_local));

            // Evaluate start index
            let start_local = env.declare_local(Type::S32);
            gen_expr(
                start, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, start_local));

            // Evaluate end index
            let end_local = env.declare_local(Type::S32);
            gen_expr(
                end, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, end_local));

            // Calculate new length: end - start
            let new_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, end_local));
            out.push_str(&format!("{}local.get {}\n", pad, start_local));
            out.push_str(&format!("{}i32.sub\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_len_local));

            // Allocate space for new string: 4 bytes len + new_len bytes
            let new_ptr_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, new_len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store new length at new_ptr
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}local.get {}\n", pad, new_len_local));
            out.push_str(&format!("{}i32.store\n", pad));

            // Copy bytes using memory.copy
            // dst: new_ptr + 4
            // src: str_local + 4 + start
            // len: new_len
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, str_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, start_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, new_len_local));
            out.push_str(&format!("{}memory.copy\n", pad));

            // Return new string pointer
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            Type::Str
        }
        // String: append - concatenate two strings
        Expr::AnyFromS32 { value } => {
            // Build a len-prefixed CGRF blob [len:u32][16B header][S32 node] for n.
            // The payload S32 node sits at cgrf offset 16 (header) + 8 (node header).
            let n_local = env.declare_local(Type::S32);
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, n_local));
            let ptr = env.declare_local(Type::S32);
            // Bump the shared heap by 32 bytes (4 len prefix + 28 CGRF scalar value).
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr));
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const 32\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));
            // A little helper writes `local.get ptr; i32.const v; <instr> offset=o`.
            {
                let mut w = |v: String, instr: &str, o: u32| {
                    out.push_str(&format!("{}local.get {}\n", pad, ptr));
                    out.push_str(&format!("{}{}\n", pad, v));
                    out.push_str(&format!("{}{} offset={}\n", pad, instr, o));
                };
                w("i32.const 28".into(), "i32.store", 0); // len prefix = CGRF byte length
                w(format!("i32.const {}", CGRF_MAGIC), "i32.store", 4);
                w(format!("i32.const {}", CGRF_VERSION), "i32.store16", 8);
                w("i32.const 0".into(), "i32.store16", 10); // flags
                w("i32.const 1".into(), "i32.store", 12); // node_count
                w("i32.const 0".into(), "i32.store", 16); // root index
                w(format!("i32.const {}", CGRF_S32), "i32.store8", 20); // node kind
                w("i32.const 0".into(), "i32.store8", 21); // node flags
                w("i32.const 0".into(), "i32.store16", 22); // reserved
                w("i32.const 4".into(), "i32.store", 24); // payload_len
                w(format!("local.get {}", n_local), "i32.store", 28); // payload value
            }
            // Result: the `any` pointer.
            out.push_str(&format!("{}local.get {}\n", pad, ptr));
            Type::Any
        }
        Expr::AnyToS32 { value } => {
            // The S32 payload sits at blob+28 (4 len prefix + 16 header + 8 node header).
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}i32.load offset=28\n", pad));
            Type::S32
        }
        Expr::AnyFromString { value } => {
            // Build a len-prefixed CGRF String node blob [len:u32][16B header][node]
            // for s. A Wisp string is a pointer to [len:u32][bytes], and the CGRF
            // string payload (at cgrf offset 24) is the same [len:u32][bytes]
            // layout, so the byte copy is a straight memcpy of the string body.
            let s_local = env.declare_local(Type::S32);
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, s_local));
            let slen = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, s_local));
            out.push_str(&format!("{}i32.load\n", pad)); // string byte length
            out.push_str(&format!("{}local.set {}\n", pad, slen));
            let ptr = env.declare_local(Type::S32);
            // Bump the heap by 32 + slen (4 len prefix + 28 CGRF fixed + bytes).
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr));
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const 32\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, slen));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));
            // Fixed header fields.
            {
                let mut w = |v: String, instr: &str, o: u32| {
                    out.push_str(&format!("{}local.get {}\n", pad, ptr));
                    out.push_str(&format!("{}{}\n", pad, v));
                    out.push_str(&format!("{}{} offset={}\n", pad, instr, o));
                };
                w(format!("i32.const {}", CGRF_MAGIC), "i32.store", 4);
                w(format!("i32.const {}", CGRF_VERSION), "i32.store16", 8);
                w("i32.const 0".into(), "i32.store16", 10); // flags
                w("i32.const 1".into(), "i32.store", 12); // node_count
                w("i32.const 0".into(), "i32.store", 16); // root index
                w(format!("i32.const {}", CGRF_STRING), "i32.store8", 20); // node kind
                w("i32.const 0".into(), "i32.store8", 21); // node flags
                w("i32.const 0".into(), "i32.store16", 22); // reserved
            }
            // Dynamic fields: len prefix (28 + slen), payload_len (4 + slen),
            // and the CGRF string length field.
            out.push_str(&format!("{}local.get {}\n", pad, ptr)); // len prefix @0
            out.push_str(&format!("{}i32.const 28\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, slen));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, ptr)); // payload_len @24
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, slen));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.store offset=24\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, ptr)); // string length @28
            out.push_str(&format!("{}local.get {}\n", pad, slen));
            out.push_str(&format!("{}i32.store offset=28\n", pad));
            // Copy the string bytes: dest = ptr+32, src = s+4, len = slen.
            out.push_str(&format!("{}local.get {}\n", pad, ptr));
            out.push_str(&format!("{}i32.const 32\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, s_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, slen));
            out.push_str(&format!("{}memory.copy\n", pad));
            // Result: the `any` pointer.
            out.push_str(&format!("{}local.get {}\n", pad, ptr));
            Type::Any
        }
        Expr::AnyToString { value } => {
            // Zero-copy: a CGRF string payload [len:u32][bytes] sits at blob+28,
            // which is exactly a Wisp string, so return a pointer to it.
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}i32.const 28\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            Type::Str
        }
        Expr::HeapAlloc { size } => {
            // Allocate on the compiler's bump heap so byte buffers the interpreter
            // builds share the same heap as the boundary codec's allocations.
            gen_expr(
                size, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}call $__alloc\n", pad));
            Type::S32
        }
        Expr::AnyAddr { value } => {
            // Pure reinterpret: an `any` is already the i32 address of its blob.
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            Type::S32
        }
        Expr::AnyFromAddr { value } => {
            // Pure reinterpret: a blob address is an `any`.
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            Type::Any
        }
        Expr::StringAddr { value } => {
            // Pure reinterpret: a string is already the i32 address of [len][bytes].
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            Type::S32
        }
        Expr::StringFromAddr { value } => {
            // Pure reinterpret: an address of [len][bytes] is a string.
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            Type::Str
        }
        Expr::RawInvoke {
            module,
            import,
            value,
        } => {
            // args-any is a len-prefixed CGRF blob [len:u32][cgrf]. Pass its bytes
            // straight to the import's raw CGRF entry point, then wrap the returned
            // CGRF (out_ptr,out_len) back into an `any` blob — the same shape the
            // import wrapper uses for an `any` result.
            let raw_sym = raw_import_symbol(module, import);
            let blob = env.declare_local(Type::S32);
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, blob));
            let slots = env.declare_local(Type::S32);
            out.push_str(&format!(
                "{}i32.const 8\n{}call $__alloc\n{}local.set {}\n",
                pad, pad, pad, slots
            ));
            // $__raw_<import>(blob+4, load(blob), slots, slots+4)
            out.push_str(&format!(
                "{}local.get {}\n{}i32.const 4\n{}i32.add\n",
                pad, blob, pad, pad
            ));
            out.push_str(&format!("{}local.get {}\n{}i32.load\n", pad, blob, pad));
            out.push_str(&format!("{}local.get {}\n", pad, slots));
            out.push_str(&format!(
                "{}local.get {}\n{}i32.const 4\n{}i32.add\n",
                pad, slots, pad, pad
            ));
            out.push_str(&format!("{}call ${}\n{}drop\n", pad, raw_sym, pad)); // ignore status
            let any_len = env.declare_local(Type::S32);
            out.push_str(&format!(
                "{}local.get {}\n{}i32.const 4\n{}i32.add\n{}i32.load\n{}local.set {}\n",
                pad, slots, pad, pad, pad, pad, any_len
            )); // out_len
            let res = env.declare_local(Type::S32);
            out.push_str(&format!(
                "{}local.get {}\n{}i32.const 4\n{}i32.add\n{}call $__alloc\n{}local.set {}\n",
                pad, any_len, pad, pad, pad, pad, res
            ));
            out.push_str(&format!(
                "{}local.get {}\n{}local.get {}\n{}i32.store\n",
                pad, res, pad, any_len, pad
            )); // len prefix
            // copy cgrf: dest=res+4, src=out_ptr (load slots), len=any_len
            out.push_str(&format!(
                "{}local.get {}\n{}i32.const 4\n{}i32.add\n",
                pad, res, pad, pad
            ));
            out.push_str(&format!("{}local.get {}\n{}i32.load\n", pad, slots, pad)); // out_ptr
            out.push_str(&format!(
                "{}local.get {}\n{}memory.copy\n",
                pad, any_len, pad
            ));
            out.push_str(&format!("{}local.get {}\n", pad, res));
            Type::Any
        }
        Expr::StringAppend { left, right } => {
            // Evaluate left string pointer
            let left_local = env.declare_local(Type::S32);
            gen_expr(
                left, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, left_local));

            // Evaluate right string pointer
            let right_local = env.declare_local(Type::S32);
            gen_expr(
                right, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, right_local));

            // Get left length
            let left_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, left_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, left_len_local));

            // Get right length
            let right_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, right_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, right_len_local));

            // Calculate total length
            let total_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}local.get {}\n", pad, right_len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, total_len_local));

            // Allocate space for new string: 4 bytes len + total_len bytes
            let new_ptr_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, total_len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store total length at new_ptr
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}local.get {}\n", pad, total_len_local));
            out.push_str(&format!("{}i32.store\n", pad));

            // Copy left string bytes
            // dst: new_ptr + 4
            // src: left_local + 4
            // len: left_len
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, left_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}memory.copy\n", pad));

            // Copy right string bytes
            // dst: new_ptr + 4 + left_len
            // src: right_local + 4
            // len: right_len
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, right_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, right_len_local));
            out.push_str(&format!("{}memory.copy\n", pad));

            // Return new string pointer
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            Type::Str
        }
        // String: equality - compare two strings
        Expr::StringEq { left, right } => {
            // Evaluate left string pointer
            let left_local = env.declare_local(Type::S32);
            gen_expr(
                left, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, left_local));

            // Evaluate right string pointer
            let right_local = env.declare_local(Type::S32);
            gen_expr(
                right, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, right_local));

            // Get left length
            let left_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, left_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, left_len_local));

            // Get right length
            let right_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, right_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, right_len_local));

            // First check if lengths are equal
            // If lengths differ, strings can't be equal
            // Use block/br structure for early return on length mismatch
            out.push_str(&format!("{}block (result i32) ;; string-eq outer\n", pad));
            out.push_str(&format!("{}  local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}  local.get {}\n", pad, right_len_local));
            out.push_str(&format!("{}  i32.ne\n", pad));
            out.push_str(&format!("{}  if (result i32)\n", pad));
            out.push_str(&format!("{}    i32.const 0\n", pad)); // lengths differ, not equal
            out.push_str(&format!("{}  else\n", pad));
            // Lengths match, compare byte by byte using loop
            let idx_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}    i32.const 0\n", pad));
            out.push_str(&format!("{}    local.set {}\n", pad, idx_local));
            out.push_str(&format!(
                "{}    block (result i32) ;; comparison result\n",
                pad
            ));
            out.push_str(&format!("{}      loop ;; compare loop\n", pad));
            // Check if idx >= len (done comparing)
            out.push_str(&format!("{}        local.get {}\n", pad, idx_local));
            out.push_str(&format!("{}        local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}        i32.ge_u\n", pad));
            out.push_str(&format!("{}        if\n", pad));
            out.push_str(&format!("{}          i32.const 1\n", pad)); // all bytes match
            out.push_str(&format!("{}          br 2 ;; exit with 1\n", pad));
            out.push_str(&format!("{}        end\n", pad));
            // Compare bytes at idx
            out.push_str(&format!("{}        local.get {}\n", pad, left_local));
            out.push_str(&format!("{}        i32.const 4\n", pad));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        local.get {}\n", pad, idx_local));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        i32.load8_u\n", pad));
            out.push_str(&format!("{}        local.get {}\n", pad, right_local));
            out.push_str(&format!("{}        i32.const 4\n", pad));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        local.get {}\n", pad, idx_local));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        i32.load8_u\n", pad));
            out.push_str(&format!("{}        i32.ne\n", pad));
            out.push_str(&format!("{}        if\n", pad));
            out.push_str(&format!("{}          i32.const 0\n", pad)); // bytes differ
            out.push_str(&format!("{}          br 3 ;; exit with 0\n", pad));
            out.push_str(&format!("{}        end\n", pad));
            // Increment idx
            out.push_str(&format!("{}        local.get {}\n", pad, idx_local));
            out.push_str(&format!("{}        i32.const 1\n", pad));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        local.set {}\n", pad, idx_local));
            out.push_str(&format!("{}        br 0 ;; continue loop\n", pad));
            out.push_str(&format!("{}      end ;; loop\n", pad));
            out.push_str(&format!(
                "{}      i32.const 1 ;; fallback (empty strings)\n",
                pad
            ));
            out.push_str(&format!("{}    end ;; comparison result block\n", pad));
            out.push_str(&format!("{}  end ;; if\n", pad));
            out.push_str(&format!("{}end ;; string-eq outer\n", pad));
            Type::S32
        }
        // String: from-bytes - create string from list<u8>
        Expr::StringFromBytes { bytes } => {
            // Evaluate bytes list pointer
            let bytes_local = env.declare_local(Type::S32);
            gen_expr(
                bytes, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, bytes_local));

            // Get length from list (list layout: [4 bytes len][data])
            let len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, bytes_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, len_local));

            // Allocate new string: 4 bytes for length + len bytes for data
            let new_ptr_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_ptr_local));

            // Bump heap pointer
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store length in new string
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.store\n", pad));

            // Copy bytes from list to string
            // dst: new_ptr + 4
            // src: bytes_local + 4
            // len: len_local
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, bytes_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}memory.copy\n", pad));

            // Return new string pointer
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            Type::Str
        }
        // String: to-bytes - convert string to list<u8>
        Expr::StringToBytes { string } => {
            // Evaluate string pointer
            let str_local = env.declare_local(Type::S32);
            gen_expr(
                string, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, str_local));

            // Get length from string (string layout: [4 bytes len][data])
            let len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, str_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, len_local));

            // Allocate new list: 4 bytes for length + len bytes for data
            let new_ptr_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_ptr_local));

            // Bump heap pointer
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store length in new list
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.store\n", pad));

            // Copy bytes from string to list
            // dst: new_ptr + 4
            // src: str_local + 4
            // len: len_local
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, str_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}memory.copy\n", pad));

            // Return new list pointer
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            Type::List(Box::new(Type::U8))
        }
    }
}

pub(crate) struct CodegenEnv {
    bindings: Vec<(String, u32)>,
    param_count: u32,
    locals: Vec<Type>,
    param_types: Vec<Type>,
}

impl CodegenEnv {
    fn new(params: &[Parameter]) -> Self {
        let mut bindings = Vec::new();
        for (idx, name) in params.iter().enumerate() {
            bindings.push((name.name.clone(), idx as u32));
        }
        Self {
            bindings,
            param_count: params.len() as u32,
            locals: Vec::new(),
            param_types: params.iter().map(|p| p.ty.clone()).collect(),
        }
    }

    fn declare_local(&mut self, ty: Type) -> u32 {
        let idx = self.param_count + self.locals.len() as u32;
        self.locals.push(ty);
        idx
    }

    fn push_binding(&mut self, name: String, idx: u32) {
        self.bindings.push((name, idx));
    }

    fn pop_binding(&mut self) {
        self.bindings.pop();
    }

    fn lookup(&self, name: &str) -> (u32, Type) {
        let (_name, idx) = self
            .bindings
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .unwrap_or_else(|| panic!("Codegen missing variable {}", name));
        let ty = if (*idx as usize) < self.param_count as usize {
            self.param_types[*idx as usize].clone()
        } else {
            let local_idx = *idx as usize - self.param_count as usize;
            self.locals[local_idx].clone()
        };
        (*idx, ty)
    }
}

pub(crate) fn expr_type(
    expr: &Expr,
    env: &CodegenEnv,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) -> Type {
    let mut vars = HashMap::new();
    for (name, idx) in &env.bindings {
        let ty = if (*idx as usize) < env.param_count as usize {
            env.param_types[*idx as usize].clone()
        } else {
            let local_idx = *idx as usize - env.param_count as usize;
            env.locals[local_idx].clone()
        };
        vars.insert(name.clone(), ty);
    }
    check_expr(expr, &vars, signatures, globals, records, variants)
        .expect("type checking already performed")
}

/// Classify an integer-family scalar for conversion:
/// `(backed_by_i64, source_is_signed, narrowing_mask)`.
///
/// `narrowing_mask` is `Some` for refinements narrower than their i32 backing
/// (u8/u16); after a wrap or extend lands a value in an i32, the mask clamps it
/// to the type's range. `source_is_signed` decides sign- vs zero-extension when
/// widening to i64. `bool` is i32-backed and handled separately (0/1 coercion).
pub(crate) fn int_kind(ty: &Type) -> Option<(bool, bool, Option<&'static str>)> {
    match ty {
        Type::S32 => Some((false, true, None)),
        Type::U32 => Some((false, false, None)),
        Type::U16 => Some((false, false, Some("i32.const 65535"))),
        Type::U8 => Some((false, false, Some("i32.const 255"))),
        Type::Bool => Some((false, false, None)),
        Type::S64 => Some((true, true, None)),
        Type::U64 => Some((true, false, None)),
        _ => None,
    }
}

/// The WASM instruction sequence that converts a value of `from` into `to`.
/// `Some(vec![])` means the conversion is a no-op (same representation);
/// `None` means there is no supported conversion.
pub(crate) fn conversion_instr(from: &Type, to: &Type) -> Option<Vec<&'static str>> {
    use Type::*;
    if from == to {
        return Some(vec![]);
    }
    // Float <-> float.
    match (from, to) {
        (F32, F64) => return Some(vec!["f64.promote_f32"]),
        (F64, F32) => return Some(vec!["f32.demote_f64"]),
        _ => {}
    }
    let from_int = int_kind(from);
    // Casting to bool is a zero-test against the source's own width, so it must
    // come before any wrap that would discard high bits.
    if matches!(to, Bool) {
        if let Some((from64, _, _)) = from_int {
            return Some(if from64 {
                vec!["i64.const 0", "i64.ne"]
            } else {
                vec!["i32.const 0", "i32.ne"]
            });
        }
        return match from {
            F32 => Some(vec!["f32.const 0", "f32.ne"]),
            F64 => Some(vec!["f64.const 0", "f64.ne"]),
            _ => None,
        };
    }
    let to_int = int_kind(to);
    // Integer family -> integer family: adjust backing width, then clamp range.
    if let (Some((from64, from_signed, _)), Some((to64, _, to_mask))) = (from_int, to_int) {
        let mut instrs = Vec::new();
        if from64 && !to64 {
            instrs.push("i32.wrap_i64");
        } else if !from64 && to64 {
            instrs.push(if from_signed {
                "i64.extend_i32_s"
            } else {
                "i64.extend_i32_u"
            });
        }
        if let Some(mask) = to_mask {
            instrs.push(mask);
            instrs.push("i32.and");
        }
        return Some(instrs);
    }
    // Integer -> float.
    if let Some((from64, from_signed, _)) = from_int {
        return match (from64, from_signed, to) {
            (false, true, F32) => Some(vec!["f32.convert_i32_s"]),
            (false, true, F64) => Some(vec!["f64.convert_i32_s"]),
            (false, false, F32) => Some(vec!["f32.convert_i32_u"]),
            (false, false, F64) => Some(vec!["f64.convert_i32_u"]),
            (true, true, F32) => Some(vec!["f32.convert_i64_s"]),
            (true, true, F64) => Some(vec!["f64.convert_i64_s"]),
            (true, false, F32) => Some(vec!["f32.convert_i64_u"]),
            (true, false, F64) => Some(vec!["f64.convert_i64_u"]),
            _ => None,
        };
    }
    // Float -> integer: truncate to the target width/signedness, then clamp.
    if let Some((to64, to_signed, to_mask)) = to_int {
        let mut instrs = Vec::new();
        match (from, to64, to_signed) {
            (F32, false, true) => instrs.push("i32.trunc_f32_s"),
            (F64, false, true) => instrs.push("i32.trunc_f64_s"),
            (F32, false, false) => instrs.push("i32.trunc_f32_u"),
            (F64, false, false) => instrs.push("i32.trunc_f64_u"),
            (F32, true, true) => instrs.push("i64.trunc_f32_s"),
            (F64, true, true) => instrs.push("i64.trunc_f64_s"),
            (F32, true, false) => instrs.push("i64.trunc_f32_u"),
            (F64, true, false) => instrs.push("i64.trunc_f64_u"),
            _ => return None,
        }
        if let Some(mask) = to_mask {
            instrs.push(mask);
            instrs.push("i32.and");
        }
        return Some(instrs);
    }
    None
}

/// Check if a type is unit (empty tuple)
pub(crate) fn is_unit_type(ty: &Type) -> bool {
    matches!(ty, Type::Tuple(elems) if elems.is_empty())
}

pub(crate) fn wat_type(ty: &Type) -> &'static str {
    match ty {
        Type::S32 => "i32",
        Type::S64 => "i64",
        Type::F32 => "f32",
        Type::F64 => "f64",
        Type::U8 => "i32",
        Type::Bool => "i32",
        Type::U16 => "i32",
        Type::U32 => "i32",
        Type::U64 => "i64",
        // All compound types are pointer-sized (i32 handles)
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str
        | Type::Tuple(_)
        // Dynamic value is a pointer to a self-contained CGRF blob
        | Type::Any => "i32",
        // Resources are i32 handles
        Type::Resource(_) | Type::Borrow(_) => "i32",
    }
}

/// Emit the (result ...) clause for a function, or nothing if unit type
pub(crate) fn emit_wat_result(ty: &Type) -> String {
    if is_unit_type(ty) {
        String::new()
    } else {
        format!("(result {})", wat_type(ty))
    }
}

pub(crate) fn wit_type(ty: &Type) -> String {
    match ty {
        Type::S32 => "s32".to_string(),
        Type::S64 => "s64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::U8 => "u8".to_string(),
        Type::Bool => "bool".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::Record(name) | Type::Variant(name) => name.clone(),
        Type::Option(inner) => format!("option<{}>", wit_type(inner)),
        Type::Result(ok, err) => format!("result<{}, {}>", wit_type(ok), wit_type(err)),
        Type::List(inner) => format!("list<{}>", wit_type(inner)),
        Type::Str => "string".to_string(),
        Type::Any => "value".to_string(),
        Type::Resource(name) => name.clone(),
        Type::Borrow(inner) => format!("borrow<{}>", wit_type(inner)),
        Type::Tuple(elems) => {
            let inner: Vec<String> = elems.iter().map(wit_type).collect();
            format!("tuple<{}>", inner.join(", "))
        }
    }
}

/// Returns the flattened canonical ABI types for a given type.
/// Records are flattened into their fields, variants into discriminant + max payload.
pub(crate) fn flatten_type(
    ty: &Type,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) -> Vec<Type> {
    match ty {
        Type::S32 | Type::S64 | Type::F32 | Type::F64 => vec![ty.clone()],
        Type::Record(name) => {
            let record = records.get(name).expect("record not found");
            let mut result = Vec::new();
            for field in &record.fields {
                result.extend(flatten_type(&field.ty, records, variants));
            }
            result
        }
        Type::Variant(name) => {
            // Variants: discriminant (i32) + flattened max payload
            let variant = variants.get(name).expect("variant not found");
            let mut result = vec![Type::S32]; // discriminant

            // Find max payload size (in number of flattened fields)
            let max_payload_size = variant
                .cases
                .iter()
                .map(|c| {
                    c.payload
                        .iter()
                        .map(|t| flatten_type(t, records, variants).len())
                        .sum::<usize>()
                })
                .max()
                .unwrap_or(0);

            // Add i32 slots for the max payload
            for _ in 0..max_payload_size {
                result.push(Type::S32);
            }
            result
        }
        Type::Option(inner) => {
            // Option: discriminant (i32) + flattened inner type
            let mut result = vec![Type::S32]; // discriminant (0=none, 1=some)
            result.extend(flatten_type(inner, records, variants));
            result
        }
        Type::Result(ok, err) => {
            // Result: discriminant (i32) + max of ok/err flattened
            let ok_flat = flatten_type(ok, records, variants);
            let err_flat = flatten_type(err, records, variants);
            let max_size = ok_flat.len().max(err_flat.len());
            let mut result = vec![Type::S32]; // discriminant (0=ok, 1=err)
            for _ in 0..max_size {
                result.push(Type::S32);
            }
            result
        }
        Type::List(_) => {
            // List is pointer + length
            vec![Type::S32, Type::S32]
        }
        Type::Str => {
            // String is pointer + length (canonical ABI)
            vec![Type::S32, Type::S32]
        }
        Type::Tuple(_) => {
            // Tuple is a pointer
            vec![Type::S32]
        }
        Type::Any => {
            // Dynamic value is a pointer to a self-contained CGRF blob
            vec![Type::S32]
        }
        Type::U8 => {
            // U8 is stored as i32
            vec![Type::S32]
        }
        Type::Bool => {
            // Bool is stored as i32 (0/1)
            vec![Type::S32]
        }
        Type::U16 | Type::U32 => {
            // u16/u32 are stored as i32
            vec![Type::S32]
        }
        Type::U64 => {
            // U64 is stored as i64
            vec![Type::S64]
        }
        Type::Resource(_) | Type::Borrow(_) => {
            // Resources and borrows are i32 handles
            vec![Type::S32]
        }
    }
}

/// Check if a type needs ABI wrapper (is not a simple scalar or handle)
pub(crate) fn needs_abi_wrapper(ty: &Type) -> bool {
    // Scalars and resource handles don't need ABI wrappers - they pass directly as primitives
    !matches!(
        ty,
        Type::S32 | Type::S64 | Type::F32 | Type::F64 | Type::Resource(_) | Type::Borrow(_)
    )
}

/// Check if a function needs an ABI wrapper for export
pub(crate) fn function_needs_abi_wrapper(func: &Function) -> bool {
    needs_abi_wrapper(&func.return_type) || func.params.iter().any(|p| needs_abi_wrapper(&p.ty))
}

/// Generate an ABI wrapper function for exported functions with rich types.
/// The wrapper takes flattened canonical ABI params and calls the internal function.
///
/// Canonical ABI rules:
/// - Record params: flattened into individual scalar fields
/// - Variant params: flattened into discriminant + max payload fields
/// - Record/Variant returns: pointer (MAX_FLAT_RESULTS=1, so complex types stay as pointers)
pub(crate) fn generate_abi_wrapper(
    out: &mut String,
    func: &Function,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    let pad = "    ";

    // Compute flattened params (but NOT returns - returns stay as pointers for complex types)
    let mut flat_params: Vec<(String, Type)> = Vec::new();

    for param in &func.params {
        let flat = flatten_type(&param.ty, records, variants);
        for (i, ty) in flat.iter().enumerate() {
            flat_params.push((format!("{}_{}", param.name, i), ty.clone()));
        }
    }

    // For returns: scalar types stay scalar, records/variants stay as pointers
    // (Canonical ABI MAX_FLAT_RESULTS = 1, so multi-field records return as pointer)
    let return_wat = wat_type(&func.return_type);

    // Start function definition
    out.push_str(&format!("  (func ${} ", func.name));
    for (name, ty) in &flat_params {
        out.push_str(&format!("(param ${} {}) ", name, wat_type(ty)));
    }
    out.push_str(&format!("(result {})\n", return_wat));

    // For each record/variant parameter, we need a local to store the pointer
    for param in &func.params {
        if matches!(&param.ty, Type::Record(_) | Type::Variant(_)) {
            out.push_str(&format!("{}(local i32)\n", pad)); // pointer to constructed value
        }
    }

    // Construct record/variant parameters from flattened values
    let mut flat_param_idx = 0;
    let mut complex_local_idx = flat_params.len();
    let mut internal_call_args: Vec<String> = Vec::new();

    for param in &func.params {
        match &param.ty {
            Type::Record(name) => {
                let record = records.get(name).expect("record not found");
                let size = type_size(&param.ty);

                // Allocate space for the record
                out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
                out.push_str(&format!("{}local.set {}\n", pad, complex_local_idx));
                out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
                out.push_str(&format!("{}i32.const {}\n", pad, size));
                out.push_str(&format!("{}i32.add\n", pad));
                out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

                // Store each field
                let mut offset = 0;
                for field in &record.fields {
                    out.push_str(&format!("{}local.get {}\n", pad, complex_local_idx));
                    if offset > 0 {
                        out.push_str(&format!("{}i32.const {}\n", pad, offset));
                        out.push_str(&format!("{}i32.add\n", pad));
                    }
                    out.push_str(&format!(
                        "{}local.get ${}\n",
                        pad, flat_params[flat_param_idx].0
                    ));
                    out.push_str(&format!("{}{}\n", pad, store_instr(&field.ty)));
                    offset += type_size(&field.ty);
                    flat_param_idx += 1;
                }

                internal_call_args.push(format!("local.get {}", complex_local_idx));
                complex_local_idx += 1;
            }
            Type::Variant(name) => {
                let variant = variants.get(name).expect("variant not found");
                let size = type_size(&param.ty);

                // Allocate space for the variant
                out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
                out.push_str(&format!("{}local.set {}\n", pad, complex_local_idx));
                out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
                out.push_str(&format!("{}i32.const {}\n", pad, size));
                out.push_str(&format!("{}i32.add\n", pad));
                out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

                // Store discriminant (first flat param)
                out.push_str(&format!("{}local.get {}\n", pad, complex_local_idx));
                out.push_str(&format!(
                    "{}local.get ${}\n",
                    pad, flat_params[flat_param_idx].0
                ));
                out.push_str(&format!("{}i32.store\n", pad));
                flat_param_idx += 1;

                // Find max payload size for this variant
                let max_payload_count = variant
                    .cases
                    .iter()
                    .map(|c| c.payload.len())
                    .max()
                    .unwrap_or(0);

                // Store payload values
                for i in 0..max_payload_count {
                    out.push_str(&format!("{}local.get {}\n", pad, complex_local_idx));
                    out.push_str(&format!("{}i32.const {}\n", pad, 4 + i * 4));
                    out.push_str(&format!("{}i32.add\n", pad));
                    out.push_str(&format!(
                        "{}local.get ${}\n",
                        pad, flat_params[flat_param_idx].0
                    ));
                    out.push_str(&format!("{}i32.store\n", pad));
                    flat_param_idx += 1;
                }

                internal_call_args.push(format!("local.get {}", complex_local_idx));
                complex_local_idx += 1;
            }
            Type::S32 | Type::S64 | Type::F32 | Type::F64 => {
                internal_call_args.push(format!("local.get ${}", flat_params[flat_param_idx].0));
                flat_param_idx += 1;
            }
            _ => {
                // For other types (options, results, etc.), consume their flattened params
                let flat_count = flatten_type(&param.ty, records, variants).len();
                for _ in 0..flat_count {
                    flat_param_idx += 1;
                }
                // TODO: properly handle options/results
                internal_call_args.push("i32.const 0".to_string());
            }
        }
    }

    // Call the internal function - return value stays on stack (pointer or scalar)
    for arg in &internal_call_args {
        out.push_str(&format!("{}{}\n", pad, arg));
    }
    out.push_str(&format!("{}call ${}__internal\n", pad, func.name));

    // Return value is already on stack from internal function call
    // For records, it's a pointer; for scalars, it's the value

    out.push_str("  )\n");
}

/// Get the store instruction for a type
pub(crate) fn store_instr(ty: &Type) -> &'static str {
    match ty {
        Type::S32 => "i32.store",
        Type::S64 => "i64.store",
        Type::U64 => "i64.store",
        Type::F32 => "f32.store",
        Type::F64 => "f64.store",
        // Compound types are pointer-sized, resources are i32 handles
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str
        | Type::Tuple(_)
        | Type::U8
        | Type::Bool
        | Type::U16
        | Type::U32
        | Type::Resource(_)
        | Type::Borrow(_)
        | Type::Any => "i32.store",
    }
}

pub(crate) fn generate_wit(prog: &Program) -> String {
    let mut out = String::new();

    // If we have a world_config with external interfaces, generate WIT that references them
    if let Some(world_config) = &prog.world_config {
        // Package name derived from world name
        out.push_str(&format!("package package:{};\n\n", world_config.name));
        out.push_str(&format!("world {} {{\n", world_config.name));

        // External imports (e.g., theater:simple/runtime)
        for ext_import in &world_config.external_imports {
            out.push_str(&format!("  import {};\n", ext_import.to_wit_ref()));
        }

        // External exports (e.g., theater:simple/actor)
        for ext_export in &world_config.external_exports {
            out.push_str(&format!("  export {};\n", ext_export.to_wit_ref()));
        }

        // Also include any local exports that aren't part of external interfaces
        for export in &prog.exports {
            let func = find_function(prog, &export.func_name);
            out.push_str(&format!("  export {}: func(", export.export_name));
            for (i, param) in func.params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format!("{}: {}", param.name, wit_type(&param.ty)));
            }
            out.push_str(&format!(") -> {};\n", wit_type(&func.return_type)));
        }

        out.push_str("}\n");
    } else {
        // Original behavior for standalone packages
        out.push_str("package example:wisp;\n\n");
        out.push_str("world wisp {\n");

        // Generate record type declarations
        for record in &prog.records {
            out.push_str(&format!("  record {} {{\n", record.name));
            for field in &record.fields {
                out.push_str(&format!("    {}: {},\n", field.name, wit_type(&field.ty)));
            }
            out.push_str("  }\n\n");
        }

        // Generate variant type declarations
        for variant in &prog.variants {
            out.push_str(&format!("  variant {} {{\n", variant.name));
            for case in &variant.cases {
                if case.payload.is_empty() {
                    // Case with no payload: just the name
                    out.push_str(&format!("    {},\n", case.name));
                } else if case.payload.len() == 1 {
                    // Case with single payload: name(type)
                    out.push_str(&format!(
                        "    {}({}),\n",
                        case.name,
                        wit_type(&case.payload[0])
                    ));
                } else {
                    // Case with multiple payloads: name(tuple<type1, type2, ...>)
                    let types: Vec<String> = case.payload.iter().map(wit_type).collect();
                    out.push_str(&format!(
                        "    {}(tuple<{}>),\n",
                        case.name,
                        types.join(", ")
                    ));
                }
            }
            out.push_str("  }\n\n");
        }

        // Generate resource type declarations
        for resource in &prog.resources {
            out.push_str(&format!("  resource {};\n\n", resource.name));
        }

        let mut imports_by_module: BTreeMap<&str, Vec<&Import>> = BTreeMap::new();
        for import in &prog.imports {
            imports_by_module
                .entry(import.module.as_str())
                .or_default()
                .push(import);
        }
        for (module, imports) in imports_by_module {
            out.push_str(&format!("  import {}: interface {{\n", module));
            for import in imports {
                out.push_str(&format!("    {}: func(", import.name));
                for (i, param) in import.params.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&format!("{}: {}", param.name, wit_type(&param.ty)));
                }
                out.push_str(&format!(") -> {};\n", wit_type(&import.return_type)));
            }
            out.push_str("  }\n");
        }
        for export in &prog.exports {
            let func = find_function(prog, &export.func_name);
            out.push_str(&format!("  export {}: func(", export.export_name));
            for (i, param) in func.params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format!("{}: {}", param.name, wit_type(&param.ty)));
            }
            out.push_str(&format!(") -> {};\n", wit_type(&func.return_type)));
        }
        out.push_str("}\n");
    }

    out
}

/// Convert a Type to Pact syntax string.
pub(crate) fn pact_type(ty: &Type) -> String {
    match ty {
        Type::S32 => "s32".to_string(),
        Type::S64 => "s64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::U8 => "u8".to_string(),
        Type::Bool => "bool".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::Record(name) | Type::Variant(name) => name.clone(),
        Type::Option(inner) => format!("option<{}>", pact_type(inner)),
        Type::Result(ok, err) => format!("result<{}, {}>", pact_type(ok), pact_type(err)),
        Type::List(inner) => format!("list<{}>", pact_type(inner)),
        Type::Str => "string".to_string(),
        Type::Any => "value".to_string(),
        Type::Resource(name) => name.clone(),
        Type::Borrow(inner) => format!("borrow<{}>", pact_type(inner)),
        Type::Tuple(elems) => {
            let inner: Vec<String> = elems.iter().map(pact_type).collect();
            format!("tuple<{}>", inner.join(", "))
        }
    }
}

/// Generate a Pact interface definition from the program.
///
/// Pact is Theater's interface definition language, replacing WIT.
/// Unlike WIT's package/world structure, Pact uses a simpler interface model.
pub fn generate_pact(prog: &Program, default_name: &str) -> String {
    let mut out = String::new();

    // Use world config name if available, otherwise use provided default (source file name)
    let interface_name = if let Some(world_config) = &prog.world_config {
        world_config.name.as_str()
    } else {
        default_name
    };

    out.push_str(&format!("interface {} {{\n", interface_name));

    // Generate record type declarations
    for record in &prog.records {
        out.push_str(&format!("    record {} {{\n", record.name));
        for field in &record.fields {
            out.push_str(&format!(
                "        {}: {},\n",
                field.name,
                pact_type(&field.ty)
            ));
        }
        out.push_str("    }\n\n");
    }

    // Generate variant type declarations
    for variant in &prog.variants {
        out.push_str(&format!("    variant {} {{\n", variant.name));
        for case in &variant.cases {
            if case.payload.is_empty() {
                out.push_str(&format!("        {},\n", case.name));
            } else if case.payload.len() == 1 {
                out.push_str(&format!(
                    "        {}({}),\n",
                    case.name,
                    pact_type(&case.payload[0])
                ));
            } else {
                let types: Vec<String> = case.payload.iter().map(pact_type).collect();
                out.push_str(&format!(
                    "        {}(tuple<{}>),\n",
                    case.name,
                    types.join(", ")
                ));
            }
        }
        out.push_str("    }\n\n");
    }

    // Generate imports block
    let has_imports = if let Some(world_config) = &prog.world_config {
        !world_config.external_imports.is_empty()
    } else {
        !prog.imports.is_empty()
    };

    if has_imports {
        out.push_str("    imports {\n");

        if let Some(world_config) = &prog.world_config {
            // External imports (e.g., theater:simple/runtime)
            for ext_import in &world_config.external_imports {
                out.push_str(&format!("        {}\n", ext_import.to_wit_ref()));
            }
        } else {
            // Group imports by module
            let mut imports_by_module: BTreeMap<&str, Vec<&Import>> = BTreeMap::new();
            for import in &prog.imports {
                imports_by_module
                    .entry(import.module.as_str())
                    .or_default()
                    .push(import);
            }
            for (module, _imports) in imports_by_module {
                out.push_str(&format!("        {}\n", module));
            }
        }

        out.push_str("    }\n\n");
    }

    // Generate exports block
    let has_exports = if let Some(world_config) = &prog.world_config {
        !world_config.external_exports.is_empty() || !prog.exports.is_empty()
    } else {
        !prog.exports.is_empty()
    };

    if has_exports {
        out.push_str("    exports {\n");

        // External exports (interface implementations)
        if let Some(world_config) = &prog.world_config {
            for ext_export in &world_config.external_exports {
                out.push_str(&format!("        {}\n", ext_export.to_wit_ref()));
            }
        }

        // Local function exports
        for export in &prog.exports {
            let func = find_function(prog, &export.func_name);
            out.push_str(&format!("        {}: func(", export.export_name));
            for (i, param) in func.params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format!("{}: {}", param.name, pact_type(&param.ty)));
            }
            out.push_str(&format!(") -> {}\n", pact_type(&func.return_type)));
        }

        out.push_str("    }\n");
    }

    out.push_str("}\n");

    out
}

pub(crate) fn find_function<'a>(prog: &'a Program, name: &str) -> &'a Function {
    prog.functions
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("Function '{}' not found during codegen", name))
}

// ============================================================================
// REPL Compilation Support
// ============================================================================

/// Compile an expression for REPL evaluation.
///
/// Takes an expression string, variable bindings to inline, and function
/// definitions to include. Returns WASM package bytes with an exported
/// `eval` function that evaluates the expression.
pub fn compile_repl_expr(
    expr_source: &str,
    bindings: &HashMap<String, InlineValue>,
    functions: &[Function],
) -> Result<Vec<u8>> {
    let ctx = CompileContext::new(expr_source.to_string(), "<repl>".to_string());

    // Parse the expression
    let tokens = tokenize(expr_source);
    if tokens.is_empty() {
        bail!("empty expression");
    }

    let (sexpr, _) = parse_sexpr(&tokens, 0);

    // Inline variable bindings by transforming the SExpr
    let inlined_sexpr = inline_bindings(&sexpr, bindings);

    // Build function signatures from provided functions
    let mut signatures: HashMap<String, Signature> = HashMap::new();
    for func in functions {
        signatures.insert(
            func.name.clone(),
            Signature {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                result: func.return_type.clone(),
            },
        );
    }

    // Parse the expression into an Expr AST
    let expr = parse_expr(
        &inlined_sexpr,
        &[], // No local variables initially
        &signatures,
        &HashMap::new(), // No records for now
        &HashMap::new(), // No variants for now
        &ctx,
    )?;

    // Infer the return type by type-checking the expression
    let return_type = check_expr(
        &expr,
        &HashMap::new(), // No local variables
        &signatures,
        &HashMap::new(), // No globals
        &HashMap::new(), // No records
        &HashMap::new(), // No variants
    )?;

    // Create the eval function
    let eval_fn = Function {
        name: "eval".to_string(),
        params: vec![],
        return_type,
        body: expr,
    };

    // Build the program with all functions + eval
    let mut all_functions = functions.to_vec();
    all_functions.push(eval_fn);

    let prog = Program {
        functions: all_functions,
        imports: vec![],
        exports: vec![ExportDef::simple("eval".to_string())],
        globals: vec![],
        records: vec![],
        variants: vec![],
        resources: vec![],
        capabilities: HashSet::new(),
        world_config: None,
        data_segments: vec![],
    };

    // Type check the full program
    let full_signatures = collect_signatures(&prog)?;
    type_check(&prog, &full_signatures, &ctx)?;

    // Generate WAT and WIT
    let wat = generate_wat(&prog, &full_signatures);
    let wit = generate_wit(&prog);

    // Encode to WASM component
    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;
    let component_bytes = encode_component(&wasm_bytes, &wit, None, Path::new("<repl>"))?;

    Ok(component_bytes)
}

/// Transform an SExpr by inlining variable bindings as literal values
pub(crate) fn inline_bindings(sexpr: &SExpr, bindings: &HashMap<String, InlineValue>) -> SExpr {
    match sexpr {
        SExpr::Sym(name, span) => {
            if let Some(value) = bindings.get(name) {
                value_to_sexpr(value, span)
            } else {
                sexpr.clone()
            }
        }
        SExpr::List(items, span) => {
            let inlined_items: Vec<SExpr> = items
                .iter()
                .map(|item| inline_bindings(item, bindings))
                .collect();
            SExpr::List(inlined_items, span.clone())
        }
        SExpr::Quasiquote(inner, span) => {
            SExpr::Quasiquote(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::Unquote(inner, span) => {
            SExpr::Unquote(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::UnquoteSplice(inner, span) => {
            SExpr::UnquoteSplice(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        // Literals don't need inlining
        SExpr::Int { .. } | SExpr::Float { .. } | SExpr::Str(..) => sexpr.clone(),
        // Syntax forms - recurse into them
        SExpr::SyntaxQuote(inner, span) => {
            SExpr::SyntaxQuote(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::Quasisyntax(inner, span) => {
            SExpr::Quasisyntax(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::Unsyntax(inner, span) => {
            SExpr::Unsyntax(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::UnsyntaxSplice(inner, span) => {
            SExpr::UnsyntaxSplice(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
    }
}

/// Convert an InlineValue to an SExpr literal
pub(crate) fn value_to_sexpr(value: &InlineValue, span: &Span) -> SExpr {
    match value {
        InlineValue::S32(n) => SExpr::Int {
            value: *n as i64,
            ty: Type::S32,
            span: span.clone(),
        },
        InlineValue::S64(n) => SExpr::Int {
            value: *n,
            ty: Type::S64,
            span: span.clone(),
        },
        InlineValue::F32(n) => SExpr::Float {
            value: *n as f64,
            ty: Type::F32,
            span: span.clone(),
        },
        InlineValue::F64(n) => SExpr::Float {
            value: *n,
            ty: Type::F64,
            span: span.clone(),
        },
        InlineValue::Str(s) => SExpr::Str(s.clone(), span.clone()),
        // Compound types need constructor calls - for now, panic with a clear message
        InlineValue::List { .. } => {
            panic!("TODO: inline list values as constructor calls")
        }
        InlineValue::Option { .. } => {
            panic!("TODO: inline option values as constructor calls")
        }
        InlineValue::Result { .. } => {
            panic!("TODO: inline result values as constructor calls")
        }
        InlineValue::Record { .. } => {
            panic!("TODO: inline record values as constructor calls")
        }
        InlineValue::Variant { .. } => {
            panic!("TODO: inline variant values as constructor calls")
        }
    }
}

// ============================================================================
// Pack Package Generation
// ============================================================================

/// CGRF format constants
const CGRF_MAGIC: u32 = 0x46524743; // "CGRF" in little-endian
const CGRF_VERSION: u16 = 3;

/// CGRF node kinds (also used as type tags for v2 encoding)
const CGRF_BOOL: u8 = 0x01;
const CGRF_S32: u8 = 0x02;
const CGRF_S64: u8 = 0x03;
const CGRF_F32: u8 = 0x04;
const CGRF_F64: u8 = 0x05;
const CGRF_STRING: u8 = 0x06;
const CGRF_LIST: u8 = 0x07;
const CGRF_VARIANT: u8 = 0x08;
const CGRF_RECORD: u8 = 0x09;
const CGRF_OPTION: u8 = 0x0A;
const CGRF_TUPLE: u8 = 0x0B;
const CGRF_U8: u8 = 0x0C;
const CGRF_U16: u8 = 0x0D;
const CGRF_U32: u8 = 0x0E;
const CGRF_U64: u8 = 0x0F;
const CGRF_RESULT: u8 = 0x14;
const CGRF_ARRAY: u8 = 0x15;

/// Memory layout for Pack packages
const METADATA_OFFSET: i32 = 0xA000; // Pack metadata segment (8KB reserved)
const HEAP_START_OFFSET: i32 = 0xC000;

/// Get the type tag byte for a type (for CGRF v2 encoding)
pub(crate) fn type_to_tag(ty: &Type) -> u8 {
    match ty {
        Type::S32 => CGRF_S32,
        Type::S64 => CGRF_S64,
        Type::F32 => CGRF_F32,
        Type::F64 => CGRF_F64,
        Type::U8 => CGRF_U8,
        Type::Bool => CGRF_BOOL,
        Type::U16 => CGRF_U16,
        Type::U32 => CGRF_U32,
        Type::U64 => CGRF_U64,
        Type::Str => CGRF_STRING,
        Type::List(_) => CGRF_LIST,
        Type::Option(_) => CGRF_OPTION,
        Type::Result(_, _) => CGRF_RESULT,
        Type::Tuple(_) => CGRF_TUPLE,
        Type::Record(_) => CGRF_RECORD,
        Type::Variant(_) => CGRF_VARIANT,
        Type::Resource(_) => CGRF_RECORD, // Resources are treated as records for now
        Type::Borrow(inner) => type_to_tag(inner), // Borrow uses inner type's tag
        // Dynamic value has no single node kind; nesting `any` inside an
        // aggregate's v2 type tag is deferred (scalar slice uses top-level any).
        Type::Any => CGRF_VARIANT,
    }
}

/// Calculate the byte size of a type tag (for CGRF v2 encoding)
/// Simple types are 1 byte, compound types include nested type info
pub(crate) fn type_tag_size(ty: &Type) -> usize {
    match ty {
        Type::S32
        | Type::S64
        | Type::F32
        | Type::F64
        | Type::Str
        | Type::U8
        | Type::Bool
        | Type::U16
        | Type::U32
        | Type::U64 => 1,
        Type::List(inner) => 1 + type_tag_size(inner),
        Type::Option(inner) => 1 + type_tag_size(inner),
        Type::Result(ok, err) => 1 + type_tag_size(ok) + type_tag_size(err),
        Type::Tuple(elems) => 1 + 4 + elems.iter().map(type_tag_size).sum::<usize>(),
        Type::Record(name) | Type::Variant(name) | Type::Resource(name) => 1 + 4 + name.len(),
        Type::Borrow(inner) => type_tag_size(inner),
        Type::Any => 1,
    }
}

/// Convert Wisp Type to Pack Type for metadata encoding
pub(crate) fn wisp_type_to_pack_type(ty: &Type) -> pack::types::Type {
    match ty {
        Type::U8 => pack::types::Type::U8,
        Type::Bool => pack::types::Type::Bool,
        Type::U16 => pack::types::Type::U16,
        Type::U32 => pack::types::Type::U32,
        Type::U64 => pack::types::Type::U64,
        Type::S32 => pack::types::Type::S32,
        Type::S64 => pack::types::Type::S64,
        Type::F32 => pack::types::Type::F32,
        Type::F64 => pack::types::Type::F64,
        Type::Str => pack::types::Type::String,
        Type::List(inner) => pack::types::Type::List(Box::new(wisp_type_to_pack_type(inner))),
        Type::Option(inner) => pack::types::Type::Option(Box::new(wisp_type_to_pack_type(inner))),
        Type::Result(ok, err) => {
            // Pack's pact parser maps result<_, E> to Result { ok: Bool, err: E }
            // For compatibility, we convert unit (empty tuple) ok type to Bool
            let ok_type = if is_unit_type(ok) {
                pack::types::Type::Bool
            } else {
                wisp_type_to_pack_type(ok)
            };
            pack::types::Type::Result {
                ok: Box::new(ok_type),
                err: Box::new(wisp_type_to_pack_type(err)),
            }
        }
        Type::Tuple(elems) => {
            pack::types::Type::Tuple(elems.iter().map(wisp_type_to_pack_type).collect())
        }
        Type::Record(name) => pack::types::Type::Ref(pack::types::TypePath::simple(name.clone())),
        Type::Variant(name) => pack::types::Type::Ref(pack::types::TypePath::simple(name.clone())),
        Type::Resource(name) => pack::types::Type::Ref(pack::types::TypePath::simple(name.clone())),
        Type::Borrow(inner) => wisp_type_to_pack_type(inner),
        Type::Any => pack::types::Type::Value,
    }
}

/// Build the Pack `TypeDef`s for a Program's records and variants.
///
/// These are registered on each interface arena so that named references in
/// import/export signatures (`Type::Ref("runtime-error")`, `actor-info`, ...)
/// resolve *structurally* when Pack computes interface hashes — making a Wisp
/// declaration of a foreign type hash-identical to the type declared in the
/// peer's pact. Pack hashes a variant/record by its (sorted) field/case names
/// and child hashes, and does not hash the typedef set itself, so registering
/// extra, unreferenced typedefs is harmless; only referenced ones affect a hash.
pub(crate) fn program_pack_typedefs(prog: &Program) -> Vec<pack::types::TypeDef> {
    use pack::types::{Case, Field, Type as PackType, TypeDef};
    let mut defs = Vec::new();
    for rec in &prog.records {
        defs.push(TypeDef::record(
            rec.name.clone(),
            rec.fields
                .iter()
                .map(|f| Field::new(f.name.clone(), wisp_type_to_pack_type(&f.ty)))
                .collect(),
        ));
    }
    for var in &prog.variants {
        defs.push(TypeDef::variant(
            var.name.clone(),
            var.cases
                .iter()
                .map(|c| match c.payload.as_slice() {
                    [] => Case::unit(c.name.clone()),
                    [one] => Case::new(c.name.clone(), wisp_type_to_pack_type(one)),
                    many => Case::new(
                        c.name.clone(),
                        PackType::tuple(many.iter().map(wisp_type_to_pack_type).collect()),
                    ),
                })
                .collect(),
        ));
    }
    defs
}

/// Encode PackageMetadata for a Program to CGRF bytes with interface hashes.
///
/// This builds a Pack Arena from the Program and uses Pack's encode_metadata_with_hashes
/// to generate CGRF bytes that include Merkle-tree interface hashes for O(1) compatibility
/// checking at runtime.
pub(crate) fn encode_pack_metadata(prog: &Program) -> Vec<u8> {
    use pack::types::{Arena, Function, Param};
    use std::collections::HashMap;

    // Named-type definitions, registered on every interface arena so that refs in
    // signatures resolve structurally during hashing (see program_pack_typedefs).
    let typedefs = program_pack_typedefs(prog);

    let mut package = Arena::new("package");

    // Build imports section - group by interface (module) name
    let mut imports_section = Arena::new("imports");
    let mut import_by_interface: HashMap<String, Vec<Function>> = HashMap::new();

    for imp in &prog.imports {
        // For unit return type, use empty results vec (not vec![Type::Tuple(vec![])])
        let results = if is_unit_type(&imp.return_type) {
            vec![]
        } else {
            vec![wisp_type_to_pack_type(&imp.return_type)]
        };
        let func = Function::with_signature(
            imp.name.clone(),
            imp.params
                .iter()
                .map(|p| Param::new(p.name.clone(), wisp_type_to_pack_type(&p.ty)))
                .collect(),
            results,
        );
        import_by_interface
            .entry(imp.module.clone())
            .or_default()
            .push(func);
    }

    for (interface_name, funcs) in import_by_interface {
        let mut interface_arena = Arena::new(interface_name);
        for td in &typedefs {
            interface_arena.add_type(td.clone());
        }
        for func in funcs {
            interface_arena.add_function(func);
        }
        imports_section.add_child(interface_arena);
    }

    package.add_child(imports_section);

    // Build exports section - group by interface name
    let mut exports_section = Arena::new("exports");
    let mut export_by_interface: HashMap<String, Vec<Function>> = HashMap::new();

    for exp in &prog.exports {
        if let Some(func) = prog.functions.iter().find(|f| f.name == exp.func_name) {
            // For unit return type, use empty results vec
            let results = if is_unit_type(&func.return_type) {
                vec![]
            } else {
                vec![wisp_type_to_pack_type(&func.return_type)]
            };
            // Split an interface-qualified export ("pkg:ns/iface.func") into its
            // interface ("pkg:ns/iface") and bare function name ("func"), and group
            // by that interface. This is what packr's arena stores and what
            // `has_export(interface, fn)` / rpc.describe read; putting everything
            // flat under a single "exports" interface made has_export always false
            // (so e.g. lifecycle delivery, gated on has_export, silently dropped).
            // Bare exports (no interface) stay under "exports".
            let (interface_name, fn_name) = match exp.export_name.rsplit_once('.') {
                Some((iface, name)) if iface.contains('/') => (iface.to_string(), name.to_string()),
                _ => ("exports".to_string(), exp.export_name.clone()),
            };
            let pack_func = Function::with_signature(
                fn_name,
                func.params
                    .iter()
                    .map(|p| Param::new(p.name.clone(), wisp_type_to_pack_type(&p.ty)))
                    .collect(),
                results,
            );
            export_by_interface
                .entry(interface_name)
                .or_default()
                .push(pack_func);
        }
    }

    for (interface_name, funcs) in export_by_interface {
        let mut interface_arena = Arena::new(interface_name);
        for td in &typedefs {
            interface_arena.add_type(td.clone());
        }
        for func in funcs {
            interface_arena.add_function(func);
        }
        exports_section.add_child(interface_arena);
    }

    package.add_child(exports_section);

    // Use Pack's encoder which includes interface hashes
    pack::metadata::encode_metadata_with_hashes(&package)
        .expect("Failed to encode pack metadata with hashes")
}

/// Generate WAT for a Pack-compatible package.
///
/// This produces WASM with:
/// - Export functions using Pack/Graph ABI calling convention: (i32, i32, i32, i32) -> i32
/// - CGRF encoding for input/output values
/// - Memory layout with input buffer at 0x0, output at 0x4000
pub(crate) fn generate_wat_pack(prog: &Program, signatures: &HashMap<String, Signature>) -> String {
    let mut out = String::new();
    out.push_str("(module\n");

    // Generate import declarations with Pack/Graph ABI signature
    // Each import is declared as (i32, i32, i32, i32) -> i32
    for import in &prog.imports {
        // Raw import with Pack/Graph ABI calling convention. The internal symbol
        // is qualified by interface to avoid collisions across interfaces.
        out.push_str(&format!(
            "  (import \"{}\" \"{}\" (func ${} (param i32 i32 i32 i32) (result i32)))\n",
            import.module,
            import.name,
            raw_import_symbol(&import.module, &import.name)
        ));
    }

    // Large fixed memory: the self-hosted compiler never frees its bump heap, so
    // self-compiling the (now ~130KB) compiler source accumulates well over 1GB of
    // intermediate strings. 32000 pages = 2GB.
    out.push_str("  (memory (export \"memory\") 32000 32000)\n");

    // Emit data segments
    for seg in &prog.data_segments {
        out.push_str(&format!("  (data (i32.const {}) \"", seg.offset));
        for byte in &seg.bytes {
            out.push_str(&format!("\\{:02x}", byte));
        }
        out.push_str("\")\n");
    }

    // Emit Pack metadata data segment
    let metadata_bytes = encode_pack_metadata(prog);
    let metadata_len = metadata_bytes.len();
    out.push_str(&format!("  (data (i32.const {}) \"", METADATA_OFFSET));
    for byte in &metadata_bytes {
        out.push_str(&format!("\\{:02x}", byte));
    }
    out.push_str("\")\n");

    // __pack_types function - returns pointer and length of CGRF-encoded metadata
    // This enables the REPL import system to auto-detect function signatures
    out.push_str(&format!(
        r#"  (func (export "__pack_types") (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    ;; Write metadata pointer to out_ptr_ptr
    local.get $out_ptr_ptr
    i32.const {}
    i32.store
    ;; Write metadata length to out_len_ptr
    local.get $out_len_ptr
    i32.const {}
    i32.store
    ;; Return 0 for success
    i32.const 0)
"#,
        METADATA_OFFSET, metadata_len
    ));

    // Heap pointer for allocations, starts after output buffer
    out.push_str(&format!(
        "  (global $__heap_ptr (mut i32) (i32.const {}))\n",
        HEAP_START_OFFSET
    ));

    // Encoder stack pointer for nested Tuple/List encoding (0xB000-0xBFFF = 4KB)
    // Each frame saves 12 bytes: (enc_tuple_header, enc_tuple_ci_pos, enc_save_root)
    out.push_str("  (global $enc_tuple_sp (mut i32) (i32.const 0xB000))\n");

    // Emit user-defined globals
    for global in &prog.globals {
        let mutability = if global.mutable { "mut" } else { "" };
        let wasm_type = wat_type(&global.ty);
        if global.mutable {
            out.push_str(&format!(
                "  (global {} ({} {}) ({}.const {}))\n",
                global.name, mutability, wasm_type, wasm_type, global.init_value
            ));
        } else {
            out.push_str(&format!(
                "  (global {} {} ({}.const {}))\n",
                global.name, wasm_type, wasm_type, global.init_value
            ));
        }
    }

    // Build maps for codegen
    let globals_map: HashMap<String, (Type, bool)> = prog
        .globals
        .iter()
        .map(|g| (g.name.clone(), (g.ty.clone(), g.mutable)))
        .collect();
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

    // Allocator helper that grows memory when needed
    // Takes size in bytes, returns pointer to allocated block
    out.push_str(
        r#"  (func $__alloc (param $size i32) (result i32)
    (local $ptr i32)
    (local $end i32)
    (local $pages_needed i32)
    ;; Get current heap pointer
    global.get $__heap_ptr
    local.set $ptr
    ;; Calculate end of allocation
    local.get $ptr
    local.get $size
    i32.add
    local.set $end
    ;; Check if we need to grow memory
    ;; memory.size returns pages, multiply by 64KB to get bytes
    memory.size
    i32.const 65536
    i32.mul
    local.get $end
    i32.lt_u
    (if
      (then
        ;; Calculate pages needed: (end - current_size + 65535) / 65536
        local.get $end
        memory.size
        i32.const 65536
        i32.mul
        i32.sub
        i32.const 65535
        i32.add
        i32.const 65536
        i32.div_u
        local.set $pages_needed
        ;; Grow memory
        local.get $pages_needed
        memory.grow
        ;; Check if grow failed (returns -1)
        i32.const -1
        i32.eq
        (if
          (then
            ;; Out of memory - trap
            unreachable
          )
        )
      )
    )
    ;; Bump heap pointer
    local.get $end
    global.set $__heap_ptr
    ;; Return old pointer
    local.get $ptr
  )
"#,
    );

    // Pack ABI: __pack_alloc and __pack_free exports for the host
    out.push_str(
        r#"  (func (export "__pack_alloc") (param $size i32) (result i32)
    local.get $size
    call $__alloc
  )
  (func (export "__pack_free") (param $ptr i32) (param $len i32)
    ;; Simple bump allocator doesn't actually free, but we need the export
    ;; for Pack's ABI. A future optimization could track free lists.
    nop
  )
"#,
    );

    // Generate import wrapper functions. These have the original wisp signature
    // and are called by bare name (describe/self/...); raw-invoked imports call the
    // qualified raw symbol instead, leaving their wrapper as dead code. The wrapper
    // is named by the bare import name, so dedup by name: two interfaces exposing
    // the same function name (store.exists / filesystem.exists) would otherwise emit
    // two `$exists` wrappers. The bare-name-called wrappers all have unique names.
    let mut wrapped = std::collections::HashSet::new();
    for import in &prog.imports {
        if wrapped.insert(import.name.clone()) {
            generate_import_wrapper(&mut out, import);
        }
    }

    // Generate internal functions
    // These use their original names ($name) so that gen_expr's function calls work correctly
    for func in &prog.functions {
        let mut body = String::new();
        let mut env = CodegenEnv::new(&func.params);
        gen_expr(
            &func.body,
            &mut body,
            4,
            &mut env,
            signatures,
            &globals_map,
            &records_map,
            &variants_map,
            true, // Function body is in tail position
        );

        out.push_str(&format!("  (func ${} ", func.name));
        for param in &func.params {
            out.push_str(&format!("(param ${} {}) ", param.name, wat_type(&param.ty)));
        }
        let result_clause = emit_wat_result(&func.return_type);
        if result_clause.is_empty() {
            out.push('\n');
        } else {
            out.push_str(&format!("{}\n", result_clause));
        }
        for local in &env.locals {
            out.push_str(&format!("    (local {})\n", wat_type(local)));
        }
        out.push_str(&body);
        out.push_str("  )\n");
    }

    // Generate Pack wrappers for exported functions
    for export in &prog.exports {
        let func = find_function(prog, &export.func_name);
        generate_pack_wrapper(
            &mut out,
            func,
            &export.export_name,
            &records_map,
            &variants_map,
        );
    }

    out.push_str(")\n");
    out
}

/// Generate a Pack-compatible export wrapper for a function.
///
/// The wrapper has signature: (in_ptr, in_len, out_ptr_ptr, out_len_ptr) -> status
/// Guest-allocates ABI: guest allocates output buffer, writes ptr/len to provided slots.
/// Returns 0 on success, -1 on error.
pub(crate) fn generate_pack_wrapper(
    out: &mut String,
    func: &Function,
    export_name: &str,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    let wrapper_name = format!("{}__export", func.name);
    let internal_name = format!("${}", func.name);

    out.push_str(&format!(
        "  (func ${} (export \"{}\") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)\n",
        wrapper_name, export_name
    ));

    // $out_ptr is now a local - we allocate the buffer ourselves
    out.push_str("    (local $out_ptr i32)\n");
    out.push_str("    (local $bytes_written i32)\n");

    // Declare local for the result value (locals must be at the top)
    // S64 and F32/F64 use their native types; everything else is i32 (value or pointer)
    match &func.return_type {
        Type::S64 => out.push_str("    (local $value i64)\n"),
        Type::F32 => out.push_str("    (local $value f32)\n"),
        Type::F64 => out.push_str("    (local $value f64)\n"),
        _ => out.push_str("    (local $value i32)\n"),
    }

    // Declare locals for parameters
    for param in &func.params {
        let local_type = match &param.ty {
            Type::S64 => "i64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            _ => "i32", // s32, string, records, etc. are all i32
        };
        out.push_str(&format!(
            "    (local $param_{} {})\n",
            param.name, local_type
        ));
    }

    // Additional locals needed for string decoding
    let needs_string_decode = func.params.iter().any(|p| matches!(p.ty, Type::Str));
    if needs_string_decode {
        out.push_str("    (local $str_len i32)\n");
        out.push_str("    (local $str_ptr i32)\n");
    }

    // Additional locals needed for record/option/variant/result decoding
    let needs_compound_decode = func.params.iter().any(|p| {
        matches!(
            p.ty,
            Type::Record(_) | Type::Option(_) | Type::Variant(_) | Type::Result(_, _)
        )
    });
    if needs_compound_decode {
        out.push_str("    (local $rec_ptr i32)\n");
        out.push_str("    (local $field_val i32)\n");
        // Tree traversal locals needed for v2 format decoding
        out.push_str("    (local $child_idx i32)\n");
        out.push_str("    (local $child_offset i32)\n");
        out.push_str("    (local $scan_i i32)\n");
        out.push_str("    (local $payload_len i32)\n");
    }

    // Local for runtime offset tracking in tuple decoding
    // Needed when any tuple element has variable size (strings, lists, etc.)
    let needs_runtime_offset = func.params.len() > 1
        && func
            .params
            .iter()
            .any(|p| matches!(p.ty, Type::Str | Type::List(_)));
    if needs_runtime_offset {
        out.push_str("    (local $node_offset i32)\n");
        out.push_str("    (local $data_len i32)\n");
    }

    // Additional locals for list decoding
    let needs_list_decode = func.params.iter().any(|p| matches!(p.ty, Type::List(_)));
    if needs_list_decode {
        out.push_str("    (local $list_ptr i32)\n");
        out.push_str("    (local $list_len i32)\n");
        out.push_str("    (local $list_data i32)\n");
        out.push_str("    (local $list_i i32)\n");
        out.push_str("    (local $elem_offset i32)\n");
    }

    // Additional locals for tree traversal (multi-param with complex types)
    let needs_tree_traversal = func.params.len() > 1
        && func.params.iter().any(|p| {
            matches!(
                p.ty,
                Type::Record(_)
                    | Type::Option(_)
                    | Type::Variant(_)
                    | Type::Result(_, _)
                    | Type::Tuple(_)
                    | Type::List(_)
            )
        });
    if needs_tree_traversal {
        out.push_str("    (local $tuple_offset i32)\n");
        // Only declare tree traversal locals not already declared by needs_compound_decode
        if !needs_compound_decode {
            out.push_str("    (local $child_idx i32)\n");
            out.push_str("    (local $child_offset i32)\n");
            out.push_str("    (local $scan_i i32)\n");
            out.push_str("    (local $payload_len i32)\n");
        }
        // Only declare if not already declared above
        if !needs_string_decode {
            out.push_str("    (local $str_len i32)\n");
        }
        if !needs_runtime_offset {
            out.push_str("    (local $data_len i32)\n");
        }
    }

    // Locals for recursive CGRF encoding of return value
    generate_cgrf_array_locals(out);
    out.push_str("    (local $buf_cursor i32)\n");
    out.push_str("    (local $node_idx i32)\n");
    out.push_str("    (local $enc_root_idx i32)\n");
    out.push_str("    (local $enc_header_start i32)\n");
    out.push_str("    (local $enc_tmp i32)\n");
    out.push_str("    (local $enc_tmp_i64 i64)\n");
    out.push_str("    (local $enc_tmp_f32 f32)\n");
    out.push_str("    (local $enc_tmp_f64 f64)\n");
    out.push_str("    (local $enc_save_child i32)\n");
    out.push_str("    (local $enc_save_root i32)\n");
    out.push_str("    (local $enc_result_ptr i32)\n");
    out.push_str("    (local $enc_tuple_header i32)\n");
    out.push_str("    (local $enc_tuple_ci_pos i32)\n");
    out.push_str("    (local $enc_list_header i32)\n");
    out.push_str("    (local $enc_list_ci_pos i32)\n");
    out.push_str("    (local $enc_list_i i32)\n");
    out.push_str("    (local $enc_list_len i32)\n");
    out.push_str("    (local $enc_list_data i32)\n");
    out.push_str("    (local $enc_list_root_idx i32)\n");

    // Locals for recursive CGRF decoding of input parameters
    out.push_str("    (local $dec_node_offset i32)\n");
    out.push_str("    (local $dec_result i32)\n");
    out.push_str("    (local $dec_child_idx i32)\n");
    out.push_str("    (local $dec_scan_offset i32)\n");
    out.push_str("    (local $dec_scan_i i32)\n");
    out.push_str("    (local $dec_payload_len i32)\n");
    out.push_str("    (local $dec_tmp i32)\n");
    out.push_str("    (local $dec_opt_ptr i32)\n");
    out.push_str("    (local $dec_opt_node_offset i32)\n");
    out.push_str("    (local $dec_tuple_ptr i32)\n");
    out.push_str("    (local $dec_tuple_node_offset i32)\n");
    out.push_str("    (local $dec_list_ptr i32)\n");
    out.push_str("    (local $dec_list_data i32)\n");
    out.push_str("    (local $dec_list_len i32)\n");
    out.push_str("    (local $dec_list_i i32)\n");
    out.push_str("    (local $dec_list_node_offset i32)\n");

    // Locals for the dynamic `any` boundary codec (len-prefixed CGRF blob)
    let needs_any = func.params.iter().any(|p| matches!(p.ty, Type::Any))
        || matches!(func.return_type, Type::Any);
    if needs_any {
        out.push_str("    (local $any_ptr i32)\n");
        out.push_str("    (local $any_len i32)\n");
    }

    // Decode input parameters from CGRF
    if !func.params.is_empty() {
        out.push_str("    ;; Decode input parameters from CGRF\n");

        if func.params.len() == 1 {
            // Single parameter: use recursive decoder for compound types,
            // fall back to old decoder for simple types
            let param = &func.params[0];
            let use_recursive = matches!(
                param.ty,
                Type::Option(_) | Type::List(_) | Type::Tuple(_) | Type::Result(_, _)
            );
            if use_recursive {
                // Read root_index from CGRF header
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $dec_child_idx\n");
                // Find root node offset
                generate_dec_find_node_by_index(out);
                // A declared tuple is the value itself, not an argument wrapper.
                if !matches!(param.ty, Type::Tuple(_)) {
                    // Check if root is a Tuple wrapper (e.g. Theater sends Tuple(state, params))
                    // If so, unwrap by following child_indices[0] to the actual parameter node
                    // Node tag byte: Tuple = 0x0B
                    out.push_str("    ;; Check if root is a Tuple wrapper and unwrap if so\n");
                    out.push_str("    local.get $in_ptr\n");
                    out.push_str("    local.get $dec_node_offset\n");
                    out.push_str("    i32.add\n");
                    out.push_str("    i32.load8_u\n");
                    out.push_str(&format!("    i32.const {}\n", CGRF_TUPLE));
                    out.push_str("    i32.eq\n");
                    out.push_str("    (if\n");
                    out.push_str("      (then\n");
                    // Tuple node: [tag:4][payload_len:4][child_count:4][child_indices:4*N]
                    // child_indices[0] is at node_offset + 12
                    out.push_str("        local.get $in_ptr\n");
                    out.push_str("        local.get $dec_node_offset\n");
                    out.push_str("        i32.add\n");
                    out.push_str("        i32.const 12\n");
                    out.push_str("        i32.add\n");
                    out.push_str("        i32.load\n");
                    out.push_str("        local.set $dec_child_idx\n");
                    // Find the actual parameter node
                    generate_dec_find_node_by_index(out);
                    out.push_str("      )\n");
                    out.push_str("    )\n");
                    // Decode recursively (now pointing at the actual parameter node)
                }
                generate_cgrf_decode_recursive(out, &param.ty);
                // Store result
                out.push_str("    local.get $dec_result\n");
                out.push_str(&format!("    local.set $param_{}\n", param.name));
            } else {
                generate_cgrf_decode_param(
                    out,
                    &param.ty,
                    &param.name,
                    0,
                    false,
                    records,
                    variants,
                );
            }
        } else {
            // Multiple parameters: root is a tuple, decode each element
            out.push_str("    ;; Multiple params - expecting tuple root\n");

            if needs_runtime_offset {
                // Initialize node offset to 16 (start of first child node)
                out.push_str("    i32.const 16\n");
                out.push_str("    local.set $node_offset\n");
            }

            // Child nodes are encoded first (depth-first), so they start at offset 16
            // and are laid out sequentially by node index
            for (idx, param) in func.params.iter().enumerate() {
                generate_cgrf_decode_tuple_param(
                    out,
                    &param.ty,
                    &param.name,
                    idx,
                    &func.params,
                    needs_runtime_offset,
                    records,
                    variants,
                );
            }
        }
    }

    // Push parameters and call the internal function
    for param in &func.params {
        out.push_str(&format!("    local.get $param_{}\n", param.name));
    }
    out.push_str(&format!("    call {}\n", internal_name));
    // Only set $value if the function returns a non-unit type
    if !is_unit_type(&func.return_type) {
        out.push_str("    local.set $value\n");
    }

    // A dynamic `any` result is already a len-prefixed CGRF blob [len:u32][cgrf].
    // The bytes are self-describing, so the boundary just copies them straight to
    // a freshly allocated output buffer; no node encoding or header write needed.
    if matches!(&func.return_type, Type::Any) {
        out.push_str("    ;; Encode `any`: copy the len-prefixed CGRF blob to output\n");
        out.push_str("    local.get $value\n");
        out.push_str("    i32.load\n"); // CGRF byte length
        out.push_str("    local.set $any_len\n");
        out.push_str("    local.get $any_len\n");
        out.push_str("    call $__alloc\n");
        out.push_str("    local.set $out_ptr\n");
        out.push_str("    local.get $out_ptr\n"); // dest
        out.push_str("    local.get $value\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n"); // src = value + 4 (past the length prefix)
        out.push_str("    local.get $any_len\n"); // len
        out.push_str("    memory.copy\n");
        out.push_str("    local.get $out_ptr_ptr\n");
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.store\n");
        out.push_str("    local.get $out_len_ptr\n");
        out.push_str("    local.get $any_len\n");
        out.push_str("    i32.store\n");
        out.push_str("    i32.const 0\n");
        out.push_str("  )\n");
        return;
    }

    // Allocate output buffer (guest-allocates ABI)
    // For variable-size types, compute the exact size needed dynamically
    match &func.return_type {
        Type::S32 | Type::F32 => {
            // Fixed size: header(16) + node(8) + payload(4) = 28, round up to 32
            out.push_str("    i32.const 32\n");
            out.push_str("    call $__alloc\n");
            out.push_str("    local.set $out_ptr\n");
        }
        Type::S64 | Type::F64 => {
            // Fixed size: header(16) + node(8) + payload(8) = 32, round up to 40
            out.push_str("    i32.const 40\n");
            out.push_str("    call $__alloc\n");
            out.push_str("    local.set $out_ptr\n");
        }
        Type::Str => {
            // Dynamic size: header(16) + node(8) + len_field(4) + string_data
            // = 28 + string_length
            out.push_str("    ;; Allocate exact size for string: 28 + string_length\n");
            out.push_str("    local.get $value\n");
            out.push_str("    i32.load\n"); // load string length
            out.push_str("    i32.const 28\n");
            out.push_str("    i32.add\n");
            out.push_str("    call $__alloc\n");
            out.push_str("    local.set $out_ptr\n");
        }
        Type::List(elem_ty) => {
            // Dynamic size based on list length and element size
            // CGRF list: header(16) + list_node(8 + 4 + 4*n) + n * element_nodes
            // For simplicity, estimate element node size and multiply
            let elem_node_size = cgrf_element_node_size(elem_ty);
            out.push_str("    ;; Allocate size for list based on length\n");
            out.push_str("    local.get $value\n");
            out.push_str("    i32.load\n"); // load list length (first field)
            out.push_str(&format!("    i32.const {}\n", elem_node_size + 4)); // per-element overhead
            out.push_str("    i32.mul\n");
            out.push_str("    i32.const 32\n"); // base overhead (header + list node header)
            out.push_str("    i32.add\n");
            out.push_str("    call $__alloc\n");
            out.push_str("    local.set $out_ptr\n");
        }
        _ => {
            // For other types, use a generous fixed size
            // TODO: compute dynamically for nested variable-size types
            out.push_str("    i32.const 16384\n");
            out.push_str("    call $__alloc\n");
            out.push_str("    local.set $out_ptr\n");
        }
    }

    // Encode result using recursive CGRF encoder
    out.push_str("    ;; Encode result value to CGRF (recursive encoder)\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    local.set $buf_cursor\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    local.set $node_idx\n");
    generate_cgrf_encode_recursive(out, &func.return_type, "$value");
    // Write CGRF header at offset 0
    out.push_str("    ;; Write CGRF header\n");
    // Magic
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");
    // Version
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");
    // Flags
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");
    // Node count
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $node_idx\n");
    out.push_str("    i32.store\n");
    // Root index
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $enc_root_idx\n");
    out.push_str("    i32.store\n");
    // Push bytes_written ($buf_cursor) for the ABI return
    out.push_str("    local.get $buf_cursor\n");

    // Guest-allocates ABI: save bytes_written, write ptr/len to slots, return 0
    out.push_str("    local.set $bytes_written\n");
    out.push_str("    ;; Write output pointer to out_ptr_ptr slot\n");
    out.push_str("    local.get $out_ptr_ptr\n");
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.store\n");
    out.push_str("    ;; Write output length to out_len_ptr slot\n");
    out.push_str("    local.get $out_len_ptr\n");
    out.push_str("    local.get $bytes_written\n");
    out.push_str("    i32.store\n");
    out.push_str("    ;; Return 0 for success\n");
    out.push_str("    i32.const 0\n");

    out.push_str("  )\n");
}

/// The internal symbol for an import's raw CGRF entry point, qualified by
/// interface so same-named functions in different interfaces don't collide
/// (e.g. store.exists vs filesystem.exists). The WASM import's module/name are
/// unchanged; only this internal func symbol is disambiguated.
pub(crate) fn raw_import_symbol(module: &str, name: &str) -> String {
    let iface: String = module
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect();
    format!("__raw_{}_{}", iface, name)
}

/// Generate an import wrapper function.
///
/// The wrapper has the original wisp signature but internally:
/// 1. Encodes arguments to CGRF in a buffer
/// 2. Calls the raw import (which has Pack/Graph ABI signature)
/// 3. Decodes the result (if any)
pub(crate) fn generate_import_wrapper(out: &mut String, import: &Import) {
    let wrapper_name = &import.name;
    let raw_name = format!("${}", raw_import_symbol(&import.module, &import.name));

    // Start function with original signature
    out.push_str(&format!("  (func ${} ", wrapper_name));
    for param in &import.params {
        out.push_str(&format!("(param ${} {}) ", param.name, wat_type(&param.ty)));
    }

    // Handle result type - unit (empty tuple) means no return value
    let result_clause = emit_wat_result(&import.return_type);
    if result_clause.is_empty() {
        out.push('\n');
    } else {
        out.push_str(&format!("{}\n", result_clause));
    }

    // Local variables for encoding (guest-allocates ABI)
    out.push_str("    (local $in_buf i32)\n");
    out.push_str("    (local $in_len i32)\n");
    out.push_str("    (local $out_ptr i32)\n");
    out.push_str("    (local $out_len i32)\n");
    out.push_str("    (local $status i32)\n");
    out.push_str("    (local $result_slots i32)\n");
    generate_cgrf_array_locals(out);

    // Check if we need decoder locals for complex return types
    let needs_complex_decode = matches!(
        &import.return_type,
        Type::Tuple(_) | Type::Option(_) | Type::List(_) | Type::Result(_, _) | Type::Str
    );
    if needs_complex_decode {
        out.push_str("    (local $in_ptr i32)\n"); // for decoder - points to output buffer
        out.push_str("    (local $dec_node_offset i32)\n");
        out.push_str("    (local $dec_result i32)\n");
        out.push_str("    (local $dec_child_idx i32)\n");
        out.push_str("    (local $dec_scan_offset i32)\n");
        out.push_str("    (local $dec_scan_i i32)\n");
        out.push_str("    (local $dec_payload_len i32)\n");
        out.push_str("    (local $dec_tmp i32)\n");
        out.push_str("    (local $dec_opt_ptr i32)\n");
        out.push_str("    (local $dec_opt_node_offset i32)\n");
        out.push_str("    (local $dec_tuple_ptr i32)\n");
        out.push_str("    (local $dec_tuple_node_offset i32)\n");
        out.push_str("    (local $dec_list_ptr i32)\n");
        out.push_str("    (local $dec_list_data i32)\n");
        out.push_str("    (local $dec_list_len i32)\n");
        out.push_str("    (local $dec_list_i i32)\n");
        out.push_str("    (local $dec_list_node_offset i32)\n");
    }

    // Local for the dynamic `any` boundary codec (len-prefixed CGRF blob)
    let needs_any = matches!(import.return_type, Type::Any)
        || import.params.iter().any(|p| matches!(p.ty, Type::Any));
    if needs_any {
        out.push_str("    (local $any_ptr i32)\n");
    }

    // Check if we need encoder locals for complex parameter types
    // Also need encoder locals when we have multiple params (including strings) since they
    // get wrapped in a tuple using the recursive encoder
    let needs_complex_encode = import.params.iter().any(|p| {
        matches!(
            &p.ty,
            Type::Tuple(_)
                | Type::Option(_)
                | Type::List(_)
                | Type::Result(_, _)
                | Type::Record(_)
                | Type::Variant(_)
                | Type::Str
        )
    }) || import.params.len() > 1
        // A single non-s32 scalar param (bool/u16/u32/u64/f32/f64/u8) uses the
        // generic single-arg encoder below, which needs $buf_cursor et al.
        || (import.params.len() == 1 && !matches!(import.params[0].ty, Type::S32));
    if needs_complex_encode {
        out.push_str("    (local $buf_cursor i32)\n");
        out.push_str("    (local $node_idx i32)\n");
        out.push_str("    (local $enc_root_idx i32)\n");
        out.push_str("    (local $enc_header_start i32)\n");
        out.push_str("    (local $enc_tmp i32)\n");
        out.push_str("    (local $enc_tmp_i64 i64)\n");
        out.push_str("    (local $enc_tmp_f32 f32)\n");
        out.push_str("    (local $enc_tmp_f64 f64)\n");
        out.push_str("    (local $enc_save_child i32)\n");
        out.push_str("    (local $enc_save_root i32)\n");
        out.push_str("    (local $enc_result_ptr i32)\n");
        out.push_str("    (local $enc_tuple_header i32)\n");
        out.push_str("    (local $enc_tuple_ci_pos i32)\n");
        out.push_str("    (local $enc_list_header i32)\n");
        out.push_str("    (local $enc_list_ci_pos i32)\n");
        out.push_str("    (local $enc_list_i i32)\n");
        out.push_str("    (local $enc_list_len i32)\n");
        out.push_str("    (local $enc_list_data i32)\n");
        out.push_str("    (local $enc_list_root_idx i32)\n");
    }

    // Allocate I/O buffers from heap (avoids collisions when modules are composed)
    // Result ptr/len slots: 8 bytes (ptr at offset 0, len at offset 4)
    out.push_str("    ;; Allocate result slots from heap (8 bytes)\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    call $__alloc\n");
    out.push_str("    local.set $result_slots\n");

    // Input buffer: allocate enough space for CGRF-encoded arguments
    // Use larger buffer for complex types (tuples with options, nested lists, etc.)
    let input_buf_size = if needs_complex_encode { 16384 } else { 4096 };
    out.push_str(&format!(
        "    ;; Allocate input buffer from heap ({} bytes)\n",
        input_buf_size
    ));
    out.push_str(&format!("    i32.const {}\n", input_buf_size));
    out.push_str("    call $__alloc\n");
    out.push_str("    local.set $in_buf\n");

    // Encode arguments to CGRF
    // For single string argument (like log), encode as a string node
    if import.params.len() == 1 && matches!(import.params[0].ty, Type::Str) {
        // String is passed as (ptr, len) in WASM
        // We need to encode it as CGRF string node
        let param_name = &import.params[0].name;

        // Write CGRF header
        out.push_str("    ;; Write CGRF header for string argument\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC)); // "CGRF"
        out.push_str("    i32.store\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION)); // version
        out.push_str("    i32.store16\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // flags
        out.push_str("    i32.store16\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n"); // node_count
        out.push_str("    i32.store\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // root_index
        out.push_str("    i32.store\n");

        // Write string node (kind=0x06 for String)
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 16\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 6\n"); // kind = String
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 17\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // flags
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 18\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // reserved
        out.push_str("    i32.store16\n");

        // Payload length = 4 (length prefix) + string length
        // CGRF string format: payload_len includes a 4-byte length prefix
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 20\n");
        out.push_str("    i32.add\n");
        // String in wisp is a pointer to (len: i32, data: bytes...)
        // Payload length = 4 + string_len
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.load\n"); // load string length
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n"); // payload_len = 4 + string_len
        out.push_str("    i32.store\n");

        // Write string length at offset 24 (part of payload)
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 24\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.load\n"); // string length
        out.push_str("    i32.store\n");

        // Copy string data to offset 28
        out.push_str("    ;; Copy string data to CGRF buffer\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 28\n");
        out.push_str("    i32.add\n"); // destination (after length prefix)
        out.push_str(&format!("    local.get ${}\n", param_name)); // source ptr (string data location)
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n"); // skip wisp string length prefix
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.load\n"); // load length
        out.push_str("    memory.copy\n");

        // Calculate total buffer length: 16 (header) + 8 (node header) + 4 (string len) + string_len
        // = 28 + string_len
        out.push_str("    i32.const 28\n"); // header + node header + length prefix
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.load\n"); // string length
        out.push_str("    i32.add\n");
        out.push_str("    local.set $in_len\n");
    } else if import.params.is_empty() {
        // No arguments - encode empty tuple
        out.push_str("    ;; No arguments - encode empty tuple\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION));
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n"); // one node (empty tuple)
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store\n");
        // Tuple node with 0 children
        // Node format: [kind:1][flags:1][reserved:2][payload_len:4][payload...]
        // Tuple payload: [element_count:4][child_indices:4*N]
        // For empty tuple: payload = [0:u32], payload_len = 4
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 16\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_TUPLE)); // kind = Tuple (0x0B)
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 17\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // flags
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 18\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // reserved
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 20\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 4\n"); // payload_len = 4 (for element_count field)
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 24\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // element_count = 0
        out.push_str("    i32.store\n");
        out.push_str("    i32.const 28\n"); // total length: 16 header + 8 node header + 4 payload
        out.push_str("    local.set $in_len\n");
    } else if import.params.len() == 1 && matches!(import.params[0].ty, Type::S32) {
        // Single s32 argument — encode as CGRF S32 node
        let param_name = &import.params[0].name;
        out.push_str("    ;; Encode single s32 argument to CGRF\n");
        // CGRF header (16 bytes)
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n"); // node_count = 1
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // root_index = 0
        out.push_str("    i32.store\n");
        // S32 node: kind=0x02, flags=0, reserved=0, payload_len=4, value
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 16\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_S32 as i32));
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 17\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 18\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 20\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 4\n"); // payload_len = 4
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 24\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.store\n");
        out.push_str("    i32.const 28\n"); // total = 16 header + 8 node header + 4 payload
        out.push_str("    local.set $in_len\n");
    } else if import.params.iter().all(|p| matches!(p.ty, Type::S32)) {
        // Multiple s32 arguments — encode as CGRF tuple of S32 nodes
        // CGRF layout: children first, tuple root last
        // header(16) + n * s32_node(12) + tuple_node(8 + 4*n)
        let n = import.params.len();
        let s32_node_size = 12; // 8 header + 4 payload
        let tuple_payload_len = 4 * n; // child indices, 4 bytes each
        let tuple_node_total = 8 + tuple_payload_len;
        let total_len = 16 + n * s32_node_size + tuple_node_total;

        out.push_str(&format!(
            "    ;; Encode {} s32 arguments as CGRF tuple (children first)\n",
            n
        ));
        // CGRF header
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", n + 1)); // node_count = n children + 1 tuple
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", n)); // root_index = n (tuple is last)
        out.push_str("    i32.store\n");

        // Write S32 child nodes first, starting at offset 16
        for (i, param) in import.params.iter().enumerate() {
            let node_offset = 16 + i * s32_node_size;
            // kind = S32
            out.push_str("    local.get $in_buf\n");
            out.push_str(&format!("    i32.const {}\n", node_offset));
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", CGRF_S32 as i32));
            out.push_str("    i32.store8\n");
            // flags
            out.push_str("    local.get $in_buf\n");
            out.push_str(&format!("    i32.const {}\n", node_offset + 1));
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 0\n");
            out.push_str("    i32.store8\n");
            // reserved
            out.push_str("    local.get $in_buf\n");
            out.push_str(&format!("    i32.const {}\n", node_offset + 2));
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 0\n");
            out.push_str("    i32.store16\n");
            // payload_len = 4
            out.push_str("    local.get $in_buf\n");
            out.push_str(&format!("    i32.const {}\n", node_offset + 4));
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.store\n");
            // payload value
            out.push_str("    local.get $in_buf\n");
            out.push_str(&format!("    i32.const {}\n", node_offset + 8));
            out.push_str("    i32.add\n");
            out.push_str(&format!("    local.get ${}\n", param.name));
            out.push_str("    i32.store\n");
        }

        // Write tuple node last, after all children
        let tuple_offset = 16 + n * s32_node_size;
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", tuple_offset));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_TUPLE as i32));
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", tuple_offset + 1));
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", tuple_offset + 2));
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", tuple_offset + 4));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", tuple_payload_len));
        out.push_str("    i32.store\n");

        // Write child indices (0, 1, 2, ...)
        for i in 0..n {
            out.push_str("    local.get $in_buf\n");
            out.push_str(&format!("    i32.const {}\n", tuple_offset + 8 + i * 4));
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", i)); // child node index
            out.push_str("    i32.store\n");
        }

        out.push_str(&format!("    i32.const {}\n", total_len));
        out.push_str("    local.set $in_len\n");
    } else if import.params.len() == 1 {
        // Use the shared encoder so primitive lists always use v3 Array nodes.
        generate_import_generic_encode(out, &import.params[0]);
    } else {
        // Multiple complex arguments - wrap in a CGRF tuple
        // Each param is already a local pointing to its value.
        // We need to:
        // 1. Encode each param as a child node
        // 2. Create a tuple node referencing all children
        let n = import.params.len();
        out.push_str(&format!(
            "    ;; Encode {} complex arguments as CGRF tuple\n",
            n
        ));

        // Set up encoding: use $in_buf as $out_ptr, cursor at 16 (after header)
        out.push_str("    local.get $in_buf\n");
        out.push_str("    local.set $out_ptr\n");
        out.push_str("    i32.const 16\n");
        out.push_str("    local.set $buf_cursor\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    local.set $node_idx\n");

        // Write tuple node header first (will patch later)
        // Reserve space for: header(8) + child_count(4) + N*child_idx(4*N)
        let tuple_payload_len = 4 + 4 * n;
        out.push_str("    ;; Write tuple node header (placeholder)\n");
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    local.set $enc_tuple_header\n");
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_TUPLE as i32));
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 2\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        // payload_len - will be patched later but set placeholder
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", tuple_payload_len));
        out.push_str("    i32.store\n");
        // child_count
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", n));
        out.push_str("    i32.store\n");
        // Save position of child_indices array
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.set $enc_tuple_ci_pos\n");

        // Save tuple's node index
        out.push_str("    local.get $node_idx\n");
        out.push_str("    local.set $enc_save_root\n");
        // Increment node_idx
        out.push_str("    local.get $node_idx\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.set $node_idx\n");
        // Advance cursor past tuple node header + payload
        out.push_str("    local.get $buf_cursor\n");
        out.push_str(&format!("    i32.const {}\n", 8 + tuple_payload_len));
        out.push_str("    i32.add\n");
        out.push_str("    local.set $buf_cursor\n");

        // Encode each param as a child and store its index
        for (i, param) in import.params.iter().enumerate() {
            out.push_str(&format!("    ;; Encode param {} ({})\n", i, param.name));
            let param_local = format!("${}", param.name);
            generate_cgrf_encode_recursive(out, &param.ty, &param_local);

            // Write child's node index to child_indices[i]
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $enc_tuple_ci_pos\n");
            out.push_str("    i32.add\n");
            if i > 0 {
                out.push_str(&format!("    i32.const {}\n", i * 4));
                out.push_str("    i32.add\n");
            }
            out.push_str("    local.get $enc_root_idx\n");
            out.push_str("    i32.store\n");
        }

        // Set root index to tuple's node index
        out.push_str("    local.get $enc_save_root\n");
        out.push_str("    local.set $enc_root_idx\n");

        // Write CGRF header
        out.push_str("    ;; Write CGRF header\n");
        // Magic
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
        out.push_str("    i32.store\n");
        // Version
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
        out.push_str("    i32.store16\n");
        // Flags
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        // Node count
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $node_idx\n");
        out.push_str("    i32.store\n");
        // Root index (the tuple)
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $enc_root_idx\n");
        out.push_str("    i32.store\n");

        // Set in_len from buf_cursor
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    local.set $in_len\n");
    }

    // Call the raw import (guest-allocates ABI)
    out.push_str("    ;; Call raw import with ptr/len slots\n");
    out.push_str("    local.get $in_buf\n");
    out.push_str("    local.get $in_len\n");
    out.push_str("    local.get $result_slots\n"); // result_ptr slot
    out.push_str("    local.get $result_slots\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n"); // result_len slot = result_slots + 4
    out.push_str(&format!("    call {}\n", raw_name));
    out.push_str("    local.set $status\n");

    // Read the result ptr and len from the slots
    out.push_str("    local.get $result_slots\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $out_ptr\n");
    out.push_str("    local.get $result_slots\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $out_len\n");

    // Decode result from CGRF output buffer
    // The result is a single CGRF node: header(16) + node_header(8) + payload
    // The scalar value is at $out_ptr + 24
    // Unit type (empty tuple) has no return value
    if !is_unit_type(&import.return_type) {
        out.push_str("    ;; Decode result from CGRF\n");
        match &import.return_type {
            Type::S32 => {
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    i32.const 24\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
            }
            Type::S64 | Type::U64 => {
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    i32.const 24\n");
                out.push_str("    i32.add\n");
                out.push_str("    i64.load\n");
            }
            Type::U8 | Type::Bool => {
                // 1-byte payload, returned as i32 (wat_type is i32)
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    i32.const 24\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load8_u\n");
            }
            Type::U16 => {
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    i32.const 24\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load16_u\n");
            }
            Type::U32 => {
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    i32.const 24\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
            }
            Type::F32 => {
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    i32.const 24\n");
                out.push_str("    i32.add\n");
                out.push_str("    f32.load\n");
            }
            Type::F64 => {
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    i32.const 24\n");
                out.push_str("    i32.add\n");
                out.push_str("    f64.load\n");
            }
            Type::Tuple(_) | Type::Option(_) | Type::List(_) | Type::Result(_, _) | Type::Str => {
                // Use recursive decoder for complex types
                // Set up $in_ptr to point to the output buffer (becomes input for decoding)
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    local.set $in_ptr\n");
                // Read root index from header (offset 12) and find that node
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $dec_child_idx\n");
                // Scan to find the root node
                generate_dec_find_node_by_index(out);
                // Decode the value recursively from the root node
                generate_cgrf_decode_recursive(out, &import.return_type);
                // Result is in $dec_result
                out.push_str("    local.get $dec_result\n");
            }
            Type::Any => {
                // Wrap the returned CGRF (out_ptr, out_len) into the uniform
                // len-prefixed blob [len:u32][cgrf bytes] the guest holds for `any`.
                out.push_str("    local.get $out_len\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n");
                out.push_str("    call $__alloc\n");
                out.push_str("    local.set $any_ptr\n");
                out.push_str("    local.get $any_ptr\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    local.get $out_len\n");
                out.push_str("    memory.copy\n");
                out.push_str("    local.get $any_ptr\n");
                out.push_str("    local.get $out_len\n");
                out.push_str("    i32.store\n");
                out.push_str("    local.get $any_ptr\n");
            }
            _ => out.push_str("    i32.const 0\n"),
        }
    }

    out.push_str("  )\n");
}

/// Generate WAT to encode a single complex parameter to CGRF using the recursive encoder.
/// Uses $in_buf as the output buffer and sets $in_len to the final encoded length.
pub(crate) fn generate_import_generic_encode(out: &mut String, param: &Parameter) {
    let param_name = &param.name;
    let param_ty = &param.ty;

    out.push_str(&format!(
        "    ;; Generic CGRF encode for {:?} parameter\n",
        param_ty
    ));

    // Set up encoding: use $in_buf as $out_ptr, cursor at 16 (after header)
    out.push_str("    local.get $in_buf\n");
    out.push_str("    local.set $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    local.set $buf_cursor\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    local.set $node_idx\n");

    // Encode the parameter value
    let value_local = format!("${}", param_name);
    generate_cgrf_encode_recursive(out, param_ty, &value_local);

    // Write CGRF header at offset 0
    out.push_str("    ;; Write CGRF header\n");
    // Magic
    out.push_str("    local.get $in_buf\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");
    // Version
    out.push_str("    local.get $in_buf\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");
    // Flags
    out.push_str("    local.get $in_buf\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");
    // Node count
    out.push_str("    local.get $in_buf\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $node_idx\n");
    out.push_str("    i32.store\n");
    // Root index
    out.push_str("    local.get $in_buf\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $enc_root_idx\n");
    out.push_str("    i32.store\n");

    // Set in_len from buf_cursor
    out.push_str("    local.get $buf_cursor\n");
    out.push_str("    local.set $in_len\n");
}

// =============================================================================
// Recursive CGRF encoding
// =============================================================================

/// Compute the type tag bytes for a type at compile time.
pub(crate) fn type_tag_bytes(ty: &Type) -> Vec<u8> {
    let mut bytes = vec![type_to_tag(ty)];
    match ty {
        Type::S32
        | Type::S64
        | Type::F32
        | Type::F64
        | Type::Str
        | Type::U8
        | Type::Bool
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::Any => {}
        Type::List(inner) => bytes.extend(type_tag_bytes(inner)),
        Type::Option(inner) => bytes.extend(type_tag_bytes(inner)),
        Type::Result(ok, err) => {
            bytes.extend(type_tag_bytes(ok));
            bytes.extend(type_tag_bytes(err));
        }
        Type::Tuple(elems) => {
            let count = elems.len() as u32;
            bytes.extend(count.to_le_bytes());
            for elem in elems {
                bytes.extend(type_tag_bytes(elem));
            }
        }
        Type::Record(name) | Type::Variant(name) | Type::Resource(name) => {
            let name_len = name.len() as u32;
            bytes.extend(name_len.to_le_bytes());
            bytes.extend(name.bytes());
        }
        Type::Borrow(inner) => return type_tag_bytes(inner),
    }
    bytes
}

/// Generate WAT to write type tag bytes at $out_ptr + $buf_cursor, advancing $buf_cursor.
pub(crate) fn generate_write_type_tag_at_cursor(out: &mut String, ty: &Type) {
    let bytes = type_tag_bytes(ty);
    for (i, byte) in bytes.iter().enumerate() {
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    local.get $buf_cursor\n");
        out.push_str("    i32.add\n");
        if i > 0 {
            out.push_str(&format!("    i32.const {}\n", i));
            out.push_str("    i32.add\n");
        }
        out.push_str(&format!("    i32.const {}\n", byte));
        out.push_str("    i32.store8\n");
    }
    // Advance cursor
    out.push_str("    local.get $buf_cursor\n");
    out.push_str(&format!("    i32.const {}\n", bytes.len()));
    out.push_str("    i32.add\n");
    out.push_str("    local.set $buf_cursor\n");
}

/// Write a CGRF node header at $buf_cursor (kind, flags=0, reserved=0, payload_len=0).
/// Advances $buf_cursor by 8. Caller must patch payload_len at $enc_header_start + 4.
pub(crate) fn generate_write_node_header(out: &mut String, kind: u8) {
    // Save header start position
    out.push_str("    local.get $buf_cursor\n");
    out.push_str("    local.set $enc_header_start\n");
    // kind
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    local.get $buf_cursor\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", kind as i32));
    out.push_str("    i32.store8\n");
    // flags = 0
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    local.get $buf_cursor\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");
    // reserved = 0
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    local.get $buf_cursor\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 2\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");
    // payload_len = 0 (placeholder)
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    local.get $buf_cursor\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store\n");
    // Advance cursor past header
    out.push_str("    local.get $buf_cursor\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.set $buf_cursor\n");
}

/// Patch the payload_len at $enc_header_start + 4 to be ($buf_cursor - $enc_header_start - 8).
pub(crate) fn generate_patch_payload_len(out: &mut String) {
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    local.get $enc_header_start\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $buf_cursor\n");
    out.push_str("    local.get $enc_header_start\n");
    out.push_str("    i32.sub\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.sub\n");
    out.push_str("    i32.store\n");
}

/// Load the inner value from a Wisp compound type at value_local + offset into $enc_tmp.
/// Handles different type sizes (i32/i64/f32/f64).
pub(crate) fn generate_load_inner_value(
    out: &mut String,
    inner_ty: &Type,
    value_local: &str,
    offset: usize,
) {
    out.push_str(&format!("    local.get {}\n", value_local));
    if offset > 0 {
        out.push_str(&format!("    i32.const {}\n", offset));
        out.push_str("    i32.add\n");
    }
    match inner_ty {
        Type::S64 => {
            out.push_str("    i64.load\n");
            out.push_str("    local.set $enc_tmp_i64\n");
        }
        Type::F32 => {
            out.push_str("    f32.load\n");
            out.push_str("    local.set $enc_tmp_f32\n");
        }
        Type::F64 => {
            out.push_str("    f64.load\n");
            out.push_str("    local.set $enc_tmp_f64\n");
        }
        _ => {
            // i32 or pointer
            out.push_str("    i32.load\n");
            out.push_str("    local.set $enc_tmp\n");
        }
    }
}

/// Get the local name for a value of a given type.
pub(crate) fn enc_local_for_type(ty: &Type) -> &'static str {
    match ty {
        Type::S64 => "$enc_tmp_i64",
        Type::F32 => "$enc_tmp_f32",
        Type::F64 => "$enc_tmp_f64",
        _ => "$enc_tmp",
    }
}

/// Width of a primitive element in a packed CGRF Array node.
pub(crate) fn cgrf_array_width(ty: &Type) -> Option<usize> {
    match ty {
        Type::U8 => Some(1),
        Type::S32 | Type::F32 => Some(4),
        Type::S64 | Type::F64 => Some(8),
        _ => None,
    }
}

// Dedicated scratch locals keep array processing from overwriting enclosing
// option, tuple, or non-primitive list encoder/decoder state.
pub(crate) fn generate_cgrf_array_locals(out: &mut String) {
    for name in ["array_ptr", "array_len", "array_data", "array_i"] {
        out.push_str(&format!("    (local ${name} i32)\n"));
    }
}

/// Primitive lists use one Array node: [element tag:u8, count:u32, packed data].
/// Wisp stores u8 list elements in four-byte slots, so those need repacking.
pub(crate) fn generate_cgrf_encode_array(out: &mut String, elem_ty: &Type, value_local: &str) {
    let width = cgrf_array_width(elem_ty).expect("primitive array element");
    let stride = type_size(elem_ty);
    let (load, store) = match width {
        1 => ("i32.load8_u", "i32.store8"),
        4 => ("i32.load", "i32.store"),
        8 => ("i64.load", "i64.store"),
        _ => unreachable!(),
    };
    out.push_str(&format!(
        "    local.get {value_local}\n    local.set $array_ptr\n"
    ));
    out.push_str(
        "    local.get $array_ptr\n    i32.load\n    local.set $array_len\n\
         local.get $array_ptr\n    i32.load offset=8\n    local.set $array_data\n\
         local.get $node_idx\n    local.set $enc_root_idx\n",
    );
    generate_write_node_header(out, CGRF_ARRAY);
    generate_write_type_tag_at_cursor(out, elem_ty);
    out.push_str(&format!(
        r#"    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    local.get $array_len
    i32.store
    local.get $buf_cursor
    i32.const 4
    i32.add
    local.set $buf_cursor
    i32.const 0
    local.set $array_i
    block $array_done
      loop $array_next
        local.get $array_i
        local.get $array_len
        i32.ge_u
        br_if $array_done
        local.get $out_ptr
        local.get $buf_cursor
        i32.add
        local.get $array_data
        local.get $array_i
        i32.const {stride}
        i32.mul
        i32.add
        {load}
        {store}
        local.get $buf_cursor
        i32.const {width}
        i32.add
        local.set $buf_cursor
        local.get $array_i
        i32.const 1
        i32.add
        local.set $array_i
        br $array_next
      end
    end
"#
    ));
    generate_patch_payload_len(out);
    out.push_str(
        "    local.get $node_idx\n    i32.const 1\n    i32.add\n    local.set $node_idx\n",
    );
}

pub(crate) fn generate_cgrf_decode_array(out: &mut String, elem_ty: &Type) {
    let width = cgrf_array_width(elem_ty).expect("primitive array element");
    let stride = type_size(elem_ty);
    let (load, store) = match width {
        1 => ("i32.load8_u", "i32.store"),
        4 => ("i32.load", "i32.store"),
        8 => ("i64.load", "i64.store"),
        _ => unreachable!(),
    };
    out.push_str(&format!(
        r#"    local.get $in_ptr
    local.get $dec_node_offset
    i32.add
    i32.load offset=9
    local.set $array_len
    i32.const 12
    call $__alloc
    local.set $array_ptr
    local.get $array_len
    i32.const {stride}
    i32.mul
    call $__alloc
    local.set $array_data
    local.get $array_ptr
    local.get $array_len
    i32.store
    local.get $array_ptr
    local.get $array_len
    i32.store offset=4
    local.get $array_ptr
    local.get $array_data
    i32.store offset=8
    i32.const 0
    local.set $array_i
    block $array_done
      loop $array_next
        local.get $array_i
        local.get $array_len
        i32.ge_u
        br_if $array_done
        local.get $array_data
        local.get $array_i
        i32.const {stride}
        i32.mul
        i32.add
        local.get $in_ptr
        local.get $dec_node_offset
        i32.add
        local.get $array_i
        i32.const {width}
        i32.mul
        i32.add
        {load} offset=13
        {store}
        local.get $array_i
        i32.const 1
        i32.add
        local.set $array_i
        br $array_next
      end
    end
    local.get $array_ptr
    local.set $dec_result
"#
    ));
}

/// Recursively encode a Wisp value as CGRF nodes using the wrapper's encoder
/// and array scratch locals. Advances $buf_cursor and $node_idx and leaves the
/// encoded subtree's root index in $enc_root_idx.
pub(crate) fn generate_cgrf_encode_recursive(out: &mut String, ty: &Type, value_local: &str) {
    match ty {
        Type::List(elem_ty) if cgrf_array_width(elem_ty).is_some() => {
            generate_cgrf_encode_array(out, elem_ty, value_local);
        }
        Type::S32
        | Type::U8
        | Type::Bool
        | Type::U16
        | Type::U32
        | Type::S64
        | Type::U64
        | Type::F32
        | Type::F64 => {
            let (kind, payload_size, store_instr) = match ty {
                Type::S32 => (CGRF_S32, 4, "i32.store"),
                Type::U8 => (CGRF_U8, 1, "i32.store8"),
                Type::Bool => (CGRF_BOOL, 1, "i32.store8"),
                Type::U16 => (CGRF_U16, 2, "i32.store16"),
                Type::U32 => (CGRF_U32, 4, "i32.store"),
                Type::S64 => (CGRF_S64, 8, "i64.store"),
                Type::U64 => (CGRF_U64, 8, "i64.store"),
                Type::F32 => (CGRF_F32, 4, "f32.store"),
                Type::F64 => (CGRF_F64, 8, "f64.store"),
                _ => unreachable!(),
            };
            out.push_str(&format!("    ;; encode {:?}\n", ty));
            out.push_str("    local.get $node_idx\n");
            out.push_str("    local.set $enc_root_idx\n");
            generate_write_node_header(out, kind);
            // Write payload
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    local.get {}\n", value_local));
            out.push_str(&format!("    {}\n", store_instr));
            // Advance cursor BEFORE patching payload_len
            out.push_str("    local.get $buf_cursor\n");
            out.push_str(&format!("    i32.const {}\n", payload_size));
            out.push_str("    i32.add\n");
            out.push_str("    local.set $buf_cursor\n");
            // Patch payload_len (now cursor is correctly positioned)
            generate_patch_payload_len(out);
            // Increment node_idx
            out.push_str("    local.get $node_idx\n");
            out.push_str("    i32.const 1\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $node_idx\n");
        }
        Type::Str => {
            out.push_str("    ;; encode string\n");
            out.push_str("    local.get $node_idx\n");
            out.push_str("    local.set $enc_root_idx\n");
            generate_write_node_header(out, CGRF_STRING);
            // Save string pointer (in case value_local is $enc_tmp which will be overwritten)
            out.push_str(&format!("    local.get {}\n", value_local));
            out.push_str("    local.set $enc_result_ptr\n");
            // Read string length
            out.push_str("    local.get $enc_result_ptr\n");
            out.push_str("    i32.load\n");
            out.push_str("    local.set $enc_tmp\n");
            // Write length to payload
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $enc_tmp\n");
            out.push_str("    i32.store\n");
            // Copy string data (use saved pointer, not value_local which may have been overwritten)
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n"); // dest
            out.push_str("    local.get $enc_result_ptr\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n"); // src
            out.push_str("    local.get $enc_tmp\n"); // len
            out.push_str("    memory.copy\n");
            // Advance cursor past payload
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    local.get $enc_tmp\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $buf_cursor\n");
            // Patch payload_len
            generate_patch_payload_len(out);
            // Increment node_idx
            out.push_str("    local.get $node_idx\n");
            out.push_str("    i32.const 1\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $node_idx\n");
        }
        Type::Option(inner_ty) => {
            // Option: [tag:i32 (0=none, 1=some), payload:T at +4]
            // CGRF: if Some, encode child first, then option node (depth-first)
            // Option payload: type_tag + presence:u8 + optional child_index:u32
            out.push_str("    ;; encode option\n");
            out.push_str(&format!("    local.get {}\n", value_local));
            out.push_str("    i32.load\n"); // tag: 0=none, 1=some
            out.push_str("    (if\n");
            out.push_str("      (then\n");
            out.push_str("        ;; Some: encode child first\n");
            generate_load_inner_value(out, inner_ty, value_local, 4);
            let child_local = enc_local_for_type(inner_ty);
            generate_cgrf_encode_recursive(out, inner_ty, child_local);
            // Save child's node index
            out.push_str("        local.get $enc_root_idx\n");
            out.push_str("        local.set $enc_save_child\n");
            // Now write the option node
            out.push_str("        local.get $node_idx\n");
            out.push_str("        local.set $enc_root_idx\n");
            generate_write_node_header(out, CGRF_OPTION);
            // Write type tag
            generate_write_type_tag_at_cursor(out, inner_ty);
            // Write presence = 1
            out.push_str("        local.get $out_ptr\n");
            out.push_str("        local.get $buf_cursor\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 1\n");
            out.push_str("        i32.store8\n");
            out.push_str("        local.get $buf_cursor\n");
            out.push_str("        i32.const 1\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.set $buf_cursor\n");
            // Write child index
            out.push_str("        local.get $out_ptr\n");
            out.push_str("        local.get $buf_cursor\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $enc_save_child\n");
            out.push_str("        i32.store\n");
            out.push_str("        local.get $buf_cursor\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.set $buf_cursor\n");
            // Patch payload_len
            generate_patch_payload_len(out);
            // Increment node_idx
            out.push_str("        local.get $node_idx\n");
            out.push_str("        i32.const 1\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.set $node_idx\n");
            out.push_str("      )\n");
            out.push_str("      (else\n");
            out.push_str("        ;; None: write option node only\n");
            out.push_str("        local.get $node_idx\n");
            out.push_str("        local.set $enc_root_idx\n");
            generate_write_node_header(out, CGRF_OPTION);
            generate_write_type_tag_at_cursor(out, inner_ty);
            // Write presence = 0
            out.push_str("        local.get $out_ptr\n");
            out.push_str("        local.get $buf_cursor\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 0\n");
            out.push_str("        i32.store8\n");
            out.push_str("        local.get $buf_cursor\n");
            out.push_str("        i32.const 1\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.set $buf_cursor\n");
            // Patch payload_len
            generate_patch_payload_len(out);
            // Increment node_idx
            out.push_str("        local.get $node_idx\n");
            out.push_str("        i32.const 1\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.set $node_idx\n");
            out.push_str("      )\n");
            out.push_str("    )\n");
        }
        Type::Result(ok_ty, err_ty) => {
            // Result: [tag:i32 (0=ok, 1=err), payload:max(T,E) at +4]
            // CGRF: encode payload child first, then result node
            // Result payload: ok_type_tag + err_type_tag + tag:u32 + has_payload:u8 + child_index:u32
            out.push_str("    ;; encode result\n");
            // Save result pointer before encoding payload (encoding may clobber value_local)
            out.push_str(&format!("    local.get {}\n", value_local));
            out.push_str("    local.set $enc_result_ptr\n");
            // Encode payload child first
            out.push_str("    local.get $enc_result_ptr\n");
            out.push_str("    i32.load\n"); // tag: 0=ok, 1=err
            out.push_str("    (if\n");
            out.push_str("      (then\n");
            out.push_str("        ;; Err branch: encode err value\n");
            generate_load_inner_value(out, err_ty, "$enc_result_ptr", 4);
            let err_local = enc_local_for_type(err_ty);
            generate_cgrf_encode_recursive(out, err_ty, err_local);
            out.push_str("      )\n");
            out.push_str("      (else\n");
            out.push_str("        ;; Ok branch: encode ok value\n");
            generate_load_inner_value(out, ok_ty, "$enc_result_ptr", 4);
            let ok_local = enc_local_for_type(ok_ty);
            generate_cgrf_encode_recursive(out, ok_ty, ok_local);
            out.push_str("      )\n");
            out.push_str("    )\n");
            // Save child index
            out.push_str("    local.get $enc_root_idx\n");
            out.push_str("    local.set $enc_save_child\n");
            // Write result node
            out.push_str("    local.get $node_idx\n");
            out.push_str("    local.set $enc_root_idx\n");
            generate_write_node_header(out, CGRF_RESULT);
            // Write ok_type tag
            generate_write_type_tag_at_cursor(out, ok_ty);
            // Write err_type tag
            generate_write_type_tag_at_cursor(out, err_ty);
            // Write tag (0=ok, 1=err) - use saved result pointer
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $enc_result_ptr\n");
            out.push_str("    i32.load\n");
            out.push_str("    i32.store\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $buf_cursor\n");
            // Write has_payload = 1
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 1\n");
            out.push_str("    i32.store8\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.const 1\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $buf_cursor\n");
            // Write child index
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $enc_save_child\n");
            out.push_str("    i32.store\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $buf_cursor\n");
            // Patch payload_len
            generate_patch_payload_len(out);
            // Increment node_idx
            out.push_str("    local.get $node_idx\n");
            out.push_str("    i32.const 1\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $node_idx\n");
        }
        Type::Tuple(elem_types) => {
            // Tuple in-memory: [field0, field1, ...] at value_local (contiguous heap)
            // Strategy: write tuple node header first, reserve space for payload,
            // then encode children. Each child's node index is written back into
            // the child_indices array.
            //
            // Uses a memory-based stack ($enc_tuple_sp) to save/restore
            // $enc_tuple_header, $enc_tuple_ci_pos, and $enc_save_root so that
            // nested Tuple encoding works correctly at arbitrary depth.
            out.push_str("    ;; encode tuple\n");
            let n = elem_types.len();

            // Save this tuple's node index and preserve it across child encoding
            out.push_str("    local.get $node_idx\n");
            out.push_str("    local.set $enc_root_idx\n");
            out.push_str("    local.get $enc_root_idx\n");
            out.push_str("    local.set $enc_save_root\n");
            // Increment node_idx for the tuple node itself
            out.push_str("    local.get $node_idx\n");
            out.push_str("    i32.const 1\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $node_idx\n");

            // Write tuple node header (saves $enc_header_start)
            generate_write_node_header(out, CGRF_TUPLE);
            // Save header start for later payload_len patching
            out.push_str("    local.get $enc_header_start\n");
            out.push_str("    local.set $enc_tuple_header\n");

            // Write child_count
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", n));
            out.push_str("    i32.store\n");

            // Save position of child_indices array
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $enc_tuple_ci_pos\n");

            // Advance cursor past child_count + child_indices slots
            out.push_str("    local.get $buf_cursor\n");
            out.push_str(&format!("    i32.const {}\n", 4 + 4 * n));
            out.push_str("    i32.add\n");
            out.push_str("    local.set $buf_cursor\n");

            // Push tuple encoder state onto memory stack (16 bytes per frame)
            // [enc_tuple_header, enc_tuple_ci_pos, enc_save_root, tuple_val]
            out.push_str("    ;; push tuple encoder state\n");
            out.push_str("    global.get $enc_tuple_sp\n");
            out.push_str("    local.get $enc_tuple_header\n");
            out.push_str("    i32.store\n");
            out.push_str("    global.get $enc_tuple_sp\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $enc_tuple_ci_pos\n");
            out.push_str("    i32.store\n");
            out.push_str("    global.get $enc_tuple_sp\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $enc_save_root\n");
            out.push_str("    i32.store\n");
            // Save the tuple value pointer so we can reload it after each element
            out.push_str("    global.get $enc_tuple_sp\n");
            out.push_str("    i32.const 12\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    local.get {}\n", value_local));
            out.push_str("    i32.store\n");
            out.push_str("    global.get $enc_tuple_sp\n");
            out.push_str("    i32.const 16\n");
            out.push_str("    i32.add\n");
            out.push_str("    global.set $enc_tuple_sp\n");

            // Encode each child element
            let mut field_offset = 0;
            for (i, elem_ty) in elem_types.iter().enumerate() {
                out.push_str(&format!("    ;; encode tuple element {}\n", i));
                generate_load_inner_value(out, elem_ty, value_local, field_offset);
                let child_local = enc_local_for_type(elem_ty);
                generate_cgrf_encode_recursive(out, elem_ty, child_local);

                // Restore tuple encoder state from stack (peek, not pop)
                out.push_str("    ;; restore tuple encoder state\n");
                out.push_str("    global.get $enc_tuple_sp\n");
                out.push_str("    i32.const 16\n");
                out.push_str("    i32.sub\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $enc_tuple_header\n");
                out.push_str("    global.get $enc_tuple_sp\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.sub\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $enc_tuple_ci_pos\n");
                out.push_str("    global.get $enc_tuple_sp\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.sub\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $enc_save_root\n");
                // Restore the tuple value pointer
                out.push_str("    global.get $enc_tuple_sp\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.sub\n");
                out.push_str("    i32.load\n");
                out.push_str(&format!("    local.set {}\n", value_local));

                // Write child's node index to child_indices[i]
                out.push_str("    local.get $out_ptr\n");
                out.push_str("    local.get $enc_tuple_ci_pos\n");
                out.push_str("    i32.add\n");
                if i > 0 {
                    out.push_str(&format!("    i32.const {}\n", i * 4));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $enc_root_idx\n");
                out.push_str("    i32.store\n");
                field_offset += type_size(elem_ty);
            }

            // Pop tuple encoder state from stack
            out.push_str("    ;; pop tuple encoder state\n");
            out.push_str("    global.get $enc_tuple_sp\n");
            out.push_str("    i32.const 16\n");
            out.push_str("    i32.sub\n");
            out.push_str("    global.set $enc_tuple_sp\n");

            // Patch tuple node's payload_len (only the tuple's own data, not children)
            // payload = child_count(4) + N * child_index(4)
            let tuple_payload_len = 4 + 4 * n;
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $enc_tuple_header\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", tuple_payload_len));
            out.push_str("    i32.store\n");

            // Restore $enc_root_idx to the tuple's node index (saved before child encoding)
            out.push_str("    local.get $enc_save_root\n");
            out.push_str("    local.set $enc_root_idx\n");
        }
        Type::List(elem_ty) => {
            // List in-memory: [len:i32, cap:i32, data_ptr:i32] at value_local
            // Strategy: write list node header first, reserve space for type_tag +
            // count + child_indices, then loop encoding each element.
            // Uses $enc_list_* locals which are NOT overwritten by Tuple encoding.
            out.push_str("    ;; encode list\n");

            let elem_size = type_size(elem_ty);

            // Read list length
            out.push_str(&format!("    local.get {}\n", value_local));
            out.push_str("    i32.load\n");
            out.push_str("    local.set $enc_list_len\n");

            // Read data pointer (at value_local + 8)
            out.push_str(&format!("    local.get {}\n", value_local));
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.load\n");
            out.push_str("    local.set $enc_list_data\n");

            // Save this list's node index
            out.push_str("    local.get $node_idx\n");
            out.push_str("    local.set $enc_root_idx\n");
            out.push_str("    local.get $enc_root_idx\n");
            out.push_str("    local.set $enc_list_root_idx\n");

            // Increment node_idx for the list node itself
            out.push_str("    local.get $node_idx\n");
            out.push_str("    i32.const 1\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $node_idx\n");

            // Write list node header
            generate_write_node_header(out, CGRF_LIST);
            out.push_str("    local.get $enc_header_start\n");
            out.push_str("    local.set $enc_list_header\n");

            // Write type tag
            generate_write_type_tag_at_cursor(out, elem_ty);

            // Write count
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $enc_list_len\n");
            out.push_str("    i32.store\n");

            // Save position of child_indices array
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $enc_list_ci_pos\n");

            // Advance cursor past count + child_indices slots (4 + 4 * len)
            out.push_str("    local.get $buf_cursor\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $enc_list_len\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.mul\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.set $buf_cursor\n");

            // Initialize loop counter
            out.push_str("    i32.const 0\n");
            out.push_str("    local.set $enc_list_i\n");

            // Loop: encode each element
            out.push_str("    block $list_break\n");
            out.push_str("      loop $list_loop\n");
            out.push_str("        local.get $enc_list_i\n");
            out.push_str("        local.get $enc_list_len\n");
            out.push_str("        i32.ge_u\n");
            out.push_str("        br_if $list_break\n");

            // Load element value from data array
            out.push_str("        local.get $enc_list_data\n");
            out.push_str("        local.get $enc_list_i\n");
            if elem_size > 1 {
                out.push_str(&format!("        i32.const {}\n", elem_size));
                out.push_str("        i32.mul\n");
            }
            out.push_str("        i32.add\n");
            // Load based on element type
            match elem_ty.as_ref() {
                Type::S64 => {
                    out.push_str("        i64.load\n");
                    out.push_str("        local.set $enc_tmp_i64\n");
                }
                Type::F32 => {
                    out.push_str("        f32.load\n");
                    out.push_str("        local.set $enc_tmp_f32\n");
                }
                Type::F64 => {
                    out.push_str("        f64.load\n");
                    out.push_str("        local.set $enc_tmp_f64\n");
                }
                _ => {
                    // i32, u8, pointers — all stored as i32 in Wisp memory
                    out.push_str("        i32.load\n");
                    out.push_str("        local.set $enc_tmp\n");
                }
            }

            let child_local = enc_local_for_type(elem_ty);
            generate_cgrf_encode_recursive(out, elem_ty, child_local);

            // Write child's node index to child_indices[$enc_list_i]
            out.push_str("        local.get $out_ptr\n");
            out.push_str("        local.get $enc_list_ci_pos\n");
            out.push_str("        local.get $enc_list_i\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $enc_root_idx\n");
            out.push_str("        i32.store\n");

            // Increment loop counter
            out.push_str("        local.get $enc_list_i\n");
            out.push_str("        i32.const 1\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.set $enc_list_i\n");
            out.push_str("        br $list_loop\n");
            out.push_str("      end\n");
            out.push_str("    end\n");

            // Patch list node's payload_len (only the list's own data, not children)
            // payload = type_tag + count(4) + len * child_index(4)
            let tag_size = type_tag_size(elem_ty);
            out.push_str("    local.get $out_ptr\n");
            out.push_str("    local.get $enc_list_header\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", tag_size + 4));
            out.push_str("    local.get $enc_list_len\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.mul\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.store\n");

            // Restore $enc_root_idx to the list's node index
            out.push_str("    local.get $enc_list_root_idx\n");
            out.push_str("    local.set $enc_root_idx\n");
        }
        _ => {
            out.push_str(&format!("    ;; TODO: recursive encode for {:?}\n", ty));
        }
    }
}

// =============================================================================
// Recursive CGRF decoding
// =============================================================================

/// Generate WAT to scan from offset 16 to find node at index $dec_child_idx.
/// After execution, $dec_node_offset holds the byte offset of the target node.
///
/// Required locals: $dec_child_idx, $dec_node_offset, $dec_scan_offset,
///   $dec_scan_i, $dec_payload_len
pub(crate) fn generate_dec_find_node_by_index(out: &mut String) {
    out.push_str("    ;; Find node at index $dec_child_idx\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    local.set $dec_scan_offset\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    local.set $dec_scan_i\n");
    out.push_str("    (block $dec_found\n");
    out.push_str("      (loop $dec_scan\n");
    out.push_str("        local.get $dec_scan_i\n");
    out.push_str("        local.get $dec_child_idx\n");
    out.push_str("        i32.ge_u\n");
    out.push_str("        br_if $dec_found\n");
    // Read payload_len at scan_offset + 4
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $dec_scan_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $dec_payload_len\n");
    // Advance: scan_offset += 8 + payload_len
    out.push_str("        local.get $dec_scan_offset\n");
    out.push_str("        i32.const 8\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $dec_payload_len\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $dec_scan_offset\n");
    // scan_i++
    out.push_str("        local.get $dec_scan_i\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $dec_scan_i\n");
    out.push_str("        br $dec_scan\n");
    out.push_str("      )\n");
    out.push_str("    )\n");
    out.push_str("    local.get $dec_scan_offset\n");
    out.push_str("    local.set $dec_node_offset\n");
}

/// Recursively decode a CGRF node into a Wisp in-memory value.
///
/// Required locals in the wrapper function:
///   $in_ptr, $dec_node_offset, $dec_result (i32),
///   $dec_child_idx, $dec_scan_offset, $dec_scan_i, $dec_payload_len,
///   $dec_tmp, $dec_opt_ptr, $dec_opt_node_offset,
///   $dec_list_ptr, $dec_list_data, $dec_list_len, $dec_list_i,
///   $dec_list_node_offset
///
/// Input: $dec_node_offset = byte offset of the node in the CGRF buffer
/// Output: $dec_result = decoded value (i32 pointer or scalar)
pub(crate) fn generate_cgrf_decode_recursive(out: &mut String, ty: &Type) {
    match ty {
        Type::List(elem_ty) if cgrf_array_width(elem_ty).is_some() => {
            generate_cgrf_decode_array(out, elem_ty);
        }
        Type::S32 => {
            out.push_str("    ;; decode s32\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.load\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::U8 => {
            out.push_str("    ;; decode u8\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.load8_u\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::Bool => {
            out.push_str("    ;; decode bool (1-byte payload, 0/1)\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.load8_u\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::U16 => {
            out.push_str("    ;; decode u16 (2-byte payload)\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.load16_u\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::U32 => {
            out.push_str("    ;; decode u32 (4-byte payload)\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.load\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::U64 => {
            // Mirrors S64: load the i64 payload, truncate to i32 in $dec_result
            // (the typed-wrapper decode's existing simplification; raw-CGRF
            // capabilities bridge u64 losslessly via unmarshal instead).
            out.push_str("    ;; decode u64 (store on heap as i64)\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i64.load\n");
            out.push_str("    i32.wrap_i64\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::S64 => {
            // For S64 params, store the i64 value in a temp location on the heap
            // and return a pointer. This is a simplification; full support would
            // need a separate $dec_result_i64 local.
            out.push_str("    ;; decode s64 (store on heap as i64)\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i64.load\n");
            // For now just truncate to i32 — proper i64 support needs separate local
            out.push_str("    i32.wrap_i64\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::F32 => {
            out.push_str("    ;; decode f32\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    f32.load\n");
            out.push_str("    i32.reinterpret_f32\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::F64 => {
            out.push_str("    ;; decode f64\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    f64.load\n");
            // Truncate for i32 result — proper f64 support needs separate local
            out.push_str("    i64.reinterpret_f64\n");
            out.push_str("    i32.wrap_i64\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::Str => {
            out.push_str("    ;; decode string\n");
            // Read string length from payload (at node + 8)
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.load\n");
            out.push_str("    local.set $dec_tmp\n"); // string length

            // Allocate wisp string: 4 + length
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str("    local.set $dec_result\n");
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $dec_tmp\n");
            out.push_str("    i32.add\n");
            out.push_str("    global.set $__heap_ptr\n");

            // Store length
            out.push_str("    local.get $dec_result\n");
            out.push_str("    local.get $dec_tmp\n");
            out.push_str("    i32.store\n");

            // Copy string data: src = in_ptr + node + 12, dest = result + 4
            out.push_str("    local.get $dec_result\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n"); // dest
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str("    i32.const 12\n");
            out.push_str("    i32.add\n"); // src: node + 8 (header) + 4 (len prefix)
            out.push_str("    local.get $dec_tmp\n"); // len
            out.push_str("    memory.copy\n");
        }
        Type::Option(inner_ty) => {
            let tag_size = type_tag_size(inner_ty);
            let presence_offset = 8 + tag_size;
            let child_idx_offset = presence_offset + 1;

            // Allocate option struct: [tag:i32, payload:T]
            let inner_size = type_size(inner_ty);
            let opt_size = 4 + inner_size;
            out.push_str("    ;; decode option\n");
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str("    local.set $dec_opt_ptr\n");
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str(&format!("    i32.const {}\n", opt_size));
            out.push_str("    i32.add\n");
            out.push_str("    global.set $__heap_ptr\n");

            // Save option node offset for reading child_index
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    local.set $dec_opt_node_offset\n");

            // Read presence byte
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", presence_offset));
            out.push_str("    i32.add\n");
            out.push_str("    i32.load8_u\n");

            out.push_str("    (if\n");
            out.push_str("      (then\n");
            // Some: tag = 1
            out.push_str("        local.get $dec_opt_ptr\n");
            out.push_str("        i32.const 1\n");
            out.push_str("        i32.store\n");

            // Read child_index
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        local.get $dec_opt_node_offset\n");
            out.push_str("        i32.add\n");
            out.push_str(&format!("        i32.const {}\n", child_idx_offset));
            out.push_str("        i32.add\n");
            out.push_str("        i32.load\n");
            out.push_str("        local.set $dec_child_idx\n");

            // Find child node → $dec_node_offset
            generate_dec_find_node_by_index(out);

            // Recursively decode child
            generate_cgrf_decode_recursive(out, inner_ty);

            // Store decoded value at $dec_opt_ptr + 4
            out.push_str("        local.get $dec_opt_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $dec_result\n");
            out.push_str("        i32.store\n");

            out.push_str("      )\n");
            out.push_str("      (else\n");
            // None: tag = 0
            out.push_str("        local.get $dec_opt_ptr\n");
            out.push_str("        i32.const 0\n");
            out.push_str("        i32.store\n");
            out.push_str("      )\n");
            out.push_str("    )\n");

            // Result = option pointer
            out.push_str("    local.get $dec_opt_ptr\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::List(elem_ty) => {
            let tag_size = type_tag_size(elem_ty);
            let count_offset = 8 + tag_size;
            let child_indices_offset = count_offset + 4;
            let elem_size = type_size(elem_ty);

            out.push_str("    ;; decode list\n");

            // Save list node offset
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    local.set $dec_list_node_offset\n");

            // Read count
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", count_offset));
            out.push_str("    i32.add\n");
            out.push_str("    i32.load\n");
            out.push_str("    local.set $dec_list_len\n");

            // Allocate list struct (12 bytes: len, cap, data_ptr)
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str("    local.set $dec_list_ptr\n");
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str("    i32.const 12\n");
            out.push_str("    i32.add\n");
            out.push_str("    global.set $__heap_ptr\n");

            // Set len
            out.push_str("    local.get $dec_list_ptr\n");
            out.push_str("    local.get $dec_list_len\n");
            out.push_str("    i32.store\n");

            // Set cap = len
            out.push_str("    local.get $dec_list_ptr\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $dec_list_len\n");
            out.push_str("    i32.store\n");

            // Allocate data array: elem_size * len
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str("    local.set $dec_list_data\n");
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str(&format!("    i32.const {}\n", elem_size));
            out.push_str("    local.get $dec_list_len\n");
            out.push_str("    i32.mul\n");
            out.push_str("    i32.add\n");
            out.push_str("    global.set $__heap_ptr\n");

            // Set data_ptr
            out.push_str("    local.get $dec_list_ptr\n");
            out.push_str("    i32.const 8\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $dec_list_data\n");
            out.push_str("    i32.store\n");

            // Loop: decode each element
            out.push_str("    i32.const 0\n");
            out.push_str("    local.set $dec_list_i\n");

            out.push_str("    block $dec_list_break\n");
            out.push_str("      loop $dec_list_loop\n");
            out.push_str("        local.get $dec_list_i\n");
            out.push_str("        local.get $dec_list_len\n");
            out.push_str("        i32.ge_u\n");
            out.push_str("        br_if $dec_list_break\n");

            // Read child_index for element i
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        local.get $dec_list_node_offset\n");
            out.push_str("        i32.add\n");
            out.push_str(&format!("        i32.const {}\n", child_indices_offset));
            out.push_str("        i32.add\n");
            out.push_str("        local.get $dec_list_i\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.load\n");
            out.push_str("        local.set $dec_child_idx\n");

            // Find child node
            generate_dec_find_node_by_index(out);

            // Decode child
            generate_cgrf_decode_recursive(out, elem_ty);

            // Store decoded value in data array
            out.push_str("        local.get $dec_list_data\n");
            out.push_str("        local.get $dec_list_i\n");
            if elem_size > 1 {
                out.push_str(&format!("        i32.const {}\n", elem_size));
                out.push_str("        i32.mul\n");
            }
            out.push_str("        i32.add\n");
            out.push_str("        local.get $dec_result\n");
            out.push_str("        i32.store\n");

            // Increment
            out.push_str("        local.get $dec_list_i\n");
            out.push_str("        i32.const 1\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.set $dec_list_i\n");
            out.push_str("        br $dec_list_loop\n");
            out.push_str("      end\n");
            out.push_str("    end\n");

            // Result = list pointer
            out.push_str("    local.get $dec_list_ptr\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::Tuple(elem_types) => {
            let child_indices_offset = 12; // 8 (node header) + 4 (child_count)

            out.push_str("    ;; decode tuple\n");

            // Save tuple node offset (survives child decoding)
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    local.set $dec_tuple_node_offset\n");

            // Allocate tuple: sum of type_size for each field
            let tuple_size: usize = elem_types.iter().map(type_size).sum();
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str("    local.set $dec_tuple_ptr\n");
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str(&format!("    i32.const {}\n", tuple_size));
            out.push_str("    i32.add\n");
            out.push_str("    global.set $__heap_ptr\n");

            // Decode each field
            let mut field_offset = 0;
            for (i, elem_ty) in elem_types.iter().enumerate() {
                out.push_str(&format!("    ;; decode tuple field {}\n", i));

                // Read child_index for field i from tuple node
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $dec_tuple_node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str(&format!("    i32.const {}\n", child_indices_offset + i * 4));
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $dec_child_idx\n");

                // Find child node
                generate_dec_find_node_by_index(out);

                // Decode child
                generate_cgrf_decode_recursive(out, elem_ty);

                // Store decoded value at tuple_ptr + field_offset
                out.push_str("    local.get $dec_tuple_ptr\n");
                if field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $dec_result\n"); // child's decoded value
                out.push_str("    i32.store\n");

                field_offset += type_size(elem_ty);
            }

            // Result = tuple pointer
            out.push_str("    local.get $dec_tuple_ptr\n");
            out.push_str("    local.set $dec_result\n");
        }
        Type::Result(ok_ty, err_ty) => {
            let ok_tag_size = type_tag_size(ok_ty);
            let err_tag_size = type_tag_size(err_ty);
            let tag_offset = 8 + ok_tag_size + err_tag_size; // after header + ok_type + err_type
            let has_payload_offset = tag_offset + 4;
            let child_idx_offset = has_payload_offset + 1;

            // Allocate result struct: [tag:i32, payload:max(T,E)]
            let ok_size = type_size(ok_ty);
            let err_size = type_size(err_ty);
            let payload_size = std::cmp::max(ok_size, err_size);
            let result_size = 4 + payload_size;

            out.push_str("    ;; decode result\n");
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str("    local.set $dec_result\n");
            out.push_str("    global.get $__heap_ptr\n");
            out.push_str(&format!("    i32.const {}\n", result_size));
            out.push_str("    i32.add\n");
            out.push_str("    global.set $__heap_ptr\n");

            // Save result pointer (will be overwritten by child decode)
            out.push_str("    local.get $dec_result\n");
            out.push_str("    local.set $dec_opt_ptr\n"); // reuse for result ptr save

            // Save node offset
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    local.set $dec_opt_node_offset\n"); // reuse for result node offset save

            // Read tag (0=ok, 1=err)
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", tag_offset));
            out.push_str("    i32.add\n");
            out.push_str("    i32.load\n");
            out.push_str("    local.set $dec_tmp\n"); // tag

            // Store tag in result struct
            out.push_str("    local.get $dec_opt_ptr\n");
            out.push_str("    local.get $dec_tmp\n");
            out.push_str("    i32.store\n");

            // Read has_payload
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $dec_opt_node_offset\n");
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", has_payload_offset));
            out.push_str("    i32.add\n");
            out.push_str("    i32.load8_u\n");

            out.push_str("    (if\n");
            out.push_str("      (then\n");

            // Read child_index
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        local.get $dec_opt_node_offset\n");
            out.push_str("        i32.add\n");
            out.push_str(&format!("        i32.const {}\n", child_idx_offset));
            out.push_str("        i32.add\n");
            out.push_str("        i32.load\n");
            out.push_str("        local.set $dec_child_idx\n");

            // Find child node
            generate_dec_find_node_by_index(out);

            // Decode based on tag: if tag == 0, it's ok (decode ok_ty), else err (decode err_ty)
            out.push_str("        local.get $dec_tmp\n"); // tag
            out.push_str("        (if\n");
            out.push_str("          (then\n");
            out.push_str("            ;; err payload\n");
            generate_cgrf_decode_recursive(out, err_ty);
            out.push_str("          )\n");
            out.push_str("          (else\n");
            out.push_str("            ;; ok payload\n");
            generate_cgrf_decode_recursive(out, ok_ty);
            out.push_str("          )\n");
            out.push_str("        )\n");

            // Store payload at result_ptr + 4
            out.push_str("        local.get $dec_opt_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $dec_result\n");
            out.push_str("        i32.store\n");

            out.push_str("      )\n");
            out.push_str("    )\n");

            // Result = result struct pointer
            out.push_str("    local.get $dec_opt_ptr\n");
            out.push_str("    local.set $dec_result\n");
        }
        _ => {
            out.push_str(&format!("    ;; TODO: recursive decode for {:?}\n", ty));
            out.push_str("    i32.const 0\n");
            out.push_str("    local.set $dec_result\n");
        }
    }
}

// =============================================================================
// CGRF Decoding functions (for export parameter decoding)
// =============================================================================

/// Generate WAT code to decode an s32 from CGRF input buffer.
/// Assumes $in_ptr contains the input buffer pointer.
/// Result is left on the stack.
pub(crate) fn generate_cgrf_decode_s32(out: &mut String) {
    // CGRF layout:
    // - Header: 16 bytes (magic, version, flags, node_count, root_index)
    // - Root node at offset 16: 8 bytes header (kind, flags, reserved, payload_len)
    // - Payload at offset 24: 4 bytes (s32 value)
    out.push_str("    ;; Decode s32 from CGRF\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n"); // 16 (header) + 8 (node header) = 24
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
}

/// Generate WAT code to decode an s64 from CGRF input buffer.
pub(crate) fn generate_cgrf_decode_s64(out: &mut String) {
    out.push_str("    ;; Decode s64 from CGRF\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    i64.load\n");
}

/// Generate WAT code to decode an f32 from CGRF input buffer.
pub(crate) fn generate_cgrf_decode_f32(out: &mut String) {
    out.push_str("    ;; Decode f32 from CGRF\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    f32.load\n");
}

/// Generate WAT code to decode an f64 from CGRF input buffer.
pub(crate) fn generate_cgrf_decode_f64(out: &mut String) {
    out.push_str("    ;; Decode f64 from CGRF\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    f64.load\n");
}

/// Generate WAT code to decode a string from CGRF input buffer.
/// Returns a pointer to a wisp string (len: i32, data: bytes) on the stack.
/// Allocates memory for the string on the heap.
pub(crate) fn generate_cgrf_decode_string(out: &mut String) {
    // CGRF string node:
    // - Header at offset 16: kind=0x06, flags, reserved, payload_len
    // - Payload at offset 24: length (u32), then UTF-8 bytes
    //
    // Wisp string layout: (len: i32, data: bytes...)
    // We need to allocate heap space and copy the string data

    out.push_str("    ;; Decode string from CGRF\n");

    // Read string length from CGRF payload (offset 24)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $str_len\n");

    // Allocate space for wisp string: 4 bytes for length + string data
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $str_ptr\n");

    // Update heap pointer: heap_ptr += 4 + str_len
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $str_len\n");
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Copy string data from CGRF to wisp string FIRST, then write the length
    // header. Order matters: the caller may place the input just below the heap
    // base, so $str_ptr (= heap_ptr) can land inside the still-unread CGRF input
    // for inputs larger than ~45 KB. Writing the 4-byte length header before the
    // copy would clobber source bytes that memory.copy is about to read,
    // corrupting the decoded string (the self-hosting size cliff). memory.copy is
    // memmove-safe, so the overlapping copy itself is fine; and after it the
    // source is consumed, so writing the header at $str_ptr is then safe.
    // Source: $in_ptr + 28 (24 for header+node + 4 for length prefix in payload)
    // Dest: $str_ptr + 4
    out.push_str("    local.get $str_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n"); // dest
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 28\n");
    out.push_str("    i32.add\n"); // src
    out.push_str("    local.get $str_len\n"); // len
    out.push_str("    memory.copy\n");

    // Write length to wisp string (after the copy has consumed the source)
    out.push_str("    local.get $str_ptr\n");
    out.push_str("    local.get $str_len\n");
    out.push_str("    i32.store\n");

    // Return the wisp string pointer
    out.push_str("    local.get $str_ptr\n");
}

/// Generate WAT code to decode a record from CGRF input buffer.
/// CGRF record layout (depth-first encoding):
/// - Field nodes come first (node 0, 1, 2, ...)
/// - Record node comes last (root)
///
/// For v2 Record([S32(10), S32(20)]):
/// - Record node at offset 16 with v2 payload
///   - Payload: [type_name_len, type_name, field_count, field_names, child_indices]
/// - Field nodes start after record node payload
///
/// We allocate heap space for the wisp record and copy field values into it.
pub(crate) fn generate_cgrf_decode_record(
    out: &mut String,
    rec_name: &str,
    param_name: &str,
    records: &HashMap<String, RecordDef>,
) {
    let record_def = match records.get(rec_name) {
        Some(r) => r,
        None => {
            out.push_str(&format!(
                "    ;; ERROR: unknown record type '{}'\n",
                rec_name
            ));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
            return;
        }
    };

    out.push_str(&format!("    ;; Decode record '{}' (CGRF v2)\n", rec_name));

    // Calculate total size needed for wisp record
    let record_size: usize = record_def.fields.iter().map(|f| type_size(&f.ty)).sum();

    // Allocate heap space
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", record_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // In CGRF, children are encoded BEFORE their parent.
    // For a record with N scalar fields, the layout is:
    // - Header: 16 bytes (magic, version, flags, node_count, root)
    // - Node 0: first child value
    // - Node 1: second child value
    // - ...
    // - Node N-1: last child value
    // - Node N: the record node (root)
    //
    // So children are at the BEGINNING of the node array, not after the record.

    // Child nodes start at offset 16 (right after CGRF header)
    let mut child_node_offset = 16;

    for (i, field) in record_def.fields.iter().enumerate() {
        let wisp_field_offset = record_def.field_offset(i);

        out.push_str(&format!(
            "    ;; Field {} '{}' at wisp offset {}\n",
            i, field.name, wisp_field_offset
        ));

        // Calculate CGRF node payload offset (skip 8-byte node header)
        let payload_offset = child_node_offset + 8;

        match &field.ty {
            Type::S32 => {
                // Load from CGRF child node
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $field_val\n");

                // Store to wisp record
                out.push_str("    local.get $rec_ptr\n");
                if wisp_field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", wisp_field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $field_val\n");
                out.push_str("    i32.store\n");

                child_node_offset += 12; // 8 header + 4 payload for S32
            }
            Type::S64 => {
                out.push_str("    local.get $rec_ptr\n");
                if wisp_field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", wisp_field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    i64.load\n");
                out.push_str("    i64.store\n");

                child_node_offset += 16; // 8 header + 8 payload for S64
            }
            Type::F32 => {
                out.push_str("    local.get $rec_ptr\n");
                if wisp_field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", wisp_field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    f32.load\n");
                out.push_str("    f32.store\n");

                child_node_offset += 12; // 8 header + 4 payload for F32
            }
            Type::F64 => {
                out.push_str("    local.get $rec_ptr\n");
                if wisp_field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", wisp_field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    f64.load\n");
                out.push_str("    f64.store\n");

                child_node_offset += 16; // 8 header + 8 payload for F64
            }
            _ => {
                out.push_str(&format!(
                    "    ;; TODO: decode non-scalar field '{}'\n",
                    field.name
                ));
                child_node_offset += 12; // Assume 12 as fallback
            }
        }
    }

    // Return the record pointer
    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode an option from CGRF input buffer.
/// CGRF option layout:
/// - For Some: child node first, then option node
/// - For None: just option node
///
/// Option node payload: [has_value: u8, child_index: u32 (if has_value)]
pub(crate) fn generate_cgrf_decode_option(out: &mut String, inner_ty: &Type, param_name: &str) {
    out.push_str("    ;; Decode option\n");

    // Wisp option layout: [tag: i32 (0=none, 1=some), payload if some]
    let inner_size = type_size(inner_ty);
    let option_size = 4 + inner_size; // tag + optional payload

    // Allocate heap space
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", option_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // For a single option param:
    // - If Some: node 0 is the inner value, node 1 is the option (root)
    // - If None: node 0 is the option (root)
    //
    // We need to check the option node's payload to determine which case.
    // The option node is the root, at variable offset depending on whether there's a child.
    //
    // Actually, we can read the root_index from header (offset 12) to find the option node.
    // Then read its has_value byte from the payload.

    // Read root_index
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $field_val\n"); // reuse as root_index

    // Calculate option node offset: 16 + root_index * node_size
    // For option node, we need to scan to find it. For simplicity, assume:
    // - If root_index == 0, option is at offset 16 (None case, or Some with 0-sized inner)
    // - If root_index == 1, there's one child node first

    // Read has_value from option node payload
    // Option node: header(8) + payload(1 byte has_value + optional 4 byte child_index)
    // If Some(scalar): child at node 0, option at node 1
    //   - Node 0 at offset 16 (inner value)
    //   - Node 1 at offset 16 + inner_node_size (option)

    // For simplicity, check node count to determine Some vs None
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n"); // node_count

    // If node_count == 1, it's None (only option node)
    // If node_count == 2, it's Some (child + option node)
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; None case: store tag = 0\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 0\n");
    out.push_str("        i32.store\n");
    out.push_str("      )\n");
    out.push_str("      (else\n");
    out.push_str("        ;; Some case: store tag = 1 and decode inner value\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.store\n");

    // Inner value is at node 0, offset 16, payload at 24
    match inner_ty {
        Type::S32 => {
            out.push_str("        local.get $rec_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 24\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.load\n");
            out.push_str("        i32.store\n");
        }
        Type::S64 => {
            out.push_str("        local.get $rec_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 24\n");
            out.push_str("        i32.add\n");
            out.push_str("        i64.load\n");
            out.push_str("        i64.store\n");
        }
        Type::F32 => {
            out.push_str("        local.get $rec_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 24\n");
            out.push_str("        i32.add\n");
            out.push_str("        f32.load\n");
            out.push_str("        f32.store\n");
        }
        Type::F64 => {
            out.push_str("        local.get $rec_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 24\n");
            out.push_str("        i32.add\n");
            out.push_str("        f64.load\n");
            out.push_str("        f64.store\n");
        }
        _ => {
            out.push_str("        ;; TODO: decode non-scalar option inner\n");
        }
    }

    out.push_str("      )\n");
    out.push_str("    )\n");

    // Return the option pointer
    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a variant parameter from CGRF.
///
/// CGRF v2 variant encoding (depth-first):
/// - No payload: variant node is at index 0 (offset 16)
///   - Payload: [type_name_len:u32, type_name:utf8, case_name_len:u32, case_name:utf8,
///     tag:u32, payload_count:u32]
/// - With payload: child node first, then variant node
///   - Child node at offset 16
///   - Variant node at offset 16 + child_size
///   - Payload: [type_name_len:u32, type_name:utf8, case_name_len:u32, case_name:utf8,
///     tag:u32, payload_count:u32, child_indices:u32*]
///
/// Wisp variant layout: [discriminant: i32, payload...]
pub(crate) fn generate_cgrf_decode_variant(
    out: &mut String,
    variant_name: &str,
    param_name: &str,
    variants: &HashMap<String, VariantDef>,
) {
    let variant_def = match variants.get(variant_name) {
        Some(v) => v,
        None => {
            out.push_str(&format!(
                "    ;; ERROR: unknown variant '{}'\n",
                variant_name
            ));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
            return;
        }
    };

    out.push_str(&format!(
        "    ;; Decode variant '{}' from CGRF v2\n",
        variant_name
    ));

    // Calculate variant size for allocation
    let variant_size = variant_def.size();

    // Allocate wisp variant on heap
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", variant_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Use node_count to determine if there's a payload
    // node_count == 1: no payload (just variant node at offset 16)
    // node_count == 2: has payload (child at offset 16, variant after)

    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n"); // node_count
    out.push_str("    local.set $field_val\n");

    out.push_str("    local.get $field_val\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; No payload case: variant node at offset 16\n");
    // For v2, we need to skip type_name and case_name to find tag
    // Payload starts at offset 24 (16 + 8 header)
    // Read type_name_len at 24, skip type_name, read case_name_len, skip case_name, then read tag
    out.push_str("        ;; Read type_name_len\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        i32.const 24\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $child_idx\n"); // reuse as type_name_len
    out.push_str("        ;; Calculate case_name_len offset: 24 + 4 + type_name_len\n");
    out.push_str("        i32.const 28\n");
    out.push_str("        local.get $child_idx\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $child_offset\n"); // case_name_len offset
    out.push_str("        ;; Read case_name_len\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $scan_i\n"); // reuse as case_name_len
    out.push_str("        ;; Calculate tag offset: case_name_len_offset + 4 + case_name_len\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $scan_i\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $payload_len\n"); // reuse as tag_offset
    out.push_str("        ;; Read tag\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $payload_len\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store tag as discriminant
    out.push_str("      )\n");
    out.push_str("      (else\n");
    out.push_str("        ;; Has payload case: child at offset 16, variant node after\n");
    // Child node (s32) is at offset 16, size 12 (8 header + 4 payload)
    // Variant node is at offset 28
    // For v2, variant payload starts at offset 36 (28 + 8)
    out.push_str("        ;; Read type_name_len from variant node\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        i32.const 36\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $child_idx\n"); // reuse as type_name_len
    out.push_str("        ;; Calculate case_name_len offset: 36 + 4 + type_name_len\n");
    out.push_str("        i32.const 40\n");
    out.push_str("        local.get $child_idx\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $child_offset\n"); // case_name_len offset
    out.push_str("        ;; Read case_name_len\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $scan_i\n"); // reuse as case_name_len
    out.push_str("        ;; Calculate tag offset: case_name_len_offset + 4 + case_name_len\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $scan_i\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $payload_len\n"); // reuse as tag_offset
    out.push_str("        ;; Read tag\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $payload_len\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store tag as discriminant
    // Read payload value from child node (offset 16 + 8 = 24)
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n"); // payload at offset 4
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        i32.const 24\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store payload value
    out.push_str("      )\n");
    out.push_str("    )\n");

    // Return the variant pointer
    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a result<T, E> parameter from CGRF.
/// For now, supports result<s32, s32>.
///
/// CGRF v2 result encoding:
/// - Result node at offset 16 (root = 0)
///   - Payload: [ok_type:type_tag*, err_type:type_tag*, tag:u32, has_payload:u8, child_index:u32]
/// - Payload value node after result node
///
/// Wisp result layout: [tag: i32 (0=ok, 1=err), payload]
pub(crate) fn generate_cgrf_decode_result(
    out: &mut String,
    ok_ty: &Type,
    err_ty: &Type,
    param_name: &str,
) {
    out.push_str("    ;; Decode result from CGRF v2 (depth-first encoded)\n");

    // Calculate result size for allocation
    let payload_size = match (ok_ty, err_ty) {
        (Type::S64, _) | (_, Type::S64) | (Type::F64, _) | (_, Type::F64) => 8,
        _ => 4,
    };
    let result_size = 4 + payload_size; // tag + payload

    // In CGRF depth-first encoding:
    // - Node 0: payload value (if present) at offset 16
    // - Node 1: Result node at offset 16 + payload_node_size (or 16 if no payload)
    //
    // Result payload v2: [ok_type:type_tag*, err_type:type_tag*, tag:u32, has_payload:u8, child_index?:u32]

    // Calculate payload node size based on type
    let payload_node_size = match (ok_ty, err_ty) {
        (Type::S64, _) | (_, Type::S64) | (Type::F64, _) | (_, Type::F64) => 16, // 8 header + 8 payload
        _ => 12, // 8 header + 4 payload
    };

    // Calculate offsets for v2 format
    let ok_tag_size = type_tag_size(ok_ty);
    let err_tag_size = type_tag_size(err_ty);

    // Allocate wisp result on heap
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", result_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Use node_count to determine if there's a payload
    // node_count == 1: just result node (at offset 16)
    // node_count == 2: payload node first (at offset 16), then result node
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n"); // node_count
    out.push_str("    local.set $child_idx\n"); // reuse as node_count

    out.push_str("    local.get $child_idx\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; No payload: Result node at offset 16\n");
    // tag is at: 16 (node start) + 8 (header) + ok_tag_size + err_tag_size
    let tag_offset_no_payload = 24 + ok_tag_size + err_tag_size;
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str(&format!("        i32.const {}\n", tag_offset_no_payload));
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store tag
    // No payload value to store
    out.push_str("      )\n");
    out.push_str("      (else\n");
    out.push_str("        ;; Has payload: payload at offset 16, Result node after\n");
    // Result node is at: 16 + payload_node_size
    let result_node_offset = 16 + payload_node_size;
    // tag is at: result_node_offset + 8 (header) + ok_tag_size + err_tag_size
    let tag_offset_with_payload = result_node_offset + 8 + ok_tag_size + err_tag_size;
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str(&format!("        i32.const {}\n", tag_offset_with_payload));
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store tag
    // Read payload value from payload node (at offset 16 + 8 = 24)
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        i32.const 24\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n");
    out.push_str("      )\n");
    out.push_str("    )\n");

    // Return the result pointer
    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a list<T> parameter from CGRF.
/// For now, only supports list<s32>.
///
/// CGRF v2 uses depth-first encoding:
/// - Child nodes (elements) are encoded FIRST at nodes 0, 1, 2, ...
/// - List node is encoded LAST (root)
/// - List payload: [elem_type:type_tag*, count:u32, child_indices:u32*]
///
/// For list [1, 2, 3]:
/// - Node 0: S32(1) at offset 16
/// - Node 1: S32(2) at offset 28
/// - Node 2: S32(3) at offset 40
/// - Node 3: List at offset 52 (root, contains type tag + count + indices)
///
/// Wisp list layout: { len: i32, cap: i32, data_ptr: i32 }
pub(crate) fn generate_cgrf_decode_list(out: &mut String, elem_ty: &Type, param_name: &str) {
    out.push_str("    ;; Decode list from CGRF v2 (depth-first encoded)\n");

    // Element nodes are encoded first, starting at offset 16
    // The list node is at the end (root)

    // Read root index from header (offset 12)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $elem_offset\n"); // reuse as root_index

    // Calculate root node offset: 16 + root_index * node_size
    // For list<s32>, element nodes are 12 bytes each
    let elem_node_size = match elem_ty {
        Type::S64 | Type::F64 => 16, // 8 header + 8 payload
        _ => 12,                     // 8 header + 4 payload
    };

    // Root offset = 16 + root_index * elem_node_size
    // (This assumes uniform node sizes, which works for homogeneous lists)
    out.push_str("    i32.const 16\n");
    out.push_str("    local.get $elem_offset\n");
    out.push_str(&format!("    i32.const {}\n", elem_node_size));
    out.push_str("    i32.mul\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.set $elem_offset\n"); // now holds root node offset

    // For v2, list payload is: [elem_type:type_tag*, count:u32, child_indices:u32*]
    // The count is at offset: 8 (node header) + type_tag_size(elem_ty)
    let elem_type_tag_size = type_tag_size(elem_ty);
    let count_offset = 8 + elem_type_tag_size;

    // Read element count from list node payload
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $elem_offset\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", count_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $list_len\n");

    // Allocate wisp list struct (12 bytes: len, cap, data_ptr)
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $list_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Store len
    out.push_str("    local.get $list_ptr\n");
    out.push_str("    local.get $list_len\n");
    out.push_str("    i32.store\n");

    // Store cap = len
    out.push_str("    local.get $list_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $list_len\n");
    out.push_str("    i32.store\n");

    // Calculate element size in wisp data
    let elem_size = match elem_ty {
        Type::S64 | Type::F64 => 8,
        _ => 4, // s32, f32, pointers
    };

    // Allocate element data: elem_size * len bytes
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $list_data\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", elem_size));
    out.push_str("    local.get $list_len\n");
    out.push_str("    i32.mul\n");
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Store data_ptr
    out.push_str("    local.get $list_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $list_data\n");
    out.push_str("    i32.store\n");

    // Element nodes are at offsets 16, 16+node_size, 16+2*node_size, ...
    // Loop to copy elements from CGRF to wisp list data
    out.push_str("    i32.const 0\n");
    out.push_str("    local.set $list_i\n");

    out.push_str("    block $break\n");
    out.push_str("      loop $loop\n");
    out.push_str("        local.get $list_i\n");
    out.push_str("        local.get $list_len\n");
    out.push_str("        i32.ge_u\n");
    out.push_str("        br_if $break\n");

    // Copy element i from CGRF node to wisp data
    // CGRF element node i: at offset 16 + i * node_size, value at +8
    // Wisp data: at $list_data + i * elem_size
    match elem_ty {
        Type::S32 => {
            // Dest: $list_data + $list_i * 4
            out.push_str("        local.get $list_data\n");
            out.push_str("        local.get $list_i\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            // Src: load from $in_ptr + 16 + $list_i * 12 + 8
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 16\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $list_i\n");
            out.push_str(&format!("        i32.const {}\n", elem_node_size));
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.load\n");
            out.push_str("        i32.store\n");
        }
        Type::S64 => {
            out.push_str("        local.get $list_data\n");
            out.push_str("        local.get $list_i\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 16\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $list_i\n");
            out.push_str(&format!("        i32.const {}\n", elem_node_size));
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.add\n");
            out.push_str("        i64.load\n");
            out.push_str("        i64.store\n");
        }
        Type::F32 => {
            out.push_str("        local.get $list_data\n");
            out.push_str("        local.get $list_i\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 16\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $list_i\n");
            out.push_str(&format!("        i32.const {}\n", elem_node_size));
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.add\n");
            out.push_str("        f32.load\n");
            out.push_str("        f32.store\n");
        }
        Type::F64 => {
            out.push_str("        local.get $list_data\n");
            out.push_str("        local.get $list_i\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 16\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $list_i\n");
            out.push_str(&format!("        i32.const {}\n", elem_node_size));
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.add\n");
            out.push_str("        f64.load\n");
            out.push_str("        f64.store\n");
        }
        _ => {
            out.push_str("        ;; TODO: decode non-scalar list element\n");
        }
    }

    // Increment loop counter
    out.push_str("        local.get $list_i\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $list_i\n");
    out.push_str("        br $loop\n");
    out.push_str("      end\n");
    out.push_str("    end\n");

    // Return the list pointer
    out.push_str("    local.get $list_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a single parameter from CGRF.
/// For single-param functions, the root node is the value.
/// `param_name` is the parameter name (for the local variable).
/// `param_idx` is unused for single params but kept for consistency.
/// `is_tuple_element` indicates if we're reading from a tuple (affects offset calculation).
pub(crate) fn generate_cgrf_decode_param(
    out: &mut String,
    param_ty: &Type,
    param_name: &str,
    _param_idx: usize,
    _is_tuple_element: bool,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    // For single param, root node is at offset 16, payload at offset 24
    match param_ty {
        Type::S32 => {
            generate_cgrf_decode_s32(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::S64 => {
            generate_cgrf_decode_s64(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::F32 => {
            generate_cgrf_decode_f32(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::F64 => {
            generate_cgrf_decode_f64(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::Str => {
            generate_cgrf_decode_string(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::Record(rec_name) => {
            generate_cgrf_decode_record(out, rec_name, param_name, records);
        }
        Type::Option(inner_ty) => {
            generate_cgrf_decode_option(out, inner_ty, param_name);
        }
        Type::List(elem_ty) => {
            generate_cgrf_decode_list(out, elem_ty, param_name);
        }
        Type::Variant(variant_name) => {
            generate_cgrf_decode_variant(out, variant_name, param_name, variants);
        }
        Type::Result(ok_ty, err_ty) => {
            generate_cgrf_decode_result(out, ok_ty, err_ty, param_name);
        }
        Type::Any => {
            // A top-level `any` param: the whole input CGRF buffer IS the value.
            // Copy it into a len-prefixed heap blob [len:u32][cgrf bytes], the
            // uniform in-guest representation shared with constructed values.
            out.push_str("    ;; Decode `any`: copy input CGRF into a len-prefixed blob\n");
            out.push_str("    local.get $in_len\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    call $__alloc\n");
            out.push_str("    local.set $any_ptr\n");
            // Copy CGRF bytes to any_ptr+4 before writing the length prefix
            // (memory.copy is memmove-safe, so any source overlap is fine).
            out.push_str("    local.get $any_ptr\n");
            out.push_str("    i32.const 4\n");
            out.push_str("    i32.add\n");
            out.push_str("    local.get $in_ptr\n");
            out.push_str("    local.get $in_len\n");
            out.push_str("    memory.copy\n");
            out.push_str("    local.get $any_ptr\n");
            out.push_str("    local.get $in_len\n");
            out.push_str("    i32.store\n");
            out.push_str("    local.get $any_ptr\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        _ => {
            // For complex types, we'd need more sophisticated decoding
            out.push_str(&format!(
                "    ;; TODO: decode complex param type for {}\n",
                param_name
            ));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
    }
}

/// Generate WAT code to find a node by index in CGRF.
/// Scans from offset 16, counting nodes until reaching the target index.
/// Result is stored in $child_offset.
///
/// Requires locals: $scan_i, $child_offset, $payload_len
/// Input: target index in $child_idx
pub(crate) fn generate_find_node_by_index(out: &mut String) {
    out.push_str("    ;; Find node at index $child_idx\n");

    // Start at offset 16 (after CGRF header)
    out.push_str("    i32.const 16\n");
    out.push_str("    local.set $child_offset\n");

    // Loop counter = 0
    out.push_str("    i32.const 0\n");
    out.push_str("    local.set $scan_i\n");

    // Loop: while scan_i < child_idx
    out.push_str("    (block $break\n");
    out.push_str("      (loop $continue\n");

    // Check if scan_i >= child_idx, break if so
    out.push_str("        local.get $scan_i\n");
    out.push_str("        local.get $child_idx\n");
    out.push_str("        i32.ge_u\n");
    out.push_str("        br_if $break\n");

    // Read payload_len from current node header (at child_offset + 4)
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $payload_len\n");

    // Advance child_offset by 8 + payload_len
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.const 8\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $payload_len\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $child_offset\n");

    // Increment scan_i
    out.push_str("        local.get $scan_i\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $scan_i\n");

    // Continue loop
    out.push_str("        br $continue\n");
    out.push_str("      )\n");
    out.push_str("    )\n");

    // Now $child_offset points to node at index $child_idx
}

/// Decode a record from the node at $child_offset.
/// The record node contains child indices for each field.
pub(crate) fn generate_decode_record_at_offset(
    out: &mut String,
    rec_name: &str,
    param_name: &str,
    records: &HashMap<String, RecordDef>,
) {
    let rec_def = match records.get(rec_name) {
        Some(r) => r,
        None => {
            out.push_str(&format!("    ;; ERROR: unknown record '{}'\n", rec_name));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
            return;
        }
    };

    out.push_str(&format!(
        "    ;; Decode record '{}' at $child_offset (CGRF v2)\n",
        rec_name
    ));

    // Allocate wisp record on heap
    let rec_size = rec_def.size();
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", rec_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // v2 record payload: [type_name_len:u32, type_name:utf8, field_count:u32,
    //                     field_names:(len:u32, name:utf8)*, child_indices:u32*]
    //
    // We need to calculate offset to child_indices:
    // - 8 bytes: node header
    // - 4 bytes: type_name_len
    // - N bytes: type_name (use rec_name.len() since we know the type)
    // - 4 bytes: field_count
    // - For each field: 4 bytes len + field_name.len() bytes
    // Then child_indices start
    //
    // Note: The encoded field names may differ from rec_def field names (e.g., "field0" vs "x"),
    // but the field count and number of child_indices should match.

    // Calculate offset to child_indices from start of node
    // We read the actual type_name_len and field_name_lens from the payload at runtime
    // because the encoder may use different names than the wisp source

    // First, read type_name_len to know where field_count is
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n"); // skip node header
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $str_len\n"); // type_name_len

    // field_count is at: header(8) + type_name_len(4) + type_name($str_len) + 0
    // = 12 + $str_len
    // We'll read field_count to verify, but we know it from rec_def

    // Calculate offset to field_names: 8 + 4 + $str_len + 4 = 16 + $str_len
    // Then for each field, skip 4 + field_name_len bytes
    // After all field names, child_indices start

    // For simplicity with variable-length field names, read them at runtime
    // Calculate base offset to payload data
    // child_indices_offset = 8 + 4 + type_name_len + 4 + sum(4 + field_name_len for each field)

    // Store base offset for child_indices calculation
    // We need to scan through field names to find child_indices

    // Start at offset for first field name: 8 + 4 + type_name_len + 4
    out.push_str("    i32.const 16\n"); // 8 header + 4 type_name_len + 4 field_count
    out.push_str("    local.get $str_len\n"); // type_name_len
    out.push_str("    i32.add\n");
    out.push_str("    local.set $data_len\n"); // offset to first field name

    // Skip all field names
    let field_count = rec_def.fields.len();
    for _i in 0..field_count {
        // Read field_name_len at current offset
        out.push_str("    local.get $in_ptr\n");
        out.push_str("    local.get $child_offset\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $data_len\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.load\n");
        out.push_str("    local.set $str_len\n"); // field_name_len

        // Advance: data_len += 4 + field_name_len
        out.push_str("    local.get $data_len\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $str_len\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.set $data_len\n");
    }

    // Now $data_len is the offset to child_indices from node start
    // For each field, read child_index, find that node, read value
    for (field_idx, field) in rec_def.fields.iter().enumerate() {
        // Read child_indices[field_idx] from record node
        out.push_str("    local.get $in_ptr\n");
        out.push_str("    local.get $child_offset\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $data_len\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", field_idx * 4));
        out.push_str("    i32.add\n");
        out.push_str("    i32.load\n");
        out.push_str("    local.set $child_idx\n");

        // Save current child_offset (we'll need it for other fields)
        out.push_str("    local.get $child_offset\n");
        out.push_str("    local.set $tuple_offset\n"); // reuse as temp

        // Find the field's node
        generate_find_node_by_index(out);

        // Read field value from that node (at child_offset + 8)
        out.push_str("    local.get $rec_ptr\n");
        let field_offset = rec_def.field_offset(field_idx);
        if field_offset > 0 {
            out.push_str(&format!("    i32.const {}\n", field_offset));
            out.push_str("    i32.add\n");
        }
        out.push_str("    local.get $in_ptr\n");
        out.push_str("    local.get $child_offset\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        match &field.ty {
            Type::S32 | Type::F32 => out.push_str("    i32.load\n"),
            Type::S64 | Type::F64 => out.push_str("    i64.load\n"),
            _ => out.push_str("    i32.load\n"), // default to i32 for now
        }
        out.push_str("    i32.store\n");

        // Restore child_offset for next field
        out.push_str("    local.get $tuple_offset\n");
        out.push_str("    local.set $child_offset\n");
    }

    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Decode an option from the node at $child_offset.
pub(crate) fn generate_decode_option_at_offset(
    out: &mut String,
    inner_ty: &Type,
    param_name: &str,
) {
    out.push_str("    ;; Decode option at $child_offset (CGRF v2)\n");

    let inner_size = type_size(inner_ty);
    let option_size = 4 + inner_size;

    // v2 option payload: [inner_type:type_tag*, presence:u8, child_index?:u32]
    // Calculate offset to presence byte (after node header + type tag)
    let inner_type_tag_size = type_tag_size(inner_ty);
    let presence_offset = 8 + inner_type_tag_size; // 8 = node header
    let child_index_offset = presence_offset + 1;

    // Allocate wisp option on heap
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", option_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Read presence byte from option node
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", presence_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.load8_u\n");
    out.push_str("    local.set $field_val\n");

    out.push_str("    local.get $field_val\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; None case: store tag = 0\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 0\n");
    out.push_str("        i32.store\n");
    out.push_str("      )\n");
    out.push_str("      (else\n");
    out.push_str("        ;; Some case: store tag = 1 and decode inner value\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.store\n");

    // Read child_index from option node
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str(&format!("        i32.const {}\n", child_index_offset));
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $child_idx\n");

    // Save option node offset
    out.push_str("        local.get $child_offset\n");
    out.push_str("        local.set $tuple_offset\n");

    // Find the inner value node
    generate_find_node_by_index(out);

    // Read inner value and store in option payload
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 8\n");
    out.push_str("        i32.add\n");
    match inner_ty {
        Type::S32 | Type::F32 => out.push_str("        i32.load\n"),
        Type::S64 | Type::F64 => out.push_str("        i64.load\n"),
        _ => out.push_str("        i32.load\n"),
    }
    out.push_str("        i32.store\n");

    out.push_str("      )\n");
    out.push_str("    )\n");

    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Decode a variant from the node at $child_offset.
pub(crate) fn generate_decode_variant_at_offset(
    out: &mut String,
    var_name: &str,
    param_name: &str,
    variants: &HashMap<String, VariantDef>,
) {
    let var_def = match variants.get(var_name) {
        Some(v) => v,
        None => {
            out.push_str(&format!("    ;; ERROR: unknown variant '{}'\n", var_name));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
            return;
        }
    };

    out.push_str(&format!(
        "    ;; Decode variant '{}' at $child_offset\n",
        var_name
    ));

    let var_size = var_def.size();
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", var_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Variant node payload: [tag: u32, has_payload: u8, child_index?: u32]
    // Read tag from variant node (at child_offset + 8)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $field_val\n"); // tag

    // Store tag as discriminant
    out.push_str("    local.get $rec_ptr\n");
    out.push_str("    local.get $field_val\n");
    out.push_str("    i32.store\n");

    // Read has_payload from variant node (at child_offset + 12)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load8_u\n");

    out.push_str("    i32.const 1\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; Has payload - read child_index and decode\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 13\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $child_idx\n");

    out.push_str("        local.get $child_offset\n");
    out.push_str("        local.set $tuple_offset\n");

    generate_find_node_by_index(out);

    // Read payload value
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 8\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n");

    out.push_str("      )\n");
    out.push_str("    )\n");

    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Decode a result from the node at $child_offset.
pub(crate) fn generate_decode_result_at_offset(
    out: &mut String,
    ok_ty: &Type,
    err_ty: &Type,
    param_name: &str,
) {
    out.push_str("    ;; Decode result at $child_offset\n");

    let payload_size = match ok_ty {
        Type::S64 | Type::F64 => 8,
        _ => 4,
    };
    let result_size = 4 + payload_size;

    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", result_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Result v2 format: [ok_type:type_tag*, err_type:type_tag*, tag:u32, has_payload:u8, child_index:u32]
    // Calculate offsets based on type tag sizes
    let ok_type_size = type_tag_size(ok_ty);
    let err_type_size = type_tag_size(err_ty);
    let tag_offset = 8 + ok_type_size + err_type_size;
    let has_payload_offset = tag_offset + 4;
    let child_index_offset = has_payload_offset + 1;

    // Read tag from result node
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", tag_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $field_val\n");

    // Store tag
    out.push_str("    local.get $rec_ptr\n");
    out.push_str("    local.get $field_val\n");
    out.push_str("    i32.store\n");

    // Read child_index
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", child_index_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $child_idx\n");

    out.push_str("    local.get $child_offset\n");
    out.push_str("    local.set $tuple_offset\n");

    generate_find_node_by_index(out);

    // Read payload value
    out.push_str("    local.get $rec_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    i32.store\n");

    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a parameter from a tuple in CGRF.
/// For multi-param functions, the root is a tuple with child nodes.
///
/// CGRF encoding order (depth-first):
/// - Child nodes are encoded FIRST (node 0, 1, 2, ...)
/// - Tuple node is encoded LAST (root)
///
/// For Tuple([S32(3), S32(5)]):
/// - Node 0: S32(3) at offset 16
/// - Node 1: S32(5) at offset 28
/// - Node 2: Tuple (root) at offset 40
///
/// So child i is at offset: 16 + sum(sizes of nodes 0..i-1)
/// For uniform scalars: 16 + i * node_size
// Decoding needs both the current parameter and the complete tuple layout.
#[allow(clippy::too_many_arguments)]
pub(crate) fn generate_cgrf_decode_tuple_param(
    out: &mut String,
    param_ty: &Type,
    param_name: &str,
    param_idx: usize,
    all_params: &[Parameter],
    use_runtime_offset: bool,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    out.push_str(&format!(
        "    ;; Decode tuple param {} ({})\n",
        param_idx, param_name
    ));

    // When using runtime offsets, $node_offset holds the current position
    // Otherwise, calculate compile-time offsets for fixed-size types
    if use_runtime_offset {
        // Runtime offset mode: use $node_offset local
        match param_ty {
            Type::S32 => {
                // Load payload from $in_ptr + $node_offset + 8
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
                // Advance offset: node_offset += 12 (8 header + 4 payload)
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            Type::S64 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    i64.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
                // Advance offset: node_offset += 16 (8 header + 8 payload)
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 16\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            Type::F32 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    f32.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            Type::F64 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    f64.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 16\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            Type::Str => {
                // String node: 8 byte header + 4 byte length + string bytes
                // Read data_len from header (at offset + 4)
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $data_len\n");

                // Read string length from payload (at offset + 8)
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $str_len\n");

                // Allocate wisp string: 4 bytes for length + string data
                out.push_str("    global.get $__heap_ptr\n");
                out.push_str("    local.set $str_ptr\n");
                out.push_str("    global.get $__heap_ptr\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.get $str_len\n");
                out.push_str("    i32.add\n");
                out.push_str("    global.set $__heap_ptr\n");

                // Write length to wisp string
                out.push_str("    local.get $str_ptr\n");
                out.push_str("    local.get $str_len\n");
                out.push_str("    i32.store\n");

                // Copy string data from CGRF (at offset + 12) to wisp string (at str_ptr + 4)
                out.push_str("    local.get $str_ptr\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n"); // dest
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n"); // src
                out.push_str("    local.get $str_len\n"); // len
                out.push_str("    memory.copy\n");

                // Set param to wisp string pointer
                out.push_str("    local.get $str_ptr\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));

                // Advance offset: node_offset += 8 + data_len
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.get $data_len\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            _ => {
                out.push_str(&format!(
                    "    ;; TODO: decode tuple element of complex type for {}\n",
                    param_name
                ));
                out.push_str("    i32.const 0\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
        }
    } else {
        // Compile-time offset mode for fixed-size types only
        let mut node_offset = 16; // Start after header
        for param in all_params.iter().take(param_idx) {
            node_offset += match &param.ty {
                Type::S64 | Type::F64 => 16, // 8 header + 8 payload
                _ => 12,                     // 8 header + 4 payload (s32, f32, etc.)
            };
        }

        // Payload is at node_offset + 8 (skip node header)
        let payload_offset = node_offset + 8;

        match param_ty {
            Type::S32 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
            Type::S64 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    i64.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
            Type::F32 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    f32.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
            Type::F64 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    f64.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
            Type::Record(_)
            | Type::Option(_)
            | Type::Variant(_)
            | Type::Result(_, _)
            | Type::Tuple(_)
            | Type::List(_) => {
                // Complex types need tree traversal
                out.push_str(&format!(
                    "    ;; Decode tuple element {} ({}) via tree traversal\n",
                    param_idx, param_name
                ));

                // Step 1: Find the tuple node (root)
                // Read root_index from header (offset 12)
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $child_idx\n"); // temporarily store root_index

                // Find root node offset
                generate_find_node_by_index(out);
                // Now $child_offset points to the tuple node

                // Step 2: Read child_indices[param_idx] from tuple payload
                // Tuple payload: [child_count: u32, child_indices: [u32; child_count]]
                // child_indices[param_idx] is at tuple_offset + 8 (header) + 4 (count) + param_idx * 4
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $child_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 12\n"); // 8 header + 4 for child_count
                out.push_str("    i32.add\n");
                out.push_str(&format!("    i32.const {}\n", param_idx * 4));
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $child_idx\n");

                // Step 3: Find that child node
                generate_find_node_by_index(out);
                // Now $child_offset points to the child node for this param

                // Step 4: Decode based on type
                match param_ty {
                    Type::Record(rec_name) => {
                        generate_decode_record_at_offset(out, rec_name, param_name, records);
                    }
                    Type::Option(inner_ty) => {
                        generate_decode_option_at_offset(out, inner_ty, param_name);
                    }
                    Type::Variant(var_name) => {
                        generate_decode_variant_at_offset(out, var_name, param_name, variants);
                    }
                    Type::Result(ok_ty, err_ty) => {
                        generate_decode_result_at_offset(out, ok_ty, err_ty, param_name);
                    }
                    Type::Tuple(_) | Type::List(_) => {
                        // Bridge to recursive decoder: $child_offset -> $dec_node_offset
                        out.push_str("    local.get $child_offset\n");
                        out.push_str("    local.set $dec_node_offset\n");
                        generate_cgrf_decode_recursive(out, param_ty);
                        out.push_str("    local.get $dec_result\n");
                        out.push_str(&format!("    local.set $param_{}\n", param_name));
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                out.push_str(&format!(
                    "    ;; TODO: decode tuple element of complex type for {}\n",
                    param_name
                ));
                out.push_str("    i32.const 0\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
        }
    }
}

/// Compile an expression for REPL evaluation, producing a Pack package.
///
/// This generates a WASM module (not a full package) with Pack/Graph ABI calling convention.
/// The module exports an `eval` function with signature (i32, i32, i32, i32) -> i32.
pub fn compile_repl_expr_pack(
    expr_source: &str,
    bindings: &HashMap<String, InlineValue>,
    functions: &[Function],
) -> Result<Vec<u8>> {
    let ctx = CompileContext::new(expr_source.to_string(), "<repl>".to_string());

    // Parse the expression
    let tokens = tokenize(expr_source);
    if tokens.is_empty() {
        bail!("empty expression");
    }

    let (sexpr, _) = parse_sexpr(&tokens, 0);

    // Inline variable bindings by transforming the SExpr
    let inlined_sexpr = inline_bindings(&sexpr, bindings);

    // Build function signatures from provided functions
    let mut signatures: HashMap<String, Signature> = HashMap::new();
    for func in functions {
        signatures.insert(
            func.name.clone(),
            Signature {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                result: func.return_type.clone(),
            },
        );
    }

    // Parse the expression into an Expr AST
    let expr = parse_expr(
        &inlined_sexpr,
        &[],
        &signatures,
        &HashMap::new(),
        &HashMap::new(),
        &ctx,
    )?;

    // Infer the return type
    let return_type = check_expr(
        &expr,
        &HashMap::new(),
        &signatures,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    )?;

    // Create the eval function
    let eval_fn = Function {
        name: "eval".to_string(),
        params: vec![],
        return_type,
        body: expr,
    };

    // Build the program
    let mut all_functions = functions.to_vec();
    all_functions.push(eval_fn);

    let prog = Program {
        functions: all_functions,
        imports: vec![],
        exports: vec![ExportDef::simple("eval".to_string())],
        globals: vec![],
        records: vec![],
        variants: vec![],
        resources: vec![],
        capabilities: HashSet::new(),
        world_config: None,
        data_segments: vec![],
    };

    // Type check
    let full_signatures = collect_signatures(&prog)?;
    type_check(&prog, &full_signatures, &ctx)?;

    // Generate Pack/Graph ABI WAT
    let wat = generate_wat_pack(&prog, &full_signatures);

    // Convert WAT to WASM bytes (raw module, not component)
    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;

    Ok(wasm_bytes)
}

/// Like compile_repl_expr_pack but returns WAT string instead of WASM bytes.
/// Useful for debugging and testing.
pub fn compile_repl_expr_pack_wat(
    expr_source: &str,
    bindings: &HashMap<String, InlineValue>,
    functions: &[Function],
) -> Result<String> {
    let ctx = CompileContext::new(expr_source.to_string(), "<repl>".to_string());

    let tokens = tokenize(expr_source);
    if tokens.is_empty() {
        bail!("empty expression");
    }

    let (sexpr, _) = parse_sexpr(&tokens, 0);
    let inlined_sexpr = inline_bindings(&sexpr, bindings);

    let mut signatures: HashMap<String, Signature> = HashMap::new();
    for func in functions {
        signatures.insert(
            func.name.clone(),
            Signature {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                result: func.return_type.clone(),
            },
        );
    }

    let expr = parse_expr(
        &inlined_sexpr,
        &[],
        &signatures,
        &HashMap::new(),
        &HashMap::new(),
        &ctx,
    )?;

    let return_type = check_expr(
        &expr,
        &HashMap::new(),
        &signatures,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    )?;

    let eval_fn = Function {
        name: "eval".to_string(),
        params: vec![],
        return_type,
        body: expr,
    };

    let mut all_functions = functions.to_vec();
    all_functions.push(eval_fn);

    let prog = Program {
        functions: all_functions,
        imports: vec![],
        exports: vec![ExportDef::simple("eval".to_string())],
        globals: vec![],
        records: vec![],
        variants: vec![],
        resources: vec![],
        capabilities: HashSet::new(),
        world_config: None,
        data_segments: vec![],
    };

    let full_signatures = collect_signatures(&prog)?;
    type_check(&prog, &full_signatures, &ctx)?;

    Ok(generate_wat_pack(&prog, &full_signatures))
}
