use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use wat::parse_str;
use wit_component::{ComponentEncoder, StringEncoding, embed_component_metadata};
use wit_parser::Resolve;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
    S32,
    S64,
    F32,
    F64,
}

#[derive(Debug)]
pub struct CompileArtifacts {
    pub wat: PathBuf,
    pub wit: PathBuf,
    pub component: PathBuf,
}

pub fn compile(source_path: &Path, out_base: &Path) -> Result<CompileArtifacts> {
    let src = fs::read_to_string(source_path)
        .with_context(|| format!("failed to read source file {}", source_path.display()))?;

    let tokens = tokenize(&src);
    let mut forms = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (sexpr, next) = parse_sexpr(&tokens, pos);
        forms.push(sexpr);
        pos = next;
    }
    if forms.is_empty() {
        bail!("no function definitions found in source");
    }

    let prog = parse_program(forms);
    let signatures = collect_signatures(&prog)?;
    type_check(&prog, &signatures)?;
    let wat = generate_wat(&prog, &signatures);
    let wit = generate_wit(&prog);
    let mut wat_path = out_base.to_path_buf();
    wat_path.set_extension("wat");
    let mut component_path = out_base.to_path_buf();
    component_path.set_extension("wasm");
    let mut wit_path = out_base.to_path_buf();
    wit_path.set_extension("wit");

    fs::write(&wat_path, &wat)
        .with_context(|| format!("failed to write {}", wat_path.display()))?;
    fs::write(&wit_path, &wit)
        .with_context(|| format!("failed to write {}", wit_path.display()))?;

    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;
    let component_bytes = encode_component(&wasm_bytes, &wit)?;
    fs::write(&component_path, component_bytes)
        .with_context(|| format!("failed to write {}", component_path.display()))?;

    Ok(CompileArtifacts {
        wat: wat_path,
        wit: wit_path,
        component: component_path,
    })
}

fn encode_component(module: &[u8], wit_source: &str) -> Result<Vec<u8>> {
    let mut resolve = Resolve::new();
    let pkg_id = resolve
        .push_str(Path::new("generated.wit"), wit_source)
        .context("failed to parse generated WIT")?;
    let world_id = resolve.packages[pkg_id]
        .worlds
        .values()
        .next()
        .copied()
        .context("generated WIT is missing a world declaration")?;
    let mut module_with_metadata = module.to_vec();
    embed_component_metadata(
        &mut module_with_metadata,
        &resolve,
        world_id,
        StringEncoding::UTF8,
    )
    .context("failed to embed component metadata")?;
    let bytes = ComponentEncoder::default()
        .module(&module_with_metadata)
        .context("failed to prepare module for component encoding")?
        .validate(true)
        .encode()
        .context("failed to encode component")?;
    Ok(bytes)
}

#[derive(Debug, Clone)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Number(NumericToken),
}

#[derive(Debug, Clone)]
enum NumericToken {
    Int { value: i64, ty: Type },
    Float { value: f64, ty: Type },
}

#[derive(Debug, Clone)]
enum SExpr {
    Sym(String),
    Int { value: i64, ty: Type },
    Float { value: f64, ty: Type },
    List(Vec<SExpr>),
}

#[derive(Debug)]
enum Expr {
    Int {
        value: i64,
        ty: Type,
    },
    Float {
        value: f64,
        ty: Type,
    },
    Ascribe {
        expr: Box<Expr>,
        ty: Type,
    },
    Var(String),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Let {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    WasmInstr {
        name: String,
        args: Vec<Expr>,
    },
    GlobalGet {
        name: String,
    },
    GlobalSet {
        name: String,
        value: Box<Expr>,
    },
}

#[derive(Debug)]
struct Function {
    name: String,
    params: Vec<Parameter>,
    return_type: Type,
    body: Expr,
}

#[derive(Debug, Clone)]
struct Parameter {
    name: String,
    ty: Type,
}

#[derive(Debug, Clone)]
struct Import {
    module: String,
    name: String,
    params: Vec<Parameter>,
    return_type: Type,
}

#[derive(Debug, Clone)]
struct Global {
    name: String,
    ty: Type,
    mutable: bool,
    init_value: i64, // For simplicity, we'll only support integer constants initially
}

struct PendingFunction {
    name: String,
    params: Vec<Parameter>,
    return_type: Type,
    body: SExpr,
}

#[derive(Debug)]
struct Program {
    functions: Vec<Function>,
    imports: Vec<Import>,
    exports: Vec<String>,
    globals: Vec<Global>,
}

#[derive(Debug, Clone)]
struct Signature {
    params: Vec<Type>,
    result: Type,
}

struct WasmInstrInfo {
    params: Vec<Type>,
    result: Type,
}

fn lookup_wasm_instr(name: &str) -> Option<WasmInstrInfo> {
    // Arithmetic instructions
    match name {
        // i32 arithmetic
        "i32.add" | "i32.sub" | "i32.mul" | "i32.div_s" | "i32.div_u" | "i32.rem_s"
        | "i32.rem_u" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        // i64 arithmetic
        "i64.add" | "i64.sub" | "i64.mul" | "i64.div_s" | "i64.div_u" | "i64.rem_s"
        | "i64.rem_u" => Some(WasmInstrInfo {
            params: vec![Type::S64, Type::S64],
            result: Type::S64,
        }),
        // f32 arithmetic
        "f32.add" | "f32.sub" | "f32.mul" | "f32.div" => Some(WasmInstrInfo {
            params: vec![Type::F32, Type::F32],
            result: Type::F32,
        }),
        // f64 arithmetic
        "f64.add" | "f64.sub" | "f64.mul" | "f64.div" => Some(WasmInstrInfo {
            params: vec![Type::F64, Type::F64],
            result: Type::F64,
        }),

        // i32 comparisons (return i32)
        "i32.eq" | "i32.ne" | "i32.lt_s" | "i32.lt_u" | "i32.gt_s" | "i32.gt_u" | "i32.le_s"
        | "i32.le_u" | "i32.ge_s" | "i32.ge_u" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        // i64 comparisons (return i32)
        "i64.eq" | "i64.ne" | "i64.lt_s" | "i64.lt_u" | "i64.gt_s" | "i64.gt_u" | "i64.le_s"
        | "i64.le_u" | "i64.ge_s" | "i64.ge_u" => Some(WasmInstrInfo {
            params: vec![Type::S64, Type::S64],
            result: Type::S32,
        }),
        // f32 comparisons (return i32)
        "f32.eq" | "f32.ne" | "f32.lt" | "f32.gt" | "f32.le" | "f32.ge" => Some(WasmInstrInfo {
            params: vec![Type::F32, Type::F32],
            result: Type::S32,
        }),
        // f64 comparisons (return i32)
        "f64.eq" | "f64.ne" | "f64.lt" | "f64.gt" | "f64.le" | "f64.ge" => Some(WasmInstrInfo {
            params: vec![Type::F64, Type::F64],
            result: Type::S32,
        }),

        // Constants (0 params, return typed value)
        "i32.const" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),
        "i64.const" => Some(WasmInstrInfo {
            params: vec![Type::S64],
            result: Type::S64,
        }),
        "f32.const" => Some(WasmInstrInfo {
            params: vec![Type::F32],
            result: Type::F32,
        }),
        "f64.const" => Some(WasmInstrInfo {
            params: vec![Type::F64],
            result: Type::F64,
        }),

        // Type conversions
        "i32.wrap_i64" => Some(WasmInstrInfo {
            params: vec![Type::S64],
            result: Type::S32,
        }),
        "i64.extend_i32_s" | "i64.extend_i32_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),
        "f32.demote_f64" => Some(WasmInstrInfo {
            params: vec![Type::F64],
            result: Type::F32,
        }),
        "f64.promote_f32" => Some(WasmInstrInfo {
            params: vec![Type::F32],
            result: Type::F64,
        }),
        "i32.trunc_f32_s" | "i32.trunc_f32_u" => Some(WasmInstrInfo {
            params: vec![Type::F32],
            result: Type::S32,
        }),
        "i32.trunc_f64_s" | "i32.trunc_f64_u" => Some(WasmInstrInfo {
            params: vec![Type::F64],
            result: Type::S32,
        }),
        "i64.trunc_f32_s" | "i64.trunc_f32_u" => Some(WasmInstrInfo {
            params: vec![Type::F32],
            result: Type::S64,
        }),
        "i64.trunc_f64_s" | "i64.trunc_f64_u" => Some(WasmInstrInfo {
            params: vec![Type::F64],
            result: Type::S64,
        }),
        "f32.convert_i32_s" | "f32.convert_i32_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::F32,
        }),
        "f32.convert_i64_s" | "f32.convert_i64_u" => Some(WasmInstrInfo {
            params: vec![Type::S64],
            result: Type::F32,
        }),
        "f64.convert_i32_s" | "f64.convert_i32_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::F64,
        }),
        "f64.convert_i64_s" | "f64.convert_i64_u" => Some(WasmInstrInfo {
            params: vec![Type::S64],
            result: Type::F64,
        }),

        // Memory operations
        "memory.size" => Some(WasmInstrInfo {
            params: vec![],
            result: Type::S32,
        }),
        "memory.grow" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),

        // Load instructions (address -> value)
        "i32.load" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),
        "i64.load" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),
        "f32.load" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::F32,
        }),
        "f64.load" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::F64,
        }),

        // Store instructions (address, value -> value)
        // Note: In WASM, stores don't return values, but for our expression-based
        // language we make them return the value that was stored for composability
        "i32.store" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        "i64.store" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S64],
            result: Type::S64,
        }),
        "f32.store" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::F32],
            result: Type::F32,
        }),
        "f64.store" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::F64],
            result: Type::F64,
        }),

        // Byte-level load operations
        "i32.load8_s" | "i32.load8_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),
        "i32.load16_s" | "i32.load16_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),
        "i64.load8_s" | "i64.load8_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),
        "i64.load16_s" | "i64.load16_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),
        "i64.load32_s" | "i64.load32_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),

        // Byte-level store operations
        "i32.store8" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        "i32.store16" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        "i64.store8" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S64],
            result: Type::S64,
        }),
        "i64.store16" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S64],
            result: Type::S64,
        }),
        "i64.store32" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S64],
            result: Type::S64,
        }),

        _ => None,
    }
}

fn type_check(prog: &Program, signatures: &HashMap<String, Signature>) -> Result<()> {
    // Build global type map
    let mut globals_map = HashMap::new();
    for global in &prog.globals {
        globals_map.insert(global.name.clone(), (global.ty, global.mutable));
    }

    for func in &prog.functions {
        let mut env = HashMap::new();
        for param in &func.params {
            env.insert(param.name.clone(), param.ty);
        }
        let body_ty = check_expr(&func.body, &env, signatures, &globals_map)?;
        if body_ty != func.return_type {
            bail!(
                "function '{}' returns {:?} but body has type {:?}",
                func.name,
                func.return_type,
                body_ty
            );
        }
    }
    Ok(())
}

fn collect_signatures(prog: &Program) -> Result<HashMap<String, Signature>> {
    let mut signatures = HashMap::new();
    for func in &prog.functions {
        let params = func.params.iter().map(|p| p.ty).collect();
        let sig = Signature {
            params,
            result: func.return_type,
        };
        if signatures.insert(func.name.clone(), sig).is_some() {
            bail!("Duplicate function '{}'", func.name);
        }
    }
    for import in &prog.imports {
        let params = import.params.iter().map(|p| p.ty).collect();
        let sig = Signature {
            params,
            result: import.return_type,
        };
        if signatures.insert(import.name.clone(), sig).is_some() {
            bail!("Duplicate function '{}'", import.name);
        }
    }
    Ok(signatures)
}

fn check_expr(
    expr: &Expr,
    env: &HashMap<String, Type>,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
) -> Result<Type> {
    match expr {
        Expr::Int { ty, .. } => Ok(*ty),
        Expr::Float { ty, .. } => Ok(*ty),
        Expr::Ascribe { expr, ty } => {
            let inner_ty = check_expr(expr, env, signatures, globals)?;
            ensure_numeric(inner_ty, "ascribe requires numeric types")?;
            ensure_numeric(*ty, "ascribe requires numeric types")?;
            Ok(*ty)
        }
        Expr::Var(name) => env
            .get(name)
            .copied()
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
                let ty = check_expr(arg, env, signatures, globals)?;
                if ty != *expected_ty {
                    bail!(
                        "argument type mismatch calling '{}': expected {:?}, got {:?}",
                        name,
                        expected_ty,
                        ty
                    );
                }
            }
            Ok(sig.result)
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = check_expr(cond, env, signatures, globals)?;
            if cond_ty != Type::S32 {
                bail!("if condition must be s32 (0/1), got {:?}", cond_ty);
            }
            let then_ty = check_expr(then_branch, env, signatures, globals)?;
            let else_ty = check_expr(else_branch, env, signatures, globals)?;
            if then_ty != else_ty {
                bail!(
                    "if branches must return the same type, got {:?} and {:?}",
                    then_ty,
                    else_ty
                );
            }
            Ok(then_ty)
        }
        Expr::Let { name, value, body } => {
            let value_ty = check_expr(value, env, signatures, globals)?;
            let mut next_env = env.clone();
            next_env.insert(name.clone(), value_ty);
            check_expr(body, &next_env, signatures, globals)
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
                let ty = check_expr(arg, env, signatures, globals)?;
                if ty != *expected_ty {
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
            Ok(*ty)
        }
        Expr::GlobalSet { name, value } => {
            let (expected_ty, mutable) = globals
                .get(name)
                .ok_or_else(|| anyhow!("unknown global '{}'", name))?;
            if !mutable {
                bail!("cannot set immutable global '{}'", name);
            }
            let value_ty = check_expr(value, env, signatures, globals)?;
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
    }
}

fn ensure_numeric(ty: Type, _msg: &str) -> Result<()> {
    match ty {
        Type::S32 | Type::S64 | Type::F32 | Type::F64 => Ok(()),
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            _ => {
                if ch.is_whitespace() {
                    chars.next();
                    continue;
                }
                let mut lexeme = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2.is_whitespace() || c2 == '(' || c2 == ')' {
                        break;
                    }
                    lexeme.push(c2);
                    chars.next();
                }
                if let Some(num) = parse_numeric_token(&lexeme) {
                    tokens.push(Token::Number(num));
                } else {
                    tokens.push(Token::Symbol(lexeme));
                }
            }
        }
    }

    tokens
}

fn parse_numeric_token(raw: &str) -> Option<NumericToken> {
    let (base, explicit_type) = strip_numeric_suffix(raw)?;

    let is_float = base.contains('.') || matches!(explicit_type, Some(Type::F32 | Type::F64));
    if is_float {
        let value: f64 = base.parse().ok()?;
        let ty = explicit_type.unwrap_or(Type::F64);
        match ty {
            Type::F32 | Type::F64 => Some(NumericToken::Float { value, ty }),
            _ => None,
        }
    } else {
        let value: i64 = base.parse().ok()?;
        let ty = explicit_type.unwrap_or(Type::S32);
        match ty {
            Type::S32 | Type::S64 => Some(NumericToken::Int { value, ty }),
            _ => None,
        }
    }
}

fn strip_numeric_suffix(raw: &str) -> Option<(&str, Option<Type>)> {
    if raw.is_empty() {
        return None;
    }
    let suffixes = [("s64", Type::S64), ("f32", Type::F32), ("f64", Type::F64)];
    for (suffix, ty) in suffixes {
        if let Some(base) = raw.strip_suffix(suffix) {
            return Some((base, Some(ty)));
        }
    }
    Some((raw, None))
}

fn parse_sexpr(tokens: &[Token], pos: usize) -> (SExpr, usize) {
    match tokens.get(pos) {
        Some(Token::LParen) => {
            let mut elems = Vec::new();
            let mut i = pos + 1;
            loop {
                match tokens.get(i) {
                    Some(Token::RParen) => {
                        return (SExpr::List(elems), i + 1);
                    }
                    Some(_) => {
                        let (sexpr, next) = parse_sexpr(tokens, i);
                        elems.push(sexpr);
                        i = next;
                    }
                    None => {
                        panic!("Unclosed parenthesis in input");
                    }
                }
            }
        }
        Some(Token::RParen) => {
            panic!("Unexpected closing parenthesis");
        }
        Some(Token::Symbol(s)) => (SExpr::Sym(s.clone()), pos + 1),
        Some(Token::Number(NumericToken::Int { value, ty })) => (
            SExpr::Int {
                value: *value,
                ty: *ty,
            },
            pos + 1,
        ),
        Some(Token::Number(NumericToken::Float { value, ty })) => (
            SExpr::Float {
                value: *value,
                ty: *ty,
            },
            pos + 1,
        ),
        None => panic!("Unexpected end of tokens"),
    }
}

fn parse_program(forms: Vec<SExpr>) -> Program {
    let mut pending = Vec::new();
    let mut defined = HashSet::new();
    let mut imports = Vec::new();
    let mut imported = HashSet::new();
    let mut exports = Vec::new();
    let mut export_set = HashSet::new();
    let mut globals = Vec::new();
    let mut global_names = HashSet::new();

    for form in forms {
        match form {
            SExpr::List(items) => {
                if items.is_empty() {
                    panic!("Top-level list cannot be empty");
                }
                match &items[0] {
                    SExpr::Sym(sym) if sym == "fn" => {
                        let func = parse_fn_form(SExpr::List(items));
                        if !defined.insert(func.name.clone()) {
                            panic!("Duplicate function '{}'", func.name);
                        }
                        pending.push(func);
                    }
                    SExpr::Sym(sym) if sym == "export" => {
                        if items.len() != 2 {
                            panic!("export expects exactly one argument");
                        }
                        match &items[1] {
                            SExpr::Sym(name) => {
                                if export_set.insert(name.clone()) {
                                    exports.push(name.clone());
                                }
                            }
                            SExpr::List(_) => {
                                let func = parse_fn_form(items[1].clone());
                                if !defined.insert(func.name.clone()) {
                                    panic!("Duplicate function '{}'", func.name);
                                }
                                if export_set.insert(func.name.clone()) {
                                    exports.push(func.name.clone());
                                }
                                pending.push(func);
                            }
                            _ => panic!("export argument must be a symbol or (fn ...)"),
                        }
                    }
                    SExpr::Sym(sym) if sym == "import" => {
                        let import = parse_import_form(&items);
                        if defined.contains(&import.name) {
                            panic!(
                                "Function '{}' is already defined and cannot be imported",
                                import.name
                            );
                        }
                        if !imported.insert(import.name.clone()) {
                            panic!("Duplicate import '{}'", import.name);
                        }
                        imports.push(import);
                    }
                    SExpr::Sym(sym) if sym == "global" => {
                        let global = parse_global_form(&items);
                        if !global_names.insert(global.name.clone()) {
                            panic!("Duplicate global '{}'", global.name);
                        }
                        globals.push(global);
                    }
                    _ => panic!("Unknown top-level form"),
                }
            }
            _ => panic!("Top-level forms must be lists"),
        }
    }

    let mut signatures = HashMap::new();
    for func in &pending {
        let params = func.params.iter().map(|p| p.ty).collect();
        let sig = Signature {
            params,
            result: func.return_type,
        };
        if signatures.insert(func.name.clone(), sig).is_some() {
            panic!("Duplicate function '{}'", func.name);
        }
    }

    for import in &imports {
        let params = import.params.iter().map(|p| p.ty).collect();
        let sig = Signature {
            params,
            result: import.return_type,
        };
        if signatures.insert(import.name.clone(), sig).is_some() {
            panic!("Duplicate function '{}'", import.name);
        }
    }

    for export in &exports {
        if !signatures.contains_key(export) {
            panic!("Cannot export undefined function '{}'", export);
        }
        if imported.contains(export) {
            panic!("Cannot export imported function '{}'", export);
        }
    }

    let mut functions = Vec::new();
    for func in pending {
        let param_names = func
            .params
            .iter()
            .map(|p| p.name.clone())
            .collect::<Vec<_>>();
        let body_expr = parse_expr(&func.body, &param_names, &signatures);
        functions.push(Function {
            name: func.name,
            params: func.params,
            return_type: func.return_type,
            body: body_expr,
        });
    }

    Program {
        functions,
        imports,
        exports,
        globals,
    }
}

fn parse_fn_form(form: SExpr) -> PendingFunction {
    let items = match form {
        SExpr::List(items) => items,
        _ => panic!("Function definition must be a list"),
    };
    if items.len() != 5 {
        panic!("Function definitions must look like (fn name ((param type) ...) return body)");
    }
    match &items[0] {
        SExpr::Sym(s) if s == "fn" => {}
        _ => panic!("Function definition must start with (fn ...)"),
    }
    let name = match &items[1] {
        SExpr::Sym(name) => name.clone(),
        _ => panic!("Function name must be a symbol"),
    };
    let params = parse_typed_params(&items[2]);
    let return_type = parse_type_expr(&items[3]);
    PendingFunction {
        name,
        params,
        return_type,
        body: items[4].clone(),
    }
}

fn parse_import_form(items: &[SExpr]) -> Import {
    if items.len() != 5 {
        panic!("Imports must look like (import module name ((param type) ...) result)");
    }

    let module = match &items[1] {
        SExpr::Sym(s) => s.clone(),
        _ => panic!("Import module must be a symbol"),
    };
    let name = match &items[2] {
        SExpr::Sym(s) => s.clone(),
        _ => panic!("Import name must be a symbol"),
    };
    let params = parse_typed_params(&items[3]);
    let return_type = parse_type_expr(&items[4]);

    Import {
        module,
        name,
        params,
        return_type,
    }
}

fn parse_global_form(items: &[SExpr]) -> Global {
    if items.len() != 5 {
        panic!("Globals must look like (global $name type mutability init-value)");
    }

    let name = match &items[1] {
        SExpr::Sym(s) => {
            if !s.starts_with('$') {
                panic!("Global name must start with $ (e.g., $heap-ptr)");
            }
            s.clone()
        }
        _ => panic!("Global name must be a symbol starting with $"),
    };

    let ty = parse_type_expr(&items[2]);

    let mutable = match &items[3] {
        SExpr::Sym(s) => match s.as_str() {
            "mut" => true,
            "const" => false,
            _ => panic!("Global mutability must be 'mut' or 'const'"),
        },
        _ => panic!("Global mutability must be 'mut' or 'const'"),
    };

    let init_value = match &items[4] {
        SExpr::Int { value, .. } => *value,
        _ => panic!("Global init value must be an integer constant"),
    };

    Global {
        name,
        ty,
        mutable,
        init_value,
    }
}

fn parse_typed_params(expr: &SExpr) -> Vec<Parameter> {
    match expr {
        SExpr::List(params) => params
            .iter()
            .map(|p| match p {
                SExpr::List(parts) => {
                    if parts.len() != 2 {
                        panic!("Parameters must be in the form (name type)");
                    }
                    let name = match &parts[0] {
                        SExpr::Sym(s) => s.clone(),
                        _ => panic!("Parameter name must be a symbol"),
                    };
                    let ty = parse_type_expr(&parts[1]);
                    Parameter { name, ty }
                }
                _ => panic!("Parameters must be in the form (name type)"),
            })
            .collect(),
        _ => panic!("Expected parameter list"),
    }
}

fn parse_type_expr(expr: &SExpr) -> Type {
    match expr {
        SExpr::Sym(s) => parse_type_symbol(s),
        _ => panic!("Type must be a symbol"),
    }
}

fn parse_type_symbol(sym: &str) -> Type {
    match sym {
        "s32" => Type::S32,
        "s64" => Type::S64,
        "f32" => Type::F32,
        "f64" => Type::F64,
        other => panic!("Unknown type '{}'", other),
    }
}

fn is_type_symbol(sym: &str) -> bool {
    matches!(sym, "s32" | "s64" | "f32" | "f64")
}

fn parse_expr(sexpr: &SExpr, vars: &[String], functions: &HashMap<String, Signature>) -> Expr {
    match sexpr {
        SExpr::Int { value, ty } => Expr::Int {
            value: *value,
            ty: *ty,
        },
        SExpr::Float { value, ty } => Expr::Float {
            value: *value,
            ty: *ty,
        },
        SExpr::Sym(s) => {
            if vars.iter().any(|name| name == s) {
                Expr::Var(s.clone())
            } else {
                panic!("Unknown symbol: {}", s);
            }
        }
        SExpr::List(items) => {
            if items.is_empty() {
                panic!("Empty list is not a valid expression");
            }
            let op = &items[0];
            match op {
                SExpr::Sym(sym) if is_type_symbol(sym) && items.len() == 2 => {
                    let ty = parse_type_symbol(sym);
                    let inner = parse_expr(&items[1], vars, functions);
                    Expr::Ascribe {
                        expr: Box::new(inner),
                        ty,
                    }
                }
                SExpr::Sym(sym) if sym == "if" => {
                    if items.len() != 4 {
                        panic!("if expects condition, then, else");
                    }
                    let cond = parse_expr(&items[1], vars, functions);
                    let then_branch = parse_expr(&items[2], vars, functions);
                    let else_branch = parse_expr(&items[3], vars, functions);
                    Expr::If {
                        cond: Box::new(cond),
                        then_branch: Box::new(then_branch),
                        else_branch: Box::new(else_branch),
                    }
                }
                SExpr::Sym(sym) if sym == "global.get" => {
                    if items.len() != 2 {
                        panic!("global.get expects exactly one argument (the global name)");
                    }
                    let name = match &items[1] {
                        SExpr::Sym(s) => {
                            if !s.starts_with('$') {
                                panic!("Global name must start with $");
                            }
                            s.clone()
                        }
                        _ => panic!("global.get argument must be a global name starting with $"),
                    };
                    Expr::GlobalGet { name }
                }
                SExpr::Sym(sym) if sym == "global.set" => {
                    if items.len() != 3 {
                        panic!("global.set expects exactly two arguments (name and value)");
                    }
                    let name = match &items[1] {
                        SExpr::Sym(s) => {
                            if !s.starts_with('$') {
                                panic!("Global name must start with $");
                            }
                            s.clone()
                        }
                        _ => panic!(
                            "global.set first argument must be a global name starting with $"
                        ),
                    };
                    let value = parse_expr(&items[2], vars, functions);
                    Expr::GlobalSet {
                        name,
                        value: Box::new(value),
                    }
                }
                SExpr::Sym(sym) if sym == "let" => {
                    if items.len() != 3 {
                        panic!("let expects binding and body");
                    }
                    let binding = match &items[1] {
                        SExpr::List(parts) => parts,
                        _ => panic!("let binding must be a list (name value)"),
                    };
                    if binding.len() != 2 {
                        panic!("let binding must have exactly a name and value");
                    }
                    let name = match &binding[0] {
                        SExpr::Sym(s) => s.clone(),
                        _ => panic!("let binding name must be a symbol"),
                    };
                    let value_expr = parse_expr(&binding[1], vars, functions);
                    let mut next_vars = vars.to_vec();
                    next_vars.push(name.clone());
                    let body_expr = parse_expr(&items[2], &next_vars, functions);
                    Expr::Let {
                        name,
                        value: Box::new(value_expr),
                        body: Box::new(body_expr),
                    }
                }
                _ => {
                    if let SExpr::Sym(sym) = op {
                        // Check if this is a WASM instruction
                        if lookup_wasm_instr(sym).is_some() {
                            let mut args = Vec::new();
                            for arg in &items[1..] {
                                args.push(parse_expr(arg, vars, functions));
                            }
                            Expr::WasmInstr {
                                name: sym.clone(),
                                args,
                            }
                        } else if let Some(expected) = functions.get(sym) {
                            if items.len() - 1 != expected.params.len() {
                                panic!(
                                    "Function '{}' expects {} arguments, got {}",
                                    sym,
                                    expected.params.len(),
                                    items.len() - 1
                                );
                            }
                            let mut args = Vec::new();
                            for arg in &items[1..] {
                                args.push(parse_expr(arg, vars, functions));
                            }
                            Expr::Call {
                                name: sym.clone(),
                                args,
                            }
                        } else {
                            panic!("Unknown operator or function: {}", sym);
                        }
                    } else {
                        panic!("List does not start with a symbol");
                    }
                }
            }
        }
    }
}

fn generate_wat(prog: &Program, signatures: &HashMap<String, Signature>) -> String {
    let mut out = String::new();
    out.push_str("(module\n");

    // Imports must come first in WAT
    for import in &prog.imports {
        out.push_str(&format!(
            "  (import \"{}\" \"{}\" (func ${} ",
            import.module, import.name, import.name
        ));
        for param in &import.params {
            out.push_str(&format!("(param ${} {}) ", param.name, wat_type(param.ty)));
        }
        out.push_str(&format!("(result {})))\n", wat_type(import.return_type)));
    }

    // Declare memory (1 page = 64KB, allow growth up to 100 pages)
    out.push_str("  (memory 1 100)\n");

    // Build global type map for codegen
    let mut globals_map = HashMap::new();
    for global in &prog.globals {
        globals_map.insert(global.name.clone(), (global.ty, global.mutable));
    }

    // Declare globals
    for global in &prog.globals {
        let mutability = if global.mutable { "(mut " } else { "" };
        let close = if global.mutable { ")" } else { "" };
        out.push_str(&format!(
            "  (global {} {}{}{} ({}.const {}))\n",
            global.name,
            mutability,
            wat_type(global.ty),
            close,
            wat_type(global.ty),
            global.init_value
        ));
    }

    for func in &prog.functions {
        let mut body = String::new();
        let mut env = CodegenEnv::new(&func.params);
        gen_expr(&func.body, &mut body, 4, &mut env, signatures, &globals_map);

        out.push_str(&format!("  (func ${} ", func.name));
        for param in &func.params {
            out.push_str(&format!("(param ${} {}) ", param.name, wat_type(param.ty)));
        }
        out.push_str(&format!("(result {})\n", wat_type(func.return_type)));
        for local in &env.locals {
            out.push_str(&format!("    (local {})\n", wat_type(*local)));
        }
        out.push_str(&body);
        out.push_str("  )\n");
    }
    for export in &prog.exports {
        out.push_str(&format!("  (export \"{}\" (func ${}))\n", export, export));
    }
    out.push_str(")\n");
    out
}

fn gen_expr(
    expr: &Expr,
    out: &mut String,
    indent: usize,
    env: &mut CodegenEnv,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
) -> Type {
    let pad = " ".repeat(indent);
    match expr {
        Expr::Int { value, ty } => {
            let instr = match ty {
                Type::S32 => "i32.const",
                Type::S64 => "i64.const",
                _ => panic!("integer literal not supported for {:?}", ty),
            };
            out.push_str(&format!("{}{} {}\n", pad, instr, *value));
            *ty
        }
        Expr::Float { value, ty } => {
            match ty {
                Type::F32 => out.push_str(&format!("{}f32.const {}\n", pad, *value as f32)),
                Type::F64 => out.push_str(&format!("{}f64.const {}\n", pad, *value)),
                _ => panic!("float literal not supported for {:?}", ty),
            }
            *ty
        }
        Expr::Ascribe { expr, ty } => {
            let from_ty = gen_expr(expr, out, indent, env, signatures, globals);
            if from_ty == *ty {
                return from_ty;
            }
            let instr = conversion_instr(from_ty, *ty)
                .unwrap_or_else(|| panic!("unsupported conversion {:?} -> {:?}", from_ty, ty));
            out.push_str(&format!("{}{}\n", pad, instr));
            *ty
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
                gen_expr(arg, out, indent, env, signatures, globals);
            }
            out.push_str(&format!("{}call ${}\n", pad, name));
            sig.result
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = gen_expr(cond, out, indent, env, signatures, globals);
            if cond_ty != Type::S32 {
                panic!("if condition must be s32");
            }
            let result_ty = expr_type(then_branch, env, signatures, globals);
            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(result_ty)));
            out.push_str(&format!("{}  (then\n", pad));
            gen_expr(then_branch, out, indent + 4, env, signatures, globals);
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{}  (else\n", pad));
            let else_ty = gen_expr(else_branch, out, indent + 4, env, signatures, globals);
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
        Expr::Let { name, value, body } => {
            let value_ty = gen_expr(value, out, indent, env, signatures, globals);
            let idx = env.declare_local(value_ty);
            out.push_str(&format!("{}local.set {}\n", pad, idx));
            env.push_binding(name.clone(), idx);
            let body_ty = gen_expr(body, out, indent, env, signatures, globals);
            env.pop_binding();
            body_ty
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
                let value_ty = gen_expr(&args[1], out, indent, env, signatures, globals);
                let value_local = env.declare_local(value_ty);
                out.push_str(&format!("{}local.set {}\n", pad, value_local));

                // Emit the address
                gen_expr(&args[0], out, indent, env, signatures, globals);

                // Get the value back
                out.push_str(&format!("{}local.get {}\n", pad, value_local));

                // Emit the store
                out.push_str(&format!("{}{}\n", pad, name));

                // Put the value back on the stack as the "return value"
                out.push_str(&format!("{}local.get {}\n", pad, value_local));
            } else {
                // Normal instructions - emit args then instruction
                for arg in args {
                    gen_expr(arg, out, indent, env, signatures, globals);
                }
                out.push_str(&format!("{}{}\n", pad, name));
            }
            instr_info.result
        }
        Expr::GlobalGet { name } => {
            out.push_str(&format!("{}global.get {}\n", pad, name));
            let (ty, _) = globals.get(name).expect("global should exist");
            *ty
        }
        Expr::GlobalSet { name, value } => {
            // Global.set consumes the value, so we save it to a local first
            // and restore it after to return the value for composability
            let value_ty = gen_expr(value, out, indent, env, signatures, globals);
            let value_local = env.declare_local(value_ty);
            out.push_str(&format!("{}local.set {}\n", pad, value_local));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            out.push_str(&format!("{}global.set {}\n", pad, name));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            value_ty
        }
    }
}

struct CodegenEnv {
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
            param_types: params.iter().map(|p| p.ty).collect(),
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
            self.param_types[*idx as usize]
        } else {
            let local_idx = *idx as usize - self.param_count as usize;
            self.locals[local_idx]
        };
        (*idx, ty)
    }
}

fn expr_type(
    expr: &Expr,
    env: &CodegenEnv,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
) -> Type {
    let mut vars = HashMap::new();
    for (name, idx) in &env.bindings {
        let ty = if (*idx as usize) < env.param_count as usize {
            env.param_types[*idx as usize]
        } else {
            let local_idx = *idx as usize - env.param_count as usize;
            env.locals[local_idx]
        };
        vars.insert(name.clone(), ty);
    }
    check_expr(expr, &vars, signatures, globals).expect("type checking already performed")
}

fn conversion_instr(from: Type, to: Type) -> Option<&'static str> {
    match (from, to) {
        (Type::S32, Type::S64) => Some("i64.extend_i32_s"),
        (Type::S64, Type::S32) => Some("i32.wrap_i64"),
        (Type::F32, Type::F64) => Some("f64.promote_f32"),
        (Type::F64, Type::F32) => Some("f32.demote_f64"),
        (Type::S32, Type::F32) => Some("f32.convert_i32_s"),
        (Type::S32, Type::F64) => Some("f64.convert_i32_s"),
        (Type::S64, Type::F32) => Some("f32.convert_i64_s"),
        (Type::S64, Type::F64) => Some("f64.convert_i64_s"),
        (Type::F32, Type::S32) => Some("i32.trunc_f32_s"),
        (Type::F32, Type::S64) => Some("i64.trunc_f32_s"),
        (Type::F64, Type::S32) => Some("i32.trunc_f64_s"),
        (Type::F64, Type::S64) => Some("i64.trunc_f64_s"),
        _ if from == to => None,
        _ => None,
    }
}

fn wat_type(ty: Type) -> &'static str {
    match ty {
        Type::S32 => "i32",
        Type::S64 => "i64",
        Type::F32 => "f32",
        Type::F64 => "f64",
    }
}

fn wit_type(ty: Type) -> &'static str {
    match ty {
        Type::S32 => "s32",
        Type::S64 => "s64",
        Type::F32 => "f32",
        Type::F64 => "f64",
    }
}

fn generate_wit(prog: &Program) -> String {
    let mut out = String::new();
    out.push_str("package example:wisp;\n\n");
    out.push_str("world wisp {\n");
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
                out.push_str(&format!("{}: {}", param.name, wit_type(param.ty)));
            }
            out.push_str(&format!(") -> {};\n", wit_type(import.return_type)));
        }
        out.push_str("  }\n");
    }
    for export in &prog.exports {
        let func = find_function(prog, export);
        out.push_str(&format!("  export {}: func(", export));
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&format!("{}: {}", param.name, wit_type(param.ty)));
        }
        out.push_str(&format!(") -> {};\n", wit_type(func.return_type)));
    }
    out.push_str("}\n");
    out
}

fn find_function<'a>(prog: &'a Program, name: &str) -> &'a Function {
    prog.functions
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("Function '{}' not found during codegen", name))
}
