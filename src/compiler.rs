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
    Var(String),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Cmp {
        op: CmpOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
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
}

#[derive(Debug, Clone, Copy)]
enum CmpOp {
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
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
}

#[derive(Debug, Clone)]
struct Signature {
    params: Vec<Type>,
    result: Type,
}

fn type_check(prog: &Program, signatures: &HashMap<String, Signature>) -> Result<()> {
    for func in &prog.functions {
        let mut env = HashMap::new();
        for param in &func.params {
            env.insert(param.name.clone(), param.ty);
        }
        let body_ty = check_expr(&func.body, &env, signatures)?;
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
) -> Result<Type> {
    match expr {
        Expr::Int { ty, .. } => Ok(*ty),
        Expr::Float { ty, .. } => Ok(*ty),
        Expr::Var(name) => env
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("unknown variable '{}'", name)),
        Expr::Add(lhs, rhs) | Expr::Sub(lhs, rhs) | Expr::Mul(lhs, rhs) => {
            let lty = check_expr(lhs, env, signatures)?;
            let rty = check_expr(rhs, env, signatures)?;
            if lty != rty {
                bail!(
                    "arithmetic operands must match types, got {:?} and {:?}",
                    lty,
                    rty
                );
            }
            ensure_numeric(lty, "arithmetic requires numeric types")?;
            Ok(lty)
        }
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
                let ty = check_expr(arg, env, signatures)?;
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
        Expr::Cmp { lhs, rhs, .. } => {
            let lty = check_expr(lhs, env, signatures)?;
            let rty = check_expr(rhs, env, signatures)?;
            if lty != rty {
                bail!(
                    "comparison operands must match types, got {:?} and {:?}",
                    lty,
                    rty
                );
            }
            ensure_numeric(lty, "comparison requires numeric types")?;
            Ok(Type::S32)
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = check_expr(cond, env, signatures)?;
            if cond_ty != Type::S32 {
                bail!("if condition must be s32 (0/1), got {:?}", cond_ty);
            }
            let then_ty = check_expr(then_branch, env, signatures)?;
            let else_ty = check_expr(else_branch, env, signatures)?;
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
            let value_ty = check_expr(value, env, signatures)?;
            let mut next_env = env.clone();
            next_env.insert(name.clone(), value_ty);
            check_expr(body, &next_env, signatures)
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
                SExpr::Sym(sym) if sym == "+" || sym == "-" || sym == "*" => {
                    if items.len() != 3 {
                        panic!("Operator {} expects exactly 2 operands", sym);
                    }
                    let lhs = parse_expr(&items[1], vars, functions);
                    let rhs = parse_expr(&items[2], vars, functions);
                    match sym.as_str() {
                        "+" => Expr::Add(Box::new(lhs), Box::new(rhs)),
                        "-" => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                        "*" => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                        _ => unreachable!(),
                    }
                }
                SExpr::Sym(sym)
                    if sym == "="
                        || sym == "=="
                        || sym == "<"
                        || sym == "<="
                        || sym == ">"
                        || sym == ">=" =>
                {
                    if items.len() != 3 {
                        panic!("Operator {} expects exactly 2 operands", sym);
                    }
                    let lhs = parse_expr(&items[1], vars, functions);
                    let rhs = parse_expr(&items[2], vars, functions);
                    let op = match sym.as_str() {
                        "=" | "==" => CmpOp::Eq,
                        "<" => CmpOp::Lt,
                        "<=" => CmpOp::Le,
                        ">" => CmpOp::Gt,
                        ">=" => CmpOp::Ge,
                        _ => unreachable!(),
                    };
                    Expr::Cmp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
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
                        if let Some(expected) = functions.get(sym) {
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
    for func in &prog.functions {
        let mut body = String::new();
        let mut env = CodegenEnv::new(&func.params);
        gen_expr(&func.body, &mut body, 4, &mut env, signatures);

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
        Expr::Var(name) => {
            let (idx, ty) = env.lookup(name);
            out.push_str(&format!("{}local.get {}\n", pad, idx));
            ty
        }
        Expr::Add(a, b) => {
            let lty = gen_expr(a, out, indent, env, signatures);
            let rty = gen_expr(b, out, indent, env, signatures);
            if lty != rty {
                panic!("Mismatched operand types {:?} and {:?}", lty, rty);
            }
            out.push_str(&format!("{}{}\n", pad, binop_for(lty, "add")));
            lty
        }
        Expr::Sub(a, b) => {
            let lty = gen_expr(a, out, indent, env, signatures);
            let rty = gen_expr(b, out, indent, env, signatures);
            if lty != rty {
                panic!("Mismatched operand types {:?} and {:?}", lty, rty);
            }
            out.push_str(&format!("{}{}\n", pad, binop_for(lty, "sub")));
            lty
        }
        Expr::Mul(a, b) => {
            let lty = gen_expr(a, out, indent, env, signatures);
            let rty = gen_expr(b, out, indent, env, signatures);
            if lty != rty {
                panic!("Mismatched operand types {:?} and {:?}", lty, rty);
            }
            out.push_str(&format!("{}{}\n", pad, binop_for(lty, "mul")));
            lty
        }
        Expr::Call { name, args } => {
            let sig = signatures
                .get(name)
                .unwrap_or_else(|| panic!("Missing signature for {}", name));
            for arg in args {
                gen_expr(arg, out, indent, env, signatures);
            }
            out.push_str(&format!("{}call ${}\n", pad, name));
            sig.result
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = gen_expr(cond, out, indent, env, signatures);
            if cond_ty != Type::S32 {
                panic!("if condition must be s32");
            }
            let result_ty = expr_type(then_branch, env, signatures);
            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(result_ty)));
            out.push_str(&format!("{}  (then\n", pad));
            gen_expr(then_branch, out, indent + 4, env, signatures);
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{}  (else\n", pad));
            let else_ty = gen_expr(else_branch, out, indent + 4, env, signatures);
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
        Expr::Cmp { op, lhs, rhs } => {
            let lty = gen_expr(lhs, out, indent, env, signatures);
            let rty = gen_expr(rhs, out, indent, env, signatures);
            if lty != rty {
                panic!("Mismatched operand types {:?} and {:?}", lty, rty);
            }
            let instr = match op {
                CmpOp::Eq => cmp_for(lty, "eq"),
                CmpOp::Lt => cmp_for(lty, "lt_s"),
                CmpOp::Le => cmp_for(lty, "le_s"),
                CmpOp::Gt => cmp_for(lty, "gt_s"),
                CmpOp::Ge => cmp_for(lty, "ge_s"),
            };
            out.push_str(&format!("{}{}\n", pad, instr));
            Type::S32
        }
        Expr::Let { name, value, body } => {
            let value_ty = gen_expr(value, out, indent, env, signatures);
            let idx = env.declare_local(value_ty);
            out.push_str(&format!("{}local.set {}\n", pad, idx));
            env.push_binding(name.clone(), idx);
            let body_ty = gen_expr(body, out, indent, env, signatures);
            env.pop_binding();
            body_ty
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

fn binop_for(ty: Type, op: &str) -> String {
    match ty {
        Type::S32 => format!("i32.{}", op),
        Type::S64 => format!("i64.{}", op),
        Type::F32 => format!("f32.{}", op),
        Type::F64 => format!("f64.{}", op),
    }
}

fn cmp_for(ty: Type, op: &str) -> String {
    match ty {
        Type::S32 => format!("i32.{}", op),
        Type::S64 => format!("i64.{}", op),
        Type::F32 => format!("f32.{}", op),
        Type::F64 => format!("f64.{}", op),
    }
}

fn expr_type(expr: &Expr, env: &CodegenEnv, signatures: &HashMap<String, Signature>) -> Type {
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
    check_expr(expr, &vars, signatures).expect("type checking already performed")
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
