use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use wat::parse_str;
use wit_component::{ComponentEncoder, StringEncoding, embed_component_metadata};
use wit_parser::Resolve;

#[derive(Debug)]
pub struct CompileArtifacts {
    pub wat: PathBuf,
    pub wit: PathBuf,
    pub component: PathBuf,
}

pub fn compile(source_path: &Path, out_stem: &str) -> Result<CompileArtifacts> {
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
    let wat = generate_wat(&prog);
    let wit = generate_wit(&prog);
    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;
    let component_bytes = encode_component(&wasm_bytes, &wit)?;

    let wat_path = PathBuf::from(format!("{}.wat", out_stem));
    let component_path = PathBuf::from(format!("{}.wasm", out_stem));
    let wit_path = PathBuf::from(format!("{}.wit", out_stem));

    fs::write(&wat_path, wat).with_context(|| format!("failed to write {}", wat_path.display()))?;
    fs::write(&component_path, component_bytes)
        .with_context(|| format!("failed to write {}", component_path.display()))?;
    fs::write(&wit_path, wit).with_context(|| format!("failed to write {}", wit_path.display()))?;

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
    Number(i32),
}

#[derive(Debug, Clone)]
enum SExpr {
    Sym(String),
    Num(i32),
    List(Vec<SExpr>),
}

#[derive(Debug)]
enum Expr {
    Int(i32),
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
    params: Vec<String>,
    body: Expr,
}

#[derive(Debug, Clone)]
struct Import {
    module: String,
    name: String,
    params: Vec<String>,
}

struct PendingFunction {
    name: String,
    params: Vec<String>,
    body: SExpr,
}

#[derive(Debug)]
struct Program {
    functions: Vec<Function>,
    imports: Vec<Import>,
    exports: Vec<String>,
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
            c if c.is_whitespace() => {
                chars.next();
            }
            c if c.is_ascii_digit() => {
                let mut num = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_ascii_digit() {
                        num.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let value = num.parse::<i32>().expect("invalid integer");
                tokens.push(Token::Number(value));
            }
            _ => {
                let mut sym = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2.is_whitespace() || c2 == '(' || c2 == ')' {
                        break;
                    }
                    sym.push(c2);
                    chars.next();
                }
                tokens.push(Token::Symbol(sym));
            }
        }
    }

    tokens
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
        Some(Token::Number(n)) => (SExpr::Num(*n), pos + 1),
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
        if signatures
            .insert(func.name.clone(), func.params.len())
            .is_some()
        {
            panic!("Duplicate function '{}'", func.name);
        }
    }

    for import in &imports {
        if signatures
            .insert(import.name.clone(), import.params.len())
            .is_some()
        {
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
        let body_expr = parse_expr(&func.body, &func.params, &signatures);
        functions.push(Function {
            name: func.name,
            params: func.params,
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
    if items.len() != 4 {
        panic!("Function definitions must look like (fn name (params...) body)");
    }
    match &items[0] {
        SExpr::Sym(s) if s == "fn" => {}
        _ => panic!("Function definition must start with (fn ...)"),
    }
    let name = match &items[1] {
        SExpr::Sym(name) => name.clone(),
        _ => panic!("Function name must be a symbol"),
    };
    let params = match &items[2] {
        SExpr::List(params) => params
            .iter()
            .map(|p| match p {
                SExpr::Sym(name) => name.clone(),
                _ => panic!("Parameters must be symbols"),
            })
            .collect::<Vec<_>>(),
        _ => panic!("Expected parameter list"),
    };
    PendingFunction {
        name,
        params,
        body: items[3].clone(),
    }
}

fn parse_import_form(items: &[SExpr]) -> Import {
    if items.len() != 5 {
        panic!("Imports must look like (import module name (params...) result)");
    }

    let module = match &items[1] {
        SExpr::Sym(s) => s.clone(),
        _ => panic!("Import module must be a symbol"),
    };
    let name = match &items[2] {
        SExpr::Sym(s) => s.clone(),
        _ => panic!("Import name must be a symbol"),
    };
    let params = match &items[3] {
        SExpr::List(params) => params
            .iter()
            .map(|p| match p {
                SExpr::Sym(name) => name.clone(),
                _ => panic!("Import parameters must be symbols"),
            })
            .collect::<Vec<_>>(),
        _ => panic!("Import parameters must be a list"),
    };

    match &items[4] {
        SExpr::Sym(s) if s == "s32" => {}
        _ => panic!("Only s32 return types are supported for imports"),
    }

    Import {
        module,
        name,
        params,
    }
}

fn parse_expr(sexpr: &SExpr, vars: &[String], functions: &HashMap<String, usize>) -> Expr {
    match sexpr {
        SExpr::Num(n) => Expr::Int(*n),
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
                    if sym == "=" || sym == "<" || sym == "<=" || sym == ">" || sym == ">=" =>
                {
                    if items.len() != 3 {
                        panic!("Operator {} expects exactly 2 operands", sym);
                    }
                    let lhs = parse_expr(&items[1], vars, functions);
                    let rhs = parse_expr(&items[2], vars, functions);
                    let op = match sym.as_str() {
                        "=" => CmpOp::Eq,
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
                            if items.len() - 1 != *expected {
                                panic!(
                                    "Function '{}' expects {} arguments, got {}",
                                    sym,
                                    expected,
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

fn generate_wat(prog: &Program) -> String {
    let mut out = String::new();
    out.push_str("(module\n");
    for import in &prog.imports {
        out.push_str(&format!(
            "  (import \"{}\" \"{}\" (func ${} ",
            import.module, import.name, import.name
        ));
        for param in &import.params {
            out.push_str(&format!("(param ${} i32) ", param));
        }
        out.push_str("(result i32)))\n");
    }
    for func in &prog.functions {
        let mut body = String::new();
        let mut env = CodegenEnv::new(&func.params);
        gen_expr(&func.body, &mut body, 4, &mut env);

        out.push_str(&format!("  (func ${} ", func.name));
        for param in &func.params {
            out.push_str(&format!("(param ${} i32) ", param));
        }
        out.push_str("(result i32)\n");
        for _ in 0..env.local_count {
            out.push_str("    (local i32)\n");
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

fn gen_expr(expr: &Expr, out: &mut String, indent: usize, env: &mut CodegenEnv) {
    let pad = " ".repeat(indent);
    match expr {
        Expr::Int(n) => {
            out.push_str(&format!("{}i32.const {}\n", pad, n));
        }
        Expr::Var(name) => {
            let idx = env.lookup(name);
            out.push_str(&format!("{}local.get {}\n", pad, idx));
        }
        Expr::Add(a, b) => {
            gen_expr(a, out, indent, env);
            gen_expr(b, out, indent, env);
            out.push_str(&format!("{}i32.add\n", pad));
        }
        Expr::Sub(a, b) => {
            gen_expr(a, out, indent, env);
            gen_expr(b, out, indent, env);
            out.push_str(&format!("{}i32.sub\n", pad));
        }
        Expr::Mul(a, b) => {
            gen_expr(a, out, indent, env);
            gen_expr(b, out, indent, env);
            out.push_str(&format!("{}i32.mul\n", pad));
        }
        Expr::Call { name, args } => {
            for arg in args {
                gen_expr(arg, out, indent, env);
            }
            out.push_str(&format!("{}call ${}\n", pad, name));
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            gen_expr(cond, out, indent, env);
            out.push_str(&format!("{}(if (result i32)\n", pad));
            out.push_str(&format!("{}  (then\n", pad));
            gen_expr(then_branch, out, indent + 4, env);
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{}  (else\n", pad));
            gen_expr(else_branch, out, indent + 4, env);
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{})\n", pad));
        }
        Expr::Cmp { op, lhs, rhs } => {
            gen_expr(lhs, out, indent, env);
            gen_expr(rhs, out, indent, env);
            let instr = match op {
                CmpOp::Eq => "i32.eq",
                CmpOp::Lt => "i32.lt_s",
                CmpOp::Le => "i32.le_s",
                CmpOp::Gt => "i32.gt_s",
                CmpOp::Ge => "i32.ge_s",
            };
            out.push_str(&format!("{}{}\n", pad, instr));
        }
        Expr::Let { name, value, body } => {
            gen_expr(value, out, indent, env);
            let idx = env.declare_local();
            out.push_str(&format!("{}local.set {}\n", pad, idx));
            env.push_binding(name.clone(), idx);
            gen_expr(body, out, indent, env);
            env.pop_binding();
        }
    }
}

struct CodegenEnv {
    bindings: Vec<(String, u32)>,
    param_count: u32,
    local_count: u32,
}

impl CodegenEnv {
    fn new(params: &[String]) -> Self {
        let mut bindings = Vec::new();
        for (idx, name) in params.iter().enumerate() {
            bindings.push((name.clone(), idx as u32));
        }
        Self {
            bindings,
            param_count: params.len() as u32,
            local_count: 0,
        }
    }

    fn declare_local(&mut self) -> u32 {
        let idx = self.param_count + self.local_count;
        self.local_count += 1;
        idx
    }

    fn push_binding(&mut self, name: String, idx: u32) {
        self.bindings.push((name, idx));
    }

    fn pop_binding(&mut self) {
        self.bindings.pop();
    }

    fn lookup(&self, name: &str) -> u32 {
        self.bindings
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, idx)| *idx)
            .unwrap_or_else(|| panic!("Codegen missing variable {}", name))
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
                out.push_str(&format!("{}: s32", param));
            }
            out.push_str(") -> s32;\n");
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
            out.push_str(&format!("{}: s32", param));
        }
        out.push_str(") -> s32;\n");
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
