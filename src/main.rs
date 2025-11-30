use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;

use wat::parse_str;

/// Tokens for a tiny S-expression tokenizer.
#[derive(Debug, Clone)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Number(i32),
}

/// Very small S-expression representation.
#[derive(Debug, Clone)]
enum SExpr {
    Sym(String),
    Num(i32),
    List(Vec<SExpr>),
}

/// Our expression AST: everything is an i32.
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

/// A parsed program: a set of functions that must include `main`.
#[derive(Debug)]
struct Program {
    functions: Vec<Function>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!(
            "Usage: {} <source-file> <out-stem>\n\nExample:\n  {} prog.lisp tiny",
            args[0], args[0]
        );
        std::process::exit(1);
    }

    let src_path = &args[1];
    let out_stem = &args[2];

    let src = fs::read_to_string(src_path).expect("failed to read source file");

    // 1) tokenize + parse
    let tokens = tokenize(&src);
    let mut forms = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (sexpr, next) = parse_sexpr(&tokens, pos);
        forms.push(sexpr);
        pos = next;
    }
    if forms.is_empty() {
        eprintln!("No function definitions found in source.");
        std::process::exit(1);
    }

    let prog = parse_program(forms);

    // 2) generate WAT + WIT
    let wat = generate_wat(&prog);
    let wit = generate_wit(&prog);

    // 3) turn WAT into binary wasm
    let wasm_bytes =
        parse_str(&wat).expect("failed to convert generated WAT to wasm (wat::parse_str)");

    // 4) write files: <stem>.wat, <stem>.wasm, <stem>.wit
    let wat_path = format!("{}.wat", out_stem);
    let wasm_path = format!("{}.wasm", out_stem);
    let wit_path = format!("{}.wit", out_stem);

    fs::write(&wat_path, wat).expect("failed to write .wat file");
    fs::write(&wasm_path, wasm_bytes).expect("failed to write .wasm file");
    fs::write(&wit_path, wit).expect("failed to write .wit file");

    println!("Wrote:");
    println!("  {}", Path::new(&wat_path).display());
    println!("  {}", Path::new(&wasm_path).display());
    println!("  {}", Path::new(&wit_path).display());
}

/// Turn source into a list of tokens.
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
                // Symbol: letters and punctuation until whitespace or paren.
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

/// Parse a single S-expression starting at `pos`.
/// Returns (sexpr, next_pos).
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
    let mut signatures = HashMap::new();
    for form in &forms {
        let (name, params) = parse_fn_signature(form);
        if signatures.insert(name.clone(), params.len()).is_some() {
            panic!("Duplicate function '{}'", name);
        }
    }

    if !signatures.contains_key("main") {
        panic!("Program must define a main function");
    }

    let mut functions = Vec::new();
    for form in forms {
        functions.push(parse_function(form, &signatures));
    }

    Program { functions }
}

fn parse_fn_signature(form: &SExpr) -> (String, Vec<String>) {
    let items = match form {
        SExpr::List(items) => items,
        _ => panic!("Top-level form must be a list"),
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
    (name, params)
}

fn parse_function(form: SExpr, functions: &HashMap<String, usize>) -> Function {
    let (name, params) = parse_fn_signature(&form);
    let items = match form {
        SExpr::List(items) => items,
        _ => unreachable!(),
    };
    let body_expr = parse_expr(&items[3], &params, functions);
    Function {
        name,
        params,
        body: body_expr,
    }
}

/// Convert an S-expression into our Expr AST.
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

/// Generate WAT module with a single function:
/// (func (param i32) (result i32) ... )
fn generate_wat(prog: &Program) -> String {
    let mut out = String::new();
    out.push_str("(module\n");
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
    out.push_str("  (export \"run\" (func $main))\n");
    out.push_str(")\n");
    out
}

/// Recursively generate WAT instructions for an Expr.
/// `indent` is the number of spaces to indent.
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

/// Generate a minimal WIT interface that matches the `main` function signature.
fn generate_wit(prog: &Program) -> String {
    let main_fn = prog
        .functions
        .iter()
        .find(|f| f.name == "main")
        .expect("main function missing during WIT generation");
    let mut out = String::new();
    out.push_str("package example:tiny\n\n");
    out.push_str("world tiny {\n");
    out.push_str("  export run: func(");
    for (i, param) in main_fn.params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&format!("{}: s32", param));
    }
    out.push_str(") -> s32\n");
    out.push_str("}\n");
    out
}
