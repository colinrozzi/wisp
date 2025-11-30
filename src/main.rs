use std::env;
use std::fs;

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
    Var, // only "x"
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

/// A parsed program: one function main(x) = body
#[derive(Debug)]
struct Program {
    body: Expr,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <source-file>", args[0]);
        std::process::exit(1);
    }

    let src = fs::read_to_string(&args[1]).expect("failed to read source file");

    let tokens = tokenize(&src);
    // Parse exactly one top-level S-expression
    let (sexpr, next) = parse_sexpr(&tokens, 0);
    if next != tokens.len() {
        eprintln!(
            "Extra tokens after first top-level form (this compiler expects exactly one form)."
        );
        std::process::exit(1);
    }

    let prog = parse_program(sexpr);

    let wat = generate_wat(&prog);
    let wit = generate_wit();

    println!(";; ---- Generated WAT (WebAssembly Text) ----");
    println!("{}", wat);
    println!();
    println!(";; ---- Generated WIT (Component Interface) ----");
    println!("{}", wit);
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

/// Parse the top-level program:
/// (fn main (x) <expr>)
fn parse_program(sexpr: SExpr) -> Program {
    match sexpr {
        SExpr::List(items) => {
            if items.len() != 4 {
                panic!("Expected (fn main (x) <expr>)");
            }
            // (fn ...
            match &items[0] {
                SExpr::Sym(s) if s == "fn" => {}
                _ => panic!("Program must start with (fn ...)"),
            }
            // name: main
            match &items[1] {
                SExpr::Sym(s) if s == "main" => {}
                _ => panic!("Only 'main' function is supported"),
            }
            // params: (x)
            match &items[2] {
                SExpr::List(params) => {
                    if params.len() != 1 {
                        panic!("Only one parameter 'x' is supported");
                    }
                    match &params[0] {
                        SExpr::Sym(s) if s == "x" => {}
                        _ => panic!("Parameter must be named 'x'"),
                    }
                }
                _ => panic!("Expected parameter list (x)"),
            }

            let body_expr = parse_expr(&items[3]);
            Program { body: body_expr }
        }
        _ => panic!("Top-level form must be a list"),
    }
}

/// Convert an S-expression into our Expr AST.
fn parse_expr(sexpr: &SExpr) -> Expr {
    match sexpr {
        SExpr::Num(n) => Expr::Int(*n),
        SExpr::Sym(s) => {
            if s == "x" {
                Expr::Var
            } else {
                panic!("Unknown symbol: {}", s);
            }
        }
        SExpr::List(items) => {
            if items.is_empty() {
                panic!("Empty list is not a valid expression");
            }
            // Expect form like (+ a b), (- a b), (* a b)
            let op = &items[0];
            match op {
                SExpr::Sym(sym) if sym == "+" || sym == "-" || sym == "*" => {
                    if items.len() != 3 {
                        panic!("Operator {} expects exactly 2 operands", sym);
                    }
                    let lhs = parse_expr(&items[1]);
                    let rhs = parse_expr(&items[2]);
                    match sym.as_str() {
                        "+" => Expr::Add(Box::new(lhs), Box::new(rhs)),
                        "-" => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                        "*" => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                        _ => unreachable!(),
                    }
                }
                _ => {
                    panic!("Only +, -, * are supported as list operators");
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
    out.push_str("  (func $main (param $x i32) (result i32)\n");
    gen_expr(&prog.body, &mut out, 4);
    out.push_str("  )\n");
    out.push_str("  (export \"run\" (func $main))\n");
    out.push_str(")\n");
    out
}

/// Recursively generate WAT instructions for an Expr.
/// `indent` is the number of spaces to indent.
fn gen_expr(expr: &Expr, out: &mut String, indent: usize) {
    let pad = " ".repeat(indent);
    match expr {
        Expr::Int(n) => {
            out.push_str(&format!("{}i32.const {}\n", pad, n));
        }
        Expr::Var => {
            // param $x is local index 0
            out.push_str(&format!("{}local.get 0\n", pad));
        }
        Expr::Add(a, b) => {
            gen_expr(a, out, indent);
            gen_expr(b, out, indent);
            out.push_str(&format!("{}i32.add\n", pad));
        }
        Expr::Sub(a, b) => {
            gen_expr(a, out, indent);
            gen_expr(b, out, indent);
            out.push_str(&format!("{}i32.sub\n", pad));
        }
        Expr::Mul(a, b) => {
            gen_expr(a, out, indent);
            gen_expr(b, out, indent);
            out.push_str(&format!("{}i32.mul\n", pad));
        }
    }
}

/// Generate a minimal WIT interface that matches the function:
/// world tiny { export run: func(x: s32) -> s32 }
fn generate_wit() -> String {
    let mut out = String::new();
    out.push_str("package example:tiny\n\n");
    out.push_str("world tiny {\n");
    out.push_str("  export run: func(x: s32) -> s32\n");
    out.push_str("}\n");
    out
}
