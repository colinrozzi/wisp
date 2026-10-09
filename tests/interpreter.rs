use std::path::PathBuf;
use std::sync::OnceLock;

use granite::{compiler, interpreter::Interpreter};

fn session() -> Interpreter {
    static PACKAGE: OnceLock<PathBuf> = OnceLock::new();
    let package = PACKAGE.get_or_init(|| {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // Compiling the evaluator recurses per expression node and exceeds the
        // default 2 MiB test stack, so build it on a large stack.
        std::thread::scope(|s| {
            std::thread::Builder::new()
                .stack_size(1 << 30)
                .spawn_scoped(s, || {
                    compiler::compile(
                        &root.join("wisp/interpreter/evaluator.wisp"),
                        &root.join(format!(
                            "target/interpreter-tests/{}/evaluator",
                            std::process::id()
                        )),
                        compiler::EmitOptions::default(),
                    )
                    .unwrap()
                    .wasm
                })
                .expect("spawn big-stack compile thread")
                .join()
                .expect("big-stack compile thread panicked")
        })
    });
    Interpreter::load(package).unwrap()
}

fn eval(session: &mut Interpreter, source: &str, expected: &str) {
    assert_eq!(session.evaluate(source).unwrap(), expected, "{source}");
}

#[test]
fn test_interpreter_persistent_bindings_and_isolated_sessions() {
    let mut a = session();
    eval(&mut a, "(define x 40)", "40");
    eval(&mut a, "(define add-x (lambda (y) (+ x y)))", "#<closure>");
    eval(&mut a, "(add-x 2)", "42");
    eval(&mut a, "(define x 10)", "10");
    eval(&mut a, "(add-x 2)", "12");
    eval(&mut a, "(define plus +)", "#<builtin +>");
    eval(&mut a, "(plus 20 22)", "42");
    let mut b = session();
    eval(&mut b, "x", "error: unbound symbol: x");
    eval(&mut a, "x", "10");
}

#[test]
fn test_interpreter_lexical_closures_do_not_share_call_bindings() {
    let mut s = session();
    eval(
        &mut s,
        "(define make-adder (lambda (x) (lambda (y) (+ x y))))",
        "#<closure>",
    );
    eval(&mut s, "(define add-two (make-adder 2))", "#<closure>");
    eval(&mut s, "(define add-ten (make-adder 10))", "#<closure>");
    eval(&mut s, "(add-two 40)", "42");
    eval(&mut s, "(add-ten 2)", "12");
    eval(&mut s, "(let (x 1000) (add-two 40))", "42");
    eval(&mut s, "(let (x 1) (begin (let (x 2) x) x))", "1");
    eval(
        &mut s,
        "(define get-x (let (x 7) (lambda () x)))",
        "#<closure>",
    );
    eval(&mut s, "(define x 99)", "99");
    eval(&mut s, "(get-x)", "7");
    eval(&mut s, "((lambda (f) (f 40)) add-two)", "42");
}

#[test]
fn test_interpreter_recursion_conditionals_and_recovery() {
    let mut s = session();
    eval(
        &mut s,
        "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))",
        "#<closure>",
    );
    eval(&mut s, "(fact 6)", "720");
    eval(&mut s, "(if 1 42 missing)", "42");
    eval(&mut s, "(if nil missing 42)", "42");
    eval(&mut s, "(if '() missing 42)", "42");
    eval(
        &mut s,
        "(define forever (lambda () (forever)))",
        "#<closure>",
    );
    eval(&mut s, "(forever)", "error: evaluation nesting limit");
    eval(&mut s, "(fact 5)", "120");
    eval(
        &mut s,
        "(define tree (lambda (n) (if (= n 0) 1 (+ (tree (- n 1)) (tree (- n 1))))))",
        "#<closure>",
    );
    eval(&mut s, "(tree 20)", "error: evaluation step limit");
    eval(&mut s, "(fact 4)", "24");
}

#[test]
fn test_interpreter_data_and_reader() {
    let mut s = session();
    for (source, expected) in [
        ("; comment\n(+ 20 22) ; trailing", "42"),
        ("-2147483648", "-2147483648"),
        ("2147483647", "2147483647"),
        ("(+ 2147483647 1)", "-2147483648"),
        (r#""hello, λ 🌍\n\t\r\"\\""#, r#""hello, λ 🌍\n\t\r\"\\""#),
        ("'(1 hello (2 3))", "(1 hello (2 3))"),
        ("(cons 1 (list 2 3))", "(1 2 3)"),
        ("(car '(7 8))", "7"),
        ("(cdr '(7 8))", "(8)"),
        ("(begin 1 2 3)", "3"),
        ("1 2 3", "3"),
        ("(begin)", "()"),
        ("", "()"),
        ("; only comment", "()"),
    ] {
        eval(&mut s, source, expected);
    }
    // List operations must not modify quoted data retained across inputs.
    eval(&mut s, "(define xs '(2 3))", "(2 3)");
    eval(&mut s, "(cons 1 xs)", "(1 2 3)");
    eval(&mut s, "xs", "(2 3)");
}

#[test]
fn test_interpreter_errors_preserve_existing_definitions() {
    let mut s = session();
    eval(&mut s, "(define x 42)", "42");
    for source in [
        "(",
        ")",
        "'",
        "\"unterminated",
        "\"bad\\q\"",
        "2147483648",
        "-2147483649",
        "1.5.0",
        "123abc",
        "(if 1)",
        "(define)",
        "(define 7 8)",
        "(define x missing)",
        "(define x (/ 1 0))",
        "(let (a) a)",
        "(let (1 2) 3)",
        "(let bad 3)",
        "(lambda (x x) x)",
        "(lambda (1) 2)",
        "(lambda x x)",
        "(lambda ())",
        "((lambda (a) a))",
        "(3 4)",
        "(+ 1)",
        "(+ 1 2 3)",
        "(+ 1 \"x\")",
        "(/ -2147483648 -1)",
        "(car nil)",
        "(cdr 1)",
        "(cons 1 2)",
        "(let (a 1) (define x 0))",
        "(define x (define other 1))",
        // Read all forms before evaluation; a malformed batch publishes nothing.
        "(define x 0) (",
    ] {
        let output = s.evaluate(source).unwrap();
        assert!(output.starts_with("error: "), "{source}: {output}");
        eval(&mut s, "x", "42");
    }
    // Successful preceding forms stay committed if a later evaluation fails.
    eval(
        &mut s,
        "(define y 7) missing",
        "error: unbound symbol: missing",
    );
    eval(&mut s, "y", "7");
    let deep = format!("{}0{}", "(".repeat(66), ")".repeat(66));
    eval(&mut s, &deep, "error: reader nesting limit");
    eval(&mut s, &" ".repeat(4097), "error: input exceeds 4096 bytes");
    eval(&mut s, "x", "42");
}

#[test]
fn test_interpreter_bool_literals_and_conditionals() {
    let mut s = session();
    // Reader -> value -> printer round-trip for the new bool type.
    eval(&mut s, "true", "true");
    eval(&mut s, "false", "false");
    // bool works as an `if` condition (not just s32 truth).
    eval(&mut s, "(if true 1 2)", "1");
    eval(&mut s, "(if false 1 2)", "2");
    // bound and returned like any other value.
    eval(&mut s, "(define flag true)", "true");
    eval(&mut s, "(if flag 100 200)", "100");
}

#[test]
fn test_interpreter_u64_literals() {
    let mut s = session();
    eval(&mut s, "0u64", "0u64");
    eval(&mut s, "11u64", "11u64");
    eval(&mut s, "(define timeout 5000u64)", "5000u64");
    eval(&mut s, "(if 0u64 1 2)", "2");
    eval(&mut s, "(if 3u64 1 2)", "1");
}

#[test]
fn test_interpreter_bytes_string_bridge() {
    let mut s = session();
    // string->bytes yields a u8 byte list; the byte codes are UTF-8 of "Hi".
    eval(
        &mut s,
        "(string->bytes \"Hi\")",
        "#<list u8 (#<u8 72> #<u8 105>)>",
    );
    // bytes->string is the inverse: the round-trip returns the original string.
    eval(
        &mut s,
        "(bytes->string (string->bytes \"hello\"))",
        "\"hello\"",
    );
    // An empty string round-trips too.
    eval(&mut s, "(bytes->string (string->bytes \"\"))", "\"\"");
    // Arity and type errors are reported, not panics.
    eval(
        &mut s,
        "(bytes->string \"nope\")",
        "error: bytes->string expects a byte list",
    );
    eval(
        &mut s,
        "(string->bytes 42)",
        "error: string->bytes expects a string",
    );
}
