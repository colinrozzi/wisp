use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use pack::abi::Value;
use wasmtime::Module;
use wisp::{compiler, interpreter::Interpreter};

#[path = "support/cgrf_guest.rs"]
mod cgrf_guest;
use cgrf_guest::Guest;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}
fn compile(source: &Path, name: &str) -> PathBuf {
    compiler::compile(
        source,
        &root().join(format!(
            "target/interpreter-parity/{}/{name}",
            std::process::id()
        )),
        compiler::EmitOptions::default(),
    )
    .unwrap()
    .wasm
}
fn session() -> Interpreter {
    static PACKAGE: OnceLock<PathBuf> = OnceLock::new();
    Interpreter::load(
        PACKAGE.get_or_init(|| compile(&root().join("interpreter/evaluator.lisp"), "evaluator")),
    )
    .unwrap()
}
fn self_hosted_compiler() -> &'static Module {
    static MODULE: OnceLock<Module> = OnceLock::new();
    MODULE.get_or_init(|| {
        // Compiling the large self-hosted compiler needs more than the Rust
        // test harness's default 2 MiB stack; guest execution uses normal limits.
        std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(|| {
                Module::from_file(
                    &cgrf_guest::engine(),
                    compile(&root().join("examples/wisp-compiler.lisp"), "compiler"),
                )
                .unwrap()
            })
            .unwrap()
            .join()
            .unwrap()
    })
}

// Same unchanged source, interpreted or compiled by either compiler.
fn compare_example(file: &str, cases: &[(&str, &[i32], i32)], compare_self_hosted: bool) {
    let path = root().join(file);
    let source = std::fs::read_to_string(&path).unwrap();
    let mut interpreted = session();
    let loaded = interpreted.evaluate(&source).unwrap();
    assert!(!loaded.starts_with("error:"), "{file}: {loaded}");
    let engine = cgrf_guest::engine();
    let module = Module::from_file(
        &engine,
        compile(&path, path.file_stem().unwrap().to_str().unwrap()),
    )
    .unwrap();
    let mut compiled = Guest::new(&module);
    let mut self_compiled = if compare_self_hosted {
        let mut compiler = Guest::new(self_hosted_compiler());
        let Value::String(wat) = compiler.call("compile-source", Value::String(source)) else {
            panic!("expected WAT")
        };
        Some(Guest::new(&Module::new(&engine, &wat).unwrap()))
    } else {
        None
    };
    for &(name, args, expected) in cases {
        let input = match args {
            [n] => Value::S32(*n),
            _ => Value::Tuple(args.iter().copied().map(Value::S32).collect()),
        };
        assert_eq!(
            compiled.call(name, input),
            Value::S32(expected),
            "Rust: {file} {name}"
        );
        // The self-hosted compiler emits a plain Wasm ABI rather than Pack wrappers.
        if let Some(self_compiled) = &mut self_compiled {
            let actual = match args {
                [] => self_compiled
                    .instance
                    .get_typed_func::<(), i32>(&mut self_compiled.store, name)
                    .unwrap()
                    .call(&mut self_compiled.store, ())
                    .unwrap(),
                [n] => self_compiled
                    .instance
                    .get_typed_func::<i32, i32>(&mut self_compiled.store, name)
                    .unwrap()
                    .call(&mut self_compiled.store, *n)
                    .unwrap(),
                _ => panic!("unsupported test arity"),
            };
            assert_eq!(actual, expected, "self-hosted: {file} {name}");
        }
        let expr = format!(
            "({name}{})",
            args.iter().map(|n| format!(" {n}")).collect::<String>()
        );
        assert_eq!(
            interpreted.evaluate(&expr).unwrap(),
            expected.to_string(),
            "interpreter: {file} {expr}"
        );
    }
}

#[test]
fn test_parity_existing_typed_factorial() {
    compare_example(
        "examples/factorial-test.lisp",
        &[("factorial", &[0], 1), ("factorial", &[6], 720)],
        true,
    );
}
#[test]
fn test_parity_existing_records() {
    compare_example(
        "examples/record-test.lisp",
        &[("test", &[], 30), ("test2", &[], 7)],
        true,
    );
}
#[test]
fn test_parity_existing_variants() {
    // Existing self-hosted codegen only binds the first variant payload; it
    // emits an unknown $h local for this fixture's (rectangle w h) arm.
    compare_example(
        "examples/variant-test.lisp",
        &[
            ("test-circle", &[], 75),
            ("test-rect", &[], 28),
            ("test-point", &[], 0),
        ],
        false,
    );
}

#[test]
fn test_typed_functions_reject_invalid_calls_and_returns() {
    let mut s = session();
    assert_eq!(
        s.evaluate("(fn identity ((n : s32)) : s32 n)").unwrap(),
        "#<function>"
    );
    assert_eq!(s.evaluate("(identity 42)").unwrap(), "42");
    for expr in ["(identity)", "(identity 1 2)", "(identity \"bad\")"] {
        assert!(s.evaluate(expr).unwrap().starts_with("error:"), "{expr}");
    }
    assert_eq!(
        s.evaluate("(fn bad () s32 \"bad\")").unwrap(),
        "#<function>"
    );
    assert_eq!(s.evaluate("(bad)").unwrap(), "error: expected s32");
    for expr in [
        "(fn broken ((x mystery)) s32 1)",
        "(fn broken ((x s32) (x s32)) s32 x)",
        "(fn broken () mystery 1)",
    ] {
        assert!(s.evaluate(expr).unwrap().starts_with("error:"), "{expr}");
    }
    assert_eq!(s.evaluate("(identity 42)").unwrap(), "42");
}

#[test]
fn test_nominal_types_and_pattern_errors_recover() {
    let mut s = session();
    assert_eq!(
        s.evaluate(
            "(record point (x : s32)) (record other (x s32)) (variant shape (circle s32) (empty))"
        )
        .unwrap(),
        "()"
    );
    assert_eq!(s.evaluate("(point.x (point 42))").unwrap(), "42");
    for expr in [
        "(point.x (other 42))",
        "(point \"bad\")",
        "(point)",
        "(point 1 2)",
        "(point.x 1)",
        "(point.x)",
        "(circle \"bad\")",
        "(empty 1)",
        "(match (circle 3) ((circle) 0))",
        "(match (circle 3) ((unknown x) x))",
        "(match (circle 3) ((empty) 0))",
        "(match (point 3) ((circle n) n))",
        "(match 1 ((empty) 0))",
        "(match (circle 3) ((circle 7) 1))",
        "(record point (z s32))",
        "(record broken (x mystery))",
        "(variant broken (a s32) (a string))",
        "(export ())",
        "(export 1)",
    ] {
        assert!(s.evaluate(expr).unwrap().starts_with("error:"), "{expr}");
        assert_eq!(s.evaluate("(point.x (point 42))").unwrap(), "42");
    }
    assert_eq!(
        s.evaluate("(match (circle 3) ((circle n) (+ n 1)) ((empty) 0))")
            .unwrap(),
        "4"
    );
    // Matching must not leak bindings into another scope.
    assert_eq!(
        s.evaluate("(let (n 42) (begin (match (circle 3) ((circle n) n) ((empty) 0)) n))")
            .unwrap(),
        "42"
    );
}

#[test]
fn test_parity_single_payload_variant() {
    compare_example(
        "tests/fixtures/interpreter_variant.lisp",
        &[("test-present", &[], 42), ("test-absent", &[], 0)],
        true,
    );
}

#[test]
fn test_parity_primitives_and_strings() {
    compare_example(
        "tests/fixtures/interpreter_primitives.lisp",
        &[
            ("wrap", &[], i32::MIN),
            ("unsigned-div", &[], i32::MAX),
            ("signed-rem", &[], 0),
            ("shift", &[], 2),
            ("rotate", &[], 1),
            ("compare-unsigned", &[], 1),
            ("utf8-length", &[], 2),
            ("byte-at", &[], 98),
            ("append-strings", &[], 1),
            ("slice-string", &[], 1),
        ],
        true,
    );
}

#[test]
fn test_annotated_let_and_string_errors() {
    let mut s = session();
    assert_eq!(
        s.evaluate("(let (x : s32 40) (i32.add x 2))").unwrap(),
        "42"
    );
    assert_eq!(
        s.evaluate("(fn echo ((s : string)) : string s) (echo \"ok\")")
            .unwrap(),
        "\"ok\""
    );
    for expr in [
        "(let (x : s32 \"bad\") x)",
        "(let (x : unknown 1) x)",
        "(i32.add 1)",
        "(i32.add 1 \"bad\")",
        "(i32.div_s 1 0)",
        "(i32.div_s -2147483648 -1)",
        "(i32.const)",
        "(string-len)",
        "(string-len 1)",
        "(string-ref \"x\" -1)",
        "(string-ref \"x\" 1)",
        "(string-ref \"x\" \"bad\")",
        "(string-append \"x\" 1)",
        "(substring \"abc\" 2 1)",
        "(substring \"abc\" -1 1)",
        "(substring \"abc\" 0 4)",
        "(substring \"abc\" 0 \"bad\")",
    ] {
        assert!(s.evaluate(expr).unwrap().starts_with("error:"), "{expr}");
        assert_eq!(
            s.evaluate("(echo \"still here\")").unwrap(),
            "\"still here\""
        );
    }
}

#[test]
fn test_malformed_declarations_do_not_trap_or_publish() {
    let mut s = session();
    for expr in [
        "(fn)",
        "(fn broken)",
        "(fn broken () s32)",
        "(fn broken 1 s32 1)",
        "(fn broken (()) s32 1)",
        "(fn broken ((x)) s32 1)",
        "(fn broken ((1 s32)) s32 1)",
        "(record)",
        "(record 1)",
        "(record broken ())",
        "(record broken (x))",
        "(record broken (x s32) (x string))",
        "(variant)",
        "(variant broken)",
        "(variant broken ())",
        "(variant broken 1)",
        "(variant broken (a mystery))",
        "(export)",
        "(export ())",
        "(export \"alias\" ())",
        "(export 1 ())",
        "(match)",
        "(match 1)",
    ] {
        let result = s.evaluate(expr).unwrap();
        assert!(result.starts_with("error:"), "{expr}: {result}");
        assert_eq!(
            s.evaluate("broken").unwrap(),
            "error: unbound symbol: broken"
        );
    }
    // A failed declaration must not reserve the type name.
    assert_eq!(
        s.evaluate("(record broken (x s32)) (broken.x (broken 42))")
            .unwrap(),
        "42"
    );
}

#[test]
fn test_parity_s64_literals_arithmetic_and_payloads() {
    let file = root().join("tests/fixtures/interpreter_s64.lisp");
    let mut interpreted = session();
    let loaded = interpreted
        .evaluate(&std::fs::read_to_string(&file).unwrap())
        .unwrap();
    assert!(!loaded.starts_with("error:"), "{loaded}");
    let module = Module::from_file(&cgrf_guest::engine(), compile(&file, "s64")).unwrap();
    let mut compiled = Guest::new(&module);
    for (name, expected) in [
        ("minimum", i64::MIN),
        ("maximum", i64::MAX),
        ("wrapping-add", i64::MIN),
        ("wrapping-multiply", 0),
        ("unsigned-divide", i64::MAX),
        ("signed-remainder", 0),
        ("unsigned-remainder", 5),
        ("return-literal", 4294967296),
        ("typed-local", 4294967298),
        ("tail-context", 4294967296),
        ("head-cast", 4294967296),
        ("colon-cast", 4294967296),
        ("signed-extend", -1),
        ("unsigned-extend", 4294967295),
        ("explicit-call", 4294967296),
        ("record-value", 4294967296),
        ("variant-value", 4294967296),
    ] {
        assert_eq!(
            compiled.call(name, Value::Tuple(vec![])),
            Value::S64(expected),
            "{name}"
        );
        assert_eq!(
            interpreted.evaluate(&format!("({name})")).unwrap(),
            format!("{expected}s64"),
            "{name}"
        );
    }
}

#[test]
fn test_parity_i64_primitives_across_both_compilers() {
    compare_example(
        "tests/fixtures/interpreter_i64_primitives.lisp",
        &[
            ("shift-mask", &[], 2),
            ("rotate-left", &[], -1),
            ("rotate-right", &[], 1),
            ("signed-shift", &[], -1),
            ("unsigned-shift", &[], 1),
            ("signed-compare", &[], 1),
            ("unsigned-compare", &[], 1),
            ("bitwise", &[], 16),
        ],
        true,
    );
}

#[test]
fn test_s64_primitive_boundary_matrix_matches_compiled_wasm() {
    let operations = [
        "add", "sub", "mul", "div_s", "div_u", "rem_s", "rem_u", "and", "or", "xor", "shl",
        "shr_s", "shr_u", "rotl", "rotr", "eq", "ne", "lt_s", "lt_u", "gt_s", "gt_u", "le_s",
        "le_u", "ge_s", "ge_u",
    ];
    let mut interpreted = session();
    let source = operations
        .iter()
        .enumerate()
        .map(|(i, op)| {
            let ty = if i < 15 { "s64" } else { "s32" };
            format!("(export (fn {op} ((a s64) (b s64)) {ty} (i64.{op} a b)))\n")
        })
        .collect::<String>();
    let loaded = interpreted.evaluate(&source).unwrap();
    assert!(!loaded.starts_with("error:"), "{loaded}");
    let path = root().join(format!(
        "target/interpreter-parity/{}/integer-matrix.lisp",
        std::process::id()
    ));
    std::fs::write(&path, source).unwrap();
    let module =
        Module::from_file(&cgrf_guest::engine(), compile(&path, "integer-matrix")).unwrap();
    let mut compiled = Guest::new(&module);
    for op in operations {
        for (a, b) in [
            (i64::MIN, -1),
            (i64::MAX, 2),
            (-7, 3),
            (-1, 65),
            (1, 63),
            (0, 0),
            (42, 42),
        ] {
            if ((op.starts_with("div") || op.starts_with("rem")) && b == 0)
                || (op == "div_s" && a == i64::MIN && b == -1)
            {
                continue; // Trapping cases have recoverable-error coverage above.
            }
            let value = compiled.call(op, Value::Tuple(vec![Value::S64(a), Value::S64(b)]));
            let expected = match value {
                Value::S64(n) => format!("{n}s64"),
                Value::S32(n) => n.to_string(),
                other => panic!("unexpected numeric result: {other:?}"),
            };
            let expr = format!("({op} {a}s64 {b}s64)");
            assert_eq!(interpreted.evaluate(&expr).unwrap(), expected, "{expr}");
        }
    }
}

#[test]
fn test_s64_reader_round_trips_and_error_recovery() {
    let mut s = session();
    for n in [
        i64::MIN,
        i64::MIN + 1,
        -4294967296,
        -1,
        0,
        1,
        4294967296,
        i64::MAX,
    ] {
        let source = format!("{n}s64");
        assert_eq!(s.evaluate(&source).unwrap(), source);
    }
    for (source, expected) in [
        (
            "'(1 2s64 (-9223372036854775808s64))",
            "(1 2s64 (-9223372036854775808s64))",
        ),
        ("(define wide 4294967296s64)", "4294967296s64"),
        ("(+ wide 2s64)", "4294967298s64"),
        ("(if 0s64 missing 42)", "42"),
        ("(s32 4294967297s64)", "1"),
        ("(i32.wrap_i64 4294967297)", "1"),
        ("(let (x -1) (s64 x))", "-1s64"),
        (
            "(fn identity64 ((n s64)) s64 n) (identity64 4294967296)",
            "4294967296s64",
        ),
        (
            "(record wide-box (n s64)) (wide-box.n (wide-box 4294967296))",
            "4294967296s64",
        ),
    ] {
        assert_eq!(s.evaluate(source).unwrap(), expected, "{source}");
    }
    for source in [
        "9223372036854775808s64",
        "-9223372036854775809s64",
        "9999999999999999999999999999999999999999999",
        "1s64x",
        "1.5s64",
        "'(2147483648)",
        "(i64.div_s -9223372036854775808 -1)",
        "(i64.div_u 1 0)",
        "(i64.rem_s 1 0)",
        "(i64.rem_u 1 0)",
        "(/ -9223372036854775808s64 -1s64)",
        "(/ 1s64 0s64)",
        "(i64.add 1)",
        "(i64.add 1 2 3)",
        "(i64.const)",
        "(i64.const 1 2)",
        "(s64)",
        "(s32 1 2)",
        "(s64 \"bad\")",
        "(1 : mystery)",
        "(i64.extend_i32_s 1s64)",
        "(i64.extend_i32_u 1s64)",
        "(i32.add 1s64 2)",
        "(+ 1s64 2)",
        // Bound and quoted integers are values, not adoptable syntax literals.
        "(let (x 1) (i64.add x 2))",
        "(identity64 (car '(1)))",
        "(i64.add '1 2)",
        "(let (x 1) (wide-box x))",
        "(fn wrong-width () s32 1s64) (wrong-width)",
    ] {
        let output = s.evaluate(source).unwrap();
        assert!(output.starts_with("error:"), "{source}: {output}");
        assert_eq!(s.evaluate("(+ wide 2s64)").unwrap(), "4294967298s64");
    }
}
