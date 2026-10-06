use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use pack::abi::{Value, ValueType};
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

fn assert_float_output(output: &str, expected: Value, source: &str) {
    match expected {
        Value::F32(n) => {
            let actual = output
                .strip_suffix("f32")
                .unwrap_or_else(|| panic!("{source}: {output}"))
                .parse::<f64>()
                .unwrap() as f32;
            if n.is_nan() {
                assert!(actual.is_nan(), "{source}: {output}");
            } else {
                assert_eq!(actual.to_bits(), n.to_bits(), "{source}: {output}");
            }
        }
        Value::F64(n) => {
            let actual = output
                .strip_suffix("f64")
                .unwrap_or_else(|| panic!("{source}: {output}"))
                .parse::<f64>()
                .unwrap();
            if n.is_nan() {
                assert!(actual.is_nan(), "{source}: {output}");
            } else {
                assert_eq!(actual.to_bits(), n.to_bits(), "{source}: {output}");
            }
        }
        Value::S32(n) => assert_eq!(output, n.to_string(), "{source}"),
        Value::S64(n) => assert_eq!(output, format!("{n}s64"), "{source}"),
        other => panic!("unexpected result {other:?}"),
    }
}

#[test]
fn test_float_decimal_rounding_and_printed_round_trips() {
    let mut s = session();
    for literal in [
        "0",
        "-0",
        "2.5",
        "0.1",
        ".5",
        "1.",
        "1.25e+2",
        "1.25E-2",
        "9007199254740993.0",
        "16777217.0",
        "1.00000000000000011102230246251565404236316680908203125",
        "1.0000000000000001110223024625156540423631668090820312500000001",
        "1.00000005960464477539062500000000000000000000001",
        "1.7976931348623157e308",
        "1.7976931348623159e308",
        "2.2250738585072014e-308",
        "2.225073858507201e-308",
        "4.9406564584124654e-324",
        "2.4703282292062327e-324",
        "2.4703282292062328e-324",
        "1.1754943508222875e-38",
        "1.401298464324817e-45",
        "3.4028234663852886e38",
        "1e10000",
        "-1e-10000",
        "inf",
        "-inf",
        "nan",
        "NaN",
        "+Infinity",
        "-INF",
    ] {
        // The compiler parses decimal text to f64, then demotes f32 literals.
        let parsed = literal.parse::<f64>().unwrap();
        for (ty, expected) in [
            ("f64", Value::F64(parsed)),
            ("f32", Value::F32(parsed as f32)),
        ] {
            let source = format!("{literal}{ty}");
            let output = s
                .evaluate(&source)
                .unwrap_or_else(|e| panic!("{source}: {e:#}"));
            assert_float_output(&output, expected.clone(), &source);
            let again = s.evaluate(&output).unwrap();
            assert_float_output(&again, expected, &output);
        }
    }
}

#[test]
fn test_float_arithmetic_matrix_matches_compiled_wasm() {
    let mut interpreted = session();
    let operations = [
        "add", "sub", "mul", "div", "eq", "ne", "lt", "gt", "le", "ge",
    ];
    let mut source = String::new();
    for ty in ["f32", "f64"] {
        for (i, op) in operations.iter().enumerate() {
            let result = if i < 4 { ty } else { "s32" };
            source.push_str(&format!(
                "(export (fn {ty}-{op} ((a {ty}) (b {ty})) {result} ({ty}.{op} a b)))\n"
            ));
        }
    }
    let loaded = interpreted.evaluate(&source).unwrap();
    assert!(!loaded.starts_with("error:"), "{loaded}");
    let path = root().join(format!(
        "target/interpreter-parity/{}/float-matrix.lisp",
        std::process::id()
    ));
    std::fs::write(&path, source).unwrap();
    let module = Module::from_file(&cgrf_guest::engine(), compile(&path, "float-matrix")).unwrap();
    let mut compiled = Guest::new(&module);
    for ty in ["f32", "f64"] {
        for op in operations {
            for (a, b) in [
                (1.5_f64, 2.25_f64),
                (-0.0, 0.0),
                (1.0, 0.0),
                (-1.0, 0.0),
                (f64::INFINITY, f64::INFINITY),
                (f64::NAN, 1.0),
                (f64::MIN_POSITIVE, 3.0),
                (f64::MAX, 2.0),
            ] {
                let name = format!("{ty}-{op}");
                let input = if ty == "f32" {
                    vec![Value::F32(a as f32), Value::F32(b as f32)]
                } else {
                    vec![Value::F64(a), Value::F64(b)]
                };
                let expected = compiled.call(&name, Value::Tuple(input));
                let expr = format!(
                    "({name} {}{ty} {}{ty})",
                    a.to_string().to_lowercase(),
                    b.to_string().to_lowercase()
                );
                let output = interpreted
                    .evaluate(&expr)
                    .unwrap_or_else(|e| panic!("{expr}: {e:#}"));
                assert_float_output(&output, expected, &expr);
            }
        }
    }
}

#[test]
fn test_float_types_casts_and_recoverable_errors() {
    let mut s = session();
    for (source, expected) in [
        ("1.5", "1.5f64"),
        ("(f32.add 1 2)", "3f32"),
        ("(f64.const 2)", "2f64"),
        (
            "(fn identity-float ((x f64)) f64 x) (identity-float 2)",
            "2f64",
        ),
        (
            "(record floating (n f32)) (floating.n (floating 2))",
            "2f32",
        ),
        (
            "(variant floating-result (success f64)) (match (success 2.5) ((success n) n))",
            "2.5f64",
        ),
        ("(let (x : f32 4) (+ x 0.5f32))", "4.5f32"),
        (
            "(fn float-tail () f64 (if 1 (let (x 1) 2) 3)) (float-tail)",
            "2f64",
        ),
        ("(2 : f32)", "2f32"),
        ("(s32 -2.75)", "-2"),
        ("(s64 4294967296.75)", "4294967296s64"),
        ("(f32.demote_f64 2.5)", "2.5f32"),
        ("(f64.promote_f32 2.5f32)", "2.5f64"),
        ("(i32.trunc_f64_s -2147483648.75)", "-2147483648"),
        ("(i32.trunc_f64_u -0.75)", "0"),
        ("(i32.trunc_f64_u 4294967295.75)", "-1"),
        ("(i64.trunc_f64_u -0.75)", "0s64"),
        ("(i64.trunc_f64_u 18446744073709549568.0)", "-2048s64"),
        (
            "(i64.trunc_f64_s -9223372036854775808.0)",
            "-9223372036854775808s64",
        ),
        ("(f64.convert_i32_u -1)", "4294967295f64"),
        ("(if -0.0 missing 42)", "42"),
        ("(if nanf64 42 missing)", "42"),
        ("'(1.5 2f32)", "(1.5f64 2f32)"),
    ] {
        assert_eq!(s.evaluate(source).unwrap(), expected, "{source}");
    }
    for source in [
        "1.2.3",
        "1.0e",
        "1.0e+",
        "1.0e-",
        "1.0e1x",
        "1f32x",
        "1.0s64",
        ".f64",
        "(s32 nanf64)",
        "(s64 inff64)",
        "(i32.trunc_f32_u -1f32)",
        "(i32.trunc_f64_s -2147483649.0)",
        "(i32.trunc_f64_s 2147483648.0)",
        "(i32.trunc_f64_u 4294967296.0)",
        "(i64.trunc_f64_s 9223372036854775808.0)",
        "(i64.trunc_f64_u 18446744073709551616.0)",
        "(i64.trunc_f64_u -1.0)",
        "(f32.add 1.0 2.0)",
        "(+ 1.0 2f32)",
        "(f32.add 1f32)",
        "(f64.add 1 2 3)",
        "(f32.demote_f64 1f32)",
        "(f64.promote_f32 1.0)",
        "(let (x 1) (f32.convert_i64_s x))",
        "(let (x 1) (f64.add x 2))",
        "(identity-float '1)",
        "(floating 1.0)",
        "(fn wrong-float () f64 1s64) (wrong-float)",
        "(f32 \"bad\")",
        "(f64)",
        "(f64.const)",
        "(i64.trunc_f64_s)",
        "(f64xadd 1.0 2.0)",
    ] {
        let output = s.evaluate(source).unwrap();
        assert!(output.starts_with("error:"), "{source}: {output}");
        assert_eq!(s.evaluate("(identity-float 2.5)").unwrap(), "2.5f64");
    }
    assert_eq!(s.evaluate("(define float-sentinel 42)").unwrap(), "42");
    assert!(
        s.evaluate("(define float-sentinel 0) 1.0e+")
            .unwrap()
            .starts_with("error:")
    );
    assert_eq!(s.evaluate("float-sentinel").unwrap(), "42");
}

#[test]
fn test_float_round_trips_across_binary_exponents() {
    let mut s = session();
    let mut bits = 0x83d2_e7a9_61b0_4c5f_u64;
    for _ in 0..96 {
        bits = bits
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        for (source, expected) in [
            (
                format!("{:e}f64", f64::from_bits(bits)).to_lowercase(),
                Value::F64(f64::from_bits(bits)),
            ),
            (
                format!("{:e}f32", f32::from_bits(bits as u32)).to_lowercase(),
                Value::F32(f32::from_bits(bits as u32)),
            ),
        ] {
            let output = s
                .evaluate(&source)
                .unwrap_or_else(|e| panic!("{source}: {e:#}"));
            assert_float_output(&output, expected.clone(), &source);
            assert_float_output(&s.evaluate(&output).unwrap(), expected, &output);
        }
    }
}

#[test]
fn test_float_conversion_matrix_matches_compiled_wasm() {
    let mut s = session();
    let mut declarations = String::new();
    let mut cases = Vec::new();
    for target in ["f32", "f64"] {
        for (source, wasm, values) in [
            (
                "s32",
                "i32",
                vec![
                    Value::S32(i32::MIN),
                    Value::S32(-1),
                    Value::S32(0),
                    Value::S32(i32::MAX),
                ],
            ),
            (
                "s64",
                "i64",
                vec![
                    Value::S64(i64::MIN),
                    Value::S64(-1),
                    Value::S64(0),
                    Value::S64(9007199254740993),
                    Value::S64(i64::MAX),
                ],
            ),
        ] {
            for sign in ["s", "u"] {
                let name = format!("{target}.convert_{wasm}_{sign}");
                declarations.push_str(&format!(
                    "(export (fn test-{name} ((x {source})) {target} ({name} x)))\n"
                ));
                for value in &values {
                    cases.push((name.clone(), value.clone()));
                }
            }
        }
    }
    for source in ["f32", "f64"] {
        for (target, wasm) in [("s32", "i32"), ("s64", "i64")] {
            for sign in ["s", "u"] {
                let name = format!("{wasm}.trunc_{source}_{sign}");
                declarations.push_str(&format!(
                    "(export (fn test-{name} ((x {source})) {target} ({name} x)))\n"
                ));
                for n in [-0.75, 0.0, 1.75, 16777215.0] {
                    cases.push((
                        name.clone(),
                        if source == "f32" {
                            Value::F32(n as f32)
                        } else {
                            Value::F64(n)
                        },
                    ));
                }
            }
        }
    }
    for (name, source, target, values) in [
        (
            "f32.demote_f64",
            "f64",
            "f32",
            vec![Value::F64(-0.0), Value::F64(0.1), Value::F64(f64::MAX)],
        ),
        (
            "f64.promote_f32",
            "f32",
            "f64",
            vec![
                Value::F32(-0.0),
                Value::F32(0.1),
                Value::F32(f32::from_bits(1)),
            ],
        ),
    ] {
        declarations.push_str(&format!(
            "(export (fn test-{name} ((x {source})) {target} ({name} x)))\n"
        ));
        for value in values {
            cases.push((name.into(), value));
        }
    }
    let loaded = s.evaluate(&declarations).unwrap();
    assert!(!loaded.starts_with("error:"), "{loaded}");
    let path = root().join(format!(
        "target/interpreter-parity/{}/float-conversions.lisp",
        std::process::id()
    ));
    std::fs::write(&path, declarations).unwrap();
    let module =
        Module::from_file(&cgrf_guest::engine(), compile(&path, "float-conversions")).unwrap();
    let mut compiled = Guest::new(&module);
    for (name, input) in cases {
        let literal = match input {
            Value::S32(n) => n.to_string(),
            Value::S64(n) => format!("{n}s64"),
            Value::F32(n) => format!("{n:e}f32"),
            Value::F64(n) => format!("{n:e}f64"),
            _ => unreachable!(),
        };
        let expected = compiled.call(&format!("test-{name}"), input);
        let source = format!("(test-{name} {literal})");
        assert_float_output(&s.evaluate(&source).unwrap(), expected, &source);
    }
}

#[test]
fn test_parity_float_primitives_across_both_compilers() {
    compare_example(
        "tests/fixtures/interpreter_floats.lisp",
        &[
            ("double-rounding", &[], 1),
            ("single-rounding", &[], 1),
            ("infinity", &[], 1),
            ("not-a-number", &[], 1),
            ("truncate", &[], -2),
        ],
        true,
    );
}

#[test]
fn test_parity_compound_operations_across_both_compilers() {
    compare_example(
        "tests/fixtures/interpreter_collections.lisp",
        &[
            ("list-sum", &[], 42),
            ("list-alias", &[], 42),
            ("nested-push", &[], 12),
            ("option-present", &[], 42),
            ("option-absent", &[], 7),
            ("result-ok", &[], 42),
            ("result-err", &[], 4),
            ("tuple-argument", &[], 42),
            ("nested-list", &[], 42),
            ("record-list", &[], 42),
            ("variant-list", &[], 42),
            ("nested-option", &[], 42),
        ],
        true,
    );
}

#[test]
fn test_compound_values_match_compiled_exports() {
    let file = root().join("tests/fixtures/interpreter_collection_values.lisp");
    let mut s = session();
    let loaded = s
        .evaluate(&std::fs::read_to_string(&file).unwrap())
        .unwrap();
    assert!(!loaded.starts_with("error:"), "{loaded}");
    let module =
        Module::from_file(&cgrf_guest::engine(), compile(&file, "collection-values")).unwrap();
    let mut compiled = Guest::new(&module);
    for (name, expected, display) in [
        (
            "make-list",
            Value::List {
                elem_type: ValueType::S64,
                items: vec![Value::S64(4294967296)],
            },
            "#<list s64 (4294967296s64)>",
        ),
        (
            "make-some",
            Value::Option {
                inner_type: ValueType::F64,
                value: Some(Box::new(Value::F64(2.5))),
            },
            "(some f64 2.5f64)",
        ),
        (
            "make-none",
            Value::Option {
                inner_type: ValueType::String,
                value: None,
            },
            "(none string)",
        ),
        (
            "make-ok",
            Value::Result {
                ok_type: ValueType::S64,
                err_type: ValueType::String,
                value: Ok(Box::new(Value::S64(4294967296))),
            },
            "(ok s64 string 4294967296s64)",
        ),
        (
            "make-err",
            Value::Result {
                ok_type: ValueType::S64,
                err_type: ValueType::String,
                value: Err(Box::new(Value::String("oops".into()))),
            },
            "(err s64 string \"oops\")",
        ),
        (
            "make-tuple",
            Value::Tuple(vec![
                Value::S32(42),
                Value::F64(2.5),
                Value::Option {
                    inner_type: ValueType::String,
                    value: Some(Box::new(Value::String("x".into()))),
                },
            ]),
            "(tuple 42 2.5f64 (some string \"x\"))",
        ),
        (
            "make-nested",
            Value::List {
                elem_type: ValueType::Option(Box::new(ValueType::S32)),
                items: vec![
                    Value::Option {
                        inner_type: ValueType::S32,
                        value: Some(Box::new(Value::S32(42))),
                    },
                    Value::Option {
                        inner_type: ValueType::S32,
                        value: None,
                    },
                ],
            },
            "#<list (option s32) ((some s32 42) (none s32))>",
        ),
    ] {
        assert_eq!(
            compiled.call(name, Value::Tuple(vec![])),
            expected,
            "{name}"
        );
        assert_eq!(s.evaluate(&format!("({name})")).unwrap(), display, "{name}");
    }
}

#[test]
fn test_compound_types_validate_nested_and_absent_payloads() {
    let mut s = session();
    for (source, expected) in [
        (
            "(fn read-list ((xs (list s64))) s64 (list-get xs 0)) (read-list (list-push (list-new s64) 42))",
            "42s64",
        ),
        (
            "(fn maybe ((v (option s32))) s32 (match v ((some n) n) ((none) 42))) (maybe (none s32))",
            "42",
        ),
        (
            "(fn result ((v (result s32 string))) s32 (match v ((ok n) n) ((err e) (string-len e)))) (result (err s32 string \"oops\"))",
            "4",
        ),
        (
            "(fn pair ((v (tuple s32 string))) (tuple s32 string) v) (pair (tuple 42 \"x\"))",
            "(tuple 42 \"x\")",
        ),
        (
            "(fn nested ((v (list (option s32)))) s32 (list-len v)) (nested (list-new (option s32)))",
            "0",
        ),
        (
            "(let (xs : (list s32) (list-push (list-new s32) 42)) (list-get xs 0))",
            "42",
        ),
        ("(some f64 42)", "(some f64 42f64)"),
        (
            "(ok (tuple s32 string) (list s32) (tuple 42 \"x\"))",
            "(ok (tuple s32 string) (list s32) (tuple 42 \"x\"))",
        ),
        (
            "(define push list-push) (list-get (push (list-new f32) 42) 0)",
            "42f32",
        ),
    ] {
        assert_eq!(s.evaluate(source).unwrap(), expected, "{source}");
    }
    for source in [
        "(read-list (list-new s32))",
        "(maybe (none string))",
        "(maybe (some string \"x\"))",
        "(result (ok s32 s32 42))",
        "(result (err string string \"oops\"))",
        "(pair (tuple 42 1))",
        "(pair (tuple 42 \"x\" 1))",
        "(pair '(42 x))",
        "(nested (list-new (option string)))",
        "(list-push (list-new (option s32)) (none string))",
        "(list-push (list-new (list s32)) (list-new string))",
        "(tuple (lambda () 1))",
        "(tuple '(1))",
        "(some s32 \"bad\")",
        "(ok s32 string \"bad\")",
        "(err s32 string 42)",
        "(fn bad-compound () (option s32) (none string)) (bad-compound)",
        "(match (some s32 42) ((some) 0))",
        "(match (none s32) ((none x) x))",
        "(match (some s32 42) ((some x y) x))",
        "(match (some s32 42) ((ok x) x))",
        "(match (some s32 42) ((none) 0))",
        "(match (ok s32 string 42) ((ok) 0))",
        "(match (ok s32 string 42) ((some x) x))",
        "(match (tuple 42) ((tuple x) x))",
    ] {
        let output = s.evaluate(source).unwrap();
        assert!(output.starts_with("error:"), "{source}: {output}");
        assert_eq!(s.evaluate("(maybe (none s32))").unwrap(), "42");
    }
}

#[test]
fn test_typed_list_aliases_and_failed_mutation_recover() {
    let mut s = session();
    assert_eq!(
        s.evaluate("(define xs (list-new s32)) (define alias xs) (list-push xs 42)")
            .unwrap(),
        "#<list s32 (42)>"
    );
    assert_eq!(s.evaluate("(list-get alias 0)").unwrap(), "42");
    assert_eq!(
        s.evaluate("(define push-one (lambda () (list-push xs 1))) (push-one) (list-len alias)")
            .unwrap(),
        "2"
    );
    for source in [
        "(list-push xs \"bad\")",
        "(list-push xs 1s64)",
        "(list-get xs -1)",
        "(list-get xs 2)",
        "(list-get xs 0s64)",
        "(list-get (list-new s32) 0)",
        "(list-len '(1 2))",
        "(list-push '(1) 2)",
        "(car xs)",
    ] {
        let output = s.evaluate(source).unwrap();
        assert!(output.starts_with("error:"), "{source}: {output}");
        assert_eq!(s.evaluate("alias").unwrap(), "#<list s32 (42 1)>");
    }
    // Nominal identity remains enforced inside mutable containers.
    s.evaluate("(record point (x s32)) (record other (x s32)) (define points (list-new point))")
        .unwrap();
    assert!(
        s.evaluate("(list-push points (other 1))")
            .unwrap()
            .starts_with("error:")
    );
    assert_eq!(
        s.evaluate("(list-push points (point 42)) (point.x (list-get points 0))")
            .unwrap(),
        "42"
    );
    // A recursive record can introduce a cycle through its mutable children.
    s.evaluate("(record node (children (list node))) (define children (list-new node)) (define root (node children))").unwrap();
    let output = s.evaluate("(list-push children root)").unwrap();
    assert!(output.contains("#<depth-limit>"), "{output}");
    assert_eq!(
        s.evaluate("(list-len (node.children (list-get children 0)))")
            .unwrap(),
        "1"
    );
    assert_eq!(s.evaluate("(list-get xs 0)").unwrap(), "42");
    // Errors do not roll back an earlier, successful mutation in a payload.
    assert!(
        s.evaluate("(list-push xs (begin (list-push xs 7) \"bad\"))")
            .unwrap()
            .starts_with("error:")
    );
    assert_eq!(s.evaluate("alias").unwrap(), "#<list s32 (42 1 7)>");
}

#[test]
fn test_malformed_compound_forms_do_not_trap_or_publish() {
    let mut s = session();
    for source in [
        "(list-new)",
        "(list-new s32 s32)",
        "(list-new unknown)",
        "(list-new ())",
        "(list-new (list))",
        "(some)",
        "(some s32)",
        "(some s32 1 2)",
        "(none)",
        "(none s32 1)",
        "(none (option))",
        "(ok)",
        "(ok s32 string)",
        "(err s32 string)",
        "(ok unknown string 42)",
        "(ok s32 unknown 42)",
        "(tuple)",
        "(list-get)",
        "(list-get (list-new s32))",
        "(list-len)",
        "(list-len 42)",
        "(list-push)",
        "(list-push (list-new s32))",
        "(list-push (list-new s32) 1 2)",
        "(fn broken ((x (list))) s32 1)",
        "(fn broken ((x (list s32 string))) s32 1)",
        "(fn broken ((x (tuple))) s32 1)",
        "(fn broken ((x (result s32))) s32 1)",
        "(fn broken () (option unknown) 1)",
        "(record broken (x (tuple s32 unknown)))",
        "(variant broken (a (list unknown)))",
        "(record broken (x (option)))",
    ] {
        let output = s.evaluate(source).unwrap();
        assert!(output.starts_with("error:"), "{source}: {output}");
        assert_eq!(
            s.evaluate("broken").unwrap(),
            "error: unbound symbol: broken"
        );
    }
    assert_eq!(
        s.evaluate(
            "(record broken (items (list s32))) (list-len (broken.items (broken (list-new s32))))"
        )
        .unwrap(),
        "0"
    );
}

#[test]
fn test_global_state_parity_across_both_compilers() {
    compare_example(
        "tests/fixtures/interpreter_globals.lisp",
        &[
            ("current", &[], 0),
            ("next", &[], 2),
            ("next", &[], 4),
            ("lexical", &[], 4),
            ("reset", &[], 0),
            ("next", &[], 2),
            ("initialize-list", &[], 42),
            ("ratio", &[], 1),
        ],
        true,
    );
}

#[test]
fn test_global_types_mutability_and_session_isolation() {
    let mut s = session();
    for (source, expected) in [
        ("(global $count s32 mut 0)", "()"),
        ("(global $fixed : s32 const 42)", "()"),
        ("(global.set $count 7)", "7"),
        ("(let ($count 99) (global.get $count))", "7"),
        (
            "(define reader (lambda () (global.get $count))) (global.set $count 8) (reader)",
            "8",
        ),
        (
            "(global $wide s64 mut 4294967296) (global.get $wide)",
            "4294967296s64",
        ),
        ("(global $single f32 mut 2) (global.get $single)", "2f32"),
        (
            "(global $double f64 mut 2) (global.set $double 2.5)",
            "2.5f64",
        ),
        (
            "(global $wrap s32 const 4294967295) (global.get $wrap)",
            "-1",
        ),
        ("(global $items (list s32) mut 0)", "()"),
        (
            "(global.set $items (list-push (list-new s32) 42)) (list-get (global.get $items) 0)",
            "42",
        ),
        (
            "(global $maybe (option s32) mut 0) (global.set $maybe (none s32))",
            "(none s32)",
        ),
        (
            "(global $tuple (tuple s32 string) mut 0) (global.set $tuple (tuple 42 \"x\"))",
            "(tuple 42 \"x\")",
        ),
        ("(global.set $count (begin (global.set $count 9) 10))", "10"),
    ] {
        assert_eq!(s.evaluate(source).unwrap(), expected, "{source}");
    }
    for source in [
        "(global.get $missing)",
        "(global.set $missing 1)",
        "(global.set $fixed 1)",
        "(global.set $count \"bad\")",
        "(global.set $items (list-new string))",
        "(global.set $maybe (none string))",
        "(global.set $tuple (tuple 1 2))",
        "(global.set $count 1s64)",
        "(global $count s32 mut 1)",
        "(global.set $fixed (begin (global.set $count 99) 1))",
        "(global.set $count (/ 1 0))",
    ] {
        let out = s.evaluate(source).unwrap();
        assert!(out.starts_with("error:"), "{source}: {out}");
        assert_eq!(s.evaluate("(global.get $count)").unwrap(), "10");
    }
    assert_eq!(s.evaluate("(global $pending string mut 0)").unwrap(), "()");
    assert!(
        s.evaluate("(global.get $pending)")
            .unwrap()
            .contains("not been initialized")
    );
    assert_eq!(
        s.evaluate("(global.set $pending \"ready\")").unwrap(),
        "\"ready\""
    );
    let mut other = session();
    assert_eq!(
        other.evaluate("(global.get $count)").unwrap(),
        "error: unknown global"
    );
    assert_eq!(s.evaluate("(reader)").unwrap(), "10");
}

#[test]
fn test_malformed_globals_do_not_publish() {
    let mut s = session();
    for source in [
        "(global)",
        "(global $bad)",
        "(global $bad s32 mut)",
        "(global $bad s32 maybe 0)",
        "(global $bad s32 : mut 0)",
        "(global plain s32 mut 0)",
        "(global 1 s32 mut 0)",
        "(global $bad unknown mut 0)",
        "(global $bad (list) mut 0)",
        "(global $bad s32 mut 1.5)",
        "(global $bad s32 mut (i32.const 1))",
        "(global $bad string mut \"x\")",
        "(global $bad (list s32) mut 1)",
        "(global $bad s32 mut 4294967296)",
        "(let (x 1) (global $bad s32 mut 0))",
        "(global.get)",
        "(global.get 1)",
        "(global.get plain)",
        "(global.set)",
        "(global.set $bad)",
        "(global.set $bad 1 2)",
    ] {
        let out = s.evaluate(source).unwrap();
        assert!(out.starts_with("error:"), "{source}: {out}");
        assert_eq!(
            s.evaluate("(global.get $bad)").unwrap(),
            "error: unknown global"
        );
    }
    assert_eq!(
        s.evaluate("(global $bad s32 mut 42) (global.get $bad)")
            .unwrap(),
        "42"
    );
}

#[test]
fn test_includes_relative_paths_cycles_and_compiled_parity() {
    let file = root().join("tests/fixtures/interpreter_include/main.lisp");
    let mut s = session();
    assert_eq!(s.load_file(&file).unwrap(), "#<function>");
    let module = Module::from_file(&cgrf_guest::engine(), compile(&file, "included")).unwrap();
    let mut compiled = Guest::new(&module);
    for expected in [2, 4, 6] {
        assert_eq!(
            compiled.call("next", Value::Tuple(vec![])),
            Value::S32(expected)
        );
        assert_eq!(s.evaluate("(next)").unwrap(), expected.to_string());
    }
    let mut interactive = session();
    let source = format!("(include {:?}) (next)", file.to_str().unwrap());
    assert_eq!(interactive.evaluate(&source).unwrap(), "2");
    // Includes are once per input graph, not cached for the lifetime of a session.
    assert!(
        interactive
            .evaluate(&source)
            .unwrap()
            .contains("already declared")
    );
    assert_eq!(interactive.evaluate("(current)").unwrap(), "2");
    let mut relative = session();
    assert_eq!(
        relative
            .evaluate("(include \"tests/fixtures/interpreter_include/main.lisp\") (next)")
            .unwrap(),
        "2"
    );
}

#[test]
fn test_include_failures_preflight_and_recovery() {
    let mut s = session();
    let dir = root().join(format!(
        "target/interpreter-parity/{}/loading",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).unwrap();
    let main = dir.join("main.lisp");
    let child = dir.join("child.lisp");
    std::fs::write(&main, "(define marker 0) (include \"child.lisp\")").unwrap();
    assert_eq!(s.evaluate("(define marker 42)").unwrap(), "42");
    let out = s.load_file(&main).unwrap();
    assert!(
        out.contains("child.lisp") && out.starts_with("error:"),
        "{out}"
    );
    assert_eq!(s.evaluate("marker").unwrap(), "42");
    std::fs::write(&child, "(define child-value 7) (").unwrap();
    assert!(s.load_file(&main).unwrap().contains("unclosed parenthesis"));
    assert_eq!(s.evaluate("marker").unwrap(), "42");
    assert!(s.evaluate("child-value").unwrap().starts_with("error:"));
    std::fs::write(&child, "(define child-value 7)").unwrap();
    assert_eq!(s.load_file(&main).unwrap(), "7");
    assert_eq!(s.evaluate("marker").unwrap(), "0");
    // Quoted strings/forms and comments do not trigger file access.
    assert_eq!(
        s.evaluate("'(include \"missing\")").unwrap(),
        "(include \"missing\")"
    );
    assert_eq!(s.evaluate("; (include \"missing\")\n42").unwrap(), "42");
    for source in [
        "(include)",
        "(include 1)",
        "(include \"missing\" 1)",
        "(begin (include \"missing\"))",
    ] {
        assert!(
            s.evaluate(source).unwrap().starts_with("error:"),
            "{source}"
        );
    }
    // Evaluation errors retain successful preceding mutations/definitions.
    std::fs::write(&child, "(define child-value 9) missing").unwrap();
    assert!(
        s.load_file(&main)
            .unwrap()
            .contains("unbound symbol: missing")
    );
    assert_eq!(s.evaluate("child-value").unwrap(), "9");
    std::fs::write(&child, vec![0xff]).unwrap();
    assert!(s.load_file(&main).unwrap().contains("UTF-8"));
    std::fs::write(&child, " ".repeat(65537)).unwrap();
    assert!(s.load_file(&main).unwrap().contains("65536"));
    std::fs::write(
        &child,
        format!("{}(define child-value 12)", "; padding\n".repeat(600)),
    )
    .unwrap();
    assert_eq!(s.load_file(&main).unwrap(), "12");
    assert_eq!(s.load_file(&child).unwrap(), "12");
}

#[test]
fn test_include_depth_limit_and_recovery() {
    let mut s = session();
    let dir = root().join(format!(
        "target/interpreter-parity/{}/include-depth",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).unwrap();
    for index in 0..66 {
        std::fs::write(
            dir.join(format!("{index}.lisp")),
            format!("(include \"{}.lisp\")", index + 1),
        )
        .unwrap();
    }
    let out = s.load_file(dir.join("0.lisp")).unwrap();
    assert!(out.contains("include nesting limit"), "{out}");
    std::fs::write(dir.join("1.lisp"), "42").unwrap();
    assert_eq!(s.load_file(dir.join("0.lisp")).unwrap(), "42");
}

#[test]
fn test_macro_parity_across_both_compilers() {
    compare_example(
        "examples/macro-test.lisp",
        &[
            ("double", &[21], 42),
            ("add-five", &[37], 42),
            ("factorial", &[6], 720),
            ("test-when", &[0], 0),
            ("test-when", &[7], 49),
        ],
        true,
    );
    compare_example(
        "tests/fixtures/interpreter_macros.lisp",
        &[
            ("answer", &[], 42),
            ("sum", &[], 42),
            ("lazy", &[], 42),
            ("duplicate", &[], 3),
            ("calls", &[], 2),
        ],
        true,
    );
}

#[test]
fn test_macro_persistence_redefinition_and_quoted_data() {
    let mut s = session();
    for (source, expected) in [
        ("(defmacro inc (x) `(i32.add ,x 1))", "()"),
        ("(inc 41)", "42"),
        ("(define saved (lambda (x) (inc x)))", "#<closure>"),
        ("(fn typed ((x s32)) s32 (inc x))", "#<function>"),
        ("(defmacro inc (x) `(i32.add ,x 2)) (inc 40)", "42"),
        ("(saved 41)", "42"),
        ("(typed 41)", "42"),
        ("'(inc 41)", "(inc 41)"),
        ("`((inc 41) ,(inc 40))", "((inc 41) 42)"),
        (
            "(defmacro sumargs (xs) `(i32.add ,@xs)) (sumargs (15 27))",
            "42",
        ),
        ("(defmacro identity (x) ,x) (identity 42)", "42"),
        ("(defmacro literal (x) 42) (literal missing)", "42"),
        // Classic defmacro uses name-based substitution, like the self-hosted compiler.
        (
            "(defmacro capture (body) `(let (x 9) ,body)) (let (x 42) (capture x))",
            "9",
        ),
    ] {
        assert_eq!(s.evaluate(source).unwrap(), expected, "{source}");
    }
    assert!(
        session()
            .evaluate("(inc 1)")
            .unwrap()
            .contains("unbound symbol")
    );
}

#[test]
fn test_quasiquote_nesting_splicing_and_side_effects() {
    let mut s = session();
    for (source, expected) in [
        ("(define x 42) `(a ,x ,@(list 1 2) z)", "(a 42 1 2 z)"),
        ("`(a ,@'() z)", "(a z)"),
        ("`,x", "42"),
        (
            "`(a `(b ,x ,,x))",
            "(a (quasiquote (b (unquote x) (unquote 42))))",
        ),
        (
            "'(a `b ,c ,@d)",
            "(a (quasiquote b) (unquote c) (unquote-splice d))",
        ),
        ("`(1, x)", "(1 42)"),
        (
            "(global $n s32 mut 0) `(,(global.set $n 1) ,(global.set $n 2))",
            "(1 2)",
        ),
        ("(global.get $n)", "2"),
        ("(defmacro data (x) `(quote (,x))) (data 42)", "(42)"),
        (
            "(defmacro delayed (x) `(quasiquote (,x ,,x))) (delayed 42)",
            "(42 42)",
        ),
    ] {
        assert_eq!(s.evaluate(source).unwrap(), expected, "{source}");
    }
    for source in [
        "`",
        ",",
        ",@",
        ",x",
        ",@x",
        "`(,@42)",
        "`,@'(1 2)",
        "(quasiquote)",
        "(quasiquote 1 2)",
        "`((unquote))",
        "`((unquote-splice 1 2))",
    ] {
        let out = s.evaluate(source).unwrap();
        assert!(out.starts_with("error:"), "{source}: {out}");
    }
    assert_eq!(s.evaluate("x").unwrap(), "42");
}

#[test]
fn test_macro_expansion_failures_preserve_session() {
    let mut s = session();
    assert_eq!(
        s.evaluate("(define marker 42) (defmacro stable (x) ,x)")
            .unwrap(),
        "42"
    );
    for source in [
        "(defmacro)",
        "(defmacro bad)",
        "(defmacro bad ())",
        "(defmacro bad () 1 2)",
        "(defmacro 1 () 42)",
        "(defmacro bad x 42)",
        "(defmacro bad (1) 42)",
        "(defmacro bad (x x) 42)",
        "(defmacro quote (x) ,x)",
        "(defmacro bad (x) ,x) (bad)",
        "(defmacro bad () 42) (bad 1)",
        "(defmacro bad (x) `(i32.add ,@x)) (bad 42)",
        "(defmacro bad () (quasiquote)) (bad)",
        "(defmacro bad () (unquote)) (bad)",
        "(defmacro bad () `(bad)) (bad)",
        "(defmacro stable (x) 99) (defmacro bad () `(bad)) (bad)",
        "(begin (defmacro bad () 42))",
    ] {
        let out = s.evaluate(&format!("(define marker 0) {source}")).unwrap();
        assert!(out.starts_with("error:"), "{source}: {out}");
        assert_eq!(s.evaluate("marker").unwrap(), "42", "{source}");
        assert_eq!(s.evaluate("(stable 42)").unwrap(), "42");
        assert!(s.evaluate("(bad)").unwrap().contains("unbound symbol: bad"));
    }
    // Expansion succeeds before evaluation starts; a later runtime error does not
    // roll back either the macro publication or earlier ordinary definitions.
    assert!(
        s.evaluate("(defmacro good () 42) (define marker 7) missing")
            .unwrap()
            .starts_with("error:")
    );
    assert_eq!(s.evaluate("(good)").unwrap(), "42");
    assert_eq!(s.evaluate("marker").unwrap(), "7");
    // Broad expansion is bounded as well as recursive expansion depth.
    assert_eq!(
        s.evaluate("(defmacro duplicate (x) `(begin ,x ,x))")
            .unwrap(),
        "()"
    );
    let mut broad = "1".to_string();
    for _ in 0..14 {
        broad = format!("(duplicate {broad})");
    }
    let out = s.evaluate(&broad).unwrap();
    assert!(out.contains("macro expansion step limit"), "{out}");
    assert_eq!(s.evaluate("(good)").unwrap(), "42");
}

#[test]
fn test_macros_collected_across_includes() {
    let mut s = session();
    let dir = root().join(format!(
        "target/interpreter-parity/{}/macro-includes",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("main.lisp"),
        "(fn answer () s32 (twice 21)) (include \"macros.lisp\") (answer)",
    )
    .unwrap();
    std::fs::write(
        dir.join("macros.lisp"),
        "(defmacro twice (x) `(i32.add ,x ,x))",
    )
    .unwrap();
    assert_eq!(s.load_file(dir.join("main.lisp")).unwrap(), "42");
    assert_eq!(s.evaluate("(twice 21)").unwrap(), "42");
}
