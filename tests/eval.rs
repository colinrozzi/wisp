// The tree-walking back-end (compiler::eval_source) shares the compiler's entire
// front/middle — parse, macro-expand, lower/monomorphize, and type-check — and only
// the back-end differs (eval vs codegen). So interpreted code gets the same types,
// generics, and substructural checks as compiled code. Each program evaluates a
// nullary `test-func`.

use wisp::compiler::{Value, eval_source};

fn ok(src: &str) -> Value {
    eval_source(src).expect("expected successful evaluation")
}

fn err(src: &str) -> String {
    match eval_source(src) {
        Ok(v) => panic!("expected an error, got {v:?}"),
        Err(e) => format!("{e:#}"),
    }
}

#[test]
fn test_eval_arithmetic() {
    assert_eq!(
        ok("(export (fn test-func () s32 (i32.add (i32.const 40) (i32.const 2))))"),
        Value::Int(42)
    );
}

#[test]
fn test_eval_recursive_function() {
    let src = r#"
(fn fact ((n s32)) s32
  (if (i32.le_s n (i32.const 1)) (i32.const 1)
      (i32.mul n (fact (i32.sub n (i32.const 1))))))
(export (fn test-func () s32 (fact (i32.const 5))))
"#;
    assert_eq!(ok(src), Value::Int(120));
}

#[test]
fn test_eval_let_and_if() {
    let src = r#"
(export (fn test-func () s32
  (let (x (i32.const 10))
    (if (i32.gt_s x (i32.const 5)) (i32.mul x (i32.const 2)) x))))
"#;
    assert_eq!(ok(src), Value::Int(20));
}

#[test]
fn test_eval_generic_function_monomorphized() {
    // A generic function, monomorphized by the shared lowering stage, then evaluated.
    let src = r#"
(fn id ((x T)) T (where T) x)
(export (fn test-func () s32 (id (i32.const 7))))
"#;
    assert_eq!(ok(src), Value::Int(7));
}

#[test]
fn test_eval_variant_and_match() {
    let src = r#"
(variant shape (circle s32) (square s32))
(fn area ((s shape)) s32
  (match s
    ((circle r) (i32.mul r r))
    ((square w) (i32.mul w w))))
(export (fn test-func () s32 (area (square (i32.const 4)))))
"#;
    assert_eq!(ok(src), Value::Int(16));
}

#[test]
fn test_eval_record_field_access() {
    let src = r#"
(record point (x s32) (y s32))
(export (fn test-func () s32
  (let (p (point (i32.const 3) (i32.const 4)))
    (i32.add (point.x p) (point.y p)))))
"#;
    assert_eq!(ok(src), Value::Int(7));
}

#[test]
fn test_eval_generic_variant() {
    // Generic ADT: monomorphized to box<s32>, constructed and matched under eval.
    let src = r#"
(variant (box T) (wrap T))
(fn unwrap ((b (box s32))) s32 (match b ((wrap x) x)))
(export (fn test-func () s32 (unwrap (wrap (i32.const 9)))))
"#;
    assert_eq!(ok(src), Value::Int(9));
}

#[test]
fn test_eval_type_state_program_runs() {
    // A type-state program (linear payload consumed once) type-checks and then runs.
    let src = r#"
(variant cell (full (lin s32)) (empty))
(fn take ((c cell)) s32 (match c ((full r) r) ((empty) (i32.const 0))))
(export (fn test-func () s32 (take (full (i32.const 5)))))
"#;
    assert_eq!(ok(src), Value::Int(5));
}

#[test]
fn test_eval_strings() {
    let src = r#"
(export (fn test-func () s32
  (string=? (string-append "ab" "c") "abc")))
"#;
    assert_eq!(ok(src), Value::Int(1));
}

// --- Coherence: the shared middle means the type system governs eval'd code too ---

#[test]
fn test_eval_catches_type_error() {
    // Body type (f64) != declared return (s32): rejected by the shared type checker,
    // before eval ever runs.
    let e = err("(export (fn test-func () s32 (f64.const 1.0)))");
    assert!(
        e.contains("returns") || e.contains("type"),
        "unexpected: {e}"
    );
}

#[test]
fn test_eval_catches_linearity_error() {
    // A linear parameter used twice: rejected by the shared substructural checker.
    let e = err("(fn dup ((x (lin s32))) s32 (i32.add x x))
(export (fn test-func () s32 (dup (i32.const 3))))");
    assert!(
        e.contains("exactly once") || e.contains("linear"),
        "unexpected: {e}"
    );
}
