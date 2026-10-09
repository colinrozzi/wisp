// The tree-walking back-end (compiler::eval_source) shares the compiler's entire
// front/middle — parse, macro-expand, lower/monomorphize, and type-check — and only
// the back-end differs (eval vs codegen). So interpreted code gets the same types,
// generics, and substructural checks as compiled code. Each program evaluates a
// nullary `test-func`.

use std::collections::HashMap;
use std::path::Path;
use wisp::compiler::{
    Host, Import, InlineValue, Type, Value, eval_repl_expr, eval_repl_expr_with_host, eval_source,
    eval_source_entry_with_host, eval_source_with_host,
};

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

// --- Host effects: a capability-gated import call, dispatched to a Host ---

/// A test host that captures `print` calls.
struct CaptureHost {
    out: Vec<String>,
}
impl Host for CaptureHost {
    fn call(&mut self, _module: &str, name: &str, args: &[Value]) -> anyhow::Result<Value> {
        if name == "print" || name == "write-line" {
            if let Some(Value::Str(s)) = args.first() {
                self.out.push(s.clone());
            }
            return Ok(Value::Int(0));
        }
        anyhow::bail!("unknown host function {name}")
    }
}

// A capability-gated host effect runs under eval: `log` requires a borrowed
// `Console`, calls the imported `print`, which the host captures. This is the
// whole point of the capability work made observable — and the mechanism the REPL
// daemon will reach the live Theater runtime through.
#[test]
fn test_eval_capability_gated_host_effect() {
    let src = r#"
(capability Console)
(import host print ((msg string)) s32)
(fn log ((c (borrow Console)) (msg string)) s32 (print msg))
(export (fn test-func () s32
  (with-cap (c Console) (log (& c) "hello"))))
"#;
    let mut host = CaptureHost { out: vec![] };
    let v = eval_source_with_host(src, &mut host).expect("eval");
    assert_eq!(v, Value::Int(0));
    assert_eq!(host.out, vec!["hello".to_string()]);
}

// `wisp eval <file>` runs a named nullary entry (default `main`), not just
// `test-func`. Same capability-gated effect, reached through a chosen entry.
#[test]
fn test_eval_entry_runs_named_function() {
    let src = r#"
(capability Console)
(import host write-line ((msg string)) s32)
(fn log ((c (borrow Console)) (msg string)) s32 (write-line msg))
(export (fn main () s32
  (with-cap (c Console) (log (& c) "hi"))))
"#;
    let mut host = CaptureHost { out: vec![] };
    let v = eval_source_entry_with_host(src, Path::new("."), "main", &mut host).expect("eval");
    assert_eq!(v, Value::Int(0));
    assert_eq!(host.out, vec!["hi".to_string()]);
}

// Without a host, an import call errors (rather than silently doing nothing).
#[test]
fn test_eval_import_without_host_errors() {
    let src = r#"
(import host print ((msg string)) s32)
(export (fn test-func () s32 (print "x")))
"#;
    assert!(eval_source(src).is_err());
}

// --- REPL evaluation primitive: typed, binding-aware eval (eval_repl_expr) ---

#[test]
fn test_eval_repl_inlines_binding() {
    // A REPL session binding `x = 41` is inlined; the expression evaluates to 42,
    // having gone through the shared parse + type-check.
    let mut bindings = HashMap::new();
    bindings.insert("x".to_string(), InlineValue::S32(41));
    let (v, _ty) = eval_repl_expr("(i32.add x (i32.const 1))", &bindings, &[]).expect("eval");
    assert_eq!(v, Value::Int(42));
}

#[test]
fn test_eval_repl_typechecks_bindings() {
    // A string binding used where an s32 is required is a type error, caught by the
    // shared checker before eval — the REPL speaks the typed language.
    let mut bindings = HashMap::new();
    bindings.insert("x".to_string(), InlineValue::Str("hi".to_string()));
    assert!(eval_repl_expr("(i32.add x (i32.const 1))", &bindings, &[]).is_err());
}

// A REPL session expression can call an imported function, which dispatches to the
// session's host — the seam the Theater REPL reaches the live runtime through. The
// import signature joins the type-check; the inferred type comes back alongside.
#[test]
fn test_eval_repl_expr_with_host_dispatches_import() {
    struct FixedHost;
    impl Host for FixedHost {
        fn call(&mut self, _module: &str, name: &str, _args: &[Value]) -> anyhow::Result<Value> {
            assert_eq!(name, "answer");
            Ok(Value::Int(42))
        }
    }
    let import = Import {
        module: "host".to_string(),
        name: "answer".to_string(),
        params: vec![],
        return_type: Type::S32,
    };
    let (v, ty) = eval_repl_expr_with_host(
        "(answer)",
        &HashMap::new(),
        &[],
        std::slice::from_ref(&import),
        &mut FixedHost,
    )
    .expect("eval");
    assert_eq!(v, Value::Int(42));
    assert_eq!(ty, Type::S32);
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
