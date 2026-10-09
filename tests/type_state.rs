// Type-state: a linear value whose structural type narrows on `match`. A variant
// can carry a linear payload; matching extracts it as a linear binding that must be
// consumed exactly once. A variant holding a linear payload is itself linear
// (infectious), so it can't be copied and re-matched to extract the resource twice.

use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_ts_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_ts_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base, compiler::EmitOptions::default())
        .expect("failed to compile");

    let wasm_bytes = std::fs::read(out_base.with_extension("wasm")).expect("failed to read wasm");
    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("engine");
    let module = Module::new(&engine, &wasm_bytes).expect("module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("instantiate");
    let func = instance
        .get_func(&mut store, "test-func")
        .expect("test-func");
    let memory = instance.get_memory(&mut store, "memory").expect("memory");

    let out_ptr_ptr: i32 = 0x2000;
    let out_len_ptr: i32 = 0x2004;
    let mut results = [wasmtime::Val::I32(0)];
    func.call(
        &mut store,
        &[
            wasmtime::Val::I32(0x1000),
            wasmtime::Val::I32(0),
            wasmtime::Val::I32(out_ptr_ptr),
            wasmtime::Val::I32(out_len_ptr),
        ],
        &mut results,
    )
    .expect("call failed");

    let mut ptr_buf = [0u8; 4];
    memory
        .read(&store, out_ptr_ptr as usize, &mut ptr_buf)
        .expect("read out ptr");
    let out_ptr = i32::from_le_bytes(ptr_buf);
    let mut buf = [0u8; 4];
    memory
        .read(&store, (out_ptr + 24) as usize, &mut buf)
        .expect("read result");
    i32::from_le_bytes(buf)
}

fn compile_error(source: &str) -> String {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_ts_err_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_ts_err_{}", test_id));
    std::fs::write(&source_path, source).expect("failed to write temp source");
    match compiler::compile(&source_path, &out_base, compiler::EmitOptions::default()) {
        Ok(_) => panic!("expected compilation to fail, but it succeeded"),
        Err(e) => format!("{:#}", e),
    }
}

const BOX: &str = "(variant box (full (lin s32)) (empty))";

// Matching extracts the linear payload, which the arm consumes exactly once.
#[test]
fn test_linear_payload_extracted_and_used() {
    let src = format!(
        "{BOX}
(fn take ((b box)) s32
  (match b
    ((full r) r)
    ((empty) (i32.const 0))))
(export (fn test-func () s32 (take (full (i32.const 5)))))"
    );
    assert_eq!(compile_and_run(&src), 5);
}

// The empty case has no linear binding; the box is still consumed by the match.
#[test]
fn test_linear_variant_empty_case() {
    let src = format!(
        "{BOX}
(fn take ((b box)) s32
  (match b
    ((full r) r)
    ((empty) (i32.const 0))))
(export (fn test-func () s32 (take (empty))))"
    );
    assert_eq!(compile_and_run(&src), 0);
}

// Dropping the extracted linear payload is rejected.
#[test]
fn test_linear_payload_dropped_rejected() {
    let err = compile_error(&format!(
        "{BOX}
(fn take ((b box)) s32
  (match b
    ((full r) (i32.const 0))
    ((empty) (i32.const 0))))
(export (fn test-func () s32 (take (full (i32.const 5)))))"
    ));
    assert!(
        err.contains("linear and must be used exactly once"),
        "unexpected error: {err}"
    );
}

// Using the extracted linear payload twice is rejected.
#[test]
fn test_linear_payload_duplicated_rejected() {
    let err = compile_error(&format!(
        "{BOX}
(fn take ((b box)) s32
  (match b
    ((full r) (i32.add r r))
    ((empty) (i32.const 0))))
(export (fn test-func () s32 (take (full (i32.const 5)))))"
    ));
    assert!(
        err.contains("linear and must be used exactly once"),
        "unexpected error: {err}"
    );
}

// Infectious linearity: a variant holding a linear payload is itself linear, so it
// cannot be matched twice (which would extract the resource twice).
#[test]
fn test_linear_variant_cannot_be_rematched() {
    let err = compile_error(&format!(
        "{BOX}
(fn twice ((b box)) s32
  (i32.add
    (match b ((full r) r) ((empty) (i32.const 0)))
    (match b ((full r) r) ((empty) (i32.const 0)))))
(export (fn test-func () s32 (twice (full (i32.const 5)))))"
    ));
    assert!(err.contains("used 2 times"), "unexpected error: {err}");
}

// A linear value can be held in a `let` and consumed exactly once. The binding is
// infectiously linear because its value constructs a linear variant.
#[test]
fn test_linear_let_bind_and_consume() {
    let src = format!(
        "{BOX}
(export (fn test-func () s32
  (let (b (full (i32.const 7)))
    (match b ((full r) r) ((empty) (i32.const 0))))))"
    );
    assert_eq!(compile_and_run(&src), 7);
}

// Dropping a linear `let` binding is rejected.
#[test]
fn test_linear_let_dropped_rejected() {
    let err = compile_error(&format!(
        "{BOX}
(export (fn test-func () s32
  (let (b (full (i32.const 7))) (i32.const 0))))"
    ));
    assert!(
        err.contains("linear binding") && err.contains("exactly once"),
        "unexpected error: {err}"
    );
}

// Using a linear `let` binding twice is rejected.
#[test]
fn test_linear_let_duplicated_rejected() {
    let err = compile_error(&format!(
        "{BOX}
(export (fn test-func () s32
  (let (b (full (i32.const 7)))
    (i32.add
      (match b ((full r) r) ((empty) (i32.const 0)))
      (match b ((full r) r) ((empty) (i32.const 0)))))))"
    ));
    assert!(
        err.contains("linear binding") && err.contains("exactly once"),
        "unexpected error: {err}"
    );
}

// A linear value returned from a function is infectiously linear when let-bound.
#[test]
fn test_linear_let_from_function_result() {
    let src = format!(
        "{BOX}
(fn mk () box (full (i32.const 4)))
(export (fn test-func () s32
  (let (b (mk))
    (match b ((full r) r) ((empty) (i32.const 0))))))"
    );
    assert_eq!(compile_and_run(&src), 4);
}

// An explicit (lin T) annotation marks a let binding linear even for a scalar.
#[test]
fn test_linear_let_explicit_annotation() {
    let src = "(export (fn test-func () s32
  (let (x : (lin s32) (i32.const 5)) x)))";
    assert_eq!(compile_and_run(src), 5);
}

#[test]
fn test_linear_let_explicit_annotation_dropped_rejected() {
    let err = compile_error(
        "(export (fn test-func () s32
  (let (x : (lin s32) (i32.const 5)) (i32.const 0))))",
    );
    assert!(
        err.contains("linear binding") && err.contains("exactly once"),
        "unexpected error: {err}"
    );
}

// An affine payload may be dropped.
#[test]
fn test_affine_payload_may_drop() {
    let src = r#"
(variant slot (present (aff s32)) (absent))
(fn get ((o slot)) s32
  (match o
    ((present r) (i32.const 0))
    ((absent) (i32.const 0))))
(export (fn test-func () s32 (get (present (i32.const 9)))))
"#;
    assert_eq!(compile_and_run(src), 0);
}
