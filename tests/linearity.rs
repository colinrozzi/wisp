use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_linearity_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_linearity_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base, compiler::EmitOptions::default())
        .expect("failed to compile");

    let wasm_path = out_base.with_extension("wasm");
    let wasm_bytes = std::fs::read(&wasm_path).expect("failed to read wasm");

    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("failed to create engine");
    let module = Module::new(&engine, &wasm_bytes).expect("failed to create module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("failed to instantiate");

    let func = instance
        .get_func(&mut store, "test-func")
        .expect("function 'test-func' not found");
    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("memory not found");

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
        .expect("failed to read output pointer");
    let out_ptr = i32::from_le_bytes(ptr_buf);
    let mut buf = [0u8; 4];
    memory
        .read(&store, (out_ptr + 24) as usize, &mut buf)
        .expect("failed to read result");
    i32::from_le_bytes(buf)
}

fn compile_error(source: &str) -> String {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_linearity_err_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_linearity_err_{}", test_id));
    std::fs::write(&source_path, source).expect("failed to write temp source");
    match compiler::compile(&source_path, &out_base, compiler::EmitOptions::default()) {
        Ok(_) => panic!("expected compilation to fail, but it succeeded"),
        Err(e) => format!("{:#}", e),
    }
}

// A linear parameter used exactly once compiles and runs like any other.
#[test]
fn test_linear_used_once_ok() {
    let source = r#"
(fn consume ((x (lin s32))) s32 (i32.add x (i32.const 1)))
(export (fn test-func () s32 (consume (i32.const 41))))
"#;
    assert_eq!(compile_and_run(source), 42);
}

// `(lin T)` has the same runtime representation as `T`; it composes with other
// types and the value flows normally.
#[test]
fn test_linear_threaded_through_call() {
    let source = r#"
(fn tag ((x (lin s32))) s32 x)
(fn use-it ((y (lin s32))) s32 (tag y))
(export (fn test-func () s32 (use-it (i32.const 7))))
"#;
    assert_eq!(compile_and_run(source), 7);
}

// A linear value consumed once on every branch of an `if` is fine.
#[test]
fn test_linear_consistent_if_ok() {
    let source = r#"
(fn pick ((x (lin s32)) (b s32)) s32
  (if b x x))
(export (fn test-func () s32 (pick (i32.const 5) (i32.const 1))))
"#;
    assert_eq!(compile_and_run(source), 5);
}

// Using a linear parameter twice is rejected.
#[test]
fn test_linear_double_use_rejected() {
    let err = compile_error(
        "(fn dup ((x (lin s32))) s32 (i32.add x x))
(export (fn test-func () s32 (dup (i32.const 3))))",
    );
    assert!(
        err.contains("used 2 times but must be used exactly once"),
        "unexpected error: {err}"
    );
}

// Dropping a linear parameter (never using it) is rejected.
#[test]
fn test_linear_unused_rejected() {
    let err = compile_error(
        "(fn drop ((x (lin s32))) s32 (i32.const 0))
(export (fn test-func () s32 (drop (i32.const 3))))",
    );
    assert!(err.contains("never used"), "unexpected error: {err}");
}

// Using a linear value in one branch of an `if` but not the other is rejected:
// the value would be consumed on one path and leaked on the other.
#[test]
fn test_linear_inconsistent_branches_rejected() {
    let err = compile_error(
        "(fn maybe ((x (lin s32)) (b s32)) s32
  (if b x (i32.const 0)))
(export (fn test-func () s32 (maybe (i32.const 5) (i32.const 1))))",
    );
    assert!(
        err.contains("every path") || err.contains("one branch"),
        "unexpected error: {err}"
    );
}

// An affine parameter used exactly once is fine.
#[test]
fn test_affine_used_once_ok() {
    let source = r#"
(fn consume ((x (aff s32))) s32 (i32.add x (i32.const 1)))
(export (fn test-func () s32 (consume (i32.const 41))))
"#;
    assert_eq!(compile_and_run(source), 42);
}

// An affine parameter may be dropped (used zero times) — unlike linear.
#[test]
fn test_affine_drop_ok() {
    let source = r#"
(fn ignore ((x (aff s32))) s32 (i32.const 7))
(export (fn test-func () s32 (ignore (i32.const 99))))
"#;
    assert_eq!(compile_and_run(source), 7);
}

// An affine value used on one branch and dropped on the other is fine (the
// per-path maximum is 1); the same shape is rejected for a linear value.
#[test]
fn test_affine_dropped_on_one_branch_ok() {
    let source = r#"
(fn maybe ((x (aff s32)) (b s32)) s32
  (if b x (i32.const 0)))
(export (fn test-func () s32 (maybe (i32.const 5) (i32.const 1))))
"#;
    assert_eq!(compile_and_run(source), 5);
}

// Using an affine parameter twice is still rejected (at most once).
#[test]
fn test_affine_double_use_rejected() {
    let err = compile_error(
        "(fn dup ((x (aff s32))) s32 (i32.add x x))
(export (fn test-func () s32 (dup (i32.const 3))))",
    );
    assert!(err.contains("at most once"), "unexpected error: {err}");
}
