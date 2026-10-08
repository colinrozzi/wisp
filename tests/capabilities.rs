use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_cap_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_cap_{}", test_id));

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
    let source_path = temp_dir.join(format!("test_cap_err_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_cap_err_{}", test_id));
    std::fs::write(&source_path, source).expect("failed to write temp source");
    match compiler::compile(&source_path, &out_base, compiler::EmitOptions::default()) {
        Ok(_) => panic!("expected compilation to fail, but it succeeded"),
        Err(e) => format!("{:#}", e),
    }
}

// A capability minted by with-cap, threaded through a function, and released
// exactly once compiles and runs. The cap is zero-cost; test-func returns 42.
#[test]
fn test_capability_thread_and_release() {
    let source = r#"
(capability Store)
(fn tag ((c Store) (x s32)) Store c)
(export (fn test-func () s32
  (with-cap (c Store)
    (begin
      (release-cap (tag c 42))
      (i32.const 42)))))
"#;
    assert_eq!(compile_and_run(source), 42);
}

// A capability parameter is linear by its type: a function may thread it (use it
// exactly once) with no `(lin ...)` annotation.
#[test]
fn test_capability_param_is_linear() {
    let source = r#"
(capability Store)
(export (fn test-func () s32
  (with-cap (c Store) (release-cap c))))
"#;
    assert_eq!(compile_and_run(source), 0);
}

// Dropping a capability (never consuming it) is rejected.
#[test]
fn test_capability_drop_rejected() {
    let err = compile_error(
        "(capability Store)
(export (fn test-func () s32
  (with-cap (c Store) (i32.const 0))))",
    );
    assert!(err.contains("never consumed"), "unexpected error: {err}");
}

// Consuming a capability twice is rejected.
#[test]
fn test_capability_double_consume_rejected() {
    let err = compile_error(
        "(capability Store)
(export (fn test-func () s32
  (with-cap (c Store) (i32.add (release-cap c) (release-cap c)))))",
    );
    assert!(err.contains("consumed 2 times"), "unexpected error: {err}");
}

// A capability is unforgeable: it has no public constructor.
#[test]
fn test_capability_unforgeable() {
    let err = compile_error(
        "(capability Store)
(fn forge () Store (Store))
(export (fn test-func () s32 (release-cap (forge))))",
    );
    assert!(
        err.contains("unknown function") || err.contains("Store"),
        "unexpected error: {err}"
    );
}

// with-cap on an undeclared capability is rejected with a helpful message.
#[test]
fn test_with_cap_unknown_capability_rejected() {
    let err = compile_error(
        "(export (fn test-func () s32
  (with-cap (c Bogus) (release-cap c))))",
    );
    assert!(
        err.contains("unknown capability"),
        "unexpected error: {err}"
    );
}

// A capability threaded through a conditional must be consumed on every path.
#[test]
fn test_capability_consumed_on_both_branches() {
    let source = r#"
(capability Store)
(export (fn test-func () s32
  (with-cap (c Store)
    (if (i32.const 1) (release-cap c) (release-cap c)))))
"#;
    assert_eq!(compile_and_run(source), 0);
}
