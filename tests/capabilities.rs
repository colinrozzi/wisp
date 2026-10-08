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

// RAII with-cap: the scope owns the capability; the body borrows it (any number
// of times) for gated operations. `peek` takes a (borrow Store); two borrows sum
// to 14. The cap is released by the scope, at zero runtime cost.
#[test]
fn test_capability_borrow_and_use() {
    let source = r#"
(capability Store)
(fn peek ((c (borrow Store))) s32 (i32.const 7))
(export (fn test-func () s32
  (with-cap (c Store)
    (i32.add (peek (& c)) (peek (& c))))))
"#;
    assert_eq!(compile_and_run(source), 14);
}

// A with-cap body that never touches the capability is fine — the scope releases
// it regardless (RAII).
#[test]
fn test_capability_unused_ok() {
    let source = r#"
(capability Store)
(export (fn test-func () s32
  (with-cap (c Store) (i32.const 5))))
"#;
    assert_eq!(compile_and_run(source), 5);
}

// A borrow may be taken on every branch of a conditional; it never consumes.
#[test]
fn test_capability_borrow_in_branches() {
    let source = r#"
(capability Store)
(fn peek ((c (borrow Store))) s32 (i32.const 9))
(export (fn test-func () s32
  (with-cap (c Store)
    (if (i32.const 1) (peek (& c)) (peek (& c))))))
"#;
    assert_eq!(compile_and_run(source), 9);
}

// Consuming a with-cap capability by value (here via release-cap) is rejected:
// the scope releases it, so the body may only borrow.
#[test]
fn test_capability_consume_in_body_rejected() {
    let err = compile_error(
        "(capability Store)
(export (fn test-func () s32
  (with-cap (c Store) (release-cap c))))",
    );
    assert!(
        err.contains("may only be borrowed"),
        "unexpected error: {err}"
    );
}

// Moving the capability out of the scope (returning it by value) is rejected.
#[test]
fn test_capability_move_out_rejected() {
    let err = compile_error(
        "(capability Store)
(fn keep ((c Store)) Store c)
(export (fn test-func () s32
  (with-cap (c Store) (begin (keep c) (i32.const 0)))))",
    );
    assert!(
        err.contains("may only be borrowed"),
        "unexpected error: {err}"
    );
}

// A borrow cannot escape its with-cap scope (be the scope's result).
#[test]
fn test_capability_borrow_escape_rejected() {
    let err = compile_error(
        "(capability Store)
(export (fn test-func () s32
  (with-cap (c Store) (& c))))",
    );
    assert!(
        err.contains("borrow cannot escape"),
        "unexpected error: {err}"
    );
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
  (with-cap (c Bogus) (i32.const 0))))",
    );
    assert!(
        err.contains("unknown capability"),
        "unexpected error: {err}"
    );
}

// The move-model primitives remain for owned (transfer/close) capabilities: a
// function taking a capability by value and releasing it once type-checks.
#[test]
fn test_owned_capability_close_compiles() {
    let source = r#"
(capability Store)
(fn close ((c Store)) s32 (release-cap c))
(export (fn test-func () s32 (i32.const 0)))
"#;
    assert_eq!(compile_and_run(source), 0);
}
