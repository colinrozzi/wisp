// Tests for compile-time deriving: `(derive Eq Type)` reflects on a record's fields
// and generates the trait instance. Each program exports `test-func` returning s32.

use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_derive_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_derive_{}", test_id));

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

    let in_ptr: i32 = 0x1000;
    let in_len: i32 = 0;
    let out_ptr_ptr: i32 = 0x2000;
    let out_len_ptr: i32 = 0x2004;

    let mut results = [wasmtime::Val::I32(0)];
    func.call(
        &mut store,
        &[
            wasmtime::Val::I32(in_ptr),
            wasmtime::Val::I32(in_len),
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
    let source_path = temp_dir.join(format!("test_derive_err_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_derive_err_{}", test_id));
    std::fs::write(&source_path, source).expect("failed to write temp source");
    match compiler::compile(&source_path, &out_base, compiler::EmitOptions::default()) {
        Ok(_) => panic!("expected compilation to fail, but it succeeded"),
        Err(e) => format!("{:#}", e),
    }
}

/// The Eq trait (deriving generates an instance of it).
const EQ_TRAIT: &str = "(trait (Eq T) (fn = ((a : T) (b : T)) : s32))";

#[test]
fn test_derive_eq_equal() {
    let src = format!(
        "{}
(record point (x s32) (y s32))
(derive Eq point)
(fn mk ((a : s32) (b : s32)) : point (point a b))
(export (fn test-func () s32
  (= (mk (i32.const 1) (i32.const 2)) (mk (i32.const 1) (i32.const 2)))))",
        EQ_TRAIT
    );
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_derive_eq_differ_first_field() {
    let src = format!(
        "{}
(record point (x s32) (y s32))
(derive Eq point)
(fn mk ((a : s32) (b : s32)) : point (point a b))
(export (fn test-func () s32
  (= (mk (i32.const 9) (i32.const 2)) (mk (i32.const 1) (i32.const 2)))))",
        EQ_TRAIT
    );
    assert_eq!(compile_and_run(&src), 0);
}

#[test]
fn test_derive_eq_differ_second_field() {
    let src = format!(
        "{}
(record point (x s32) (y s32))
(derive Eq point)
(fn mk ((a : s32) (b : s32)) : point (point a b))
(export (fn test-func () s32
  (= (mk (i32.const 1) (i32.const 9)) (mk (i32.const 1) (i32.const 2)))))",
        EQ_TRAIT
    );
    assert_eq!(compile_and_run(&src), 0);
}

#[test]
fn test_derive_eq_three_fields() {
    // A mixed-width record: s32, s64, f64 fields, each compared with the right op.
    let src = format!(
        "{}
(record rec (a s32) (b s64) (c f64))
(derive Eq rec)
(fn mk ((a : s32) (b : s64) (c : f64)) : rec (rec a b c))
(export (fn test-func () s32
  (= (mk (i32.const 1) (i64.const 2) (f64.const 3.0))
     (mk (i32.const 1) (i64.const 2) (f64.const 3.0)))))",
        EQ_TRAIT
    );
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_derive_eq_three_fields_differ() {
    let src = format!(
        "{}
(record rec (a s32) (b s64) (c f64))
(derive Eq rec)
(fn mk ((a : s32) (b : s64) (c : f64)) : rec (rec a b c))
(export (fn test-func () s32
  (= (mk (i32.const 1) (i64.const 2) (f64.const 3.0))
     (mk (i32.const 1) (i64.const 2) (f64.const 9.0)))))",
        EQ_TRAIT
    );
    assert_eq!(compile_and_run(&src), 0);
}

#[test]
fn test_derive_unknown_trait_is_rejected() {
    let src = format!(
        "{}
(record point (x s32) (y s32))
(derive Nope point)",
        EQ_TRAIT
    );
    let err = compile_error(&src);
    assert!(err.contains("cannot derive"), "got: {err}");
}

#[test]
fn test_derive_eq_on_non_record_is_rejected() {
    let src = format!(
        "{}
(derive Eq nonesuch)",
        EQ_TRAIT
    );
    let err = compile_error(&src);
    assert!(
        err.contains("not a record") || err.contains("cannot derive"),
        "got: {err}"
    );
}
