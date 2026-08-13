// Tests for the generic list standard library (std/list.lisp): length, sum,
// contains, reverse — monomorphized per element type. Each program exports
// `test-func` returning s32.

use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_list_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_list_{}", test_id));

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

/// Include the generic list stdlib by absolute path, with a `build` helper that
/// makes the s32 list [10, 20, 30].
fn with_list(body: &str) -> String {
    format!(
        "(include \"{}/std/list.lisp\")
(fn build () : (list s32)
  (list-push (list-push (list-push (list-new s32)
    (i32.const 10)) (i32.const 20)) (i32.const 30)))
{}",
        env!("CARGO_MANIFEST_DIR"),
        body
    )
}

#[test]
fn test_list_length() {
    assert_eq!(
        compile_and_run(&with_list(
            "(export (fn test-func () s32 (length (build))))"
        )),
        3
    );
}

#[test]
fn test_list_length_empty() {
    assert_eq!(
        compile_and_run(&with_list(
            "(export (fn test-func () s32 (length (list-new s32))))"
        )),
        0
    );
}

#[test]
fn test_list_sum() {
    assert_eq!(
        compile_and_run(&with_list("(export (fn test-func () s32 (sum (build))))")),
        60
    );
}

#[test]
fn test_list_sum_f64() {
    // The same generic sum monomorphizes at f64.
    let src = format!(
        "(include \"{}/std/list.lisp\")
(export (fn test-func () s32
  (let (xs (list-push (list-push (list-new f64) (f64.const 1.5)) (f64.const 2.5)))
    (if (f64.eq (sum xs) (f64.const 4.0)) (i32.const 1) (i32.const 0)))))",
        env!("CARGO_MANIFEST_DIR")
    );
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_list_contains_present() {
    assert_eq!(
        compile_and_run(&with_list(
            "(export (fn test-func () s32 (contains (build) (i32.const 20))))"
        )),
        1
    );
}

#[test]
fn test_list_contains_absent() {
    assert_eq!(
        compile_and_run(&with_list(
            "(export (fn test-func () s32 (contains (build) (i32.const 99))))"
        )),
        0
    );
}

#[test]
fn test_list_reverse_first_element() {
    // reverse([10,20,30]) = [30,20,10]; its first element is 30.
    assert_eq!(
        compile_and_run(&with_list(
            "(export (fn test-func () s32 (list-get (reverse (build)) (i32.const 0))))"
        )),
        30
    );
}

#[test]
fn test_list_reverse_last_element() {
    // reverse([10,20,30]) = [30,20,10]; its last element is 10.
    assert_eq!(
        compile_and_run(&with_list(
            "(export (fn test-func () s32 (list-get (reverse (build)) (i32.const 2))))"
        )),
        10
    );
}

#[test]
fn test_list_fold() {
    // fold (+) 0 [10,20,30] = 60 (this is how sum is defined).
    assert_eq!(
        compile_and_run(&with_list(
            "(export (fn test-func () s32 (fold + (zero) (build))))"
        )),
        60
    );
}

#[test]
fn test_list_map() {
    // map double [10,20,30] = [20,40,60]; first element is 20.
    let src = with_list(
        "(fn dbl ((n : s32)) : s32 (i32.add n n))
(export (fn test-func () s32 (list-get (map dbl (build)) (i32.const 0))))",
    );
    assert_eq!(compile_and_run(&src), 20);
}

#[test]
fn test_list_filter() {
    // filter (> 15) [10,20,30] = [20,30]; its length is 2.
    let src = with_list(
        "(fn is-big ((n : s32)) : s32 (i32.gt_s n (i32.const 15)))
(export (fn test-func () s32 (length (filter is-big (build)))))",
    );
    assert_eq!(compile_and_run(&src), 2);
}

#[test]
fn test_list_filter_first_kept() {
    // filter (> 15) [10,20,30] = [20,30]; first kept element is 20.
    let src = with_list(
        "(fn is-big ((n : s32)) : s32 (i32.gt_s n (i32.const 15)))
(export (fn test-func () s32 (list-get (filter is-big (build)) (i32.const 0))))",
    );
    assert_eq!(compile_and_run(&src), 20);
}
