// Tests for the minimal numeric standard library and the (include ...) form.
// Programs pull in std/num.lisp and use its operators and constants. Each exports
// `test-func` returning s32.

use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_stdlib_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_stdlib_{}", test_id));

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

    // Pack CGRF ABI: (in_ptr, in_len, out_ptr_ptr, out_len_ptr).
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

/// Include the real std/num.lisp by absolute path (robust from the temp dir).
fn with_std(body: &str) -> String {
    format!(
        "(include \"{}/std/num.lisp\")\n{}",
        env!("CARGO_MANIFEST_DIR"),
        body
    )
}

// === Arithmetic operators from the stdlib ====================================

#[test]
fn test_stdlib_add() {
    assert_eq!(
        compile_and_run(&with_std(
            "(export (fn test-func () s32 (+ (i32.const 40) (i32.const 2))))"
        )),
        42
    );
}

#[test]
fn test_stdlib_sub() {
    assert_eq!(
        compile_and_run(&with_std(
            "(export (fn test-func () s32 (- (i32.const 44) (i32.const 2))))"
        )),
        42
    );
}

#[test]
fn test_stdlib_mul() {
    assert_eq!(
        compile_and_run(&with_std(
            "(export (fn test-func () s32 (* (i32.const 6) (i32.const 7))))"
        )),
        42
    );
}

#[test]
fn test_stdlib_div() {
    assert_eq!(
        compile_and_run(&with_std(
            "(export (fn test-func () s32 (/ (i32.const 84) (i32.const 2))))"
        )),
        42
    );
}

// === Comparisons return s32 booleans =========================================

#[test]
fn test_stdlib_less_than_true() {
    assert_eq!(
        compile_and_run(&with_std(
            "(export (fn test-func () s32 (< (i32.const 1) (i32.const 2))))"
        )),
        1
    );
}

#[test]
fn test_stdlib_less_than_false() {
    assert_eq!(
        compile_and_run(&with_std(
            "(export (fn test-func () s32 (< (i32.const 2) (i32.const 1))))"
        )),
        0
    );
}

#[test]
fn test_stdlib_equal() {
    assert_eq!(
        compile_and_run(&with_std(
            "(export (fn test-func () s32 (= (i32.const 42) (i32.const 42))))"
        )),
        1
    );
}

// === Typed constants =========================================================

#[test]
fn test_stdlib_zero() {
    assert_eq!(
        compile_and_run(&with_std("(export (fn test-func () s32 (zero)))")),
        0
    );
}

#[test]
fn test_stdlib_one() {
    assert_eq!(
        compile_and_run(&with_std("(export (fn test-func () s32 (one)))")),
        1
    );
}

// === f64 operators through the stdlib ========================================

#[test]
fn test_stdlib_f64_mul() {
    assert_eq!(
        compile_and_run(&with_std(
            "(export (fn test-func () s32
               (if (f64.eq (* (f64.const 6.0) (f64.const 7.0)) (f64.const 42.0))
                 (i32.const 1) (i32.const 0))))"
        )),
        1
    );
}

// === A generic constrained by a stdlib trait =================================

#[test]
fn test_stdlib_generic_over_add() {
    let src = with_std(
        "(fn triple ((x : T)) : T (where (Add T)) (+ x (+ x x)))
(export (fn test-func () s32 (triple (i32.const 14))))",
    );
    assert_eq!(compile_and_run(&src), 42);
}

// === The (include ...) form itself: relative path, resolved from source dir ===

#[test]
fn test_include_relative_path() {
    // Write a helper next to the (temp) source and include it by a relative name.
    let temp_dir = std::env::temp_dir();
    let helper = temp_dir.join("wisp_test_include_helper.lisp");
    std::fs::write(&helper, "(fn helper-answer () : s32 (i32.const 42))\n")
        .expect("failed to write helper");

    let src = "(include \"wisp_test_include_helper.lisp\")
(export (fn test-func () s32 (helper-answer)))";
    assert_eq!(compile_and_run(src), 42);
}
