use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn read_string_from_memory(memory: &wasmtime::Memory, store: &wasmtime::Store<()>, ptr: i32) -> String {
    let mut len_buf = [0u8; 4];
    memory.read(store, ptr as usize, &mut len_buf).expect("failed to read len");
    let len = i32::from_le_bytes(len_buf) as usize;

    let mut str_buf = vec![0u8; len];
    memory.read(store, (ptr + 4) as usize, &mut str_buf).expect("failed to read string");
    String::from_utf8(str_buf).expect("invalid utf8")
}

fn compile_and_call_string(source: &str, func_name: &str) -> String {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_selfhost_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_selfhost_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base).expect("failed to compile");

    let wasm_path = out_base.with_extension("wasm");
    let wasm_bytes = std::fs::read(&wasm_path).expect("failed to read wasm");

    // Use larger stack size for deeply nested self-hosted compiler
    let mut config = Config::new();
    config.max_wasm_stack(8 * 1024 * 1024); // 8MB stack
    let engine = Engine::new(&config).expect("failed to create engine");
    let module = Module::new(&engine, &wasm_bytes).expect("failed to create module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("failed to instantiate");

    let func = instance
        .get_func(&mut store, func_name)
        .expect("function not found");

    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("memory not found");

    let in_ptr: i32 = 0x1000;
    let in_len: i32 = 0;
    let out_ptr: i32 = 0x2000;
    let out_cap: i32 = 4096;

    let mut results = [wasmtime::Val::I32(0)];
    func.call(
        &mut store,
        &[
            wasmtime::Val::I32(in_ptr),
            wasmtime::Val::I32(in_len),
            wasmtime::Val::I32(out_ptr),
            wasmtime::Val::I32(out_cap),
        ],
        &mut results,
    )
    .expect("call failed");

    // CGRF string format: offset 24 = string length, offset 28 = string bytes (inline)
    let mut len_buf = [0u8; 4];
    memory.read(&store, (out_ptr + 24) as usize, &mut len_buf).expect("failed to read string len");
    let str_len = i32::from_le_bytes(len_buf) as usize;

    let mut str_buf = vec![0u8; str_len];
    memory.read(&store, (out_ptr + 28) as usize, &mut str_buf).expect("failed to read string data");
    String::from_utf8(str_buf).expect("invalid utf8")
}

// Read the self-hosted compiler source
fn get_compiler_source() -> String {
    std::fs::read_to_string("examples/wisp-compiler.lisp").expect("failed to read wisp-compiler.lisp")
}

#[test]
fn test_self_hosted_compiles() {
    // Just verify the compiler can be compiled
    let source = get_compiler_source();
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_selfhost_compile_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_selfhost_compile_{}", test_id));

    std::fs::write(&source_path, &source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base).expect("self-hosted compiler failed to compile");

    // Verify output files exist
    assert!(out_base.with_extension("wasm").exists());
    assert!(out_base.with_extension("wat").exists());
}

#[test]
fn test_self_hosted_identity_wat() {
    let wat = compile_and_call_string(&get_compiler_source(), "get-identity-wat");

    // Check that the output looks like valid WAT
    assert!(wat.contains("(module"), "should contain module: {}", wat);
    assert!(wat.contains("(func $identity"), "should contain identity func: {}", wat);
    assert!(wat.contains("(param $x i32)"), "should contain param: {}", wat);
    assert!(wat.contains("(result i32)"), "should contain result: {}", wat);
    assert!(wat.contains("(local.get $x)"), "should contain local.get: {}", wat);
}

#[test]
fn test_self_hosted_factorial_wat() {
    let wat = compile_and_call_string(&get_compiler_source(), "get-factorial-wat");

    // Check that the output looks like valid WAT for factorial
    assert!(wat.contains("(module"), "should contain module: {}", wat);
    assert!(wat.contains("(func $factorial"), "should contain factorial func: {}", wat);
    assert!(wat.contains("(param $n i32)"), "should contain param: {}", wat);
    assert!(wat.contains("(result i32)"), "should contain result: {}", wat);
    assert!(wat.contains("call $factorial"), "should contain recursive call: {}", wat);
    assert!(wat.contains("i32.le_s"), "should contain comparison: {}", wat);
    assert!(wat.contains("i32.mul"), "should contain multiply: {}", wat);
    assert!(wat.contains("(export"), "should contain export: {}", wat);
}
