//! Wisp compiler (dual back-end) — see docs/changes/WISP-COMPILER.md.
//!
//! B0: prove the target. A first-order Wisp function, compiled by specializing the
//! interpreter over the real `value` representation (tests/fixtures/
//! wisp-compiled-add1.wisp), builds to a Pack package and runs correctly. This also
//! pins that the interpreter's value runtime stays includable standalone.

use std::path::PathBuf;

use granite::compiler;
use wasmtime::{Config, Engine, Instance, Module, Store};

/// Compile a fixture and call a nullary exported function returning s32, decoding the
/// Pack CGRF result (s32 payload at +24: 16-byte CGRF header + 8-byte node header).
fn compile_and_call_i32(fixture: &str, func: &str) -> i32 {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source = root.join(fixture);
    let out_base = std::env::temp_dir().join(format!("wisp_compiler_{}", std::process::id()));

    // The value runtime recurses per expression node; build on a large stack.
    let wasm_path = std::thread::scope(|s| {
        std::thread::Builder::new()
            .stack_size(1 << 30)
            .spawn_scoped(s, || {
                compiler::compile(&source, &out_base, compiler::EmitOptions::default())
                    .expect("failed to compile fixture")
                    .wasm
            })
            .expect("spawn big-stack compile thread")
            .join()
            .expect("big-stack compile thread panicked")
    });
    let wasm = std::fs::read(&wasm_path).expect("read compiled wasm");

    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("engine");
    let module = Module::new(&engine, &wasm).expect("module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("instantiate");
    let f = instance
        .get_func(&mut store, func)
        .unwrap_or_else(|| panic!("export {func} not found"));
    let memory = instance.get_memory(&mut store, "memory").expect("memory");

    let (in_ptr, in_len, out_ptr_ptr, out_len_ptr) = (0x1000, 0, 0x2000, 0x2004);
    let mut results = [wasmtime::Val::I32(0)];
    f.call(
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
        .expect("read out ptr");
    let out_ptr = i32::from_le_bytes(ptr_buf);
    let mut buf = [0u8; 4];
    memory
        .read(&store, (out_ptr + 24) as usize, &mut buf)
        .expect("read result");
    i32::from_le_bytes(buf)
}

#[test]
fn b0_first_order_compiled_function_over_value_runs() {
    // (lambda (x) (+ x 1)) applied to 41, specialized over the real `value` type,
    // compiled to a Pack package, unwrapped to s32 at the boundary.
    assert_eq!(
        compile_and_call_i32("tests/fixtures/wisp-compiled-add1.wisp", "demo-int"),
        42
    );
}
