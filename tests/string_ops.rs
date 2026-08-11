use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    // Create unique temp files for each test
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_string_ops_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_string_ops_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");

    // Compile
    compiler::compile(&source_path, &out_base, compiler::EmitOptions::default())
        .expect("failed to compile");

    // Read the WASM file
    let wasm_path = out_base.with_extension("wasm");
    let wasm_bytes = std::fs::read(&wasm_path).expect("failed to read wasm");

    // Run with wasmtime Module API
    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("failed to create engine");
    let module = Module::new(&engine, &wasm_bytes).expect("failed to create module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("failed to instantiate");

    // Get the CGRF wrapper function
    let func = instance
        .get_func(&mut store, "test-func")
        .expect("function 'test-func' not found");

    // Get memory for input/output buffers
    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("memory not found");

    // Allocate buffers in WASM linear memory
    // Use addresses that won't conflict with the heap (heap starts at 49152)
    let in_ptr: i32 = 0x1000; // input buffer at 4096
    let in_len: i32 = 0; // no input params
    let out_ptr: i32 = 0x2000; // output buffer at 8192
    let out_cap: i32 = 256; // 256 bytes capacity

    // Call the CGRF wrapper
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

    // Read the result from output buffer
    // CGRF format: 4 bytes magic + header + type info + value
    // For s32: magic(4) + version(2) + flags(2) + num_values(4) + padding(4) + type_tag(1) + padding(3) + value(4)
    // The value is at offset 24 from out_ptr
    let mut buf = [0u8; 4];
    memory
        .read(&store, (out_ptr + 24) as usize, &mut buf)
        .expect("failed to read result");
    i32::from_le_bytes(buf)
}

// === String length tests ===

#[test]
fn test_string_len() {
    let source = r#"
(export (fn test-func () s32
  (let (s "hello")
    (string-len s))))
"#;
    assert_eq!(compile_and_run(source), 5);
}

#[test]
fn test_empty_string_len() {
    let source = r#"
(export (fn test-func () s32
  (let (s "")
    (string-len s))))
"#;
    assert_eq!(compile_and_run(source), 0);
}

// === String-ref tests ===

#[test]
fn test_string_ref_first() {
    let source = r#"
(export (fn test-func () s32
  (let (s "hello")
    (string-ref s (i32.const 0)))))
"#;
    assert_eq!(compile_and_run(source), 104); // 'h'
}

#[test]
fn test_string_ref_middle() {
    let source = r#"
(export (fn test-func () s32
  (let (s "hello")
    (string-ref s (i32.const 2)))))
"#;
    assert_eq!(compile_and_run(source), 108); // 'l'
}

#[test]
fn test_string_ref_last() {
    let source = r#"
(export (fn test-func () s32
  (let (s "hello")
    (string-ref s (i32.const 4)))))
"#;
    assert_eq!(compile_and_run(source), 111); // 'o'
}

// === Substring tests ===

#[test]
fn test_substring_len() {
    let source = r#"
(export (fn test-func () s32
  (let (s "hello")
    (let (sub (substring s (i32.const 0) (i32.const 3)))
      (string-len sub)))))
"#;
    assert_eq!(compile_and_run(source), 3); // "hel"
}

#[test]
fn test_substring_middle() {
    let source = r#"
(export (fn test-func () s32
  (let (s "hello world")
    (let (sub (substring s (i32.const 6) (i32.const 11)))
      (string-len sub)))))
"#;
    assert_eq!(compile_and_run(source), 5); // "world"
}

#[test]
fn test_substring_content() {
    // Verify the content of the substring by checking first char
    let source = r#"
(export (fn test-func () s32
  (let (s "hello world")
    (let (sub (substring s (i32.const 6) (i32.const 11)))
      (string-ref sub (i32.const 0))))))
"#;
    assert_eq!(compile_and_run(source), 119); // 'w'
}

// === String-append tests ===

#[test]
fn test_string_append_len() {
    let source = r#"
(export (fn test-func () s32
  (let (s1 "hello")
    (let (s2 " world")
      (let (result (string-append s1 s2))
        (string-len result))))))
"#;
    assert_eq!(compile_and_run(source), 11); // "hello world"
}

#[test]
fn test_string_append_first_char() {
    let source = r#"
(export (fn test-func () s32
  (let (s1 "hello")
    (let (s2 " world")
      (let (result (string-append s1 s2))
        (string-ref result (i32.const 0)))))))
"#;
    assert_eq!(compile_and_run(source), 104); // 'h'
}

#[test]
fn test_string_append_boundary() {
    // Check the character at the boundary (first char of second string)
    let source = r#"
(export (fn test-func () s32
  (let (s1 "hello")
    (let (s2 " world")
      (let (result (string-append s1 s2))
        (string-ref result (i32.const 5)))))))
"#;
    assert_eq!(compile_and_run(source), 32); // ' '
}

#[test]
fn test_string_append_last_char() {
    let source = r#"
(export (fn test-func () s32
  (let (s1 "hello")
    (let (s2 " world")
      (let (result (string-append s1 s2))
        (string-ref result (i32.const 10)))))))
"#;
    assert_eq!(compile_and_run(source), 100); // 'd'
}

// === String equality tests ===

#[test]
fn test_string_eq_same() {
    let source = r#"
(export (fn test-func () s32
  (let (s1 "hello")
    (let (s2 "hello")
      (string=? s1 s2)))))
"#;
    assert_eq!(compile_and_run(source), 1); // true
}

#[test]
fn test_string_eq_different() {
    let source = r#"
(export (fn test-func () s32
  (let (s1 "hello")
    (let (s2 "world")
      (string=? s1 s2)))))
"#;
    assert_eq!(compile_and_run(source), 0); // false
}

#[test]
fn test_string_eq_different_len() {
    let source = r#"
(export (fn test-func () s32
  (let (s1 "hello")
    (let (s2 "hi")
      (string=? s1 s2)))))
"#;
    assert_eq!(compile_and_run(source), 0); // false
}

#[test]
fn test_string_eq_empty() {
    let source = r#"
(export (fn test-func () s32
  (let (s1 "")
    (let (s2 "")
      (string=? s1 s2)))))
"#;
    assert_eq!(compile_and_run(source), 1); // true
}

#[test]
fn test_string_eq_one_empty() {
    let source = r#"
(export (fn test-func () s32
  (let (s1 "")
    (let (s2 "a")
      (string=? s1 s2)))))
"#;
    assert_eq!(compile_and_run(source), 0); // false
}
