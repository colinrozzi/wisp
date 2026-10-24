use granite::compiler;
use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    // Create unique temp files for each test
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_string_ops_{}.wisp", test_id));
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

    // Pack CGRF ABI: (in_ptr, in_len, out_ptr_ptr, out_len_ptr). The callee
    // allocates the output buffer and writes its address into the out_ptr_ptr slot
    // and the byte length into the out_len_ptr slot.
    let in_ptr: i32 = 0x1000;
    let in_len: i32 = 0;
    let out_ptr_ptr: i32 = 0x2000;
    let out_len_ptr: i32 = 0x2004;

    // Call the CGRF wrapper
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

    // Read the allocated output pointer, then the s32 payload of the CGRF result
    // node (16-byte CGRF header + 8-byte node header, so the value sits at +24).
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

// === Control-character escapes ===

#[test]
fn test_string_escape_esc_nul_hex() {
    for (lit, index, byte) in [
        (r#""\e""#, 0, 27),
        (r#""\0""#, 0, 0),
        (r#""\x41""#, 0, 65),
        (r#""\x1b[0m""#, 0, 27),
        (r#""a\eb""#, 1, 27),
        (r#""a\0b""#, 2, 98),
    ] {
        let source =
            format!("(export (fn test-func () s32 (string-ref {lit} (i32.const {index}))))");
        assert_eq!(compile_and_run(&source), byte, "{lit}[{index}]");
    }
    // Each escape is one byte: "a\0b" has 3 bytes, "\e[0m" has 4.
    for (lit, len) in [(r#""a\0b""#, 3), (r#""\e[0m""#, 4), (r#""\x00\x7f""#, 2)] {
        let source = format!("(export (fn test-func () s32 (string-len {lit})))");
        assert_eq!(compile_and_run(&source), len, "{lit}");
    }
}

// === Static string literals ===

/// Run `test-func` and return its s32 result with the bytes it allocated (the
/// heap frontier, `__pack_alloc(0)`, before and after the call).
fn run_counting_alloc(source: &str) -> (i32, i32) {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_string_ops_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_string_ops_{}", test_id));
    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base, compiler::EmitOptions::default())
        .expect("failed to compile");
    let wasm_bytes = std::fs::read(out_base.with_extension("wasm")).expect("failed to read wasm");

    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("failed to create engine");
    let module = Module::new(&engine, &wasm_bytes).expect("failed to create module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("failed to instantiate");
    let func = instance
        .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, "test-func")
        .expect("function 'test-func' not found");
    let alloc = instance
        .get_typed_func::<i32, i32>(&mut store, "__pack_alloc")
        .expect("__pack_alloc not found");
    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("memory not found");

    let slots = alloc.call(&mut store, 8).unwrap();
    let before = alloc.call(&mut store, 0).unwrap();
    let status = func.call(&mut store, (0, 0, slots, slots + 4)).unwrap();
    let after = alloc.call(&mut store, 0).unwrap();
    assert_eq!(status, 0);

    let mut buf = [0u8; 4];
    memory.read(&store, slots as usize, &mut buf).unwrap();
    let out_ptr = i32::from_le_bytes(buf);
    memory
        .read(&store, (out_ptr + 24) as usize, &mut buf)
        .unwrap();
    (i32::from_le_bytes(buf), after - before)
}

fn literal_loop(n: i32) -> String {
    format!(
        r#"
(fn go ((i s32) (acc s32)) s32
  (if (i32.eq i (i32.const 0))
    acc
    (go (i32.sub i (i32.const 1))
        (i32.add acc (i32.add (string-len "hello")
                              (string=? "f32" "f32"))))))
(export (fn test-func () s32 (go (i32.const {n}) (i32.const 0))))
"#
    )
}

#[test]
fn test_string_literal_in_loop_allocates_nothing() {
    let (one, bytes_one) = run_counting_alloc(&literal_loop(1));
    let (many, bytes_many) = run_counting_alloc(&literal_loop(1000));
    assert_eq!(one, 6);
    assert_eq!(many, 6000);
    // Only the export's output buffer is allocated, however many times the
    // literals are evaluated.
    assert_eq!(bytes_one, bytes_many);
}

#[test]
fn test_string_literal_is_shared() {
    // Every evaluation of equal literals yields the same static object.
    let source = r#"
(fn lit () string "shared")
(export (fn test-func () s32
  (i32.add
    (i32.eq (string-addr (lit)) (string-addr (lit)))
    (i32.eq (string-addr "shared") (string-addr (lit))))))
"#;
    assert_eq!(compile_and_run(source), 2);
}
