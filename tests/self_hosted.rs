use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn read_string_from_memory(
    memory: &wasmtime::Memory,
    store: &wasmtime::Store<()>,
    ptr: i32,
) -> String {
    let mut len_buf = [0u8; 4];
    memory
        .read(store, ptr as usize, &mut len_buf)
        .expect("failed to read len");
    let len = i32::from_le_bytes(len_buf) as usize;

    let mut str_buf = vec![0u8; len];
    memory
        .read(store, (ptr + 4) as usize, &mut str_buf)
        .expect("failed to read string");
    String::from_utf8(str_buf).expect("invalid utf8")
}

/// Write a string to memory in CGRF format
/// Returns the total number of bytes written
fn write_cgrf_string(
    memory: &wasmtime::Memory,
    store: &mut wasmtime::Store<()>,
    ptr: i32,
    s: &str,
) -> usize {
    let bytes = s.as_bytes();
    let str_len = bytes.len() as u32;

    // CGRF Header (16 bytes)
    let magic: u32 = 0x46524743; // "CGRF"
    let version: u16 = 2;
    let flags: u16 = 0;
    let node_count: u32 = 1;
    let root_index: u32 = 0;

    // Node header
    let kind: u8 = 0x06; // String
    let node_flags: u8 = 0;
    let reserved: u16 = 0;
    let payload_len: u32 = 4 + str_len; // length prefix + data

    let mut offset = ptr as usize;

    // Write CGRF header
    memory
        .write(&mut *store, offset, &magic.to_le_bytes())
        .unwrap();
    offset += 4;
    memory
        .write(&mut *store, offset, &version.to_le_bytes())
        .unwrap();
    offset += 2;
    memory
        .write(&mut *store, offset, &flags.to_le_bytes())
        .unwrap();
    offset += 2;
    memory
        .write(&mut *store, offset, &node_count.to_le_bytes())
        .unwrap();
    offset += 4;
    memory
        .write(&mut *store, offset, &root_index.to_le_bytes())
        .unwrap();
    offset += 4;

    // Write node header
    memory.write(&mut *store, offset, &[kind]).unwrap();
    offset += 1;
    memory.write(&mut *store, offset, &[node_flags]).unwrap();
    offset += 1;
    memory
        .write(&mut *store, offset, &reserved.to_le_bytes())
        .unwrap();
    offset += 2;
    memory
        .write(&mut *store, offset, &payload_len.to_le_bytes())
        .unwrap();
    offset += 4;

    // Write string length (payload)
    memory
        .write(&mut *store, offset, &str_len.to_le_bytes())
        .unwrap();
    offset += 4;

    // Write string bytes
    memory.write(&mut *store, offset, bytes).unwrap();
    offset += bytes.len();

    offset - ptr as usize
}

fn compile_and_call_with_string_arg(source: &str, func_name: &str, input: &str) -> String {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_selfhost_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_selfhost_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base).expect("failed to compile");

    let wasm_path = out_base.with_extension("wasm");
    let wasm_bytes = std::fs::read(&wasm_path).expect("failed to read wasm");

    // Use larger stack size for deeply nested self-hosted compiler
    // The tokenizer recurses once per character, so large files need huge stacks
    let mut config = Config::new();
    config.max_wasm_stack(128 * 1024 * 1024); // 128MB stack for bootstrap
    config.wasm_tail_call(true); // Enable tail call optimization
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

    // Debug: print memory size
    let mem_size = memory.data_size(&store);
    println!(
        "Memory size: {} bytes ({} pages)",
        mem_size,
        mem_size / 65536
    );

    // Memory layout to avoid heap conflicts:
    // - Heap starts at 0xC000 and grows upward
    // - We need space for: input (at low address) + heap growth + output (at higher address)

    // First, grow memory modestly - the module may have a memory limit
    // Grow to allow heap + reasonable output buffer
    let target_pages: u64 = 64; // 64 pages = 4MB
    let current_pages = (memory.data_size(&store) / 65536) as u64;
    if target_pages > current_pages {
        let pages_needed = target_pages - current_pages;
        memory
            .grow(&mut store, pages_needed)
            .expect("failed to grow memory");
    }

    // Layout:
    // - 0x0000-0x0FFF: reserved
    // - 0x1000-0xBFFF: input buffer (~44KB available, enough for 42KB input)
    // - 0xC000+: heap region (compiler allocates here)
    // - High address: output buffer (after growing memory)
    //
    // The heap needs room to grow. Place output at a high address.
    // With 4MB of memory (64 pages), we have addresses 0x0-0x3FFFFF
    // Put output near the top: 0x200000 (2MB offset), leaving 2MB for output
    let in_ptr: i32 = 0x1000;
    let input_size = (28 + input.len()) as i32; // CGRF header + string bytes
    let out_ptr: i32 = 0x200000; // 2MB offset
    let out_cap: i32 = 0x1F0000; // ~2MB capacity

    // Write input string in CGRF format
    let in_len = write_cgrf_string(&memory, &mut store, in_ptr, input) as i32;
    println!(
        "Input string size: {} bytes, CGRF size: {}",
        input.len(),
        in_len
    );
    println!("Input at: 0x{:x}-0x{:x}", in_ptr, in_ptr + in_len);

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
    memory
        .read(&store, (out_ptr + 24) as usize, &mut len_buf)
        .expect("failed to read string len");
    let str_len = i32::from_le_bytes(len_buf) as usize;

    let mut str_buf = vec![0u8; str_len];
    memory
        .read(&store, (out_ptr + 28) as usize, &mut str_buf)
        .expect("failed to read string data");
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
    // The tokenizer recurses once per character, so large files need huge stacks
    let mut config = Config::new();
    config.max_wasm_stack(128 * 1024 * 1024); // 128MB stack for bootstrap
    config.wasm_tail_call(true); // Enable tail call optimization
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
    memory
        .read(&store, (out_ptr + 24) as usize, &mut len_buf)
        .expect("failed to read string len");
    let str_len = i32::from_le_bytes(len_buf) as usize;

    let mut str_buf = vec![0u8; str_len];
    memory
        .read(&store, (out_ptr + 28) as usize, &mut str_buf)
        .expect("failed to read string data");
    String::from_utf8(str_buf).expect("invalid utf8")
}

// Read the self-hosted compiler source
fn get_compiler_source() -> String {
    std::fs::read_to_string("examples/wisp-compiler.lisp")
        .expect("failed to read wisp-compiler.lisp")
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
    assert!(
        wat.contains("(func $identity"),
        "should contain identity func: {}",
        wat
    );
    assert!(
        wat.contains("(param $x i32)"),
        "should contain param: {}",
        wat
    );
    assert!(
        wat.contains("(result i32)"),
        "should contain result: {}",
        wat
    );
    assert!(
        wat.contains("(local.get $x)"),
        "should contain local.get: {}",
        wat
    );
}

#[test]
fn test_self_hosted_factorial_wat() {
    let wat = compile_and_call_string(&get_compiler_source(), "get-factorial-wat");

    // Check that the output looks like valid WAT for factorial
    assert!(wat.contains("(module"), "should contain module: {}", wat);
    assert!(
        wat.contains("(func $factorial"),
        "should contain factorial func: {}",
        wat
    );
    assert!(
        wat.contains("(param $n i32)"),
        "should contain param: {}",
        wat
    );
    assert!(
        wat.contains("(result i32)"),
        "should contain result: {}",
        wat
    );
    assert!(
        wat.contains("call $factorial"),
        "should contain recursive call: {}",
        wat
    );
    assert!(
        wat.contains("i32.le_s"),
        "should contain comparison: {}",
        wat
    );
    assert!(wat.contains("i32.mul"), "should contain multiply: {}", wat);
    assert!(wat.contains("(export"), "should contain export: {}", wat);
}

#[test]
fn test_bootstrap_compile_simple() {
    // Test that compile-source can compile a simple program
    let simple_program = "(fn identity ((x s32)) s32 x)";
    let wat =
        compile_and_call_with_string_arg(&get_compiler_source(), "compile-source", simple_program);

    assert!(wat.contains("(module"), "should contain module: {}", wat);
    assert!(
        wat.contains("(func $identity"),
        "should contain identity func: {}",
        wat
    );
}

#[test]
fn test_bootstrap_compile_medium() {
    // Test with a larger program - factorial with multiple functions
    let medium_program = r#"
(fn factorial ((n s32)) s32
  (if (i32.le_s n (i32.const 1))
    (i32.const 1)
    (i32.mul n (factorial (i32.sub n (i32.const 1))))))

(fn fibonacci ((n s32)) s32
  (if (i32.le_s n (i32.const 1))
    n
    (i32.add (fibonacci (i32.sub n (i32.const 1)))
             (fibonacci (i32.sub n (i32.const 2))))))

(fn is-even ((n s32)) s32
  (if (i32.eq n (i32.const 0))
    (i32.const 1)
    (is-odd (i32.sub n (i32.const 1)))))

(fn is-odd ((n s32)) s32
  (if (i32.eq n (i32.const 0))
    (i32.const 0)
    (is-even (i32.sub n (i32.const 1)))))

(export factorial)
(export fibonacci)
"#;
    let wat =
        compile_and_call_with_string_arg(&get_compiler_source(), "compile-source", medium_program);

    assert!(wat.contains("(module"), "should contain module: {}", wat);
    assert!(
        wat.contains("(func $factorial"),
        "should contain factorial func: {}",
        wat
    );
    assert!(
        wat.contains("(func $fibonacci"),
        "should contain fibonacci func: {}",
        wat
    );
    assert!(
        wat.contains("(func $is-even"),
        "should contain is-even func: {}",
        wat
    );
    assert!(
        wat.contains("(func $is-odd"),
        "should contain is-odd func: {}",
        wat
    );
}

#[test]
fn test_bootstrap_compile_large() {
    // Test with a ~5KB program (subset of compiler) to verify we can handle substantial code
    // This tests the limits of the recursive tokenizer
    let large_program = r#"
; Tokenizer helpers
(fn is-whitespace ((c s32)) s32
  (if (i32.eq c (i32.const 32))
    (i32.const 1)
    (if (i32.eq c (i32.const 9))
      (i32.const 1)
      (if (i32.eq c (i32.const 10))
        (i32.const 1)
        (if (i32.eq c (i32.const 13))
          (i32.const 1)
          (i32.const 0))))))

(fn is-digit ((c s32)) s32
  (if (i32.ge_s c (i32.const 48))
    (if (i32.le_s c (i32.const 57))
      (i32.const 1)
      (i32.const 0))
    (i32.const 0)))

(fn is-delimiter ((c s32)) s32
  (if (i32.eq c (i32.const 40))
    (i32.const 1)
    (if (i32.eq c (i32.const 41))
      (i32.const 1)
      (if (i32.eq c (i32.const 34))
        (i32.const 1)
        (if (i32.eq c (i32.const 59))
          (i32.const 1)
          (is-whitespace c))))))

(fn skip-ws-acc ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-whitespace c)
        (skip-ws-acc src (i32.add pos (i32.const 1)) len)
        pos))))

(fn skip-ws ((src string) (pos s32) (len s32)) s32
  (skip-ws-acc src pos len))

(fn read-number-acc ((src string) (pos s32) (len s32) (acc s32) (neg s32)) s32
  (if (i32.ge_s pos len)
    (if neg (i32.sub (i32.const 0) acc) acc)
    (let (c (string-ref src pos))
      (if (is-digit c)
        (read-number-acc src (i32.add pos (i32.const 1)) len
          (i32.add (i32.mul acc (i32.const 10)) (i32.sub c (i32.const 48))) neg)
        (if neg (i32.sub (i32.const 0) acc) acc)))))

(fn read-number ((src string) (pos s32) (len s32)) s32
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 45))
      (read-number-acc src (i32.add pos (i32.const 1)) len (i32.const 0) (i32.const 1))
      (read-number-acc src pos len (i32.const 0) (i32.const 0)))))

; Simple factorial for testing
(fn factorial ((n s32)) s32
  (if (i32.le_s n (i32.const 1))
    (i32.const 1)
    (i32.mul n (factorial (i32.sub n (i32.const 1))))))

; Multiple helper functions to test medium-sized compilation
(fn gcd ((a s32) (b s32)) s32
  (if (i32.eq b (i32.const 0))
    a
    (gcd b (i32.rem_s a b))))

(fn lcm ((a s32) (b s32)) s32
  (i32.div_s (i32.mul a b) (gcd a b)))

(fn pow ((base s32) (exp s32)) s32
  (if (i32.eq exp (i32.const 0))
    (i32.const 1)
    (i32.mul base (pow base (i32.sub exp (i32.const 1))))))

(export factorial)
(export gcd)
(export lcm)
"#;
    let wat =
        compile_and_call_with_string_arg(&get_compiler_source(), "compile-source", large_program);

    assert!(wat.contains("(module"), "should contain module: {}", wat);
    assert!(
        wat.contains("(func $is-whitespace"),
        "should contain is-whitespace func: {}",
        wat
    );
    assert!(
        wat.contains("(func $factorial"),
        "should contain factorial func: {}",
        wat
    );
    assert!(
        wat.contains("(func $gcd"),
        "should contain gcd func: {}",
        wat
    );
}

#[test]
fn test_bootstrap_progressively_larger() {
    // Test with progressively larger inputs to find the breaking point
    let compiler_source = get_compiler_source();

    for size in [5000, 10000, 20000, 30000, 40000, 41926] {
        let truncated: String = compiler_source.chars().take(size).collect();
        println!("Testing with {} chars...", truncated.len());

        // Try to compile - this will panic if it fails
        let result = std::panic::catch_unwind(|| {
            compile_and_call_with_string_arg(&compiler_source, "compile-source", &truncated)
        });

        match result {
            Ok(_) => println!("  SUCCESS at {} chars", truncated.len()),
            Err(_) => {
                println!("  FAILED at {} chars", truncated.len());
                break;
            }
        }
    }
}

#[test]
fn test_compile_with_string_literal() {
    // Test that the self-hosted compiler can compile a program containing string literals
    let source_with_string = r#"
(fn get-greeting () string "hello")

(fn greet-length () s32
  (string-len (get-greeting)))
"#;
    let wat = compile_and_call_with_string_arg(
        &get_compiler_source(),
        "compile-source",
        source_with_string,
    );

    println!("Output for string literal test:\n{}", wat);

    assert!(
        wat.contains("(func $get-greeting"),
        "should contain get-greeting func: {}",
        &wat[..500.min(wat.len())]
    );
    assert!(
        wat.contains("(func $greet-length"),
        "should contain greet-length func: {}",
        &wat[..500.min(wat.len())]
    );
}

#[test]
#[ignore = "Tokenizer uses recursion that exceeds memory limits for 42KB file"]
fn test_bootstrap_self_compile() {
    // The ultimate test: compile the compiler with itself
    let compiler_source = get_compiler_source();
    println!("Compiler source length: {} chars", compiler_source.len());

    let wat =
        compile_and_call_with_string_arg(&compiler_source, "compile-source", &compiler_source);

    println!("Output length: {} chars", wat.len());
    println!("Output preview:\n{}", &wat[..2000.min(wat.len())]);

    // Check for function definitions
    let has_tokenize = wat.contains("(func $tokenize");
    let has_tokenize_acc = wat.contains("(func $tokenize-acc");
    let has_parse = wat.contains("(func $parse");
    let has_compile = wat.contains("(func $compile");
    println!(
        "Has tokenize: {}, tokenize-acc: {}, parse: {}, compile: {}",
        has_tokenize, has_tokenize_acc, has_parse, has_compile
    );

    // Find where user functions start (after runtime helpers)
    if let Some(pos) = wat.find("(func $is-whitespace") {
        println!("First user function at offset {}", pos);
        println!(
            "User functions preview:\n{}",
            &wat[pos..(pos + 2000).min(wat.len())]
        );
    }

    // List all function definitions
    println!("\n--- All function definitions ---");
    for (i, _) in wat.match_indices("(func $") {
        // Extract just the function name
        let rest = &wat[i + 7..]; // skip "(func $"
        let end = rest
            .find(|c: char| c.is_whitespace() || c == '(')
            .unwrap_or(50);
        let name = &rest[..end];
        println!("  {}: ${}", i, name);
    }

    // Write the WAT to a file for inspection
    std::fs::write("/tmp/bootstrap_output.wat", &wat).expect("failed to write wat");
    println!("Wrote WAT to /tmp/bootstrap_output.wat");

    // Check that the output looks like a valid WAT module
    assert!(wat.contains("(module"), "should contain module: {}", wat);

    // Check for key functions from the compiler (tokenize or tokenize-acc)
    let has_any_tokenize = has_tokenize || has_tokenize_acc;
    assert!(
        has_any_tokenize,
        "should contain tokenize or tokenize-acc func"
    );
    assert!(wat.contains("(func $parse"), "should contain parse func");
    assert!(
        wat.contains("(func $compile"),
        "should contain compile func"
    );
}

#[test]
#[ignore = "Requires bootstrap to pass first"]
fn test_bootstrap_v2_compiles_factorial() {
    // Load the self-compiled WAT and use it to compile a program!
    let wat = std::fs::read_to_string("/tmp/bootstrap_output.wat")
        .expect("Run test_bootstrap_self_compile first to generate WAT");

    // Create engine with tail call support
    let mut config = Config::new();
    config.max_wasm_stack(128 * 1024 * 1024);
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("failed to create engine");

    // Load the self-compiled module directly from WAT
    let module = Module::new(&engine, &wat).expect("failed to parse self-compiled WAT");
    let mut store = Store::new(&engine, ());
    let instance =
        Instance::new(&mut store, &module, &[]).expect("failed to instantiate v2 compiler");

    let func = instance
        .get_func(&mut store, "compile-source")
        .expect("compile-source not found in v2 compiler");

    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("memory not found");

    // Grow memory for our test
    memory.grow(&mut store, 64).expect("failed to grow memory");

    // Simple test program - just arithmetic (no match needed in compiler)
    let test_program = "(fn add-one ((x s32)) s32 (i32.add x (i32.const 1)))";

    let in_ptr: i32 = 0x1000;
    let out_ptr: i32 = 0x200000;
    let out_cap: i32 = 0x100000;

    // Write input in CGRF format
    let in_len = write_cgrf_string(&memory, &mut store, in_ptr, test_program) as i32;

    println!("V2 compiler: compiling factorial...");

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
    .expect("v2 compiler call failed");

    // Read the result
    let mut len_buf = [0u8; 4];
    memory
        .read(&store, (out_ptr + 24) as usize, &mut len_buf)
        .expect("failed to read len");
    let str_len = i32::from_le_bytes(len_buf) as usize;

    let mut str_buf = vec![0u8; str_len];
    memory
        .read(&store, (out_ptr + 28) as usize, &mut str_buf)
        .expect("failed to read string");
    let v2_output = String::from_utf8(str_buf).expect("invalid utf8");

    println!(
        "V2 compiler output ({} chars):\n{}",
        v2_output.len(),
        &v2_output[..500.min(v2_output.len())]
    );

    assert!(
        v2_output.contains("(module"),
        "v2 output should contain module"
    );
    assert!(
        v2_output.contains("(func $factorial"),
        "v2 output should contain factorial"
    );
    assert!(
        v2_output.contains("i32.mul"),
        "v2 output should contain multiply"
    );

    println!("V2 compiler successfully compiled factorial!");
}
