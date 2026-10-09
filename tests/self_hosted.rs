use granite::compiler;
use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

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
    let version: u16 = 3;
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

/// Run `f` on a thread with a large stack. The self-hosted compiler running inside
/// wasm recurses deeply (the tokenizer recurses once per input character), which
/// consumes host stack during `func.call`, so the default 2 MiB test stack overflows.
fn run_big_stack<T: Send>(f: impl FnOnce() -> T + Send) -> T {
    std::thread::scope(|s| {
        match std::thread::Builder::new()
            .stack_size(1 << 30) // 1 GiB
            .spawn_scoped(s, f)
            .expect("failed to spawn big-stack thread")
            .join()
        {
            Ok(v) => v,
            Err(e) => std::panic::resume_unwind(e),
        }
    })
}

fn compile_and_call_with_string_arg(source: &str, func_name: &str, input: &str) -> String {
    run_big_stack(|| compile_and_call_with_string_arg_inner(source, func_name, input))
}

fn compile_and_call_with_string_arg_inner(source: &str, func_name: &str, input: &str) -> String {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_selfhost_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_selfhost_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base, compiler::EmitOptions::default())
        .expect("failed to compile");

    let wasm_path = out_base.with_extension("wasm");
    let wasm_bytes = std::fs::read(&wasm_path).expect("failed to read wasm");

    // Use larger stack size for deeply nested self-hosted compiler
    // The tokenizer recurses once per character, so large files need huge stacks
    let mut config = Config::new();
    config.max_wasm_stack(512 * 1024 * 1024); // 512MB: gen-1 (Rust-compiled) does not TCO the per-char tokenizer recursion
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
    // Scratch slots for the returned pointer/length, in the reserved low region.
    let out_ptr_ptr: i32 = 0x800;
    let out_len_ptr: i32 = 0x804;

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
            wasmtime::Val::I32(out_ptr_ptr),
            wasmtime::Val::I32(out_len_ptr),
        ],
        &mut results,
    )
    .expect("call failed");

    // The callee allocated the output buffer; read its pointer from the slot.
    let mut ptr_buf = [0u8; 4];
    memory
        .read(&store, out_ptr_ptr as usize, &mut ptr_buf)
        .expect("failed to read output pointer");
    let out_ptr = i32::from_le_bytes(ptr_buf);

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
    run_big_stack(|| compile_and_call_string_inner(source, func_name))
}

fn compile_and_call_string_inner(source: &str, func_name: &str) -> String {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_selfhost_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_selfhost_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base, compiler::EmitOptions::default())
        .expect("failed to compile");

    let wasm_path = out_base.with_extension("wasm");
    let wasm_bytes = std::fs::read(&wasm_path).expect("failed to read wasm");

    // Use larger stack size for deeply nested self-hosted compiler
    // The tokenizer recurses once per character, so large files need huge stacks
    let mut config = Config::new();
    config.max_wasm_stack(512 * 1024 * 1024); // 512MB: gen-1 (Rust-compiled) does not TCO the per-char tokenizer recursion
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
    // Scratch slots for the returned pointer/length, in the reserved low region.
    let out_ptr_ptr: i32 = 0x800;
    let out_len_ptr: i32 = 0x804;

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

    // The callee allocated the output buffer; read its pointer from the slot.
    let mut ptr_buf = [0u8; 4];
    memory
        .read(&store, out_ptr_ptr as usize, &mut ptr_buf)
        .expect("failed to read output pointer");
    let out_ptr = i32::from_le_bytes(ptr_buf);

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
    std::fs::read_to_string("examples/wisp-compiler.wisp")
        .expect("failed to read wisp-compiler.wisp")
}

#[test]
fn test_self_hosted_compiles() {
    run_big_stack(|| {
        // Just verify the compiler can be compiled
        let source = get_compiler_source();
        let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        let temp_dir = std::env::temp_dir();
        let source_path = temp_dir.join(format!("test_selfhost_compile_{}.wisp", test_id));
        let out_base = temp_dir.join(format!("test_selfhost_compile_{}", test_id));

        std::fs::write(&source_path, &source).expect("failed to write temp source");
        // Emit the wat too, since this test asserts both artifacts are produced.
        compiler::compile(
            &source_path,
            &out_base,
            compiler::EmitOptions {
                wat: true,
                pact: false,
            },
        )
        .expect("self-hosted compiler failed to compile");

        // Verify output files exist
        assert!(out_base.with_extension("wasm").exists());
        assert!(out_base.with_extension("wat").exists());
    });
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
fn test_bootstrap_self_compile() {
    // The ultimate test: compile the compiler with itself.
    //
    // This used to trap "out of bounds memory access": codegen assembled its
    // ~150 KB WAT output with linear left-fold `string-append` accumulators,
    // which are O(N^2), and the bump heap never frees -- the 5 KB runtime
    // literal alone (emitted as ~40 WAT bytes per source byte) blew past 1 GB.
    // Codegen now assembles output with divide-and-conquer string-append
    // (O(N log N)); the self-compile completes in ~1s.
    //
    // NOTE: the produced module is not yet a valid WAT *fixpoint*. The
    // compiler's own source uses four forms it cannot yet compile
    // (top-level `(global ...)`, `begin`, `global.get`, `global.set`), so the
    // output contains one `(error: unknown form)` and miscompiles the i64
    // number helpers. Closing that gap is the next self-hosting step; see
    // test_bootstrap_v2_compiles_factorial. This test guards only that the
    // O(N^2) blow-up stays fixed and the compiler runs to completion.
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

/// The real fixpoint proof: the self-compiled compiler (gen-2) is a valid module
/// AND works as a compiler. gen-2 uses the plain ABI it emits for its own exports:
/// `compile-source(src_ptr) -> out_ptr`, where a string is `[len: u32][utf8 bytes]`
/// in linear memory (NOT the CGRF ABI the Rust bootstrap wraps exports in).
#[test]
fn test_bootstrap_v2_compiles_factorial() {
    // Produce gen-2 directly (gen-1 compiling its own source), so this test does
    // not depend on another test having written a temp file.
    let compiler_source = get_compiler_source();
    let wat =
        compile_and_call_with_string_arg(&compiler_source, "compile-source", &compiler_source);
    run_big_stack(move || {
        let mut config = Config::new();
        config.max_wasm_stack(512 * 1024 * 1024); // 512MB: gen-1 (Rust-compiled) does not TCO the per-char tokenizer recursion
        config.wasm_tail_call(true);
        let engine = Engine::new(&config).expect("failed to create engine");
        let module = Module::new(&engine, &wat).expect("failed to parse self-compiled WAT");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("failed to instantiate v2 compiler");

        let func = instance
            .get_typed_func::<i32, i32>(&mut store, "compile-source")
            .expect("compile-source not found in v2 compiler");
        let memory = instance
            .get_memory(&mut store, "memory")
            .expect("memory not found");

        // Give gen-2's bump heap (base 0xC000) room to grow.
        memory.grow(&mut store, 64).expect("failed to grow memory");

        let test_program = "(fn factorial ((n s32)) s32 (if (i32.le_s n (i32.const 1)) (i32.const 1) (i32.mul n (factorial (i32.sub n (i32.const 1))))))";

        // Write the input as a wisp string [len][bytes] at a low address, below the
        // heap base. The program is tiny, so heap growth won't reach it.
        let in_ptr: i32 = 0x1000;
        let bytes = test_program.as_bytes();
        memory
            .write(
                &mut store,
                in_ptr as usize,
                &(bytes.len() as u32).to_le_bytes(),
            )
            .unwrap();
        memory
            .write(&mut store, (in_ptr + 4) as usize, bytes)
            .unwrap();

        println!("V2 compiler: compiling factorial...");
        let out_ptr = func
            .call(&mut store, in_ptr)
            .expect("v2 compiler call failed");

        // Read the output wisp string [len][bytes].
        let mut len_buf = [0u8; 4];
        memory
            .read(&store, out_ptr as usize, &mut len_buf)
            .expect("failed to read output length");
        let out_len = i32::from_le_bytes(len_buf) as usize;
        let mut str_buf = vec![0u8; out_len];
        memory
            .read(&store, (out_ptr + 4) as usize, &mut str_buf)
            .expect("failed to read output bytes");
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
    });
}

/// Run a gen-2 (plain-ABI) compiler module on `input`, returning its emitted WAT.
/// gen-2 exports `compile-source(in_ptr) -> out_ptr`; strings are `[len:u32][utf8]`.
fn run_plain_abi_compile_source(wat: &str, input: &str) -> String {
    let mut config = Config::new();
    config.max_wasm_stack(512 * 1024 * 1024); // 512MB: gen-1 (Rust-compiled) does not TCO the per-char tokenizer recursion
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("failed to create engine");
    let module = Module::new(&engine, wat).expect("failed to parse gen-2 WAT");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("failed to instantiate gen-2");

    let func = instance
        .get_typed_func::<i32, i32>(&mut store, "compile-source")
        .expect("compile-source not found in gen-2");
    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("memory not found");

    // Grow generously: the divide-and-conquer string-append never frees, so peak
    // heap is many times the ~1MB output. gen-1's Rust-emitted module gets 2 GB, so
    // match that for gen-2 to remove memory size as a variable.
    let target_pages: u64 = 32768; // 2 GB
    let current_pages = (memory.data_size(&store) / 65536) as u64;
    if target_pages > current_pages {
        memory
            .grow(&mut store, target_pages - current_pages)
            .expect("failed to grow memory");
    }

    // Input as [len][bytes], placed HIGH -- above the heap's working region. gen-2's
    // bump heap grows from 0xC000 upward (peak < 256 MB for the full source), so an
    // input at 0x1000 would be clobbered mid-compile. Put it at 512 MB, well above the
    // heap peak and below the 1 GB memory top. (gen-1 avoids this because its CGRF
    // wrapper copies the input onto the heap before compiling.)
    let in_ptr: i32 = 0x2000_0000; // 512 MB
    let bytes = input.as_bytes();
    memory
        .write(
            &mut store,
            in_ptr as usize,
            &(bytes.len() as u32).to_le_bytes(),
        )
        .unwrap();
    memory
        .write(&mut store, (in_ptr + 4) as usize, bytes)
        .unwrap();

    let out_ptr = func
        .call(&mut store, in_ptr)
        .expect("gen-2 compile call failed");

    let mut len_buf = [0u8; 4];
    memory
        .read(&store, out_ptr as usize, &mut len_buf)
        .expect("failed to read output length");
    let out_len = i32::from_le_bytes(len_buf) as usize;
    println!("gen-2 output: ptr=0x{:x} len={}", out_ptr, out_len);
    let mut str_buf = vec![0u8; out_len];
    memory
        .read(&store, (out_ptr + 4) as usize, &mut str_buf)
        .expect("failed to read output bytes");
    match String::from_utf8(str_buf) {
        Ok(s) => s,
        Err(e) => {
            let pos = e.utf8_error().valid_up_to();
            let bytes = e.as_bytes();
            let lo = pos.saturating_sub(120);
            println!(
                "gen-3 output not valid UTF-8: first bad byte at {} (0x{:02x})",
                pos, bytes[pos]
            );
            println!(
                "context before bad byte:\n{}",
                String::from_utf8_lossy(&bytes[lo..pos])
            );
            println!(
                "context after (lossy):\n{}",
                String::from_utf8_lossy(&bytes[pos..(pos + 200).min(bytes.len())])
            );
            String::from_utf8_lossy(bytes).into_owned()
        }
    }
}

/// The self-hosting fixpoint: gen-2 (gen-1 compiling the compiler source) must compile
/// the SAME source into a gen-3 that is byte-identical to gen-2. Equality proves the
/// compiler reproduces itself exactly -- the definition of a closed self-hosting loop.
#[test]
fn test_bootstrap_fixpoint() {
    let compiler_source = get_compiler_source();
    println!("Compiler source: {} chars", compiler_source.len());

    // gen-2 = gen-1 (Rust-compiled) compiling its own source, read via the CGRF export.
    let gen2 =
        compile_and_call_with_string_arg(&compiler_source, "compile-source", &compiler_source);
    println!("gen-2: {} chars", gen2.len());
    assert!(gen2.contains("(module"), "gen-2 should be a module");

    // gen-3 = gen-2 compiling the same source, via gen-2's own plain ABI.
    let gen3 = run_big_stack({
        let gen2 = gen2.clone();
        let src = compiler_source.clone();
        move || run_plain_abi_compile_source(&gen2, &src)
    });
    println!("gen-3: {} chars", gen3.len());

    if gen2 == gen3 {
        println!("FIXPOINT REACHED: gen-2 == gen-3 ({} chars)", gen2.len());
    } else {
        // Report the first divergence to make debugging tractable.
        let first_diff = gen2
            .bytes()
            .zip(gen3.bytes())
            .position(|(a, b)| a != b)
            .unwrap_or(gen2.len().min(gen3.len()));
        let lo = first_diff.saturating_sub(80);
        println!(
            "DIVERGENCE at byte {} (gen2 len {}, gen3 len {})",
            first_diff,
            gen2.len(),
            gen3.len()
        );
        println!(
            "gen-2 context:\n{}",
            &gen2[lo..(first_diff + 120).min(gen2.len())]
        );
        println!(
            "gen-3 context:\n{}",
            &gen3[lo..(first_diff + 120).min(gen3.len())]
        );
        std::fs::write("/tmp/gen2.wat", &gen2).ok();
        std::fs::write("/tmp/gen3.wat", &gen3).ok();
    }

    assert_eq!(
        gen2.len(),
        gen3.len(),
        "gen-2 and gen-3 differ in length -- not a fixpoint"
    );
    assert_eq!(gen2, gen3, "gen-2 and gen-3 differ -- not a fixpoint");
}

/// Compile `program` with the self-hosted compiler (via gen-1) and run a no-arg i32
/// export, returning its result. The self-hosted compiler emits plain WAT exports, so
/// the function is called directly as `() -> i32`.
fn selfhost_run_i32(program: &str, func_name: &str) -> i32 {
    let wat = compile_and_call_with_string_arg(&get_compiler_source(), "compile-source", program);
    run_big_stack(move || {
        let mut config = Config::new();
        config.max_wasm_stack(512 * 1024 * 1024); // 512MB: gen-1 (Rust-compiled) does not TCO the per-char tokenizer recursion
        config.wasm_tail_call(true);
        let engine = Engine::new(&config).expect("engine");
        let module = Module::new(&engine, &wat).unwrap_or_else(|e| {
            panic!("self-hosted output is not valid WAT: {e}\n--- WAT ---\n{wat}")
        });
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("instantiate");
        // Variant construction allocates on the heap (base 0xC000); give it room.
        if let Some(memory) = instance.get_memory(&mut store, "memory") {
            let have = (memory.data_size(&store) / 65536) as u64;
            if have < 64 {
                memory.grow(&mut store, 64 - have).expect("grow");
            }
        }
        let func = instance
            .get_typed_func::<(), i32>(&mut store, func_name)
            .unwrap_or_else(|e| panic!("export {func_name} not found: {e}\n--- WAT ---\n{wat}"));
        func.call(&mut store, ()).expect("call failed")
    })
}

/// Option/Result are built-in parametric variants in the self-hosted compiler:
/// some/none/ok/err construct, and `match` destructures them.
#[test]
fn test_selfhosted_option_result() {
    // Rust-annotated syntax: (some T v), (none T), (ok T E v), (err T E v).
    // some payload flows through match
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (match (some s32 (i32.const 42)) ((some x) x) ((none) (i32.const 0)))))",
            "f",
        ),
        42
    );
    // none takes the none arm
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (match (none s32) ((some x) x) ((none) (i32.const 7)))))",
            "f",
        ),
        7
    );
    // ok payload
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (match (ok s32 s32 (i32.const 5)) ((ok v) v) ((err e) (i32.const 0)))))",
            "f",
        ),
        5
    );
    // err payload
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (match (err s32 s32 (i32.const 9)) ((ok v) (i32.const 0)) ((err e) e))))",
            "f",
        ),
        9
    );
    // Leniency: the un-annotated (some v) form also works (payload is the last arg)
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (match (some (i32.const 13)) ((some x) x) ((none) (i32.const 0)))))",
            "f",
        ),
        13
    );
}

/// Tuples are construct-only: (tuple v...) builds a heap record with field i at +4*i.
/// There is no element-access form (matching the Rust compiler), so read fields directly.
#[test]
fn test_selfhosted_tuple() {
    // field 0 of a 2-tuple
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (i32.load (tuple (i32.const 10) (i32.const 20)))))",
            "f",
        ),
        10
    );
    // field 1 of a 2-tuple (offset 4)
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (let (t (tuple (i32.const 10) (i32.const 20))) (i32.load (i32.add t (i32.const 4))))))",
            "f",
        ),
        20
    );
    // field 2 of a 3-tuple (offset 8)
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (i32.load (i32.add (tuple (i32.const 10) (i32.const 20) (i32.const 30)) (i32.const 8)))))",
            "f",
        ),
        30
    );
}

/// Compound type-param inference: unify a (list T) param against the argument's structured
/// type to bind T, and propagate list element types through list-get.
#[test]
fn test_selfhosted_compound_generics() {
    // T inferred from a (list T) param
    assert_eq!(
        selfhost_run_i32(
            "(fn head ((xs (list T))) T (where T) (list-get xs (i32.const 0)))\
             (export (fn f () s32 (head (list-push (list-new s32) (i32.const 42)))))",
            "f",
        ),
        42
    );
    // distinguishing: the element type (point) flows to trait dispatch -> Eq--eq--point.
    // Under the old s32 default this would dispatch the undefined Eq--eq--s32 and fail.
    assert_eq!(
        selfhost_run_i32(
            "(trait (Eq T) (fn eq ((a T) (b T)) s32))\
             (record point (x s32) (y s32))\
             (derive Eq point)\
             (fn firsteq ((xs (list T))) s32 (where (Eq T)) (eq (list-get xs (i32.const 0)) (list-get xs (i32.const 0))))\
             (export (fn f () s32 (firsteq (list-push (list-new point) (point (i32.const 3) (i32.const 4))))))",
            "f",
        ),
        1
    );
}

/// Higher-order function params (Increment 6): a (-> ...) param takes a function name,
/// which is inlined and dropped from the signature (defunctionalization), with the name
/// mangled in (apply--inc / apply2--inc--s32).
#[test]
fn test_selfhosted_hof() {
    // HOF, no type param: apply--inc ((x s32)) s32 (inc x)
    assert_eq!(
        selfhost_run_i32(
            "(fn inc ((n s32)) s32 (i32.add n (i32.const 1)))\
             (fn apply ((f (-> s32 s32)) (x s32)) s32 (f x))\
             (export (fn g () s32 (apply inc (i32.const 41))))",
            "g",
        ),
        42
    );
    // HOF with a type param: apply2--inc--s32
    assert_eq!(
        selfhost_run_i32(
            "(fn inc ((n s32)) s32 (i32.add n (i32.const 1)))\
             (fn apply2 ((f (-> T T)) (x T)) T (where T) (f x))\
             (export (fn g () s32 (apply2 inc (i32.const 41))))",
            "g",
        ),
        42
    );
}

/// Deriving (Increment 5): (derive Eq Type) reflects a record's fields and generates an
/// Eq instance (per-field i32.eq, and-ed), resolved like any hand-written instance.
#[test]
fn test_selfhosted_derive() {
    let prog = "(trait (Eq T) (fn eq ((a T) (b T)) s32))\
                (record point (x s32) (y s32))\
                (derive Eq point)";
    // equal records -> 1
    assert_eq!(
        selfhost_run_i32(
            &format!(
                "{prog}(export (fn f () s32 (eq (point (i32.const 3) (i32.const 4)) (point (i32.const 3) (i32.const 4)))))"
            ),
            "f",
        ),
        1
    );
    // differing second field -> 0
    assert_eq!(
        selfhost_run_i32(
            &format!(
                "{prog}(export (fn f () s32 (eq (point (i32.const 3) (i32.const 4)) (point (i32.const 3) (i32.const 5)))))"
            ),
            "f",
        ),
        0
    );
}

/// Traits + instances (Increment 4): trait-method calls resolve to the instance for the
/// first argument's type -- both directly and inside trait-constrained generic bodies.
#[test]
fn test_selfhosted_traits() {
    // direct dispatch on arg type -> Add--add--s32
    assert_eq!(
        selfhost_run_i32(
            "(trait (Add T) (fn add ((a T) (b T)) T))\
             (instance (Add s32) (fn add ((a s32) (b s32)) s32 (i32.add a b)))\
             (export (fn f () s32 (add (i32.const 20) (i32.const 22))))",
            "f",
        ),
        42
    );
    // trait-constrained generic: double specialized at s32, (add x x) -> Add--add--s32
    assert_eq!(
        selfhost_run_i32(
            "(trait (Add T) (fn add ((a T) (b T)) T))\
             (instance (Add s32) (fn add ((a s32) (b s32)) s32 (i32.add a b)))\
             (fn double ((x T)) T (where (Add T)) (add x x))\
             (export (fn f () s32 (double (i32.const 21))))",
            "f",
        ),
        42
    );
}

/// Generics (Increment 3): (where ...) templates monomorphized per concrete type arg,
/// with mangled names, dedup, and transitive specialization.
#[test]
fn test_selfhosted_generics() {
    // identity: id--s32 emitted, call rewritten
    assert_eq!(
        selfhost_run_i32(
            "(fn id ((x T)) T (where T) x)\
             (export (fn f () s32 (id (i32.const 42))))",
            "f",
        ),
        42
    );
    // two type params, projection: fst--s32--s32 returns first
    assert_eq!(
        selfhost_run_i32(
            "(fn fst ((a T) (b U)) T (where T U) a)\
             (export (fn f () s32 (fst (i32.const 7) (i32.const 99))))",
            "f",
        ),
        7
    );
    // dedup: two calls at the same type share one specialization
    assert_eq!(
        selfhost_run_i32(
            "(fn id ((x T)) T (where T) x)\
             (export (fn f () s32 (i32.add (id (i32.const 1)) (id (i32.const 2)))))",
            "f",
        ),
        3
    );
    // transitive: a generic whose body calls another generic
    assert_eq!(
        selfhost_run_i32(
            "(fn id ((x T)) T (where T) x)\
             (fn id2 ((x T)) T (where T) (id x))\
             (export (fn f () s32 (id2 (i32.const 42))))",
            "f",
        ),
        42
    );
    // non-i32 specialization: id--s64 has an i64 param/result (would fail to assemble if
    // the generic ran as the i32 default instead of being monomorphized at s64)
    assert_eq!(
        selfhost_run_i32(
            "(fn id ((x T)) T (where T) x)\
             (export (fn f () s32 (s32 (id (i64.const 5)))))",
            "f",
        ),
        5
    );
}

/// Numeric casts (s32|s64|f32|f64 expr): infer the source scalar type and emit the right
/// conversion, or the bare value when source == target.
#[test]
fn test_selfhosted_casts() {
    // s64 -> s32 (i32.wrap_i64)
    assert_eq!(
        selfhost_run_i32("(export (fn f () s32 (s32 (i64.const 5))))", "f"),
        5
    );
    // f64 -> s32 (i32.trunc_f64_s)
    assert_eq!(
        selfhost_run_i32("(export (fn f () s32 (s32 (f64.const 3.7))))", "f"),
        3
    );
    // f32 -> s32 (i32.trunc_f32_s)
    assert_eq!(
        selfhost_run_i32("(export (fn f () s32 (s32 (f32.const 2.9))))", "f"),
        2
    );
    // round-trip s32 -> f64 -> s32 (f64.convert_i32_s then i32.trunc_f64_s)
    assert_eq!(
        selfhost_run_i32("(export (fn f () s32 (s32 (f64 (i32.const 9)))))", "f"),
        9
    );
    // same type -> no-op (bare value)
    assert_eq!(
        selfhost_run_i32("(export (fn f () s32 (s32 (i32.const 11))))", "f"),
        11
    );
}

/// Increment 2: casts driven by inferred types of variables (fn params) and call results,
/// not just literals. Without the type env these would wrongly infer s32 and mis-compile.
#[test]
fn test_selfhosted_casts_inferred() {
    // s64 param, cast to s32 inside the fn (param type comes from the signature)
    assert_eq!(
        selfhost_run_i32(
            "(fn g ((x s64)) s32 (s32 x))\
             (export (fn f () s32 (g (i64.const 42))))",
            "f",
        ),
        42
    );
    // f64 param, cast to s32 (truncates)
    assert_eq!(
        selfhost_run_i32(
            "(fn g ((x f64)) s32 (s32 x))\
             (export (fn f () s32 (g (f64.const 3.7))))",
            "f",
        ),
        3
    );
    // call result: g returns s64; casting the call to s32 uses the signature table
    assert_eq!(
        selfhost_run_i32(
            "(fn g () s64 (i64.const 9))\
             (export (fn f () s32 (s32 (g))))",
            "f",
        ),
        9
    );
}

/// Increment 2.5: a `let` binding a non-i32 value declares the local with the right WAT
/// type (inferred from the value), so s64/f64 locals round-trip. Previously all locals
/// were declared i32, so these would fail to assemble.
#[test]
fn test_selfhosted_typed_locals() {
    // i64 local
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (let (y (i64.const 7)) (s32 y))))",
            "f",
        ),
        7
    );
    // f64 local (truncates on cast out)
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (let (z (f64.const 4.9)) (s32 z))))",
            "f",
        ),
        4
    );
    // i32 local still works (regression)
    assert_eq!(
        selfhost_run_i32(
            "(export (fn f () s32 (let (a (i32.const 5)) (i32.add a (i32.const 1)))))",
            "f",
        ),
        6
    );
}

/// Unhygienic macros with quasiquotation: defmacro + `/,/,@, expanded before codegen.
#[test]
fn test_selfhosted_macros() {
    // simple substitution: (dbl 21) -> (i32.mul 21 2)
    assert_eq!(
        selfhost_run_i32(
            "(defmacro dbl (x) `(i32.mul ,x (i32.const 2)))\
             (export (fn f () s32 (dbl (i32.const 21))))",
            "f",
        ),
        42
    );
    // control-flow macro, true branch
    assert_eq!(
        selfhost_run_i32(
            "(defmacro when1 (c body) `(if ,c ,body (i32.const 0)))\
             (export (fn f () s32 (when1 (i32.const 1) (i32.const 42))))",
            "f",
        ),
        42
    );
    // control-flow macro, false branch
    assert_eq!(
        selfhost_run_i32(
            "(defmacro when1 (c body) `(if ,c ,body (i32.const 99)))\
             (export (fn f () s32 (when1 (i32.const 0) (i32.const 42))))",
            "f",
        ),
        99
    );
    // unquote-splice: xs = ((i32.const 15) (i32.const 27)) spliced into i32.add
    assert_eq!(
        selfhost_run_i32(
            "(defmacro sumargs (xs) `(i32.add ,@xs))\
             (export (fn f () s32 (sumargs ((i32.const 15) (i32.const 27)))))",
            "f",
        ),
        42
    );
    // recursive expansion: a macro whose template calls another macro
    assert_eq!(
        selfhost_run_i32(
            "(defmacro inc (x) `(i32.add ,x (i32.const 1)))\
             (defmacro inc2 (x) `(inc (inc ,x)))\
             (export (fn f () s32 (inc2 (i32.const 40))))",
            "f",
        ),
        42
    );
}
