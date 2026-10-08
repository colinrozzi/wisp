use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_pattern_match_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_pattern_match_{}", test_id));

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

    // Pack CGRF ABI: (in_ptr, in_len, out_ptr_ptr, out_len_ptr). The callee
    // allocates the output buffer and writes its address into the out_ptr_ptr slot
    // and the byte length into the out_len_ptr slot.
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

// Test matching on variant with string payload
#[test]
fn test_match_symbol() {
    let source = r#"
(variant expr
  (sym string)
  (num s32))

(fn is-symbol ((e expr)) s32
  (match e
    ((sym s) (i32.const 1))
    ((num n) (i32.const 0))))

(export (fn test-func () s32
  (let (e (sym "hello"))
    (is-symbol e))))
"#;
    assert_eq!(compile_and_run(source), 1);
}

// Test matching on variant with numeric payload
#[test]
fn test_match_number() {
    let source = r#"
(variant expr
  (sym string)
  (num s32))

(fn is-number ((e expr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 1))))

(export (fn test-func () s32
  (let (e (num (i32.const 42)))
    (is-number e))))
"#;
    assert_eq!(compile_and_run(source), 1);
}

// Test extracting value from pattern
#[test]
fn test_extract_number() {
    let source = r#"
(variant expr
  (sym string)
  (num s32))

(fn get-num ((e expr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) n)))

(export (fn test-func () s32
  (let (e (num (i32.const 42)))
    (get-num e))))
"#;
    assert_eq!(compile_and_run(source), 42);
}

// Test extracting string and getting length
#[test]
fn test_extract_string_len() {
    let source = r#"
(variant expr
  (sym string)
  (num s32))

(fn get-sym-len ((e expr)) s32
  (match e
    ((sym s) (string-len s))
    ((num n) (i32.const 0))))

(export (fn test-func () s32
  (let (e (sym "hello"))
    (get-sym-len e))))
"#;
    assert_eq!(compile_and_run(source), 5);
}

// Test string comparison inside match
#[test]
fn test_match_with_string_compare() {
    let source = r#"
(variant expr
  (sym string)
  (num s32))

(fn is-fn-keyword ((e expr)) s32
  (match e
    ((sym s) (string=? s "fn"))
    ((num n) (i32.const 0))))

(export (fn test-func () s32
  (let (e (sym "fn"))
    (is-fn-keyword e))))
"#;
    assert_eq!(compile_and_run(source), 1);
}

// Test match returns correct result for wrong case
#[test]
fn test_match_wrong_case() {
    let source = r#"
(variant expr
  (sym string)
  (num s32))

(fn is-symbol ((e expr)) s32
  (match e
    ((sym s) (i32.const 1))
    ((num n) (i32.const 0))))

(export (fn test-func () s32
  (let (e (num (i32.const 42)))
    (is-symbol e))))
"#;
    assert_eq!(compile_and_run(source), 0);
}

// Test variant with no payload
#[test]
fn test_match_no_payload() {
    let source = r#"
(variant token
  (lparen)
  (rparen)
  (number s32))

(fn is-lparen ((t token)) s32
  (match t
    ((lparen) (i32.const 1))
    ((rparen) (i32.const 0))
    ((number n) (i32.const 0))))

(export (fn test-func () s32
  (let (t (lparen))
    (is-lparen t))))
"#;
    assert_eq!(compile_and_run(source), 1);
}

// Test variant with multiple cases
#[test]
fn test_match_multiple_no_payload() {
    let source = r#"
(variant token
  (lparen)
  (rparen)
  (number s32))

(fn is-paren ((t token)) s32
  (match t
    ((lparen) (i32.const 1))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 0))))

(export (fn test-func () s32
  (let (t (rparen))
    (is-paren t))))
"#;
    assert_eq!(compile_and_run(source), 1);
}

// A single-case variant always matches, so its match emits no discriminant test.
// Previously the comparison was emitted anyway and left on the stack with no
// branch to consume it, producing an invalid module.
#[test]
fn test_match_single_case_variant() {
    let source = r#"
(variant boxed
  (wrapped s32))

(fn unwrap ((b boxed)) s32
  (match b
    ((wrapped x) x)))

(export (fn test-func () s32
  (unwrap (wrapped (i32.const 42)))))
"#;
    assert_eq!(compile_and_run(source), 42);
}

// Test variant with list payload
#[test]
fn test_match_list_payload() {
    let source = r#"
(variant expr
  (atom s32)
  (compound (list s32)))

(fn is-compound ((e expr)) s32
  (match e
    ((atom n) (i32.const 0))
    ((compound items) (i32.const 1))))

(export (fn test-func () s32
  (let (items (list-new s32))
    (let (e (compound items))
      (is-compound e)))))
"#;
    assert_eq!(compile_and_run(source), 1);
}

// Test extracting list and getting length
#[test]
fn test_match_extract_list_len() {
    let source = r#"
(variant expr
  (atom s32)
  (compound (list s32)))

(fn get-list-len ((e expr)) s32
  (match e
    ((atom n) (i32.const 0))
    ((compound items) (list-len items))))

(export (fn test-func () s32
  (let (items (list-new s32))
    (let (items2 (list-push items (i32.const 1)))
      (let (items3 (list-push items2 (i32.const 2)))
        (let (e (compound items3))
          (get-list-len e)))))))
"#;
    assert_eq!(compile_and_run(source), 2);
}

// Test recursive variant (tree structure)
#[test]
fn test_recursive_variant() {
    let source = r#"
(variant tree
  (leaf s32)
  (node (list tree)))

(fn is-leaf ((t tree)) s32
  (match t
    ((leaf n) (i32.const 1))
    ((node children) (i32.const 0))))

(export (fn test-func () s32
  (let (t (leaf (i32.const 42)))
    (is-leaf t))))
"#;
    assert_eq!(compile_and_run(source), 1);
}

// Test recursive variant - create node with children
#[test]
fn test_recursive_variant_node() {
    let source = r#"
(variant tree
  (leaf s32)
  (node (list tree)))

(fn is-leaf ((t tree)) s32
  (match t
    ((leaf n) (i32.const 1))
    ((node children) (i32.const 0))))

(export (fn test-func () s32
  (let (children (list-new tree))
    (let (children2 (list-push children (leaf (i32.const 1))))
      (let (t (node children2))
        (is-leaf t))))))
"#;
    assert_eq!(compile_and_run(source), 0);
}

// Test full S-expression variant
#[test]
fn test_sexpr_variant() {
    let source = r#"
(variant sexpr
  (sym string)
  (num s32)
  (lst (list sexpr)))

(fn is-sym ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 1))
    ((num n) (i32.const 0))
    ((lst l) (i32.const 0))))

(fn is-lst ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((lst l) (i32.const 1))))

(export (fn test-func () s32
  (let (items (list-new sexpr))
    (let (items2 (list-push items (sym "hello")))
      (let (items3 (list-push items2 (num (i32.const 42))))
        (let (e (lst items3))
          (is-lst e)))))))
"#;
    assert_eq!(compile_and_run(source), 1);
}

// Match codegen must retain the language result type, even when several types
// happen to share an i32 pointer representation in Wasm.
#[test]
fn test_match_preserves_compound_result_type() {
    let source = r#"
(variant expr (num s32) (str string))
(fn choose ((e expr)) expr
  (if 1 (num 42)
    (match e ((num n) (num n)) ((str s) (str s)))))
(export (fn test-func () s32
  (let (chosen (choose (str "unused")))
    (match chosen ((num n) n) ((str s) 0)))))
"#;
    assert_eq!(compile_and_run(source), 42);
}

#[test]
fn test_match_option_and_result_return_non_i32_values() {
    let source = r#"
(fn label ((v (option s32))) string
  (if 1
    (match v ((none) "absent") ((some n) "present"))
    "unused"))
(fn number ((v (result s32 string))) f64
  (match v ((ok n) (f64.add 20.0 22.0)) ((err message) 0.0)))
(export (fn test-func () s32
  (if (string=? (label (some s32 1)) "present")
    (i32.trunc_f64_s (number (ok s32 string 1)))
    0)))
"#;
    assert_eq!(compile_and_run(source), 42);
}

#[test]
fn test_match_variant_returns_wide_call_result() {
    let source = r#"
(variant choice (yes) (no))
(fn wide () s64 (i64.const 4294967338))
(fn choose ((v choice)) s64
  (match v ((yes) (wide)) ((no) (i64.const 0))))
(export (fn test-func () s32 (i32.wrap_i64 (choose (yes)))))
"#;
    assert_eq!(compile_and_run(source), 42);
}

fn compile_error(source: &str) -> String {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_pattern_match_err_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_pattern_match_err_{}", test_id));
    std::fs::write(&source_path, source).expect("failed to write temp source");
    match compiler::compile(&source_path, &out_base, compiler::EmitOptions::default()) {
        Ok(_) => panic!("expected compilation to fail, but it succeeded"),
        Err(e) => format!("{:#}", e),
    }
}

#[test]
fn test_match_variant_non_exhaustive_rejected() {
    // Missing the 'no' case -> compile error naming the gap.
    let err = compile_error(
        "(variant choice (yes) (no))
(export (fn test-func ((v choice)) s32 (match v ((yes) (i32.const 1)))))",
    );
    assert!(err.contains("non-exhaustive"), "got: {err}");
    assert!(err.contains("choice"), "got: {err}");
}

#[test]
fn test_match_option_non_exhaustive_rejected() {
    // Missing the 'none' case -> compile error.
    let err =
        compile_error("(export (fn test-func ((v (option s32))) s32 (match v ((some n) n))))");
    assert!(err.contains("non-exhaustive"), "got: {err}");
}

#[test]
fn test_match_exhaustive_still_compiles() {
    // Covering every case compiles and runs as before.
    let source = r#"
(variant choice (yes) (no))
(fn choose ((v choice)) s32 (match v ((yes) (i32.const 42)) ((no) (i32.const 0))))
(export (fn test-func () s32 (choose (yes))))
"#;
    assert_eq!(compile_and_run(source), 42);
}

#[test]
fn test_match_wildcard_covers_rest() {
    // `_` satisfies exhaustiveness and dispatches correctly for uncovered cases.
    let source = r#"
(variant color (red) (green) (blue))
(fn pick ((c color)) s32 (match c ((red) (i32.const 1)) ((_) (i32.const 0))))
(export (fn test-func () s32 (i32.add (pick (red)) (pick (blue)))))
"#;
    // red -> 1, blue -> 0 (via wildcard)
    assert_eq!(compile_and_run(source), 1);
}

#[test]
fn test_match_wildcard_only() {
    // A bare `_` arm is a total catch-all.
    let source = r#"
(variant color (red) (green) (blue))
(fn pick ((c color)) s32 (match c ((_) (i32.const 7))))
(export (fn test-func () s32 (pick (green))))
"#;
    assert_eq!(compile_and_run(source), 7);
}

#[test]
fn test_match_option_wildcard() {
    // `_` covers the 'none' case on an option.
    let source = r#"
(fn f ((o (option s32))) s32 (match o ((some n) n) ((_) (i32.const 99))))
(export (fn test-func () s32 (f (none s32))))
"#;
    assert_eq!(compile_and_run(source), 99);
}
