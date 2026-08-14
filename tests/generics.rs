// Tests for the generics/traits pipeline: monomorphization, operator resolution,
// return-type dispatch, and literal adoption. Each program exports `test-func`
// returning s32; the harness runs it and reads the s32 result.

use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_generics_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_generics_{}", test_id));

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

/// Traits and instances shared by the trait/generic tests.
const PREAMBLE: &str = r#"
(trait (Add T) (fn + ((a : T) (b : T)) : T))
(instance (Add s32) (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))
(instance (Add f64) (fn + ((a : f64) (b : f64)) : f64 (f64.add a b)))

(trait (Zero T) (fn zero () : T))
(instance (Zero s32) (fn zero () : s32 (i32.const 0)))
(instance (Zero f64) (fn zero () : f64 (f64.const 0.0)))

(trait (One T) (fn one () : T))
(instance (One s32) (fn one () : s32 (i32.const 1)))
(instance (One f64) (fn one () : f64 (f64.const 1.0)))

(fn double ((x : T)) : T (where (Add T)) (+ x x))
"#;

fn with_preamble(body: &str) -> String {
    format!("{}\n{}", PREAMBLE, body)
}

// === Generics: monomorphization at two types =================================

#[test]
fn test_generic_double_s32() {
    let src = with_preamble("(export (fn test-func () s32 (double (i32.const 21))))");
    assert_eq!(compile_and_run(&src), 42);
}

#[test]
fn test_generic_double_f64() {
    // double at f64 resolves + to f64.add; compare to 3.0 and return a bool.
    let src = with_preamble(
        "(export (fn test-func () s32
           (if (f64.eq (double (f64.const 1.5)) (f64.const 3.0)) (i32.const 1) (i32.const 0))))",
    );
    assert_eq!(compile_and_run(&src), 1);
}

// === Operators resolve at the top level ======================================

#[test]
fn test_top_level_plus_s32() {
    let src = with_preamble("(export (fn test-func () s32 (+ (i32.const 40) (i32.const 2))))");
    assert_eq!(compile_and_run(&src), 42);
}

#[test]
fn test_top_level_plus_f64() {
    let src = with_preamble(
        "(export (fn test-func () s32
           (if (f64.eq (+ (f64.const 20.0) (f64.const 22.0)) (f64.const 42.0))
             (i32.const 1) (i32.const 0))))",
    );
    assert_eq!(compile_and_run(&src), 1);
}

// === Return-type dispatch (top-down expected type) ===========================

#[test]
fn test_return_dispatch_return_position() {
    // (one) resolves purely from the s32 return type.
    let src = with_preamble("(export (fn test-func () s32 (one)))");
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_return_dispatch_sibling_argument() {
    // (one) takes s32 from its sibling argument.
    let src = with_preamble("(export (fn test-func () s32 (+ (i32.const 41) (one))))");
    assert_eq!(compile_and_run(&src), 42);
}

#[test]
fn test_return_dispatch_ascription() {
    let src = with_preamble("(export (fn test-func () s32 ((one) : s32)))");
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_return_dispatch_if_zero_branch() {
    // Both branches inherit the expected s32; the taken branch is (zero).
    let src = with_preamble("(export (fn test-func () s32 (if (i32.const 0) (one) (zero))))");
    assert_eq!(compile_and_run(&src), 0);
}

#[test]
fn test_return_dispatch_f64_via_operand() {
    // (one) dispatches to f64 because f64.eq expects an f64 operand.
    let src = with_preamble(
        "(export (fn test-func () s32
           (if (f64.eq (one) (f64.const 1.0)) (i32.const 1) (i32.const 0))))",
    );
    assert_eq!(compile_and_run(&src), 1);
}

// === Literal adoption (no traits needed) =====================================

#[test]
fn test_literal_i64_operands() {
    // Bare 1 and 2 adopt s64 from the i64.add operand type.
    let src = "(export (fn test-func () s32 (i64.eq (i64.add 1 2) (i64.const 3))))";
    assert_eq!(compile_and_run(src), 1);
}

#[test]
fn test_literal_f64_operands() {
    // Integer literals promote to f64 in an f64.add.
    let src = "(export (fn test-func () s32 (f64.eq (f64.add 1 2) (f64.const 3.0))))";
    assert_eq!(compile_and_run(src), 1);
}

#[test]
fn test_literal_f32_operands() {
    let src = "(export (fn test-func () s32 (f32.eq (f32.add 1 2) (f32.const 3.0))))";
    assert_eq!(compile_and_run(src), 1);
}

#[test]
fn test_literal_return_position_s64() {
    // A literal in a s64 return position widens to s64.
    let src = "(fn c () : s64 5)
(export (fn test-func () s32 (i64.eq (c) (i64.const 5))))";
    assert_eq!(compile_and_run(src), 1);
}

#[test]
fn test_literal_ascription_s64() {
    let src = "(export (fn test-func () s32 (i64.eq (5 : s64) (i64.const 5))))";
    assert_eq!(compile_and_run(src), 1);
}

// === Structural unification: generics over (list T) ==========================

/// A generic `sum` over `(list T)`, recursing by index. `T` is solved by unifying
/// the parameter shape `(list T)` against the argument's `(list s32)` / `(list f64)`.
const SUM_DEFS: &str = r#"
(fn sum-go ((xs : (list T)) (i : s32) (acc : T)) : T
  (where (Add T) (Zero T))
  (if (i32.lt_s i (list-len xs))
      (sum-go xs (i32.add i (i32.const 1)) (+ acc (list-get xs i)))
      acc))

(fn sum ((xs : (list T))) : T
  (where (Add T) (Zero T))
  (sum-go xs (i32.const 0) (zero)))
"#;

#[test]
fn test_generic_list_sum_s32() {
    let src = with_preamble(&format!(
        "{}
(export (fn test-func () s32
  (let (xs (list-push (list-push (list-push (list-new s32)
             (i32.const 10)) (i32.const 20)) (i32.const 12)))
    (sum xs))))",
        SUM_DEFS
    ));
    assert_eq!(compile_and_run(&src), 42);
}

#[test]
fn test_generic_list_sum_f64() {
    // The same `sum` monomorphizes at f64; T is solved from (list f64).
    let src = with_preamble(&format!(
        "{}
(export (fn test-func () s32
  (let (xs (list-push (list-push (list-new f64) (f64.const 1.5)) (f64.const 2.0)))
    (if (f64.eq (sum xs) (f64.const 3.5)) (i32.const 1) (i32.const 0)))))",
        SUM_DEFS
    ));
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_generic_over_list_return_element() {
    // `first` takes (list T) and returns T: T is used in both the parameter shape
    // and the return, and (zero) return-dispatches for the empty case.
    let src = with_preamble(
        "(fn first ((xs : (list T))) : T
           (where (Zero T))
           (if (i32.lt_s (list-len xs) (i32.const 1)) (zero) (list-get xs (i32.const 0))))
(export (fn test-func () s32
  (let (xs (list-push (list-new s32) (i32.const 42)))
    (first xs))))",
    );
    assert_eq!(compile_and_run(&src), 42);
}

#[test]
fn test_generic_over_list_empty_uses_zero() {
    // An empty list falls to the (zero) branch, return-dispatched to s32.
    let src = with_preamble(
        "(fn first ((xs : (list T))) : T
           (where (Zero T))
           (if (i32.lt_s (list-len xs) (i32.const 1)) (zero) (list-get xs (i32.const 0))))
(export (fn test-func () s32 (first (list-new s32))))",
    );
    assert_eq!(compile_and_run(&src), 0);
}

// === Higher-order functions via monomorphization =============================

/// map/fold over `(list T)` taking a function argument, plus an `inc` helper and a
/// list builder. Function arguments are compile-time; `(map f xs)` specializes to
/// `map--f--T` with `f` inlined.
const HOF_DEFS: &str = r#"
(fn inc ((n : s32)) : s32 (i32.add n (i32.const 1)))

(fn build () : (list s32)
  (list-push (list-push (list-new s32) (i32.const 10)) (i32.const 20)))

(fn map-go ((f : (-> T T)) (xs : (list T)) (i : s32) (acc : (list T))) : (list T)
  (where T)
  (if (i32.lt_s i (list-len xs))
      (map-go f xs (i32.add i (i32.const 1)) (list-push acc (f (list-get xs i))))
      acc))
(fn map ((f : (-> T T)) (xs : (list T))) : (list T)
  (where T)
  (map-go f xs (i32.const 0) (list-new T)))

(fn fold-go ((f : (-> T T T)) (xs : (list T)) (i : s32) (acc : T)) : T
  (where T)
  (if (i32.lt_s i (list-len xs))
      (fold-go f xs (i32.add i (i32.const 1)) (f acc (list-get xs i)))
      acc))
(fn fold ((f : (-> T T T)) (init : T) (xs : (list T))) : T
  (where T)
  (fold-go f xs (i32.const 0) init))
"#;

fn with_hof(body: &str) -> String {
    format!("{}\n{}\n{}", PREAMBLE, HOF_DEFS, body)
}

#[test]
fn test_hof_apply_twice() {
    // A monomorphic higher-order function; `f` is a compile-time argument.
    let src = "(fn inc ((n : s32)) : s32 (i32.add n (i32.const 1)))
(fn apply-twice ((f : (-> s32 s32)) (x : s32)) : s32 (f (f x)))
(export (fn test-func () s32 (apply-twice inc (i32.const 40))))";
    assert_eq!(compile_and_run(src), 42);
}

#[test]
fn test_hof_map_inc_first() {
    // map inc [10,20] = [11,21]; first element is 11.
    let src = with_hof("(export (fn test-func () s32 (list-get (map inc (build)) (i32.const 0))))");
    assert_eq!(compile_and_run(&src), 11);
}

#[test]
fn test_hof_map_inc_last() {
    let src = with_hof("(export (fn test-func () s32 (list-get (map inc (build)) (i32.const 1))))");
    assert_eq!(compile_and_run(&src), 21);
}

#[test]
fn test_hof_map_generic_function_argument() {
    // The function argument may itself be generic: `double` is a generic fn.
    // map double [10,20] = [20,40]; first element is 20.
    let src =
        with_hof("(export (fn test-func () s32 (list-get (map double (build)) (i32.const 0))))");
    assert_eq!(compile_and_run(&src), 20);
}

#[test]
fn test_hof_map_f64() {
    // The same map monomorphizes at f64 with a f64 function argument.
    let src = with_hof(
        "(fn inc-f ((x : f64)) : f64 (f64.add x (f64.const 1.0)))
(export (fn test-func () s32
  (let (xs (list-push (list-new f64) (f64.const 41.0)))
    (if (f64.eq (list-get (map inc-f xs) (i32.const 0)) (f64.const 42.0))
      (i32.const 1) (i32.const 0)))))",
    );
    assert_eq!(compile_and_run(&src), 1);
}

// === Multiple type parameters ================================================

#[test]
fn test_multi_param_from_values() {
    // Two type parameters, each inferred from a value argument.
    let src = "(fn second ((a : T) (b : U)) : U (where T U) b)
(export (fn test-func () s32 (second (f64.const 1.0) (i32.const 42))))";
    assert_eq!(compile_and_run(src), 42);
}

#[test]
fn test_type_changing_map() {
    // map : (list T) -> (list U). T is solved from the list; U from the function
    // argument's return type. Here s32 -> f64.
    let src = "(fn map-go ((f : (-> T U)) (xs : (list T)) (i : s32) (acc : (list U))) : (list U)
  (where T U)
  (if (i32.lt_s i (list-len xs))
      (map-go f xs (i32.add i (i32.const 1)) (list-push acc (f (list-get xs i))))
      acc))
(fn map ((f : (-> T U)) (xs : (list T))) : (list U)
  (where T U)
  (map-go f xs (i32.const 0) (list-new U)))
(fn to-f ((n : s32)) : f64 (f64.convert_i32_s n))
(fn build () : (list s32)
  (list-push (list-push (list-new s32) (i32.const 10)) (i32.const 20)))
(export (fn test-func () s32
  (if (f64.eq (list-get (map to-f (build)) (i32.const 1)) (f64.const 20.0))
    (i32.const 1) (i32.const 0))))";
    assert_eq!(compile_and_run(src), 1);
}

// === Multi-parameter traits ==================================================

/// A two-parameter conversion trait with two instances that share the input type,
/// so the *expected* type must choose between them.
const CONVERT_DEFS: &str = r#"
(trait (Convert T U) (fn convert ((x : T)) : U))
(instance (Convert s32 f64) (fn convert ((x : s32)) : f64 (f64.convert_i32_s x)))
(instance (Convert s32 s64) (fn convert ((x : s32)) : s64 (i64.extend_i32_s x)))
"#;

#[test]
fn test_multi_trait_convert_to_f64() {
    // (convert 42) resolves to Convert s32 f64 because the context expects f64.
    let src = format!(
        "{}
(export (fn test-func () s32
  (if (f64.eq (convert (i32.const 42)) (f64.const 42.0)) (i32.const 1) (i32.const 0))))",
        CONVERT_DEFS
    );
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_multi_trait_convert_to_s64() {
    // The same call resolves to Convert s32 s64 in an s64 context.
    let src = format!(
        "{}
(export (fn test-func () s32
  (if (i64.eq (convert (i32.const 42)) (i64.const 42)) (i32.const 1) (i32.const 0))))",
        CONVERT_DEFS
    );
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_multi_trait_convert_ascription() {
    // An ascription supplies the output type directly.
    let src = format!(
        "{}
(export (fn test-func () s32 (i64.eq ((convert (i32.const 7)) : s64) (i64.const 7))))",
        CONVERT_DEFS
    );
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_multi_trait_ambiguous_is_rejected() {
    // With no expected type, U cannot be chosen between the two instances.
    let src = format!(
        "{}
(export (fn test-func () s32 (let (x (convert (i32.const 1))) (i32.const 0))))",
        CONVERT_DEFS
    );
    let err = compile_error(&src);
    assert!(err.contains("cannot resolve trait method"), "got: {err}");
}

#[test]
fn test_generic_constrained_by_multi_trait() {
    // A generic over (T, U) constrained by a multi-parameter trait.
    let src = format!(
        "{}
(fn widen ((x : T)) : U (where (Convert T U)) (convert x))
(export (fn test-func () s32
  (if (f64.eq (widen (i32.const 42)) (f64.const 42.0)) (i32.const 1) (i32.const 0))))",
        CONVERT_DEFS
    );
    assert_eq!(compile_and_run(&src), 1);
}

#[test]
fn test_hof_fold_with_trait_method() {
    // fold can take a trait method (+) as its function argument; (zero) is the seed.
    let src = with_hof(
        "(fn build3 () : (list s32)
  (list-push (list-push (list-push (list-new s32)
    (i32.const 10)) (i32.const 20)) (i32.const 30)))
(export (fn test-func () s32 (fold + (zero) (build3))))",
    );
    assert_eq!(compile_and_run(&src), 60);
}

// === Trait checker: bad programs must be rejected with a clear message =======

fn compile_error(source: &str) -> String {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_generics_err_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_generics_err_{}", test_id));
    std::fs::write(&source_path, source).expect("failed to write temp source");
    match compiler::compile(&source_path, &out_base, compiler::EmitOptions::default()) {
        Ok(_) => panic!("expected compilation to fail, but it succeeded"),
        Err(e) => format!("{:#}", e),
    }
}

#[test]
fn test_error_unknown_trait_in_instance() {
    let err = compile_error("(instance (Nope s32) (fn foo ((a : s32)) : s32 a))");
    assert!(err.contains("unknown trait"), "got: {err}");
}

#[test]
fn test_error_duplicate_instance() {
    let src = "(trait (Add T) (fn + ((a : T) (b : T)) : T))
(instance (Add s32) (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))
(instance (Add s32) (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))";
    let err = compile_error(src);
    assert!(err.contains("duplicate instance"), "got: {err}");
}

#[test]
fn test_error_missing_method() {
    // The trait declares two methods; the instance provides only one.
    let src = "(trait (Pair T) (fn lo ((a : T)) : T) (fn hi ((a : T)) : T))
(instance (Pair s32) (fn lo ((a : s32)) : s32 a))";
    let err = compile_error(src);
    assert!(err.contains("missing method"), "got: {err}");
}

#[test]
fn test_error_signature_mismatch() {
    // Instance method's return type does not match the trait signature.
    let src = "(trait (Id T) (fn id ((a : T)) : T))
(instance (Id s32) (fn id ((a : s32)) : s64 (i64.extend_i32_s a)))";
    let err = compile_error(src);
    assert!(
        err.contains("do not match") || err.contains("does not match"),
        "got: {err}"
    );
}

#[test]
fn test_error_unresolved_return_dispatch() {
    // (zero) sits in an unannotated let value: no argument, no expected type, and
    // two instances exist — so it cannot be resolved. (An expected type from a
    // wasm operand or an annotation would resolve it; that is tested elsewhere.)
    let src = "(trait (Zero T) (fn zero () : T))
(instance (Zero s32) (fn zero () : s32 (i32.const 0)))
(instance (Zero f64) (fn zero () : f64 (f64.const 0.0)))
(export (fn test-func () s32 (let (x (zero)) (i32.const 5))))";
    let err = compile_error(src);
    assert!(err.contains("cannot resolve trait method"), "got: {err}");
}
