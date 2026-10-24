// Tests for amortized `list-push` (docs/proposals/MEMORY.md, stage 0a): the
// data buffer grows by doubling, so n pushes allocate O(n) bytes, and push
// still updates the list in place. Allocation is measured as the growth of
// live heap bytes (`__granite_heap_stats`) across the call: compiled code does
// not free yet, so that is every byte the call allocated.

use granite::compiler;
use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store, TypedFunc};

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Compile `source`, call its exported `test-func`, and return the s32 result
/// and the bytes the call allocated.
fn run_measured(source: &str) -> (i32, i64) {
    run_measured_rc(source, true)
}

/// `run_measured` with reference counting on or off.
fn run_measured_rc(source: &str, rc: bool) -> (i32, i64) {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_list_push_{}.wisp", test_id));
    let out_base = temp_dir.join(format!("test_list_push_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    let emit = compiler::EmitOptions {
        rc,
        ..Default::default()
    };
    compiler::compile(&source_path, &out_base, emit).expect("failed to compile");
    let wasm_bytes = std::fs::read(out_base.with_extension("wasm")).expect("failed to read wasm");

    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("failed to create engine");
    let module = Module::new(&engine, &wasm_bytes).expect("failed to create module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("failed to instantiate");

    let alloc: TypedFunc<i32, i32> = instance
        .get_typed_func(&mut store, "__pack_alloc")
        .expect("__pack_alloc not exported");
    let func: TypedFunc<(i32, i32, i32, i32), i32> = instance
        .get_typed_func(&mut store, "test-func")
        .expect("function 'test-func' not found");
    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("memory not found");

    let stats: TypedFunc<(), i32> = instance
        .get_typed_func(&mut store, "__granite_heap_stats")
        .expect("__granite_heap_stats not exported");
    let live_bytes = |store: &mut Store<()>| {
        let s = stats.call(&mut *store, ()).unwrap() as usize;
        let mut b = [0u8; 4];
        memory.read(&*store, s, &mut b).unwrap();
        u32::from_le_bytes(b) as i64
    };

    let slots = alloc.call(&mut store, 8).unwrap();
    let before = live_bytes(&mut store);
    func.call(&mut store, (0, 0, slots, slots + 4))
        .expect("call failed");
    let after = live_bytes(&mut store);

    let mut buf = [0u8; 4];
    memory.read(&store, slots as usize, &mut buf).unwrap();
    let out_ptr = i32::from_le_bytes(buf);
    memory
        .read(&store, (out_ptr + 24) as usize, &mut buf)
        .unwrap();
    (i32::from_le_bytes(buf), after - before)
}

/// The in-place tests below compile without reference counting (`--no-rc`),
/// whose list-push writes in place; GRANITE_RC=1 forces RC on for every compile.
fn rc_forced_on() -> bool {
    std::env::var_os("GRANITE_RC").is_some_and(|v| v != "0" && !v.is_empty())
}

const N: i64 = 100_000;

/// Push 0..N into a list of `ty`, then return 1 if every element reads back.
fn push_n_program(ty: &str, cast: &str) -> String {
    format!(
        "(fn fill ((xs (list {ty})) (i s32) (n s32)) (list {ty})
  (if (i32.lt_s i n) (fill (list-push xs ({cast} i)) (i32.add i 1) n) xs))
(fn check ((xs (list {ty})) (i s32)) s32
  (if (i32.ge_s i (list-len xs)) (i32.const 1)
    (if (i32.eq (s32 (list-get xs i)) i) (check xs (i32.add i 1)) (i32.const 0))))
(fn test-func () s32
  (let (xs (fill (list-new {ty}) (i32.const 0) (i32.const {N})))
    (if (i32.eq (list-len xs) (i32.const {N})) (check xs (i32.const 0)) (i32.const 0))))
(export test-func)"
    )
}

fn assert_linear(ty: &str, cast: &str, elem_size: i64) {
    let (ok, allocated) = run_measured(&push_n_program(ty, cast));
    assert_eq!(ok, 1, "list<{ty}>: elements did not read back");
    // Doubling allocates at most 2 * next_pow2(N) slots of data in total, plus
    // the header and the export's output buffer. Re-allocating on every push
    // would need about N^2/2 slots (tens of GB).
    let bound = 4 * N * elem_size + 4096;
    assert!(
        allocated <= bound,
        "list<{ty}>: {N} pushes allocated {allocated} bytes, want <= {bound}"
    );
}

#[test]
fn push_100k_s32_allocates_linear() {
    assert_linear("s32", "s32", 4);
}

#[test]
fn push_100k_s64_allocates_linear() {
    assert_linear("s64", "s64", 8);
}

#[test]
fn push_100k_f64_allocates_linear() {
    assert_linear("f64", "f64", 8);
}

#[test]
fn push_is_still_in_place() {
    if rc_forced_on() {
        return; // GRANITE_RC=1: pushes copy (tests/value_lists.rs)
    }
    // Today's semantics: push mutates the header, so every alias sees it.
    let (result, _) = run_measured_rc(
        "(fn test-func () s32
  (let (a (list-new s32))
    (let (b (list-push a (i32.const 7)))
      (let (c (list-push b (i32.const 8)))
        (i32.add (i32.mul (list-len a) (i32.const 100)) (list-get a 1))))))
(export test-func)",
        false,
    );
    assert_eq!(result, 208);
}

#[test]
fn push_value_that_pushes_same_list() {
    if rc_forced_on() {
        return; // GRANITE_RC=1: pushes copy (tests/value_lists.rs)
    }
    // The value expression pushes into the same list first; the outer push
    // must read the header after that, landing at index 1.
    let (result, _) = run_measured_rc(
        "(fn test-func () s32
  (let (a (list-new s32))
    (let (b (list-push a (list-len (list-push a (i32.const 5)))))
      (i32.add (i32.mul (list-len a) (i32.const 100))
        (i32.add (i32.mul (list-get a 0) (i32.const 10)) (list-get a 1))))))
(export test-func)",
        false,
    );
    assert_eq!(result, 251);
}
