// Value-semantics `list-push` under reference counting (`EmitOptions::rc`,
// MEMORY.md §3.4, §7 stage 4): `(list-push xs v)` leaves `xs` unchanged, as
// `granite eval` always has, and still writes in place when the list's count is 1.

use granite::compiler::{self, Value};
use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store, TypedFunc};

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Compile `source` and call its nullary s32 export `test-func`; also return the bytes
/// the call allocated (`total_bytes` of `__granite_heap_stats`).
fn run_compiled(source: &str, rc: bool) -> (i32, u64) {
    let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let dir = std::env::temp_dir();
    let source_path = dir.join(format!("test_value_lists_{}_{id}.wisp", std::process::id()));
    let out_base = dir.join(format!("test_value_lists_{}_{id}", std::process::id()));
    std::fs::write(&source_path, source).unwrap();
    let emit = compiler::EmitOptions {
        rc,
        ..Default::default()
    };
    let wasm = compiler::compile(&source_path, &out_base, emit)
        .expect("compile failed")
        .wasm;

    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_file(&engine, wasm).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).unwrap();
    let alloc: TypedFunc<i32, i32> = instance.get_typed_func(&mut store, "__pack_alloc").unwrap();
    let stats: TypedFunc<(), i32> = instance
        .get_typed_func(&mut store, "__granite_heap_stats")
        .unwrap();
    let entry: TypedFunc<(i32, i32, i32, i32), i32> =
        instance.get_typed_func(&mut store, "test-func").unwrap();
    let memory = instance.get_memory(&mut store, "memory").unwrap();
    let total = |store: &mut Store<()>| {
        let s = stats.call(&mut *store, ()).unwrap() as usize;
        let mut b = [0u8; 8];
        memory.read(&*store, s + 16, &mut b).unwrap();
        u64::from_le_bytes(b)
    };

    let slots = alloc.call(&mut store, 8).unwrap();
    let before = total(&mut store);
    entry.call(&mut store, (0, 0, slots, slots + 4)).unwrap();
    let after = total(&mut store);
    let mut b = [0u8; 4];
    memory.read(&store, slots as usize, &mut b).unwrap();
    let out = i32::from_le_bytes(b) as usize;
    memory.read(&store, out + 24, &mut b).unwrap();
    (i32::from_le_bytes(b), after - before)
}

fn run_eval(source: &str) -> i32 {
    match compiler::eval_source(source).expect("eval failed") {
        Value::Int(n) => n as i32,
        other => panic!("eval: {other:?}"),
    }
}

/// With the flag on, compiled code agrees with `granite eval`; with it off it
/// still writes in place (`in_place` is that answer).
fn check(source: &str, value_semantics: i32, in_place: i32) {
    assert_eq!(run_eval(source), value_semantics, "granite eval\n{source}");
    assert_eq!(
        run_compiled(source, true).0,
        value_semantics,
        "rc\n{source}"
    );
    // GRANITE_RC=1 turns reference counting on for every compile.
    if std::env::var_os("GRANITE_RC").is_none_or(|v| v == "0" || v.is_empty()) {
        assert_eq!(
            run_compiled(source, false).0,
            in_place,
            "in place\n{source}"
        );
    }
}

#[test]
fn push_leaves_its_argument_unchanged() {
    // 10 * (list-len xs) + (list-len ys)
    check(
        "(export (fn test-func () s32
  (let (xs (list-push (list-new s32) 1))
    (let (ys (list-push xs 2))
      (i32.add (i32.mul (list-len xs) 10) (list-len ys))))))",
        12,
        22,
    );
}

#[test]
fn a_discarded_push_changes_nothing() {
    check(
        "(export (fn test-func () s32
  (let (xs (list-push (list-new s32) 1))
    (begin (list-push xs 2) (list-len xs)))))",
        1,
        2,
    );
}

/// The cycle from MEMORY.md §2.2: `b` holds `xs`, then `b` is pushed into `xs`.
/// In place that makes b -> xs -> b; with value semantics `b`'s list stays empty.
#[test]
fn pushing_a_holder_into_its_list_makes_no_cycle() {
    check(
        "(variant node (leaf s32) (branch (list node)))
(fn depth ((n node) (fuel s32)) s32
  (if (i32.eq fuel 0) 0
    (match n
      ((leaf v) 0)
      ((branch kids)
        (if (list-len kids) (i32.add 1 (depth (list-get kids 0) (i32.sub fuel 1))) 0)))))
(export (fn test-func () s32
  (let (xs (list-new node))
    (let (b (branch xs))
      (begin (list-push xs b) (depth b 1000))))))",
        0,
        1000,
    );
}

/// MEMORY.md §7 stage 4: the §2.2 program, using the list the push returns, gives
/// 1 (b's own list stays empty) instead of following the cycle to the fuel limit.
#[test]
fn the_cycle_program_returns_one() {
    check(
        "(variant node (leaf s32) (branch (list node)))
(fn depth ((n node) (fuel s32)) s32
  (if (i32.eq fuel 0) 0
    (match n
      ((leaf v) 0)
      ((branch kids)
        (if (list-len kids) (i32.add 1 (depth (list-get kids 0) (i32.sub fuel 1))) 0)))))
(export (fn test-func () s32
  (let (xs (list-new node))
    (let (b (branch xs))
      (depth (branch (list-push xs b)) 1000)))))",
        1,
        1000,
    );
}

/// The outer push already holds `xs` while its value runs, so the inner push (the
/// last use of `xs`) must not write into it.
#[test]
fn a_push_inside_the_pushed_value_copies() {
    // 100 * (list-len xs) + 10 * (list-len ys) + (list-get ys 1)
    check(
        "(export (fn test-func () s32
  (let (xs (list-push (list-new s32) 1))
    (let (ys (list-push xs (list-len (list-push xs 7))))
      (i32.add (i32.mul (list-len xs) 100)
        (i32.add (i32.mul (list-len ys) 10) (list-get ys 1)))))))",
        122,
        337,
    );
}

/// A `let` alias and a `match` binding both share their list with something
/// still live (count 2), so a push through them copies.
#[test]
fn pushes_through_aliases_copy() {
    check(
        "(export (fn test-func () s32
  (let (xs (list-push (list-new s32) 1))
    (let (ys xs)
      (begin (list-push ys 3) (list-len xs))))))",
        1,
        2,
    );
    check(
        "(variant holder (box (list s32)))
(export (fn test-func () s32
  (let (h (box (list-push (list-new s32) 1)))
    (begin
      (match h ((box l) (list-push l 5)))
      (match h ((box l) (list-len l)))))))",
        1,
        2,
    );
}

/// An accumulator threaded through a loop has a count of 1 at every push, so it
/// is still written in place: 100,000 pushes allocate O(n) bytes, not O(n^2).
#[test]
fn accumulators_still_push_in_place() {
    let source = "(fn fill ((xs (list s32)) (i s32) (n s32)) (list s32)
  (if (i32.lt_s i n) (fill (list-push xs i) (i32.add i 1) n) xs))
(export (fn test-func () s32 (list-len (fill (list-new s32) 0 100000))))";
    let (n, bytes) = run_compiled(source, true);
    assert_eq!(n, 100_000);
    assert!(bytes < 4 << 20, "{bytes} bytes for 100,000 pushes");
}
