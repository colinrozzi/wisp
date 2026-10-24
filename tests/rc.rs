// Reference counting (MEMORY.md §3, §5.5, §7 stage 4), compiled with
// `EmitOptions::rc` and the debug heap: exact counts, the verifier's checks (each
// error made on purpose by the host and caught), and the use-after-free traps.
// tests/value_lists.rs has list-push's value semantics; tests/interpreter_engine.rs
// the Wisp session leak tests.

use granite::compiler;
use pack::abi::Value;
use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Memory, Module, Store, TypedFunc};

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

const RAW_RC: u32 = 0xABAB_ABAB;
const POISON: u32 = 0xDDDD_DDDD;

fn compile(source: &str, debug_heap: bool) -> Vec<u8> {
    let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let dir = std::env::temp_dir();
    let source_path = dir.join(format!("test_rc_{}_{id}.wisp", std::process::id()));
    let out_base = dir.join(format!("test_rc_{}_{id}", std::process::id()));
    std::fs::write(&source_path, source).unwrap();
    let emit = compiler::EmitOptions {
        rc: true,
        debug_heap,
        ..Default::default()
    };
    let wasm = compiler::compile(&source_path, &out_base, emit)
        .expect("compile failed")
        .wasm;
    std::fs::read(wasm).unwrap()
}

struct Guest {
    store: Store<()>,
    instance: Instance,
    memory: Memory,
}

impl Guest {
    fn new(wasm: &[u8]) -> Self {
        let mut config = Config::new();
        config.wasm_tail_call(true);
        let engine = Engine::new(&config).unwrap();
        let module = Module::new(&engine, wasm).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();
        let memory = instance.get_memory(&mut store, "memory").unwrap();
        Guest {
            store,
            instance,
            memory,
        }
    }

    fn u32_at(&self, addr: u32) -> u32 {
        let mut b = [0u8; 4];
        self.memory
            .read(&self.store, addr as usize, &mut b)
            .unwrap();
        u32::from_le_bytes(b)
    }

    fn set_u32(&mut self, addr: u32, v: u32) {
        self.memory
            .write(&mut self.store, addr as usize, &v.to_le_bytes())
            .unwrap();
    }

    fn rc(&self, p: u32) -> u32 {
        self.u32_at(p - 8)
    }

    fn set_rc(&mut self, p: u32, v: u32) {
        self.set_u32(p - 8, v);
    }

    fn func<P: wasmtime::WasmParams, R: wasmtime::WasmResults>(
        &mut self,
        name: &str,
    ) -> TypedFunc<P, R> {
        self.instance.get_typed_func(&mut self.store, name).unwrap()
    }

    fn live_blocks(&mut self) -> u32 {
        let stats: TypedFunc<(), i32> = self.func("__granite_heap_stats");
        let s = stats.call(&mut self.store, ()).unwrap() as u32;
        self.u32_at(s + 4)
    }

    /// Call export `name` with an s32 the way the packr host does; the s32 result
    /// or the trap.
    fn try_call(&mut self, name: &str, arg: i32) -> wasmtime::Result<i32> {
        self.try_call_after(name, arg, |_| {})
    }

    /// Like `try_call`, running `before` once the call's buffers are allocated.
    fn try_call_after(
        &mut self,
        name: &str,
        arg: i32,
        before: impl FnOnce(&mut Self),
    ) -> wasmtime::Result<i32> {
        let alloc: TypedFunc<i32, i32> = self.func("__pack_alloc");
        let free: TypedFunc<(i32, i32), ()> = self.func("__pack_free");
        let bytes = pack::encode(&Value::S32(arg)).unwrap();
        let in_ptr = alloc.call(&mut self.store, bytes.len() as i32)?;
        self.memory
            .write(&mut self.store, in_ptr as usize, &bytes)
            .unwrap();
        let slots = alloc.call(&mut self.store, 8)?;
        before(self);
        let f: TypedFunc<(i32, i32, i32, i32), i32> = self.func(name);
        f.call(
            &mut self.store,
            (in_ptr, bytes.len() as i32, slots, slots + 4),
        )?;
        free.call(&mut self.store, (in_ptr, bytes.len() as i32))?;
        let out = self.u32_at(slots as u32);
        let out_len = self.u32_at(slots as u32 + 4);
        let value =
            pack::decode(&self.memory.data(&self.store)[out as usize..(out + out_len) as usize])
                .unwrap();
        free.call(&mut self.store, (out as i32, out_len as i32))?;
        free.call(&mut self.store, (slots, 8))?;
        match value {
            Value::S32(n) => Ok(n),
            other => panic!("{name}: {other:?}"),
        }
    }

    fn call(&mut self, name: &str, arg: i32) -> i32 {
        self.try_call(name, arg).unwrap()
    }

    fn verify(&mut self) -> wasmtime::Result<i32> {
        let verify: TypedFunc<(), i32> = self.func("__granite_heap_verify");
        verify.call(&mut self.store, ())
    }
}

/// The trap's backtrace names the function that trapped.
fn assert_trap(r: wasmtime::Result<i32>, name: &str) {
    match r {
        Ok(v) => panic!("expected a trap in {name}, got {v}"),
        Err(e) => {
            let text = format!("{e:?}");
            assert!(text.contains(name), "expected {name} in:\n{text}");
        }
    }
}

/// A string `$s` held twice: by the global and by an element of `$xs`.
const SHARED: &str = r#"
(global $s string mut 0)
(global $xs (list string) mut 0)
(export (fn setup ((n s32)) s32
  (begin
    (global.set $s (string-append "ab" "cd"))
    (global.set $xs (list-push (list-push (list-new string) (global.get $s)) "literal"))
    0)))
(export (fn s-addr ((n s32)) s32 (string-addr (global.get $s))))
(export (fn s-len ((n s32)) s32 (string-len (global.get $s))))
(export (fn s-hold ((n s32)) s32 (list-len (list-push (list-new string) (global.get $s)))))
(export (fn reset-s ((n s32)) s32 (begin (global.set $s "x") 0)))
(export (fn reset-xs ((n s32)) s32 (begin (global.set $xs (list-new string)) 0)))
"#;

fn shared() -> (Guest, u32) {
    let mut g = Guest::new(&compile(SHARED, true));
    assert_eq!(g.call("setup", 0), 0);
    let p = g.call("s-addr", 0) as u32;
    (g, p)
}

#[test]
fn counts_are_exact() {
    let (mut g, p) = shared();
    assert_eq!(g.rc(p), 2, "the global and the list element");
    assert_eq!(g.call("s-len", 0), 4);
    assert_eq!(g.rc(p), 2, "a read takes its reference back");
    assert_eq!(g.call("reset-xs", 0), 0);
    assert_eq!(g.rc(p), 1, "the list and its element went");
    let blocks = g.live_blocks();
    assert_eq!(g.call("reset-s", 0), 0);
    assert_eq!(g.rc(p), POISON, "the last reference freed it");
    assert_eq!(g.live_blocks(), blocks - 1);
    assert_eq!(g.verify().unwrap(), 0);
}

#[test]
fn the_verifier_accepts_a_consistent_heap() {
    let (mut g, _) = shared();
    assert_eq!(g.verify().unwrap(), 0);
    // It puts every count back: verifying twice works, and counts are unchanged.
    assert_eq!(g.verify().unwrap(), 0);
    let p = g.call("s-addr", 0) as u32;
    assert_eq!(g.rc(p), 2);
}

#[test]
fn the_verifier_finds_a_count_too_high() {
    let (mut g, p) = shared();
    g.set_rc(p, 3);
    assert_trap(g.verify(), "__granite_verify_count_high");
}

#[test]
fn the_verifier_finds_a_count_too_low() {
    let (mut g, p) = shared();
    g.set_rc(p, 1);
    assert_trap(g.verify(), "__granite_verify_count_low");
}

#[test]
fn the_verifier_finds_a_leak() {
    let (mut g, _) = shared();
    // A block that looks counted (rc 1) but nothing reaches.
    let alloc: TypedFunc<i32, i32> = g.func("__pack_alloc");
    let q = alloc.call(&mut g.store, 16).unwrap() as u32;
    assert_eq!(g.rc(q), RAW_RC);
    g.set_rc(q, 1);
    assert_trap(g.verify(), "__granite_verify_leak");
}

#[test]
fn the_verifier_finds_a_dangling_reference() {
    let (mut g, p) = shared();
    // Free the string behind the program's back: both references now dangle.
    let free: TypedFunc<(i32, i32), ()> = g.func("__pack_free");
    free.call(&mut g.store, (p as i32, 4)).unwrap();
    assert_trap(g.verify(), "__granite_verify_dangling");
}

#[test]
fn every_export_call_is_verified_in_debug_builds() {
    let (mut g, p) = shared();
    g.set_rc(p, 3);
    // Any export call ends with the verifier: this one finds the bad count.
    assert_trap(g.try_call("s-len", 0), "__granite_verify_count_high");
}

#[test]
fn a_freed_object_traps_when_used() {
    let (mut g, p) = shared();
    // Storing $s in a list takes a reference: a dup of the freed string. (A read
    // like string-len takes none; the verifier at the end of the call finds the
    // dangling global instead.) Free it after the call's buffers are allocated,
    // or one of them would reuse its block.
    let r = g.try_call_after("s-hold", 0, |g| {
        let free: TypedFunc<(i32, i32), ()> = g.func("__pack_free");
        free.call(&mut g.store, (p as i32, 4)).unwrap();
    });
    assert_trap(r, "__granite_use_after_free");
}

#[test]
fn a_drop_below_zero_traps() {
    let (mut g, p) = shared();
    g.set_rc(p, 0);
    // global.set drops the old value of $s.
    assert_trap(g.try_call("reset-s", 0), "__granite_rc_underflow");
}

#[test]
fn a_counted_operation_on_a_raw_block_traps() {
    let (mut g, p) = shared();
    g.set_rc(p, RAW_RC);
    assert_trap(g.try_call("s-hold", 0), "__granite_rc_on_raw_block");
}

/// Release build: objects are freed when their last reference goes, so an export
/// that builds and drops structure keeps memory flat however often it runs.
#[test]
fn garbage_is_freed_in_release_builds() {
    let source = r#"
(record point (x s32) (name string))
(variant tree (leaf s32) (node tree tree))
(fn build ((d s32)) tree (if (i32.eq d 0) (leaf d) (node (build (i32.sub d 1)) (build (i32.sub d 1)))))
(fn size ((t tree)) s32 (match t ((leaf v) 1) ((node l r) (i32.add (size l) (size r)))))
(fn names ((n s32) (acc (list point))) (list point)
  (if (i32.eq n 0) acc
    (names (i32.sub n 1) (list-push acc (point n (string-append "p" "q"))))))
(export (fn work ((n s32)) s32
  (i32.add (size (build n)) (list-len (names (i32.mul n 10) (list-new point))))))
"#;
    for debug in [false, true] {
        let mut g = Guest::new(&compile(source, debug));
        assert_eq!(g.call("work", 6), 64 + 60);
        let blocks = g.live_blocks();
        for _ in 0..1000 {
            assert_eq!(g.call("work", 6), 64 + 60);
        }
        assert_eq!(g.live_blocks(), blocks, "debug={debug}");
    }
}

impl Guest {
    fn total_bytes(&mut self) -> u64 {
        let stats: TypedFunc<(), i32> = self.func("__granite_heap_stats");
        let s = stats.call(&mut self.store, ()).unwrap() as u32;
        self.u32_at(s + 16) as u64 | (self.u32_at(s + 20) as u64) << 32
    }
}

/// Bytes `export` allocates when called once with `n`.
fn bytes_for(source: &str, export: &str, n: i32, expect: i32) -> u64 {
    let mut g = Guest::new(&compile(source, false));
    let before = g.total_bytes();
    assert_eq!(g.call(export, n), expect);
    g.total_bytes() - before
}

/// MEMORY.md §7 stage 5: a list-push accumulator threaded through a loop has a
/// count of 1 at every push, so 100,000 pushes never copy the list: the bytes
/// allocated are the doubling buffers (under twice the final buffer) plus the
/// call's own small blocks, not the quadratic total a copy per push would cost.
#[test]
fn a_threaded_accumulator_never_copies() {
    let source = r#"
(fn fill ((xs (list s32)) (i s32) (n s32)) (list s32)
  (if (i32.lt_s i n) (fill (list-push xs i) (i32.add i 1) n) xs))
(export (fn push-n ((n s32)) s32 (list-len (fill (list-new s32) 0 n))))
"#;
    let bytes = bytes_for(source, "push-n", 100_000, 100_000);
    assert!(
        bytes < 2 * 4 * 131_072 + 4096,
        "{bytes} bytes for 100,000 pushes"
    );
}

/// MEMORY.md §3.5, the take rule: `(global.set $g (list-push (global.get $g) x))`
/// moves the list out of the global instead of taking a second reference, so the
/// push writes in place. The same holds when the global goes to an owned
/// parameter of a function that does not touch the global itself.
#[test]
fn pushes_onto_a_global_do_not_copy() {
    let direct = r#"
(global $xs (list s32) mut 0)
(fn fill ((i s32) (n s32)) s32
  (if (i32.lt_s i n)
    (begin (global.set $xs (list-push (global.get $xs) i)) (fill (i32.add i 1) n))
    (list-len (global.get $xs))))
(export (fn push-n ((n s32)) s32 (begin (global.set $xs (list-new s32)) (fill 0 n))))
"#;
    let through_call = r#"
(global $xs (list s32) mut 0)
(fn add ((xs (list s32)) (x s32)) (list s32) (list-push xs x))
(fn fill ((i s32) (n s32)) s32
  (if (i32.lt_s i n)
    (begin (global.set $xs (add (global.get $xs) i)) (fill (i32.add i 1) n))
    (list-len (global.get $xs))))
(export (fn push-n ((n s32)) s32 (begin (global.set $xs (list-new s32)) (fill 0 n))))
"#;
    for source in [direct, through_call] {
        let bytes = bytes_for(source, "push-n", 20_000, 20_000);
        assert!(
            bytes < 2 * 4 * 32_768 + 4096,
            "{bytes} bytes for 20,000 pushes"
        );
    }
    // A global the pushed value reads cannot be moved out: still correct.
    let reads = r#"
(global $xs (list s32) mut 0)
(export (fn push-len ((n s32)) s32
  (begin
    (global.set $xs (list-push (list-new s32) 7))
    (global.set $xs (list-push (global.get $xs) (list-len (global.get $xs))))
    (i32.add (i32.mul (list-len (global.get $xs)) 10) (list-get (global.get $xs) 1)))))
"#;
    bytes_for(reads, "push-len", 0, 21);
}

/// MEMORY.md §3.4: `string-append` writes a left string held only by the
/// append into spare room in its block, and a new string gets half again the
/// room it needs, so left-fold string building is O(n): 10,000 one-byte appends
/// allocate a few times the final length, not the ~50 MB copying would.
#[test]
fn left_fold_string_building_is_linear() {
    let source = r#"
(fn build ((s string) (i s32) (n s32)) string
  (if (i32.lt_s i n) (build (string-append s "x") (i32.add i 1) n) s))
(export (fn build-n ((n s32)) s32 (string-len (build "" 0 n))))
"#;
    let bytes = bytes_for(source, "build-n", 10_000, 10_000);
    assert!(bytes < 200_000, "{bytes} bytes for 10,000 appends");
    for debug in [false, true] {
        let mut g = Guest::new(&compile(source, debug));
        assert_eq!(g.call("build-n", 3000), 3000);
        assert_eq!(g.verify_if(debug), 0);
    }
}

impl Guest {
    fn verify_if(&mut self, debug: bool) -> i32 {
        if debug { self.verify().unwrap() } else { 0 }
    }
}

/// MEMORY.md §3.4, drop-guided reuse: a match on a uniquely held variant whose
/// arm rebuilds the same variant writes the new case into the old block, so
/// mapping over a 1,000-link chain allocates nothing new. A shared chain is
/// copied instead, and both versions stay intact.
#[test]
fn a_unique_match_reuses_its_block() {
    let source = r#"
(variant chain (end) (link s32 chain))
(fn make ((n s32) (acc chain)) chain (if (i32.eq n 0) acc (make (i32.sub n 1) (link n acc))))
(fn incr ((c chain)) chain (match c ((end) (end)) ((link v rest) (link (i32.add v 1) (incr rest)))))
(fn total ((c chain) (acc s32)) s32 (match c ((end) acc) ((link v rest) (total rest (i32.add acc v)))))
(global $keep chain mut 0)
(export (fn map-unique ((n s32)) s32 (total (incr (make n (end))) 0)))
(export (fn map-shared ((n s32)) s32
  (begin
    (global.set $keep (make n (end)))
    (i32.sub (total (incr (global.get $keep)) 0) (total (global.get $keep) 0)))))
"#;
    for debug in [false, true] {
        let mut g = Guest::new(&compile(source, debug));
        // sum of 2..=1001
        assert_eq!(g.call("map-unique", 1000), 501_500);
        assert_eq!(g.call("map-shared", 1000), 1000);
        assert_eq!(g.verify_if(debug), 0);
    }
    // make allocates 1,001 blocks; incr reuses every one of them.
    let make_only = bytes_for(
        &source.replace(
            "(total (incr (make n (end))) 0)",
            "(total (make n (end)) 0)",
        ),
        "map-unique",
        1000,
        500_500,
    );
    let mapped = bytes_for(source, "map-unique", 1000, 501_500);
    assert!(
        mapped < make_only + 2048,
        "make {make_only} bytes, make+incr {mapped}"
    );
}
