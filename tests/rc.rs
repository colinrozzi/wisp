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
    // global.get dups the freed string. (Free it after the call's buffers are
    // allocated, or one of them would reuse its block.)
    let r = g.try_call_after("s-len", 0, |g| {
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
    assert_trap(g.try_call("s-len", 0), "__granite_rc_on_raw_block");
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
