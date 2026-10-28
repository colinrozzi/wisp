// Tests for the heap allocator (docs/proposals/MEMORY.md §5, stage 1): block
// headers, size classes, large buckets, reuse after free, memory growth, the
// `__granite_heap_stats` record, the debug heap, and the host protocol's
// `__pack_free` keeping an export's memory flat; and stage 2's frees at obvious
// owners (import buffers, raw-invoke, list-push growth, heap-free).

use granite::compiler;
use pack::abi::{Value, ValueType};
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{
    Caller, Config, Engine, ExternType, Instance, Linker, Memory, Module, Store, StoreLimits,
    StoreLimitsBuilder, TypedFunc,
};

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

const SMALL_CLASSES: [u32; 23] = [
    16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 448, 512, 640, 768,
    896, 1024,
];
const NUM_CLASSES: usize = 44;

/// The block size the allocator should pick for `n` payload bytes.
fn expected_block(n: u32) -> u32 {
    let b = n + 8;
    match SMALL_CLASSES.iter().find(|&&c| c >= b) {
        Some(&c) => c,
        None => b.next_power_of_two().max(2048),
    }
}

/// The class index of a block size.
fn class_of_block(b: u32) -> usize {
    match SMALL_CLASSES.iter().position(|&c| c == b) {
        Some(k) => k,
        None => SMALL_CLASSES.len() + (b.trailing_zeros() - 11) as usize,
    }
}

fn compile(source: &str, debug_heap: bool) -> Vec<u8> {
    let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let dir = std::env::temp_dir();
    let source_path = dir.join(format!("test_heap_{}_{}.wisp", std::process::id(), id));
    let out_base = dir.join(format!("test_heap_{}_{}", std::process::id(), id));
    std::fs::write(&source_path, source).unwrap();
    let emit = compiler::EmitOptions {
        debug_heap,
        ..Default::default()
    };
    let wasm = compiler::compile(&source_path, &out_base, emit)
        .expect("compile failed")
        .wasm;
    std::fs::read(wasm).unwrap()
}

/// A program with a few exports for the boundary tests.
const PROGRAM: &str = r#"
(export (fn inc ((x s32)) s32 (i32.add x (i32.const 1))))
(export (fn greet ((x s32)) string "hello"))
(export (fn alloc-words ((n s32)) s32 (heap-alloc n)))
"#;

#[derive(Debug, Clone, Copy, PartialEq)]
struct Stats {
    live_bytes: u32,
    live_blocks: u32,
    frontier: u32,
    pages: u32,
    total_bytes: u64,
    heap_start: u32,
}

struct Heap {
    store: Store<StoreLimits>,
    instance: Instance,
    memory: Memory,
    alloc: TypedFunc<i32, i32>,
    free: TypedFunc<(i32, i32), ()>,
    stats: TypedFunc<(), i32>,
}

impl Heap {
    fn new(wasm: &[u8]) -> Self {
        Self::with_limit(wasm, usize::MAX)
    }

    /// Instantiate with the memory capped at `max_bytes`, so a grow past it fails.
    fn with_limit(wasm: &[u8], max_bytes: usize) -> Self {
        let mut config = Config::new();
        config.wasm_tail_call(true);
        let engine = Engine::new(&config).unwrap();
        let module = Module::new(&engine, wasm).unwrap();
        let limits = StoreLimitsBuilder::new().memory_size(max_bytes).build();
        let mut store = Store::new(&engine, limits);
        store.limiter(|l| l);
        let mut linker = Linker::new(&engine);
        add_test_host(&mut linker);
        let instance = linker.instantiate(&mut store, &module).unwrap();
        let memory = instance.get_memory(&mut store, "memory").unwrap();
        let alloc = instance.get_typed_func(&mut store, "__pack_alloc").unwrap();
        let free = instance.get_typed_func(&mut store, "__pack_free").unwrap();
        let stats = instance
            .get_typed_func(&mut store, "__granite_heap_stats")
            .unwrap();
        Heap {
            store,
            instance,
            memory,
            alloc,
            free,
            stats,
        }
    }

    fn alloc(&mut self, n: u32) -> u32 {
        self.alloc.call(&mut self.store, n as i32).unwrap() as u32
    }

    fn try_alloc(&mut self, n: u32) -> wasmtime::Result<i32> {
        self.alloc.call(&mut self.store, n as i32)
    }

    fn free(&mut self, p: u32, len: u32) {
        self.free
            .call(&mut self.store, (p as i32, len as i32))
            .unwrap();
    }

    fn u32_at(&self, addr: u32) -> u32 {
        let mut b = [0u8; 4];
        self.memory
            .read(&self.store, addr as usize, &mut b)
            .unwrap();
        u32::from_le_bytes(b)
    }

    fn rc(&self, p: u32) -> u32 {
        self.u32_at(p - 8)
    }

    fn block_size(&self, p: u32) -> u32 {
        self.u32_at(p - 4)
    }

    fn stats_addr(&mut self) -> u32 {
        self.stats.call(&mut self.store, ()).unwrap() as u32
    }

    fn stats(&mut self) -> Stats {
        let s = self.stats_addr();
        let total = self.u32_at(s + 16) as u64 | (self.u32_at(s + 20) as u64) << 32;
        Stats {
            live_bytes: self.u32_at(s),
            live_blocks: self.u32_at(s + 4),
            frontier: self.u32_at(s + 8),
            pages: self.u32_at(s + 12),
            total_bytes: total,
            heap_start: self.u32_at(s + 24),
        }
    }

    fn live_count(&mut self, class: usize) -> u32 {
        let s = self.stats_addr();
        assert_eq!(self.u32_at(s + 28), NUM_CLASSES as u32);
        self.u32_at(s + 32 + 4 * class as u32)
    }

    /// Call export `name` the way the packr host does: input from __pack_alloc,
    /// freed after the call; the output read, then freed.
    fn call(&mut self, name: &str, input: Value) -> Value {
        let bytes = pack::encode(&input).unwrap();
        let in_ptr = self.alloc(bytes.len() as u32);
        self.memory
            .write(&mut self.store, in_ptr as usize, &bytes)
            .unwrap();
        let slots = self.alloc(8);
        let f: TypedFunc<(i32, i32, i32, i32), i32> =
            self.instance.get_typed_func(&mut self.store, name).unwrap();
        let status = f
            .call(
                &mut self.store,
                (
                    in_ptr as i32,
                    bytes.len() as i32,
                    slots as i32,
                    slots as i32 + 4,
                ),
            )
            .unwrap();
        assert_eq!(status, 0);
        self.free(in_ptr, bytes.len() as u32);
        let out_ptr = self.u32_at(slots);
        let out_len = self.u32_at(slots + 4);
        let value = pack::decode(
            &self.memory.data(&self.store)[out_ptr as usize..(out_ptr + out_len) as usize],
        )
        .unwrap();
        self.free(out_ptr, out_len);
        self.free(slots, 8);
        value
    }
}

/// Where a test host import puts its result (MEMORY.md §4).
#[derive(Clone, Copy)]
enum Reply {
    /// In a buffer from the guest's `__pack_alloc`; status 1, the guest frees it.
    GuestOwned,
    /// In the host's fixed scratch area at 0x4008; status 0, the guest must not.
    Scratch,
}

/// Host imports for the stage 2 tests, with packr's raw calling convention
/// `(in_ptr, in_len, out_ptr_ptr, out_len_ptr) -> status`:
/// `host.bump(x) = x + 1` and `host.echo(s) = s` reply in a guest-owned buffer,
/// `host.bump-scratch(x) = x + 1` in the scratch area.
fn add_test_host(linker: &mut Linker<StoreLimits>) {
    fn bump(v: Value) -> Value {
        match v {
            Value::S32(x) => Value::S32(x + 1),
            other => panic!("bump: {other:?}"),
        }
    }
    type HostFn = fn(Value) -> Value;
    let imports: [(&str, Reply, HostFn); 3] = [
        ("bump", Reply::GuestOwned, bump),
        ("bump-scratch", Reply::Scratch, bump),
        ("echo", Reply::GuestOwned, |v| v),
    ];
    for (name, reply, f) in imports {
        linker
            .func_wrap(
                "host",
                name,
                move |mut caller: Caller<'_, StoreLimits>,
                      in_ptr: i32,
                      in_len: i32,
                      out_ptr_ptr: i32,
                      out_len_ptr: i32|
                      -> i32 {
                    let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
                    let input = {
                        let data = memory.data(&caller);
                        pack::decode(&data[in_ptr as usize..(in_ptr + in_len) as usize]).unwrap()
                    };
                    let bytes = pack::encode(&f(input)).unwrap();
                    let (ptr, status) = match reply {
                        Reply::GuestOwned => {
                            let alloc = caller
                                .get_export("__pack_alloc")
                                .unwrap()
                                .into_func()
                                .unwrap()
                                .typed::<i32, i32>(&caller)
                                .unwrap();
                            (alloc.call(&mut caller, bytes.len() as i32).unwrap(), 1)
                        }
                        Reply::Scratch => (0x4008, 0),
                    };
                    memory.write(&mut caller, ptr as usize, &bytes).unwrap();
                    memory
                        .write(&mut caller, out_ptr_ptr as usize, &ptr.to_le_bytes())
                        .unwrap();
                    memory
                        .write(
                            &mut caller,
                            out_len_ptr as usize,
                            &(bytes.len() as i32).to_le_bytes(),
                        )
                        .unwrap();
                    status
                },
            )
            .unwrap();
    }
}

fn trap_text(e: wasmtime::Error) -> String {
    format!("{e:?}")
}

#[test]
fn every_small_class() {
    let mut h = Heap::new(&compile(PROGRAM, false));
    for n in 0..=1016 {
        let p = h.alloc(n);
        let b = expected_block(n);
        assert_eq!(p % 8, 0, "payload of {n} bytes is not 8-aligned");
        assert_eq!(h.block_size(p), b, "block size for {n} bytes");
        assert_eq!(h.rc(p), raw_rc(false), "rc at birth");
    }
    // Every class got blocks: count each payload size's class.
    for (k, &c) in SMALL_CLASSES.iter().enumerate() {
        let want = (0..=1016u32).filter(|&n| expected_block(n) == c).count() as u32;
        assert_eq!(h.live_count(k), want, "live blocks in class {c}");
    }
}

#[test]
fn large_blocks_use_power_of_two_buckets() {
    let mut h = Heap::new(&compile(PROGRAM, false));
    for n in [
        1017,
        2040,
        2041,
        4088,
        4089,
        100_000,
        1 << 20,
        (1 << 24) - 8,
    ] {
        let b = expected_block(n);
        let before = h.live_count(class_of_block(b));
        let p = h.alloc(n);
        assert_eq!(h.block_size(p), b, "block size for {n} bytes");
        assert!(b.is_power_of_two() && b >= 2048);
        assert_eq!(h.live_count(class_of_block(b)), before + 1);
        // The whole payload is writable.
        h.memory
            .write(&mut h.store, (p + n - 1) as usize, &[7])
            .unwrap();
    }
}

#[test]
fn freed_blocks_are_reused() {
    let mut h = Heap::new(&compile(PROGRAM, false));
    for n in [0, 20, 100, 1000, 5000, 70_000] {
        let a = h.alloc(n);
        let b = h.alloc(n);
        let before = h.stats().frontier;
        h.free(a, n);
        h.free(b, n);
        // LIFO: the last block freed comes back first; neither carves new memory.
        assert_eq!(h.alloc(n), b);
        assert_eq!(h.alloc(n), a);
        assert_eq!(h.stats().frontier, before);
        // A size in the same class reuses the block too.
        h.free(a, n);
        assert_eq!(h.alloc(expected_block(n) - 8), a);
    }
    // A block is only reused by its own class.
    let a = h.alloc(16);
    h.free(a, 16);
    assert_ne!(h.alloc(40), a);
    assert_eq!(h.alloc(16), a);
}

#[test]
fn stats_track_live_blocks() {
    let mut h = Heap::new(&compile(PROGRAM, false));
    let s0 = h.stats();
    assert!(s0.heap_start > 0 && s0.frontier >= s0.heap_start);
    let a = h.alloc(100); // 112-byte block
    let b = h.alloc(3000); // 4 KiB block
    let s1 = h.stats();
    assert_eq!(s1.live_bytes - s0.live_bytes, 112 + 4096);
    assert_eq!(s1.live_blocks - s0.live_blocks, 2);
    assert_eq!(s1.total_bytes - s0.total_bytes, 112 + 4096);
    h.free(a, 100);
    h.free(b, 3000);
    let s2 = h.stats();
    assert_eq!(s2.live_bytes, s0.live_bytes);
    assert_eq!(s2.live_blocks, s0.live_blocks);
    assert_eq!(s2.total_bytes, s1.total_bytes);
    assert_eq!(s2.pages, (h.memory.data_size(&h.store) / 65536) as u32);
}

#[test]
fn pointers_below_the_heap_are_not_freed() {
    let mut h = Heap::new(&compile(PROGRAM, true));
    let s0 = h.stats();
    // Null, the host's scratch buffer, and static data are ignored, even in debug.
    h.free(0, 0);
    h.free(0x4008, 100);
    h.free(s0.heap_start - 16, 4);
    assert_eq!(h.stats(), s0);
}

#[test]
fn memory_grows_past_the_initial_size() {
    let mut h = Heap::new(&compile(PROGRAM, false));
    let initial = h.stats().pages;
    // The module declares a small memory with no maximum.
    assert!(initial <= 32, "initial memory is {initial} pages");
    let mut blocks = Vec::new();
    for i in 0..64u32 {
        let p = h.alloc(1 << 20);
        h.memory
            .write(&mut h.store, (p + (1 << 20) - 4) as usize, &i.to_le_bytes())
            .unwrap();
        blocks.push(p);
    }
    let pages = h.stats().pages;
    assert!(pages as u64 * 65536 >= 64 << 21, "{pages} pages");
    assert_eq!(pages, (h.memory.data_size(&h.store) / 65536) as u32);
    for (i, &p) in blocks.iter().enumerate() {
        assert_eq!(
            h.u32_at(p + (1 << 20) - 4),
            i as u32,
            "block {i} kept its data"
        );
    }
    // Growth is geometric: a run of small allocations grows the memory by at least
    // a quarter at a time, not a page at a time.
    let mut grows = 0;
    let mut last = h.stats().pages;
    for _ in 0..200_000 {
        h.alloc(1000);
        let now = h.stats().pages;
        if now != last {
            assert!(now - last >= last / 4, "grew {last} -> {now}");
            grows += 1;
            last = now;
        }
    }
    assert!(grows > 0 && grows < 10, "{grows} grows");
}

#[test]
fn failed_grow_traps_with_a_clear_message() {
    let wasm = compile(PROGRAM, false);
    let mut h = Heap::with_limit(&wasm, 8 << 20);
    let e = loop {
        if let Err(e) = h.try_alloc(1 << 20) {
            break e;
        }
    };
    assert!(trap_text(e).contains("__granite_out_of_memory"));
    // A request no block can hold traps the same way.
    let mut h = Heap::new(&wasm);
    let e = h.try_alloc(0x7FFF_FFF9).unwrap_err();
    assert!(trap_text(e).contains("__granite_out_of_memory"));
}

#[test]
fn debug_heap_poisons_and_traps() {
    let wasm = compile(PROGRAM, true);

    // Free fills the payload with 0xDD (past the free-list link) and poisons rc.
    let mut h = Heap::new(&wasm);
    let p = h.alloc(40);
    h.free(p, 40);
    assert_eq!(h.rc(p), 0xDDDD_DDDD);
    assert_eq!(h.u32_at(p + 4), 0xDDDD_DDDD);
    assert_eq!(h.u32_at(p + 36), 0xDDDD_DDDD);
    assert_eq!(h.block_size(p), 48);

    // A double free traps.
    let e = h.free.call(&mut h.store, (p as i32, 40)).unwrap_err();
    assert!(trap_text(e).contains("__granite_double_free"));

    // A __pack_free length larger than the block traps.
    let mut h = Heap::new(&wasm);
    let p = h.alloc(40);
    let e = h.free.call(&mut h.store, (p as i32, 41)).unwrap_err();
    assert!(trap_text(e).contains("__granite_bad_free"));

    // Freeing an address that is not a block traps.
    let mut h = Heap::new(&wasm);
    let p = h.alloc(40);
    let e = h.free.call(&mut h.store, (p as i32 + 16, 4)).unwrap_err();
    assert!(trap_text(e).contains("__granite_bad_free"));

    // Writing a free block's header is caught when the block is reused.
    let mut h = Heap::new(&wasm);
    let p = h.alloc(40);
    h.free(p, 40);
    h.memory
        .write(&mut h.store, (p - 8) as usize, &1u32.to_le_bytes())
        .unwrap();
    let e = h.try_alloc(40).unwrap_err();
    assert!(trap_text(e).contains("__granite_free_block_written"));
}

#[test]
fn release_heap_does_not_poison() {
    if std::env::var_os("GRANITE_DEBUG_HEAP").is_some() {
        return; // every compile builds the debug heap
    }
    let mut h = Heap::new(&compile(PROGRAM, false));
    let p = h.alloc(40);
    h.memory
        .write(&mut h.store, (p + 4) as usize, &[1, 2, 3, 4])
        .unwrap();
    h.free(p, 40);
    assert_eq!(h.rc(p), 1);
    assert_eq!(h.u32_at(p + 4), 0x0403_0201);
}

#[test]
fn compiled_allocations_have_headers() {
    let mut h = Heap::new(&compile(PROGRAM, true));
    // heap-alloc goes through $__alloc like every allocation site.
    let Value::S32(p) = h.call("alloc-words", Value::S32(100)) else {
        panic!()
    };
    assert_eq!(h.block_size(p as u32), 112);
    // A raw block is never counted; debug RC builds mark it for the verifier.
    assert_eq!(h.rc(p as u32), raw_rc(true));
}

/// The host frees every input and output buffer, so an export that allocates
/// nothing else runs in flat memory however often it is called.
#[test]
fn export_called_10000_times_keeps_memory_flat() {
    for debug in [false, true] {
        let mut h = Heap::new(&compile(PROGRAM, debug));
        for name in ["inc", "greet"] {
            for _ in 0..100 {
                h.call(name, Value::S32(1));
            }
            let warm = h.stats();
            for i in 0..10_000 {
                let v = h.call(name, Value::S32(i));
                if name == "inc" {
                    assert_eq!(v, Value::S32(i + 1));
                } else {
                    assert_eq!(v, Value::String("hello".into()));
                }
            }
            let after = h.stats();
            assert_eq!(after.live_bytes, warm.live_bytes, "{name}: live bytes");
            assert_eq!(after.live_blocks, warm.live_blocks, "{name}: live blocks");
            assert_eq!(after.frontier, warm.frontier, "{name}: frontier");
            assert_eq!(after.pages, warm.pages, "{name}: pages");
            assert!(after.total_bytes > warm.total_bytes);
        }
    }
}

/// The Wisp actor (committed `wisp/actor/actor.wasm`) declares a small, growable
/// memory instead of a fixed 2 GiB.
#[test]
fn actor_memory_is_growable() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("wisp/actor/actor.wasm");
    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_file(&engine, path).unwrap();
    let memory = module
        .exports()
        .find_map(|e| match e.ty() {
            ExternType::Memory(m) if e.name() == "memory" => Some(m),
            _ => None,
        })
        .expect("actor exports memory");
    assert_eq!(memory.maximum(), None);
    assert!(memory.minimum() <= 64, "{} initial pages", memory.minimum());
    assert!(module.exports().any(|e| e.name() == "__granite_heap_stats"));
}

// ---- Stage 2: frees at obvious owners ----

/// `GRANITE_RC` forces reference counting on (`1`) or off (`0`) for every
/// compile; unset, each compile's own option decides (on by default).
fn rc_forced() -> Option<bool> {
    std::env::var_os("GRANITE_RC")
        .filter(|v| !v.is_empty())
        .map(|v| v != "0")
}

/// Whether this file's compiles (default options) count references.
fn rc() -> bool {
    rc_forced().unwrap_or(true)
}

/// `rc` of a raw block (`__pack_alloc`, `heap-alloc`) at birth: 1, except in
/// debug RC builds, which mark raw blocks for the verifier (rc.rs).
fn raw_rc(debug_compile: bool) -> u32 {
    let debug = debug_compile
        || std::env::var_os("GRANITE_DEBUG_HEAP").is_some_and(|v| v != "0" && !v.is_empty());
    if rc() && debug { 0xABAB_ABAB } else { 1 }
}

/// The stage 2 program. Without RC the exports free what the imports return by
/// hand; with RC those objects are counted and freed when dropped, so freeing
/// them by hand would free them twice (MEMORY.md §3.6).
fn imports() -> String {
    let free = |e: &str| {
        if rc() {
            "0".to_string()
        } else {
            format!("(heap-free {e})")
        }
    };
    IMPORTS
        .replace("(heap-free (string-addr r))", &free("(string-addr r)"))
        .replace("(heap-free (any-addr arg))", &free("(any-addr arg)"))
        .replace("(heap-free (any-addr r))", &free("(any-addr r)"))
}

const IMPORTS: &str = r#"
(import host bump ((x s32)) s32)
(import host bump-scratch ((x s32)) s32)
(import host echo ((s string)) string)
(export (fn call-bump ((x s32)) s32 (bump x)))
(export (fn call-scratch ((x s32)) s32 (bump-scratch x)))
(export (fn call-echo ((x s32)) s32
  (let (r (echo "hello from the guest"))
    (let (n (string-len r))
      (begin (heap-free (string-addr r)) n)))))
(export (fn call-raw ((x s32)) s32
  (let (arg (any-s32 x))
    (let (r (raw-invoke "host" "bump" arg))
      (let (y (any-as-s32 r))
        (begin (heap-free (any-addr arg)) (heap-free (any-addr r)) y))))))
(export (fn push-n ((n s32)) s32
  (push-loop (list-new s32) (i32.const 0) n)))
(fn push-loop ((xs (list s32)) (i s32) (n s32)) s32
  (if (i32.ge_s i n) (list-len xs)
    (push-loop (list-push xs i) (i32.add i (i32.const 1)) n)))
(export (fn live-bytes ((x s32)) s32 (i32.load (heap-stats))))
(export (fn live-blocks ((x s32)) s32 (i32.load (i32.add (heap-stats) (i32.const 4)))))
(export (fn alloc-free ((n s32)) s32 (heap-free (heap-alloc n))))
"#;

/// Calling a host import frees its argument buffer and result slots, and the
/// result buffer when the host says the guest owns it, so 10,000 calls run in
/// flat memory. Under the debug heap a free before decoding would poison the
/// result, so the values also check the order.
#[test]
fn import_called_10000_times_keeps_memory_flat() {
    for debug in [false, true] {
        let mut h = Heap::new(&compile(&imports(), debug));
        for name in ["call-bump", "call-scratch", "call-echo", "call-raw"] {
            let expect = |i: i32| match name {
                "call-echo" => "hello from the guest".len() as i32,
                _ => i + 1,
            };
            for i in 0..100 {
                assert_eq!(h.call(name, Value::S32(i)), Value::S32(expect(i)));
            }
            let warm = h.stats();
            for i in 0..10_000 {
                assert_eq!(h.call(name, Value::S32(i)), Value::S32(expect(i)));
            }
            let after = h.stats();
            assert_eq!(after.live_bytes, warm.live_bytes, "{name}: live bytes");
            assert_eq!(after.live_blocks, warm.live_blocks, "{name}: live blocks");
            assert_eq!(after.frontier, warm.frontier, "{name}: frontier");
            assert_eq!(after.pages, warm.pages, "{name}: pages");
        }
    }
}

/// list-push frees the old data buffer when it grows: after n pushes only the
/// header and the last buffer are live, not every buffer on the way. With RC the
/// list itself is dropped when the export returns, so nothing stays live.
#[test]
fn list_push_frees_the_old_buffer() {
    for debug in [false, true] {
        let mut h = Heap::new(&compile(&imports(), debug));
        let before = h.stats();
        assert_eq!(h.call("push-n", Value::S32(10_000)), Value::S32(10_000));
        let after = h.stats();
        let (bytes, blocks) = if rc() {
            (0, 0)
        } else {
            (expected_block(12) + expected_block(4 * 10_000), 2)
        };
        assert_eq!(after.live_bytes - before.live_bytes, bytes);
        assert_eq!(after.live_blocks - before.live_blocks, blocks);
    }
}

/// heap-stats reads the same record as `__granite_heap_stats`, and heap-free
/// returns a block to its class.
#[test]
fn heap_stats_and_heap_free_from_granite() {
    let mut h = Heap::new(&compile(&imports(), true));
    // The export's own buffers are live while it runs: the input (16 bytes,
    // a 24-byte block) and the slots; the output comes after the read.
    let Value::S32(live) = h.call("live-bytes", Value::S32(0)) else {
        panic!()
    };
    let Value::S32(blocks) = h.call("live-blocks", Value::S32(0)) else {
        panic!()
    };
    let s = h.stats();
    assert!(live as u32 > s.live_bytes && blocks as u32 > s.live_blocks);
    let before = h.stats();
    for n in [8, 100, 5000] {
        assert_eq!(h.call("alloc-free", Value::S32(n)), Value::S32(0));
    }
    assert_eq!(h.stats().live_bytes, before.live_bytes);
}

// ---- Issue #89: outputs and arguments of any size ----

/// Exports whose results go through the CGRF encoder, and imports whose
/// arguments do. The encoder grows its buffer as it writes, so none of these
/// has a size limit.
const BIG: &str = r#"
(import host echo ((s string)) string)
(export (fn str-id ((s string)) string s))
(export (fn some-str ((s string)) (option string) (some string s)))
(export (fn ok-nums ((xs (list s32))) (result (list s32) s32) (ok (list s32) s32 xs)))
(export (fn pair-id ((value (tuple string s32))) (tuple string s32) value))
(export (fn strs-id ((xs (list string))) (list string) xs))
(export (fn nums-id ((xs (list s32))) (list s32) xs))
(export (fn via-host ((s string)) string (echo s)))
"#;

/// The host's `echo` takes any value, so the same import name can be declared
/// with a list type.
const BIG_LIST_IMPORT: &str = r#"
(import host echo ((xs (list string))) (list string))
(export (fn strs-via-host ((xs (list string))) (list string) (echo xs)))
"#;

/// `n` bytes of text that differ from position to position, so a lost or
/// moved byte shows.
fn text(n: usize, seed: usize) -> String {
    (0..n)
        .map(|i| (b'a' + ((i * 7 + seed) % 26) as u8) as char)
        .collect()
}

fn strings(count: usize, each: usize) -> Value {
    Value::List {
        elem_type: ValueType::String,
        items: (0..count).map(|i| Value::String(text(each, i))).collect(),
    }
}

/// Each export returns its 1 MiB value intact (32 times the old 32 KiB block),
/// in release and debug builds (the debug build verifies the heap after every
/// export call), and repeated calls leave live memory flat.
#[test]
fn large_values_round_trip_with_flat_memory() {
    const MIB: usize = 1 << 20;
    let s = || Value::String(text(MIB, 3));
    let nums = || Value::List {
        elem_type: ValueType::S32,
        items: (0..MIB as i32 / 4).map(Value::S32).collect(),
    };
    let cases: Vec<(&str, &str, Value, Value)> = vec![
        (BIG, "str-id", s(), s()),
        (
            BIG,
            "some-str",
            s(),
            Value::Option {
                inner_type: ValueType::String,
                value: Some(Box::new(s())),
            },
        ),
        // Not a string payload: the string encoder overwrites $enc_result_ptr,
        // so a result holding a string comes back as err (a separate bug).
        (
            BIG,
            "ok-nums",
            nums(),
            Value::Result {
                ok_type: ValueType::List(Box::new(ValueType::S32)),
                err_type: ValueType::S32,
                value: Ok(Box::new(nums())),
            },
        ),
        (
            BIG,
            "pair-id",
            Value::Tuple(vec![s(), Value::S32(-7)]),
            Value::Tuple(vec![s(), Value::S32(-7)]),
        ),
        (BIG, "strs-id", strings(1024, 1024), strings(1024, 1024)),
        (BIG, "strs-id", strings(65536, 4), strings(65536, 4)),
        (BIG, "nums-id", nums(), nums()),
        (BIG, "via-host", s(), s()),
        (
            BIG_LIST_IMPORT,
            "strs-via-host",
            strings(1024, 1024),
            strings(1024, 1024),
        ),
    ];
    for debug in [false, true] {
        let mut programs = [BIG, BIG_LIST_IMPORT].map(|p| (p, Heap::new(&compile(p, debug))));
        for (program, name, input, expect) in &cases {
            let h = &mut programs.iter_mut().find(|(p, _)| p == program).unwrap().1;
            for _ in 0..2 {
                assert!(
                    h.call(name, input.clone()) == *expect,
                    "{name} debug={debug}"
                );
            }
            let warm = h.stats();
            for _ in 0..3 {
                assert!(
                    h.call(name, input.clone()) == *expect,
                    "{name} debug={debug}"
                );
            }
            let after = h.stats();
            assert_eq!(after.live_bytes, warm.live_bytes, "{name} debug={debug}");
            assert_eq!(after.live_blocks, warm.live_blocks, "{name} debug={debug}");
        }
    }
}
