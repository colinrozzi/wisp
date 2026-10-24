// Tests for the heap allocator (docs/proposals/MEMORY.md §5, stage 1): block
// headers, size classes, large buckets, reuse after free, memory growth, the
// `__granite_heap_stats` record, the debug heap, and the host protocol's
// `__pack_free` keeping an export's memory flat.

use granite::compiler;
use pack::abi::Value;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{
    Config, Engine, ExternType, Instance, Memory, Module, Store, StoreLimits, StoreLimitsBuilder,
    TypedFunc,
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
        let instance = Instance::new(&mut store, &module, &[]).unwrap();
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
        assert_eq!(h.rc(p), 1, "rc at birth");
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
    assert_eq!(h.rc(p as u32), 1);
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
