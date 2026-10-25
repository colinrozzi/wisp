// The heap allocator the Pack code generator emits (docs/proposals/MEMORY.md §5):
// an 8-byte block header, size classes with free lists, power-of-two buckets for
// large blocks, a bump frontier, and geometric `memory.grow`.
//
// Every heap block starts with a header; the program only sees the payload after it,
// so payload layouts (and field offsets) are unchanged:
//
//   block + 0: rc   i32  reference count, 1 at birth (counted with RC on, rc.rs)
//   block + 4: size u32  block size in bytes, header included (the block's class)
//   block + 8: payload   <- the pointer `$__alloc` returns
//
// A free block keeps its header; the first payload word links it into its class's
// free list.
//
// The allocator's tables sit at `$__heap_base`, before the first block:
//
//   HEADS       free-list heads, one i32 per class
//   STATS       the record `__granite_heap_stats` returns (layout below)
//   CLASS_SIZE  block size of each class, one i32 per class
//   CLASS_OF    small class index by (block size - 1) / 8, one byte per entry
//   heap start  the first block; the frontier starts here

use super::rc::RAW_RC;

/// Block sizes of the small classes, header included (§5.2): at most 25% waste.
const SMALL_CLASSES: [u32; 23] = [
    16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 448, 512, 640, 768,
    896, 1024,
];
/// The largest small block. Larger blocks use power-of-two buckets (§5.3).
const SMALL_MAX: u32 = 1024;
/// Large buckets: 2 KiB, 4 KiB, ... 2 GiB, numbered after the small classes.
const LARGE_BUCKETS: u32 = 21;
const NUM_CLASSES: u32 = SMALL_CLASSES.len() as u32 + LARGE_BUCKETS;
/// The largest payload: it fills the 2 GiB bucket.
const MAX_PAYLOAD: u32 = (1 << 31) - BLOCK_HEADER;
pub(crate) const BLOCK_HEADER: u32 = 8;
/// `rc` of a freed block in debug builds; any use of it traps.
const POISON: u32 = 0xDDDD_DDDD;
/// The first heap chunk past the static data, in bytes (§5.4).
const INITIAL_HEAP: u32 = 1 << 20;

/// `__granite_heap_stats()` returns the address of this record:
///
///   +0  live_bytes   u32  bytes in live blocks, headers included
///   +4  live_blocks  u32
///   +8  frontier     u32  the next address the frontier carves
///   +12 pages        u32  memory size in 64 KiB pages
///   +16 total_bytes  u64  bytes of every block ever allocated, headers included
///   +24 heap_start   u32  the first block's address
///   +28 classes      u32  number of classes (the length of live_count)
///   +32 live_count   u32 × classes: live blocks per class (small classes, then
///                         the large buckets)
const STATS_LIVE_COUNT: u32 = 32;

/// Absolute addresses of the allocator's tables for one module.
struct HeapLayout {
    heads: u32,
    stats: u32,
    class_size: u32,
    class_of: u32,
    heap_start: u32,
}

impl HeapLayout {
    fn new(heap_base: u32) -> Self {
        let heads = heap_base;
        let stats = heads + 4 * NUM_CLASSES;
        let class_size = stats + STATS_LIVE_COUNT + 4 * NUM_CLASSES;
        let class_of = class_size + 4 * NUM_CLASSES;
        let heap_start = (class_of + SMALL_MAX / 8 + 15) & !15;
        HeapLayout {
            heads,
            stats,
            class_size,
            class_of,
            heap_start,
        }
    }

    fn live_count(&self) -> u32 {
        self.stats + STATS_LIVE_COUNT
    }
}

/// Block size of every class: the small classes, then the large buckets.
fn class_sizes() -> Vec<u32> {
    let mut sizes = SMALL_CLASSES.to_vec();
    sizes.extend((0..LARGE_BUCKETS).map(|b| 2048u32 << b));
    sizes
}

/// The small class whose blocks hold `payload` bytes, if one does.
fn small_class(payload: usize) -> Option<usize> {
    let block = payload.checked_add(BLOCK_HEADER as usize)?;
    SMALL_CLASSES.iter().position(|&c| c as usize >= block)
}

/// Allocate `size` payload bytes, a compile-time constant; leaves the pointer on the
/// stack. A size that fits a small class calls `$__alloc_class` with the class index,
/// skipping the class lookup.
pub(crate) fn emit_alloc_const(out: &mut String, pad: &str, size: usize) {
    match small_class(size) {
        Some(k) => out.push_str(&format!("{pad}i32.const {k}\n{pad}call $__alloc_class\n")),
        None => out.push_str(&format!("{pad}i32.const {size}\n{pad}call $__alloc\n")),
    }
}

/// `GRANITE_DEBUG_HEAP=1` turns the debug heap on for every compile, so a whole test
/// suite can run with it.
pub(crate) fn debug_heap_from_env() -> bool {
    std::env::var_os("GRANITE_DEBUG_HEAP").is_some_and(|v| v != "0" && !v.is_empty())
}

/// Emit the memory, the allocator tables, and the allocator functions for a module
/// whose static data ends at `heap_base`. `data_end` is the end of the highest user
/// data segment; the initial memory covers it.
///
/// With `debug`, `$__free` fills the payload with 0xDD and poisons `rc`; freeing a
/// poisoned block, freeing something that is not a block, a `__pack_free` length
/// larger than the block, and reusing a free block whose header was written all trap.
/// The lowest payload address of a heap block for a module whose static data ends
/// at `heap_base`: anything below it is static data, a table, or null.
pub(crate) fn heap_payload_start(heap_base: u32) -> u32 {
    HeapLayout::new(heap_base).heap_start + BLOCK_HEADER
}

/// With `rc` as well, `__pack_alloc` marks the blocks it hands the host as raw, so
/// the verifier's census can tell them from counted objects (rc.rs).
pub(crate) fn emit_heap_runtime(
    out: &mut String,
    heap_base: u32,
    data_end: u64,
    debug: bool,
    rc: bool,
) {
    let l = HeapLayout::new(heap_base);
    let first_chunk_end = (l.heap_start as u64 + INITIAL_HEAP as u64).max(data_end);
    let initial_pages = first_chunk_end.div_ceil(65536);

    // No maximum: `$__grow` grows the memory as the frontier needs it.
    out.push_str(&format!("  (memory (export \"memory\") {initial_pages})\n"));

    // Constant tables: block size per class, and small class by size.
    let sizes = class_sizes();
    let mut class_size_bytes = Vec::new();
    for s in &sizes {
        class_size_bytes.extend_from_slice(&s.to_le_bytes());
    }
    let class_of: Vec<u8> = (0..SMALL_MAX / 8)
        .map(|i| small_class(((i + 1) * 8 - BLOCK_HEADER) as usize).unwrap() as u8)
        .collect();
    for (addr, bytes) in [(l.class_size, &class_size_bytes), (l.class_of, &class_of)] {
        out.push_str(&format!("  (data (i32.const {addr}) \""));
        for byte in bytes {
            out.push_str(&format!("\\{:02x}", byte));
        }
        out.push_str("\")\n");
    }

    out.push_str(&format!(
        "  (global $__heap_base i32 (i32.const {heap_base}))\n"
    ));
    // The frontier: blocks past it have never been allocated.
    out.push_str(&format!(
        "  (global $__heap_ptr (mut i32) (i32.const {}))\n",
        l.heap_start
    ));
    out.push_str("  (global $__live_bytes (mut i32) (i32.const 0))\n");
    out.push_str("  (global $__live_blocks (mut i32) (i32.const 0))\n");
    out.push_str("  (global $__total_bytes (mut i64) (i64.const 0))\n");

    // Traps. Wasm traps carry no message, so each cause has its own function: the
    // name shows in the trap's backtrace.
    for name in [
        "__granite_out_of_memory",
        "__granite_double_free",
        "__granite_bad_free",
        "__granite_free_block_written",
    ] {
        out.push_str(&format!("  (func ${name} unreachable)\n"));
    }

    // $__alloc(size): a block with room for `size` payload bytes.
    out.push_str(&format!(
        r#"  (func $__alloc (param $size i32) (result i32)
    (local $b i32)
    local.get $size
    i32.const {MAX_PAYLOAD}
    i32.gt_u
    if
      call $__granite_out_of_memory
    end
    local.get $size
    i32.const {BLOCK_HEADER}
    i32.add
    local.tee $b
    i32.const {SMALL_MAX}
    i32.le_u
    if (result i32)
      ;; small: CLASS_OF[(b - 1) / 8]
      local.get $b
      i32.const 1
      i32.sub
      i32.const 3
      i32.shr_u
      i32.load8_u offset={class_of}
    else
      ;; large: the bucket of the next power of two (2 KiB is class {first_large})
      i32.const {large_base}
      local.get $b
      i32.const 1
      i32.sub
      i32.clz
      i32.sub
    end
    call $__alloc_class
  )
"#,
        class_of = l.class_of,
        first_large = SMALL_CLASSES.len(),
        // bucket(b) = first_large + ceil(log2 b) - 11 = first_large + 21 - clz(b - 1)
        large_base = SMALL_CLASSES.len() + 21,
    ));

    // $__alloc_class(k): pop class k's free list, or carve a block from the frontier.
    let check_popped = if debug {
        format!(
            r#"      local.get $p
      i32.load
      i32.const {POISON}
      i32.ne
      if
        call $__granite_free_block_written
      end
"#
        )
    } else {
        String::new()
    };
    out.push_str(&format!(
        r#"  (func $__alloc_class (param $k i32) (result i32)
    (local $p i32)
    (local $b i32)
    (local $end i64)
    local.get $k
    i32.const 2
    i32.shl
    local.tee $k
    i32.load offset={heads}
    local.tee $p
    if
      ;; pop: the link is the free block's first payload word
{check_popped}      local.get $k
      local.get $p
      i32.load offset=8
      i32.store offset={heads}
      local.get $p
      i32.load offset=4
      local.set $b
    else
      local.get $k
      i32.load offset={class_size}
      local.set $b
      global.get $__heap_ptr
      local.tee $p
      i64.extend_i32_u
      local.get $b
      i64.extend_i32_u
      i64.add
      local.tee $end
      memory.size
      i64.extend_i32_u
      i64.const 16
      i64.shl
      i64.gt_u
      if
        local.get $end
        call $__grow
      end
      local.get $end
      i32.wrap_i64
      global.set $__heap_ptr
      local.get $p
      local.get $b
      i32.store offset=4
    end
    local.get $p
    i32.const 1
    i32.store
    global.get $__live_bytes
    local.get $b
    i32.add
    global.set $__live_bytes
    global.get $__live_blocks
    i32.const 1
    i32.add
    global.set $__live_blocks
    global.get $__total_bytes
    local.get $b
    i64.extend_i32_u
    i64.add
    global.set $__total_bytes
    local.get $k
    local.get $k
    i32.load offset={live_count}
    i32.const 1
    i32.add
    i32.store offset={live_count}
    local.get $p
    i32.const {BLOCK_HEADER}
    i32.add
  )
"#,
        heads = l.heads,
        class_size = l.class_size,
        live_count = l.live_count(),
    ));

    // $__grow(end): make the memory reach `end`, growing by at least a quarter of
    // its size so growth is geometric (§5.4).
    out.push_str(
        r#"  (func $__grow (param $end i64)
    (local $need i32)
    (local $by i32)
    local.get $end
    i64.const 65535
    i64.add
    i64.const 16
    i64.shr_u
    i32.wrap_i64
    memory.size
    i32.sub
    local.tee $need
    memory.size
    i32.const 2
    i32.shr_u
    local.tee $by
    local.get $need
    local.get $by
    i32.gt_u
    select
    memory.grow
    i32.const -1
    i32.eq
    if
      ;; the geometric step may not fit; try the exact need before giving up
      local.get $need
      memory.grow
      i32.const -1
      i32.eq
      if
        call $__granite_out_of_memory
      end
    end
  )
"#,
    );

    // $__free(p): push p's block onto its class's free list. Pointers below the heap
    // (null, static data, host scratch) are not blocks and are ignored.
    let check_free = if debug {
        format!(
            r#"    local.get $p
    i32.load
    i32.const {POISON}
    i32.eq
    if
      call $__granite_double_free
    end
    ;; a size no class has: not a block
    local.get $p
    i32.load offset=4
    i32.const 16
    i32.sub
    i32.const {max_size}
    i32.gt_u
    if
      call $__granite_bad_free
    end
"#,
            max_size = (1u32 << 31) - 16,
        )
    } else {
        String::new()
    };
    let check_class = if debug {
        format!(
            r#"    local.get $k
    i32.const {max_k}
    i32.gt_u
    if
      call $__granite_bad_free
    end
    local.get $k
    i32.load offset={class_size}
    local.get $b
    i32.ne
    if
      call $__granite_bad_free
    end
    local.get $p
    i32.const {BLOCK_HEADER}
    i32.add
    i32.const 0xDD
    local.get $b
    i32.const {BLOCK_HEADER}
    i32.sub
    memory.fill
    local.get $p
    i32.const {POISON}
    i32.store
"#,
            max_k = 4 * (NUM_CLASSES - 1),
            class_size = l.class_size,
        )
    } else {
        String::new()
    };
    out.push_str(&format!(
        r#"  (func $__free (param $p i32)
    (local $b i32)
    (local $k i32)
    local.get $p
    i32.const {heap_payload}
    i32.lt_u
    if
      return
    end
    local.get $p
    i32.const {BLOCK_HEADER}
    i32.sub
    local.set $p
{check_free}    local.get $p
    i32.load offset=4
    local.tee $b
    i32.const {SMALL_MAX}
    i32.le_u
    if (result i32)
      local.get $b
      i32.const 1
      i32.sub
      i32.const 3
      i32.shr_u
      i32.load8_u offset={class_of}
    else
      i32.const {large_base}
      local.get $b
      i32.const 1
      i32.sub
      i32.clz
      i32.sub
    end
    i32.const 2
    i32.shl
    local.set $k
{check_class}    local.get $p
    local.get $k
    i32.load offset={heads}
    i32.store offset=8
    local.get $k
    local.get $p
    i32.store offset={heads}
    global.get $__live_bytes
    local.get $b
    i32.sub
    global.set $__live_bytes
    global.get $__live_blocks
    i32.const 1
    i32.sub
    global.set $__live_blocks
    local.get $k
    local.get $k
    i32.load offset={live_count}
    i32.const 1
    i32.sub
    i32.store offset={live_count}
  )
"#,
        heap_payload = l.heap_start + BLOCK_HEADER,
        class_of = l.class_of,
        large_base = SMALL_CLASSES.len() + 21,
        heads = l.heads,
        live_count = l.live_count(),
    ));

    // Pack ABI (§4): the host allocates every input buffer with __pack_alloc and
    // frees every input and output buffer with __pack_free.
    let check_len = if debug {
        format!(
            r#"    local.get $ptr
    i32.const {heap_payload}
    i32.ge_u
    if
      local.get $len
      local.get $ptr
      i32.const {BLOCK_HEADER}
      i32.sub
      i32.load offset=4
      i32.const {BLOCK_HEADER}
      i32.sub
      i32.gt_u
      if
        call $__granite_bad_free
      end
    end
"#,
            heap_payload = l.heap_start + BLOCK_HEADER,
        )
    } else {
        String::new()
    };
    let mark_raw = if debug && rc {
        format!(
            "    local.tee $size\n    i32.const {BLOCK_HEADER}\n    i32.sub\n    i32.const {RAW_RC}\n    i32.store\n    local.get $size\n"
        )
    } else {
        String::new()
    };
    out.push_str(&format!(
        r#"  (func (export "__pack_alloc") (param $size i32) (result i32)
    local.get $size
    call $__alloc
{mark_raw}  )
  (func (export "__pack_free") (param $ptr i32) (param $len i32)
{check_len}    local.get $ptr
    call $__free
  )
"#
    ));

    // __granite_heap_stats(): fill in the STATS record and return its address.
    out.push_str(&format!(
        r#"  (func $__heap_stats (export "__granite_heap_stats") (result i32)
    i32.const {stats}
    global.get $__live_bytes
    i32.store
    i32.const {stats}
    global.get $__live_blocks
    i32.store offset=4
    i32.const {stats}
    global.get $__heap_ptr
    i32.store offset=8
    i32.const {stats}
    memory.size
    i32.store offset=12
    i32.const {stats}
    global.get $__total_bytes
    i64.store offset=16
    i32.const {stats}
    i32.const {heap_start}
    i32.store offset=24
    i32.const {stats}
    i32.const {NUM_CLASSES}
    i32.store offset=28
    i32.const {stats}
  )
"#,
        stats = l.stats,
        heap_start = l.heap_start,
    ));
}
