# Live REPL via WASM dynamic linking

**Status**: M1 complete ✅ → M2 next
**Started**: 2026-08-22
**Related**: [WISP-REPL-ARCHITECTURE.md](WISP-REPL-ARCHITECTURE.md) (the current REPL),
[GENERICS-AND-TRAITS.md](GENERICS-AND-TRAITS.md) / [DERIVING.md](DERIVING.md) (the
language features the image must keep supporting).

## Objective

Turn the REPL into a **live image**: definitions and values survive between lines, and
each new line compiles **only itself** and joins a world that never resets.

```
wisp> (define p (point 1 2))
wisp> (point.x p)      ; reads the SAME p, still on the heap
=> 1
```

This is a Lisp/Smalltalk image built on WebAssembly. It also *drives wisp*: it forces
imported memory, a shared heap, and cross-module linking — muscles a serious language
needs anyway.

## Why the current REPL is not live

`eval_expression` (`crates/test-runtime/src/main.rs`, ~3033) **recompiles the whole
world every line**:

- each `(define x 5)` is re-inlined as `(i32.const 5)` into the next expression
  (main.rs ~3173), so only *constants* survive — never a record, list, or mutated value;
- every prior `(fn …)` is re-accumulated as source text (main.rs ~3047) and recompiled;
- each eval builds a **fresh** `Engine`/`Store`/`Instance` (main.rs ~3375) whose module
  **defines its own memory**, so the heap is wiped every line.

Two costs: no real state, and O(n) recompile per line (O(n²) per session).

## Why WASM makes an image hard — and the way through

Two WASM facts cause the problem:

1. **A module is closed after instantiation** — you cannot add a function to a running
   instance. So a new `(fn …)` cannot be injected into the running world.
2. **Each instance owns its own linear memory** — a fresh module gets a fresh, zeroed
   heap, and a wisp pointer is just an `i32` offset into *one* memory.

The escape is that **memory, globals, and functions can be imported**. So:

- one **shared `Memory`** (host-owned) is imported by every line → the heap persists;
- one **shared `$__heap_ptr` mutable global** (host-owned) is imported by every line →
  the bump allocator never resets, so allocations keep growing across lines;
- prior definitions are reached by **imports wired, at instantiation, to the exported
  functions of the still-alive prior instances** — the host's symbol table is the
  image's namespace, and the shared memory makes cross-instance pointer arguments valid.

### The linking seam already exists

The self-hosted compiler (`examples/wisp-compiler.lisp`) returns its output as a **WAT
string**; the host assembles it with `wat::parse_str` (main.rs ~3370). The host already
rewrites that string — exporting memory, injecting globals, adding data segments,
wiring Pack imports (main.rs ~3209–3361). Dynamic linking is **more of the same
rewrite**, so the first milestones need **no change to the self-hosted compiler**.

### Facts this plan relies on (verified 2026-08-22)

- Self-hosted compiler emits `(memory (export "memory") 1)` and
  `(global $__heap_ptr (mut i32) (i32.const 49152))`; the bump heap starts at byte
  **49152** (`examples/wisp-compiler.lisp` ~1498, ~1513).
- Below 49152 the REPL places **string data segments** at fixed low addresses
  (`preprocess_string_literals`) — this region must be reserved and, for the image, its
  allocation pointer must also persist (see M1 risk).
- The compiler already emits plain `(import iface name (params) ret)` WAT for host
  imports (main.rs ~3143), and the host wires those to `wasmtime::Func`s — exactly the
  mechanism M2 reuses to reach prior definitions.

## Milestones (each is a real part of A — nothing is thrown away)

### M1 — Shared, persistent heap ✅ DONE
Implemented in `crates/test-runtime/src/main.rs`: a `ReplSession` holds one `Engine`,
`Store`, `Memory` (16 pages, growable), and a mutable `$__heap_ptr` `Global` (init 49152),
created once in `run_repl`. `eval_expression` now runs `rewrite_for_shared_memory` on the
final WAT (memory + heap-ptr become `env` imports, memory re-exported), instantiates every
line into the session `Store` via a name-based `Linker`, and continues the string arena
from `session.next_string_addr`. Proven live: two `(point …)` lines allocated at 0xC000
then 0xC008 then 0xC010 — one heap, never reset — via a new `(heap)` REPL command. Tests:
`m1_shared_memory_tests` (WAT rewrite + shared-heap instantiation) and `spike_shared_heap`.

Original plan:

- Host owns one `Memory` and one `$__heap_ptr` `Global`, both created **once per REPL
  session** in **one persistent `Store`** (today it is one Store per eval).
- Post-process WAT: rewrite the memory definition to `(import "env" "memory" …)` and the
  heap global to `(import "env" "__heap_ptr" (global (mut i32)))`; provide both in the
  imports list.
- All per-line instances live in the one session Store, so they share those objects.
- **Proof**: allocate a value on line 1, read it back through its pointer on line 2.
- *Risk*: the low-memory string-data region uses fixed addresses per compile; with a
  shared memory those would collide. Reserve a data arena and persist a data pointer
  (or move strings above a per-session watermark). Track here.

### M2 — Incremental function linking
- Stop concatenating prior `(fn …)` source. Compile **only** the new form.
- Keep every prior instance alive; record `name → (Func, signature)` in a host symbol
  table.
- For each prior name a new line references, emit an `(import …)` declaration and wire
  it to that `Func`. Intra-image calls are plain wasm `call`s with pointer args in the
  shared memory (NOT CGRF-bridged — that path is for external Pack packages only).
- **Proof**: define `sq` on line 2; call it on line 5 without recompiling line 2; O(1)
  compile per line.

### M3 — `define` as a live value
- Running a `define` allocates on the shared heap and returns a pointer; store
  `name → pointer` (+ type) in the symbol table; later lines inline the pointer.
- Print results by decoding CGRF from the **persistent** memory (the REPL already has
  the decoders). Support redefine and, later, `set!`.
- **Proof**: `(define p (point 1 2))` then `(point.x p)` ⇒ `1` across lines.

### M4 — Strings as heap values (in the compiler)
The principled removal of the low string arena. Today string literals are a **host-side
text shim** (`preprocess_string_literals`): each `(str.const "…")` is placed at a fixed
low address `[0x100, 49152)` via an active data segment — a second, parallel memory
region that sits *below* the heap. M1 only persists that arena's bump pointer so it stops
colliding; the region itself remains.

For a live image there is **one shared heap**, so the coherent end state is "**a string is
just another heap value**" — a pointer into the shared heap, reachable across lines like a
record or a list. That deletes the arena, its 49152 ceiling, and the exception it carves
out of the one-heap model.

- **Home**: the compiler, not the host. The self-hosted compiler
  (`examples/wisp-compiler.lisp`) should lower a literal to a heap allocation directly, so
  the host shim **disappears**. This means editing Wisp and re-bootstrapping (the heavy
  loop), so pair it with M2 — we are already in the compiler teaching it linking mode.
- **Do NOT** build the intermediate host-side `memory.init` / passive-segment shim: it is
  genuinely throwaway (deleted the moment the compiler owns strings). The cheap M1 counter
  is fine until then; the compiler-native version is the real landing.
- **Proof**: a string `define`d on one line reads back correctly on a later line with the
  low arena removed entirely; a session with >48 KB of string literals no longer risks the
  heap ceiling.

### Later
- Dedupe monomorphized specializations by mangled name (avoid re-emitting per line).
- Garbage collection (the image grows monotonically until then — acceptable first).
- Fold the second `theater-repl` path in, or retire it.

## Open questions
- Imported **mutable** global needs the mutable-globals feature (standard in wasmtime) —
  confirm it is enabled for the session engine.
- Data/string arena persistence — M1 persists the low-arena bump pointer (cheap); the
  principled fix is M4 (strings as heap values, in the compiler). Do not build the
  host-side `memory.init` shim in between.
- Whether M2 wiring can reuse the host-import path wholesale, or needs a distinct
  "internal import" tag to keep the plain-call ABI.
