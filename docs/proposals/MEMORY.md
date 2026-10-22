# Proposal: Memory Management for Granite

**Status:** Design (ticket 0011). Not implemented.
**Related:** [WISP-RETENTION.md](WISP-RETENTION.md) (ticket 0012, PR #80: what the Wisp
interpreter keeps alive on purpose),
[changes/SELF-HOSTED-COMPILER.md](../changes/SELF-HOSTED-COMPILER.md) ("GC or arena per
compilation?"), [changes/LIVE-REPL-DYNAMIC-LINKING.md](../changes/LIVE-REPL-DYNAMIC-LINKING.md)
("Garbage collection").

## Summary

Granite never frees memory. Every allocation bumps `$__heap_ptr`, `__pack_free` is a
`nop`, and the compiled module declares a fixed 2 GiB memory. A Wisp REPL session is a
Granite program, so every session grows until it traps.

The decision:

1. **The Granite heap is not acyclic today, but one change makes it acyclic.**
   `list-push` writes the new length and data pointer back into the list it was given.
   That is the only operation that changes a heap object after it is built, and it can
   make a list that contains itself (demonstrated in compiled Granite and in a live Wisp
   session, below). We give `list-push` **value semantics**: it returns a list with one
   more element and does not change its argument. The tree-walking back-end
   (`eval.rs`) already does this. After that change, every edge in the heap is written
   while the object is new and unshared, so no cycle can exist.
2. **Reference counting inserted at compile time, in the style of Koka's Perceus.**
   The compiler inserts `dup`/`drop`, infers borrowed parameters, and reuses a dying
   object's memory in place (this is how `list-push` stays O(1) when the list is
   unique). There is no tracing, no shadow stack, and no pause. Memory is freed as soon
   as the last reference goes.
3. **A real allocator**: size-class free lists for small blocks, power-of-two buckets
   for large blocks, an 8-byte header, and a memory that starts small and grows.
4. **The boundary already works for us**: the Pack host calls `__pack_free` on every
   input and output buffer. A real `__pack_free` reclaims them with no host change.
5. **Two fixes come before any freeing, and they remove most of the garbage.**
   Measured: one `list-push` fix takes the self-hosted compiler build from **1408 MiB
   to 62 MiB** allocated and from **0.79 s to 0.05 s**. String literals are rebuilt on
   the heap byte by byte every time they are evaluated, and they are **91%** of what a
   Wisp loop allocates.

What we give up: a future feature that adds a mutable cell (Wisp `set!`, `letrec`, a
mutable box, a mutable list) can make a cycle, and RC will leak that cycle. Such a
feature must refer back by name (as top-level recursion does today), or it must bring
its own cycle handling. See [What we give up](#what-we-give-up).

---

## 1. Inventory

Line numbers are `src/compiler/codegen.rs` at `9657b59` unless another file is named.

### 1.1 Two code generators

| | `generate_wat` (5-180) | `generate_wat_pack` (3286-3526) |
|---|---|---|
| Used by | `compile_repl_expr` only (`wisp-repl/src/lib.rs:146`, the non-Pack REPL mode) | `compiler::compile` (`mod.rs:451`): `granite compile`, every test, the Wisp actor, the Pack REPL paths |
| Memory | `(memory 4000 4000)`, 250 MiB fixed | `(memory (export "memory") 32000 32000)`, 2 GiB fixed |
| Heap start | 0 (overlaps user data segments) | `HEAP_START_OFFSET` = 0xC000 |
| Allocator exports | `cabi_realloc` (bump; ignores the old block) | `__pack_alloc` (= `$__alloc`), `__pack_free` (`nop`, 3452) |

This design is for `generate_wat_pack`. The legacy `generate_wat` path should be
retired, not upgraded (see stage 6).

The self-hosted compiler (`examples/wisp-compiler.wisp`) emits its own runtime (line
2048): heap at 0xC000, `(memory (export "memory") 1)`, and a `$__list_push` that already
doubles capacity but also writes into the list header in place.

### 1.2 Heap object kinds

`type_size` (`mod.rs:1091`) is 4 for every compound type, so a field or element of
compound type is always a pointer, never stored inline. There is no alignment padding
anywhere.

| Kind | Layout | Pointer fields (what `drop` must follow) | Built at |
|---|---|---|---|
| Record | fields packed in declaration order | each compound field | `RecordConstruct` 454-520 |
| Variant | `[tag:i32][payload from +4]`; size = 4 + the largest case, for **every** case | the compound slots of the case named by the tag | `VariantConstruct` 574-649 |
| Nullary case | allocated at the full variant size; only the tag is written | none | 583-599 |
| Option | `[disc:i32][payload]`; `none` is allocated too | the payload, if `some` | `Some`/`None` 997-1059 |
| Result | `[disc:i32][payload of the larger side]` | the payload of the active side | `Ok`/`Err` 1061-1148 |
| Tuple | elements packed, no header | compound elements | `TupleConstruct` 1151-1198 |
| List | 12-byte header `{len, cap, data}` + a separate data buffer, stride `type_size` (so `u8` takes 4 bytes) | `data`; then each slot if the element type is compound | `ListNew` 1230-1266, `ListPush` 1268-1367, decoders |
| String | `[len:u32][utf-8 bytes]` | none (leaf) | literals 213-244, `Substring`, `StringAppend`, decoders |
| `any` | `[len:u32][self-contained CGRF]` | none (CGRF children are node indices) | `AnyFromS32`, `AnyFromString`, `RawInvoke`, boundary |
| Raw block | `n` untyped bytes | unknown to the compiler | `HeapAlloc` 1636-1643 |
| Closure | does not exist in Granite. Function parameters are defunctionalized. A Wisp closure is an ordinary variant value (`values.wisp:17`) | | |
| Resource, capability | an `i32` handle or witness, not a heap pointer | | 1200-1229 |

### 1.3 Allocation sites

There are two mechanisms:

- **Inline bump** (`global.get $__heap_ptr` … `global.set $__heap_ptr`): no bounds
  check, no `memory.grow`, no alignment. Used by string literals, every constructor,
  `list-new`, `list-push`, `substring`, `string-append`, `string-from-bytes`,
  `string-to-bytes`, `any-s32`, `any-string`, and every CGRF decoder.
- **`call $__alloc`** (3391-3443): checks `memory.size` and calls `memory.grow`. Used by
  `heap-alloc`, `raw-invoke`, export output buffers, import wrapper buffers, `any`
  boundary copies, and primitive-array decode. Because the memory's minimum equals its
  maximum, the grow can never succeed.

Classes that matter for the design:

| Class | What it allocates | Today |
|---|---|---|
| String literal | 4+len, written one `i32.store8` per byte, **every evaluation** (213-244) | 91% of a Wisp loop's bytes (§6) |
| Constructors | record, variant, option, result, tuple, list header | never freed |
| `list-push` | a new data buffer of `len+1` slots **on every push**; the old buffer leaks; `cap` is never read | 96% of the self-compile's bytes (§6) |
| String operations | `substring` and `string-append` copy into new strings | never freed |
| Boundary decode | fresh objects for every export argument and import result; strings are copied out of the buffer | never freed |
| Boundary buffers | export output (exact size for scalars and strings, else a fixed 16 KiB), import argument buffer (4 or 16 KiB) and 8-byte result slots on **every** import call | never freed |
| Host-owned results | an async host import writes its result into a buffer it got from `__pack_alloc`, and the status code says the guest owns it; the wrapper ignores the status (4534, 1704) | leaks |
| Raw | `heap-alloc` | never freed |

### 1.4 Fixed regions

| Range | Use |
|---|---|
| user offsets | `(data OFFSET …)` segments, placed wherever the source says; nothing checks overlap |
| 0x4000, 0x4004 | the host's result ptr/len slots (packr) |
| 0x4008-0xC008 | the host's 32 KiB scratch buffer for synchronous host returns (packr `host.rs:64-71`) |
| 0xA000 | Pack type metadata (`METADATA_OFFSET`) |
| 0xB000-0xBFFF | encoder stack (`$enc_tuple_sp`), no overflow check |
| 0xC000- | heap |

The host scratch range overlaps the metadata, the encoder stack, and the first 8 heap
bytes when a synchronous host return is larger than about 24 KiB. That is a bug today,
independent of this design (listed in §8).

---

## 2. Can a Granite heap contain a cycle?

### 2.1 The hypothesis

claude's hypothesis: Granite data is immutable, so each object points only at objects
older than itself, so the heap is acyclic, so compile-time RC is complete.

### 2.2 The evidence: false today, from one source

**`list-push` changes its argument.** Codegen loads the header, builds a new data
buffer, then stores the new `len` and `data` back into the **same** header (1350-1362)
and returns the same pointer (1364). An object built before the push that holds the list
can be pushed into it.

Compiled Granite:

```lisp
(variant node (leaf s32) (branch (list node)))
(fn depth ((n node) (fuel s32)) s32
  (if (i32.eq fuel 0) 0
    (match n
      ((leaf v) 0)
      ((branch kids) (i32.add 1 (depth (list-get kids 0) (i32.sub fuel 1)))))))
(fn main () s32
  (let (xs (list-new node))
    (let (b (branch xs))
      (begin (list-push xs b) (depth b 1000)))))
```

`granite run-module` gives **1000**: `b → xs → b → …` is a cycle. `granite eval` on the
same file fails with "list index 0 out of bounds", because `eval.rs:410-414` copies the
list before it pushes. **The two back-ends disagree**, so the in-place write is a codegen
defect, not a language rule.

A live Wisp session (the Wisp `list-push` builtin calls the Granite one on the list
inside the value, `collections.wisp:98-100`):

```
> (variant node (leaf s32) (branch (list node)))
> (define ns (list-new node))
> (list-push ns (branch ns))
#<list node ((branch #<list node ((branch … #<depth-limit> …
> (define ys (list-new (list s32)))
> (list-push ys (list-new s32))
> ys                       ; changed without a redefine
#<list (list s32) (#<list s32 ()>)>
```

A second Wisp route makes a cycle through a closure: a closure captures its environment
list, the environment binds a list, and the closure is pushed into that list. (Found by
the inventory pass: `(define mk (lambda (gs) (begin (list-push gs (lambda (n) (list-len gs))) gs)))`.)

### 2.3 Everything else was checked and is not a source

| Candidate | Finding |
|---|---|
| Records, variants, options, results, tuples | written only while being built (constructors and decoders). No field-set operation exists. |
| Strings, `any` | leaves. `substring` and `string-append` copy. `any` holds node indices, not pointers. |
| Raw stores (`i32.store`, …) | Allowed on any `s32` address. But casts are scalar-only (`typecheck.rs:442-453`), so a program cannot get the address of a record, variant, list, option or tuple. Only `string-addr` and `any-addr` give addresses, and those objects contain no pointers. All raw stores in the tree write into fresh `heap-alloc` blobs (`marshal.wisp`, `reader.wisp:73-77`, `primitives.wisp:138-142`). Raw memory is invisible to RC, so it can leak or dangle (§3.6), but it cannot take part in a cycle that RC follows. |
| Mutable globals | Any type, so they hold heap pointers (the interpreter has about 15, all lists and strings initialized to 0). A global is a **root**, not an object: nothing on the heap can point at a global. They are owners (§3.5). |
| Wisp closures | `(closure params body env home)`. The environment is a list of bindings, **copied before it is extended** (`evaluator.wisp:57-64`, `copy-env`), and top-level names resolve **by name** through `$bindings` (`evaluator.wisp:46-55`). So `define`, recursion, and mutual recursion make no pointer cycle. The only route to a cycle is a captured local list plus the in-place `list-push`. |
| Modules | `switch-module` saves the current globals into a `module` record and restores another (`modules.wisp:52-75`). These are snapshots stored by value; no back-pointer. |
| Construction order | Record, variant, `some`, `ok` and `err` allocate the parent **before** they evaluate the fields, so a parent can sit at a lower address than its child. "Older by address" is false, but it does not matter: the argument below uses the time an edge is written, not addresses. |

### 2.4 Why value-semantics `list-push` makes the heap acyclic

Rule: **a pointer field is written only while its object is new**, that is, after
allocation and before the object's own address is stored anywhere a later write could
reach it from. With value-semantics `list-push`, every heap write in typed Granite
obeys this rule: constructors, decoders, and the in-place fast path of `list-push`, which
runs only when the list is uniquely owned (§3.4) and so is unreachable from anything
else.

Proof sketch. Suppose a cycle `o1 → o2 → … → ok → o1` exists. Take the edge in it that
was written last, say `x → y`. When it was written, the rest of the cycle already
existed, so `y` could already reach `x`, so `x` was reachable from another heap object,
so `x` was not new. That breaks the rule. A self-edge `x → x` would require storing
`x`'s own address into `x` during construction; codegen never does that.

### 2.5 Does the code rely on the in-place write?

Mostly no. Every `list-push` in the tree uses the returned list, threading an
accumulator (`(list-push out …)`). The places that look like they depend on it:

- `evaluator.wisp:57-64` copies the environment **because** push mutates. With value
  semantics the copy is no longer needed; removing it is a speed-up.
- `types.wisp:73` (`publish`) does `(global.set $bindings (list-push (global.get $bindings) …))`.
  It stores the result, so it works with value semantics. To stay O(1) it needs the
  global-take rule in §3.5.
- `modules.wisp:58` stores `$bindings` in a `module` record, but only as a snapshot that
  is restored whole later, so it works with value semantics.
- `syntax-rules.wisp:156` (`sr-accumulate`) pushes onto an existing sequence's list and
  wraps the result in a new sequence; it works with value semantics.

The user-visible change: in Wisp, `(list-push xs v)` no longer changes `xs`. Only the
returned list has `v`. ticket 0012 (nico) has been told.

### 2.6 The choice

| Option | For | Against |
|---|---|---|
| **Compile-time RC (Perceus)** | Complete once the heap is acyclic. Frees memory as soon as it dies, with no pause, which suits long-lived actors. No root finding: Wasm locals are invisible to a collector, but RC does not need to see them. Reuse makes functional updates (`list-push`, `string-append`, rebuilding a variant) run in place. Fits the type system: types are monomorphic at codegen, so every `drop` is a direct call to a known per-type function. `lin`/`aff` values need no counting at all. | Counting costs time on every pointer copy (borrow inference and reuse remove most of it). A codegen bug becomes a use-after-free or a leak (the debug verifier in §5.5 finds both). Cycles leak if a mutable feature is ever added. |
| Precise tracing GC in linear memory | Handles cycles. No per-copy cost. | Wasm has no stack scanning, so every live pointer in a local must be spilled to a shadow stack at every call and allocation that can collect: a large cost in code size and time across all of codegen. Pauses grow with the live set; a REPL's live set grows with use. Non-moving, so it has the same fragmentation as RC. Gives no in-place reuse. |
| Arena per export call | Trivial for stateless exports. | Wrong for the REPL: its state lives in globals across calls, so the live state must be copied out of the arena after each call. That is a copying collector at the call boundary, with a cost proportional to the whole session on every call. |
| Wasm GC proposal (engine-managed) | No collector to write. | All codegen would change; Pack's ABI is linear memory plus CGRF, so the boundary copies anyway; the raw-memory primitives the interpreter depends on do not exist there. |

**Decision: compile-time RC.** Tracing would buy only cycle collection, and after the
`list-push` change Granite has no cycles to collect.

### What we give up

- **No mutable cells without a cycle plan.** A future Wisp `set!`, `letrec`, mutable box
  or mutable list can form a cycle (closure → env → box → closure), and RC will leak it.
  The rule for such features: refer back **by name** through a table owned by a root (as
  top-level recursion does today), or add a cycle collector scoped to that feature.
  This constraint goes into the language docs with stage 4.
- **Wisp lists stop being shared and mutable.** Code that used `list-push` to change a
  list in place must use the returned value.
- **Time spent counting.** §6.2 sets a budget for it.
- **Raw memory stays manual.** `heap-alloc` blocks get an explicit `heap-free`.

---

## 3. Reference counting

### 3.1 Object header

Every heap block gets an 8-byte header **before** the address the program sees, so all
existing field offsets stay the same:

```
p - 8: rc    : i32   reference count (1 at birth)
p - 4: size  : u32   block size in bytes, header included (the allocator's size class)
p    : payload (unchanged layouts from §1.2)
```

- The `size` word makes `free(p)` independent of the type, keeps payloads 8-aligned,
  lets the allocator check `__pack_free(ptr, len)`, lets `string-append` grow a unique
  string into spare room in its block (§3.4), and makes a heap census possible for leak
  tests (§7).
- Cost: 8 bytes per object. Koka uses the same size.
- Actors are single-threaded, so counts are plain loads and stores, not atomics.

**Static objects.** String literals move into data segments (stage 0b) at addresses
below `$__heap_base`, as does any other object built at compile time. `dup` and `drop`
skip any pointer below `$__heap_base`, and that one comparison also skips null (the
interpreter's globals start at 0). Static objects need no header.

```wat
;; dup p
(if (i32.ge_u (local.get $p) (global.get $__heap_base))
  (then (i32.store offset=-8 … (i32.add (i32.load offset=-8 …) (i32.const 1)))))
;; drop p : T
(if (i32.ge_u (local.get $p) (global.get $__heap_base))
  (then (if (i32.eqz (rc-- p)) (then (call $__drop_T (local.get $p))))))
```

(Wasm has no negative `offset=`; the emitted code subtracts 8 first. This is a sketch.)

### 3.2 Per-type drop functions

Generics are monomorphized, so each concrete type `T` gets one `$__drop_T`, emitted
only if used:

- Record, tuple: `drop` each compound field, then `free(p)`.
- Variant, option, result: `br_table` on the tag; `drop` the compound slots of that
  case; `free(p)`.
- List: for each element (if the element type is compound) `drop` it; `free(data)`;
  `free(p)`.
- String, `any`: `free(p)`.

Deep structures: the drop of the last compound field is a `return_call` (Granite already
emits tail calls), so a long chain (a variant used as a linked list) drops in constant
stack. Trees use stack proportional to their depth, the same as building or walking them.

### 3.3 Inserting dup and drop (Perceus)

Run on the typed AST after type checking and linearity checking, before codegen. It
only adds `Dup`/`Drop` nodes and a few rewrites; codegen emits them. For each
function:

- Every variable of compound type is **owned** or **borrowed**.
- An owned variable is consumed exactly once on every path: passed to an owned
  position (a constructor field, an owned parameter, a return, a `global.set`), or
  dropped right after its last use. If it is used owned more than once, `dup` it before
  every owned use except the last.
- A borrowed variable is never dropped. Passing it to an owned position needs a `dup`.
- `match` on an owned scrutinee: the arm's bindings are `dup`ed if used, then the
  scrutinee is dropped, unless the arm reuses it (§3.4).
- Drops go as early as possible (right after the last use), so a tail call has no
  pending drop after it and stays a `return_call`.
- `lin`/`aff` bindings are already proven to be used exactly or at most once. They are
  never `dup`ed; an `aff` binding not used on a path is dropped there.

**Borrowed parameters.** A parameter is inferred borrowed when the function only reads
it: it reads fields, passes it to borrowed parameters, and never stores, returns, or
`global.set`s it. This is a fixpoint over the call graph (Koka does the same). Export
parameters are owned (the export wrapper made them). This is what keeps predicates like
`(string=? name "+")`, `lookup-local`, and `list-get` free of any counting.

### 3.4 Reuse: functional updates in place

- **`list-push xs v`** with `xs` owned and `rc == 1`: write `v` into the spare capacity
  and bump `len`; grow the data buffer by doubling when full. Otherwise copy the header
  and the data (with spare capacity) and `dup` the elements. Threaded accumulators
  (`(f … (list-push out x))`), which is nearly all the code, are always unique, so they
  never copy.
- **`string-append a b`** with `a` owned and `rc == 1`: if the block's `size` has room,
  append in place; otherwise allocate 1.5 times the size. This makes left-fold string
  building O(n) and removes the reason for the divide-and-conquer workaround in the
  self-hosted compiler (`041feea`).
- **Drop-guided reuse.** When a `match` arm drops a unique scrutinee and then builds an
  object of the same size, the arm reuses the block instead of `free` + `alloc`.
  Granite variants have one size for all cases, so **any case of the same variant**
  qualifies: `(match v ((a x) (b (f x))))` reuses `v`'s block for the `b`. Records and
  tuples of the same layout qualify the same way.

### 3.5 Globals

- A global owns its value. `(global.set $g e)`: evaluate `e` (owned), store it, drop
  the old value. `GlobalSet` returns its value today (441-452), so a used result is a `dup`.
- `(global.get $g)` is a borrow when the value is only read before the next point that
  could `global.set $g` (borrow inference treats it like a borrowed parameter). Otherwise
  it is a `dup`.
- **The take rule.** `(global.set $g (list-push (global.get $g) x))` is how the
  interpreter grows `$bindings`, `$modules`, `$events` and others. If `global.get` were a
  `dup`, `rc` would be 2 and every push would copy the whole list. Rule: when the list
  operand of `list-push` (or the left operand of `string-append`) is `(global.get $g)`
  and the whole expression is the value of `(global.set $g …)`, codegen evaluates the
  other operand first, then **moves** the value out of `$g` (reads it and stores 0), then
  pushes, then stores the result. This is valid only when the other operand cannot write
  or read `$g`. A small effect analysis (which functions transitively touch which
  globals) decides that; if it fails, the code falls back to `dup` (correct, but it
  copies).

### 3.6 Raw memory and interior pointers

The escape hatches stay, with rules RC can live with:

| Primitive | Rule |
|---|---|
| `heap-alloc n` | Returns a raw block (it has a header so `free` works). The program owns it and must call a new `(heap-free p)`. Raw blocks are never counted. |
| `string-from-addr p`, `any-from-addr p` | **Copy** into a fresh counted object. Today they reinterpret the address, which creates strings that point into the middle of `any` blobs (`marshal.wisp:360, 401, 514, 556, 596-598`; 401 is not even aligned). A copy of a short string costs little. |
| `any-as-string x` | **Copy** (today it returns `blob + 28`, 1626-1634). |
| `string-addr s`, `any-addr x` | A borrow: the address is valid while `s` is alive. The compiler keeps `s` alive until the end of the enclosing `let` body. Storing the address in raw memory past that point is undefined behavior, as in unsafe Rust. |
| `i32.store` etc. | Unchanged. Writes into counted objects are undefined behavior (already impossible to address, except through `string-addr`). |

The interpreter's call sites change with stage 4: `reader.wisp:73-77` and
`primitives.wisp:138-142` build a string in a raw block, copy it, and free the block;
`marshal.wisp` frees its 16 KiB scratch buffer after `any-from-addr`, or keeps one
reusable buffer in a global.

---

## 4. The component boundary

The packr host's protocol (`~/work/pack/src/runtime/mod.rs:641-740`, `host.rs:503-566`):

1. The host calls `__pack_alloc(in_len)`, writes the CGRF input, and calls the export
   with `out_ptr_ptr = 0x4000`, `out_len_ptr = 0x4004`.
2. After the call it calls **`__pack_free(in_ptr, in_len)`**.
3. It reads the output and calls **`__pack_free(out_ptr, out_len)`**.
4. For an async host import, the host gets the result buffer from the guest's
   `__pack_alloc` and says so in the status code: **the guest owns it and must free it**.
   For a synchronous one, the result is in the host's fixed scratch at 0x4008, and the
   guest must **not** free it.

So there is no `cabi_realloc` and no post-return function in the Pack path; `__pack_free`
plays both roles. The design:

| Point | Ownership |
|---|---|
| `__pack_alloc(n)` | allocates a raw block |
| `__pack_free(p, n)` | frees `p` (in debug builds, checks that `n` fits the header's `size`) |
| Export wrapper | Decodes the input into fresh counted objects (decoders already copy; nothing points into the input buffer, so step 2 is safe). Those values are **owned** arguments. Calls the function. Encodes the result into a raw output block. **Drops the result.** Returns the block; the host frees it in step 3. |
| Import wrapper | Encodes the arguments (borrowed: the wrapper only reads them) into a raw block, sized from the values instead of a fixed 4 or 16 KiB. Calls the host. Frees the argument block and the slots (or uses one static slot pair). **Reads the status**: if the guest owns the result buffer, decode and then free it; otherwise decode from the scratch area and do not free. The decoded result is owned by the caller. |
| `raw-invoke` | Same as an import wrapper, and the same status fix (1704). |
| `any` arguments and results | an `any` is a counted leaf; the boundary copies blobs in and out as it does now. |

Theater state (`option<list<u8>>` threaded through `init` and handlers) is an ordinary
argument and result, so the same rules cover it.

Re-entrancy: an async host import re-enters the guest through `__pack_alloc` in the
middle of a call. The allocator is a plain function with no state held across calls, so
this is safe; RC state is consistent at every call site.

---

## 5. The allocator and memory growth

### 5.1 Layout

```
0 … user data segments, host slots, scratch, metadata, encoder stack   (as today, fixed)
… static objects (string literals, compile-time constants)          (new, stage 0b)
$__heap_base: free-list table, then the heap, growing upward
```

`$__heap_base` is a compile-time constant computed after placing static data. User
`(data OFFSET …)` segments get an overlap check against the fixed regions at compile time.

### 5.2 Small blocks (block size ≤ 1 KiB, header included)

- Size classes with at most 25% waste: 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128,
  160, 192, 224, 256, 320, 384, 448, 512, 640, 768, 896, 1024.
- One singly linked free list per class; the heads live in a table at `$__heap_base`.
  Allocation pops the head; free pushes the block. Both are O(1).
- An empty list carves a new block from the **frontier** (the old bump pointer).
- The class index comes from a 128-entry lookup table indexed by `size / 8`, so the hot
  path has no loop.
- Allocation sites with a size known at compile time (all constructors) get the class
  index as a constant and call `$__alloc_class(k)` directly.

### 5.3 Large blocks (> 1 KiB)

List data buffers and long strings. Power-of-two buckets from 2 KiB up, with free lists
and no coalescing. Doubling growth of lists and strings asks for these sizes exactly,
so the waste is small. If measurements show fragmentation, replace this with TLSF
(two-level segregated fit, O(1), with coalescing) without changing any interface.

### 5.4 Memory growth

- Declare `(memory (export "memory") N)` with no maximum. `N` covers the fixed regions,
  the static data, and a first heap chunk (about 1 MiB).
- When the frontier reaches the end, `memory.grow` by `max(needed, current / 4)` pages,
  so growth is geometric. If the grow fails, trap with a clear message (today: an
  out-of-bounds trap far from the cause).
- Wasm memory cannot shrink. A peak stays committed; freed blocks are reused. This is
  acceptable for actors whose live set is stable, and stage 5 measures it.
- So a REPL session's committed memory is set by its largest single evaluation (for
  example a big `require`), not by its steady state.

### 5.5 Debug support (the same code with a flag)

- `free` fills the block with 0xDD and sets `rc` to a poison value; `dup`/`drop` of a
  poisoned header traps (use-after-free and double free detection).
- `__granite_heap_stats` export: live bytes, live objects per size class, frontier,
  memory pages. Leak tests read it.
- `__granite_heap_verify` export: walks every object reachable from the globals,
  counts references, and checks that each `rc` equals the count it found and that the
  census has no extra live objects. Exact, because the heap is acyclic and every type is
  known.

---

## 6. Performance

### 6.1 Today (measured)

Release build, wasmtime 21, this machine. Harness: `tests/memory_baseline.rs` (run with
`cargo test --release --test memory_baseline -- --ignored --nocapture`). Allocation
profiles came from instrumenting every `global.set $__heap_ptr` in the emitted WAT with a
per-site byte counter.

**Self-hosted compiler build** (gen-1 = the Rust-compiled `examples/wisp-compiler.wisp`
compiling its own 151 KB source into 1.0 MB of WAT):

| | Allocated | Time |
|---|---|---|
| Today | **1408 MiB** (777,713 allocations) | 0.79 s |
| …of which `list-push` copying in `tokenize-acc` | 1348 MiB (95.7%; 27,070 pushes, 52 KB average) | |
| …string operations | 56 MiB (3.9%) | |
| Prototype: `list-push` with amortized doubling (codegen experiment, reverted; all `list`, `self_hosted`, `interpreter` tests passed) | **61.5 MiB** | **0.05 s** |
| gen-2 (its emitted runtime already doubles) compiling the same source | 60.2 MiB | 0.05 s |

The fixed 2 GiB memory exists only because of the O(n²) push.

**Wisp interpreter** (`wisp/interpreter/evaluator.wisp`, the core of every REPL
session; bytes allocated per `evaluate` call):

| Input | Bytes per eval | Time per eval |
|---|---|---|
| `(+ 1 2)` | 4,437 | 7.9 µs |
| `"hello"` | 902 | 3.7 µs |
| `(define x (list 1 2 3))` | 11,423 | 19.2 µs |
| `(count 1000)` (a tail-recursive loop) | 6,102,757 (**6.1 KB per iteration**) | 22-31 ms (varies between runs) |
| `(on-message (list 1 2))` (a trigger-style call) | 10,511 | 30.7 µs |

Profile of `(count 1000)`: 500 allocations per iteration, and **90.6% of the bytes are
string literals** rebuilt on every evaluation (`"f32"` in `float-type?`, the names in
`host-builtin?`, `quotation-prefix?`, …). The other 9.4% are variants, lists and
strings that are garbage the moment the iteration ends. At 6.1 KB per iteration a
session fills its 2 GiB after about 330,000 loop iterations in total, over its whole
life.

### 6.2 Expected after each stage

| Stage | Self-compile | Wisp loop |
|---|---|---|
| 0a amortized `list-push` | 62 MiB, 0.05 s (measured by prototype) | 17.6 ms per 1000 iterations in one run (copying envs gets cheaper; timings are noisy), bytes unchanged |
| 0b static literals | slightly less | about 0.6 KB per iteration (estimated from the profile: −90.6%) |
| 1-3 allocator, boundary frees | same | same per eval, minus the input/output buffers of every actor call |
| 4 RC (everything owned, no borrowing) | peak drops to the live set; time may go up as much as 2× | **flat**: memory stops growing |
| 5 borrowing, reuse, take rule | time within **1.25×** of stage 0 (budget) | time at or below stage 0; the interpreter's defensive `copy-env`/`copy-list` can go |

The budget in stage 5 is a gate: if the self-compile is more than 1.25 times slower than
after stage 0, stage 5 does not ship until it is. The numbers come from the same
harness, run before and after each stage and recorded in the PR.

Why RC stays cheap here: most of the interpreter's work is predicates and lookups over
borrowed data (`string=?`, `lookup-local`, `list-get`), which borrow inference makes
free of counting. The rest is accumulator threading, which reuse runs in place. The
remaining cost is one compare and an increment or decrement per owned pointer copy.

---

## 7. Staged plan

Every stage ships on its own and keeps `cargo test --workspace` and the
`theater-repl` suite green. "Leak test" means: instantiate the module, run a workload
`N` times after a warm-up, and assert that `__granite_heap_stats().live_bytes` (and the
memory page count) is the same after `N` and `2N` runs. The interpreter leak tests use
the actor workloads from §6.1: a Wisp loop, repeated `define`, a trigger handler called
10,000 times.

The changes go in the Rust compiler (it builds the actor and the self-hosted compiler).
The self-hosted compiler's emitted runtime takes the same changes as part of the parity
work (`docs/changes/SELF-HOSTED-PARITY.md`); it is not on any session's path.

**Stage 0a: amortized `list-push`.** Use the `cap` field: grow by doubling, store into
spare capacity. Semantics unchanged (still in place). Tests: existing suites; a new test
pushes 100,000 elements and checks total allocation is O(n) via `__pack_alloc(0)`
before and after. Ship the 2 GiB → smaller memory change only after stage 1.

**Stage 0b: static string literals.** Emit each distinct literal once in a data segment
below the heap; a literal evaluates to its address. Nothing writes into strings today,
so sharing them is safe. Tests: string suites; a test that a literal evaluated in a loop
allocates nothing.

**Stage 0c: the interpreter's own cheap wins (Wisp side, ticket 0012).** `marshal.wisp`
allocates a fresh 16,388-byte buffer for every marshal (`marshal.wisp:299, 319`), about
16 KB of a host call's 22-33 KB. Keep one buffer in a global and reuse it. The other
interpreter-side fixes are in [WISP-RETENTION.md](WISP-RETENTION.md): §2.1 (`$bindings`
and `$session-macros` upsert on redefine, so they are bounded by distinct names) and
§2.3 (`$events` becomes a bounded buffer with a drop counter). They need no compiler
change and can land any time. The combined order is stages 0a, 0b, 0c, then 1. Tests:
the host-call bytes in `memory_baseline` drop by about 16 KB per call.

**Stage 1: the allocator and growable memory.** §5: header, size classes, large
buckets, frontier, `memory.grow` without a maximum, a real `__pack_free`, stats export,
debug poisoning. Nothing in compiled code frees yet except `__pack_free`. Tests:
allocator unit tests (alloc/free patterns, every class, large blocks, growth past the
initial memory); an export called 10,000 times keeps flat memory (input and output
buffers are now reclaimed by the host's `__pack_free`); the actor no longer declares 2 GiB.

**Stage 2: frees at sites with an obvious owner.** No counting yet:
- the import wrapper frees its argument buffer and slots, and frees a guest-owned result
  buffer after decoding (read the status);
- `raw-invoke` the same;
- `list-push` frees the old data buffer when it grows (a header is never copied, so the
  old buffer has no other owner);
- `heap-free` added; `marshal.wisp` frees its scratch buffer.

Tests: a leak test that calls a host import 10,000 times; the Wisp `(sessions)` or
`(now)` verb in a loop with flat memory.

**Stage 3: value-semantics groundwork.** Behind a compiler flag, `list-push` copies when
it cannot prove the list is unique (no RC yet, so: copies unless the list operand is a
local's last use, a simple syntactic check). Run the whole suite and the interpreter
with the flag on to find any code that depends on the in-place write; fix those sites.
Performance with the flag on: every push that is not a last use copies the whole list,
O(n) per push, so loops over lists held in globals (`$bindings`, `$events`) go back to
O(n²). The flag is for finding dependencies only. It must stay off by default until
stage 4 gives it a uniqueness test.
Tests: a new codegen test that `(list-push xs v)` leaves `xs` unchanged, matching
`granite eval` (the back-ends agree for the first time).

**Stage 4: RC, correct before fast.** Headers carry counts; the dup/drop pass (§3.3)
with every parameter owned; per-type drop functions; `global.set` drops the old value;
`list-push` value semantics with the `rc == 1` fast path; raw-memory rules (§3.6) with
the interpreter call sites updated; `__granite_heap_verify` in debug builds. Behind a
flag until the leak tests pass, then on by default. Tests: run every test with the
verifier after each export call (exact counts, no extra live objects); leak tests for a
Wisp loop, repeated `define`, repeated `require`, 10,000 trigger firings; a
use-after-free test suite (debug poisoning traps); the cycle program from §2.2 now
returns 1 (no cycle).

**Stage 5: make it cheap.** Borrowed-parameter inference, drop-guided reuse,
`string-append` in place, the global take rule, early drops before tail calls. Remove
`copy-env`/`copy-list` from the interpreter. Tests: the performance gate in §6.2; the
verifier still passes; a test that a threaded `list-push` accumulator of 100,000
elements does not copy (allocation count).

**Stage 6: clean up.** Remove the inline bump code and the 2 GiB comment; retire
`generate_wat` (the non-Pack REPL path) or port it; mirror the runtime changes in the
self-hosted compiler; document the "no mutable cells without a cycle plan" rule in the
language docs.

---

## 8. Problems found on the way (not fixed here)

- The host scratch buffer (0x4008-0xC008) overlaps the metadata, the encoder stack,
  and the heap start if a synchronous host return is larger than about 24 KiB.
- `string-to-bytes` and `string-from-bytes` (1904-2003) use a `[len][bytes]` layout
  that is not the list layout, and `string-from-bytes` reads bytes from the `cap` field.
  Nothing uses them yet.
- A `u64` payload in `some`/`ok`/`err`, and a `u64` list element, are written with
  `i32.store` (upper half lost).
- The export output buffer is a fixed 16 KiB for most types, and the encoder cursor is
  not checked; the import argument buffer is a fixed 4 or 16 KiB, also unchecked.
  `marshal.wisp` uses a fixed 16,388-byte buffer with no bounds check.
- The import wrappers ignore the host's ownership status (4534, 1704).
- The recursive CGRF encoder and decoder have `TODO` arms for records, variants and
  `any` (5554, 6094).
- The legacy `generate_wat` starts the heap at 0, on top of user data segments, and
  its ABI wrapper allocates 4 bytes for a record parameter and writes all its fields.
- `granite eval` and compiled code disagree on `list-push` (§2.2).
