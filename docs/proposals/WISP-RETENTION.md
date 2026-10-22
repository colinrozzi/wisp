# Wisp retention: what the interpreter keeps alive, and a policy for it

Status: proposal (ticket 0012). Companion to `MEMORY.md` (ticket 0011), which
designs how Granite frees memory. This doc covers the other half: what the Wisp
interpreter actor (`wisp/interpreter/`, `wisp/actor/`) keeps **reachable on
purpose**, so no collector can free it.

Every theater-repl session is one of these actors, and some live for days (the
registry, github-events, everyone's REPL). Today Granite frees nothing, so a
session grows on every allocation. After 0011, a session grows only by what it
retains. This doc lists that retained state, says how each item grows, and
proposes a limit or a release rule for each.

## Summary

- **Roots.** The interpreter has 23 mutable globals. Ten hold data that lives
  across evaluations. The others are per-evaluation scratch or scalars.
- **Growth that never stops today, even with a collector:**
  1. **`$bindings`: redefinition appends.** `publish` pushes a new binding and
     never removes the old one, so each `(define f ...)` of an existing name
     keeps the old value alive. Fix: make `publish` replace the binding
     (an upsert). No program can see the change, because lookup already finds
     the newest binding.
  2. **`$events`: firings buffer with no limit until `(poll-events)` drains
     them.** Each firing keeps the full event. A message is held as a
     `(list u8)` of 24-byte value cells, so a buffered message costs about
     **28 times its size**. Fix: a bounded buffer that drops the oldest
     firing and counts the drops. Keep the event only when it was not handled.
  3. **`$session-macros`: redefinition appends** (the same issue as
     `$bindings`). Fix: upsert.
- **Bounded by the program text, no action needed:** modules (one entry for
  each name), types, traits, instances, and explicit globals.
- **Reload** leaves no old module versions in the registry. An old version
  stays reachable only through values copied out by hand, and each copy keeps
  only its own closure body. Keep the current semantics.
- **Cycles.** Closures and environments cannot form a cycle. **User data can:**
  Wisp `list-push` mutates a list in place, so a value of a recursive nominal
  type can contain itself. This is the one cycle source. 0011 (mona) has
  decided to give `list-push` value semantics and to use compile-time
  reference counting. That removes this source, so with 0011 the Wisp heap is
  **acyclic**. Any future Wisp mutable cell (`set!` on locals, `letrec`,
  boxes) must keep back-references by name. See §3.
- **Measurement.** Today almost all growth is garbage, not retained state. A
  registry lookup allocates **~454 KB**, not ~1 KB. A 1 KB inbound message
  allocates ~2 MB, and `(reload registry)` allocates ~215 MB. With a 2 GB fixed
  memory, the registry runs out after about 4,500 requests. After 0011 frees
  garbage, a well-behaved service stays flat. What grows after that is
  `$events` in sessions nobody drains, and `$bindings` in interactive REPLs that
  redefine names.

## 1. Inventory

### 1.1 How Wisp data is laid out

This affects every size below:

- A `value` (`values.wisp`) is a Granite variant. Its size is a 4-byte
  discriminant plus its largest payload (`generic-function`, five pointers),
  so **every value is a 24-byte cell**. That includes a `byte-value`, an
  `integer` and a `boolean`.
- A Granite list is a 12-byte header (len, cap, data) plus a data array of
  4 bytes per element (pointer-sized, `type_size` in `src/compiler/mod.rs`).
  `list-push` **mutates the header in place**. It also **allocates a new data
  array one element larger on every push** and copies the old one
  (`codegen.rs`, `Expr::ListPush`). So building an n-element list one push at
  a time allocates about 2n² bytes, and only 4n of them stay live.
- So a `(list u8)` holding a string of n bytes is live at ~28n bytes. It costs
  about 2n² bytes to build, and the reader, `unmarshal` and `string->bytes` all
  build it one push at a time.

### 1.2 Roots: the 23 mutable globals

"Grows" means across evaluations, in a session that lives for a long time.
"Shrinks" means that today some operation makes entries unreachable.

| Global (file) | Holds | Grows by | Shrinks today? |
|---|---|---|---|
| `$bindings` (evaluator) | Env of the **current** module: `(binding name value)`, newest last | every `define`, `fn`, `record`/`variant` (constructor and each accessor `T.f`), `trait` (one per method), each name a `require` imports, accessors that `unmarshal` registers for new host record shapes | **No.** Redefinition appends; the old binding stays. Only `(reload M)` removes, and only the names it rebinds (`drop-binding`) |
| `$modules` (modules) | Suspended modules: name, env, exports, imports, source | one entry for each new module name | Replaced on switch and reload; **one entry for each name**, never removed |
| `$current-exports`, `$current-imports` (modules) | Current module's `provide` names and `(name . source-module)` imports | each `provide`, each imported name | No. `provide` of the same name appends a duplicate |
| `$current-module`, `$current-source` (modules) | Two strings | — | Replaced |
| `$types` (types) | Every named type: `(named-type name id variant? schema)`, **session-wide, not per module** | `record`, `variant`, host record shapes registered by `unmarshal` (`register-record`, one for each name and shape) | **Never, by design.** A value carries its type id as an index into this list |
| `$traits`, `$instances` (traits) | Trait signatures; instance method tables (closures) | `trait`, `instance`, `derive` | Never. Redeclaration is refused, so they are bounded by distinct names |
| `$session-globals` (globals) | `(global $g ...)` cells: name, type, one value | each new global | Bounded by distinct names. `global.set` replaces the value |
| `$session-macros` (macros) | `defmacro` / `define-syntax` definitions | each macro definition | **No.** Redefinition appends, like `$bindings` |
| `$events` (rpc) | Buffered trigger firings: `(handler-name event result)` | **every inbound event**: message, tick, tcp, process output, lifecycle event, … | Only `(poll-events)` empties it. **No limit** |
| `$pending-macros` (macros) | A copy of `$session-macros` made for each evaluation | — | Replaced on each evaluation (a garbage copy each time) |
| `$include-seen`, `$loading` (loading, modules) | Paths and names for the current evaluation | — | Reset on each evaluation |
| `$sr-vars` (syntax-rules) | Pattern variables of the last `syntax-rules` compiled | — | Reset for each definition; small |
| `$started`, `$steps`, `$macro-steps`, `$syntax-id`, `$enc-cur`, `$enc-idx`, `$tag-end` | Scalars | — | — |

Wisp values can also refer to other data, and that data stays alive with them:

- **Closures**: `(closure params body env home)`. `body` is the syntax tree
  (the reader's output). `env` is the lexical env list at creation (empty at
  top level). `home` is the module **name**, as a string.
- **Typed functions, generic functions, constructors**: syntax and type
  schemas. They have no env.
- **Syntax trees**: kept by every closure and macro. The evaluation that
  created them keeps no other copy.

### 1.3 Resources outside the wasm heap

These live in Theater, not in the actor's memory. Wisp holds them only as id
**strings**, so a collector cannot release them on unreachability: an id can
be copied, printed and typed again. Each one stays open until it is closed
explicitly or the actor stops.

| Resource | Opened by | Released by | Note |
|---|---|---|---|
| Timers | `set-interval` | `clear-interval` | Each firing also adds an `$events` entry |
| TCP listeners and connections | `tcp-listen`, `tcp-connect`, accept | `tcp-close`, `tcp-close-listener` | `on-data` firings buffer |
| Processes | `process-spawn`, `process-run` | `process-close`, `process-kill` | `on-output` firings buffer, one for each chunk |
| Channels | `msg-open`, inbound open | `msg-close-channel` | |
| Message registration, spawn subscriptions, monitors, links | `msg-register`, `subscribe-spawns`, `monitor`, `link` | `unsubscribe-spawns`, `unmonitor`, `unlink` | A bare `monitor` delivers every chain event of the actor it watches (the registry header notes this ran a registry out of memory) |
| Spawned actors | `spawn-actor` | `stop-actor`, `kill-actor` | |
| Content stores | `store-new`, `store-put`, `store-put-at` | — | **On disk; they outlive the actor.** Content-addressed: relabelling a label (the registry's table) leaves old content behind |

`(process-list)` lists processes, but there is no way to list a session's
open timers, sockets or channels.

### 1.4 Garbage (for scale; 0011 frees all of it)

None of this is retained, but today it is most of the growth (§4):

- every eval step: `lookup` builds a failure string (`"unbound symbol: ..."`)
  for each name that misses the local env, and calls copy their env and
  argument lists;
- every host call that marshals: `marshal` allocates a fixed 16,388-byte
  buffer (`marshal.wisp` has a TODO for this);
- every byte list: the 2n² build cost (§1.1);
- every evaluation: a copy of `$session-macros`;
- every call into a closure from another module: `switch-module` rebuilds
  `$modules` twice;
- printing: `show` builds strings by repeated `string-append`.

## 2. Policy

There are three rules, as the ticket frames them: **bounded** (a stated
limit, plus what happens at it), **released when unreachable** (relies on
0011), or **freed explicitly** (a verb).

### 2.1 `$bindings`: upsert on redefine (bounded by distinct names)

Change `publish` to replace an existing binding of the same name instead of
appending. Then `$bindings` is bounded by the number of distinct top-level names,
and an old value becomes unreachable as soon as nothing else holds it (0011
then frees it).

No program can see the difference. `lookup-local` scans from the end, so the
newest binding already shadows every older one. `reload` already removes
the old bindings with `drop-binding` for the names it rebinds.

The upsert must not depend on in-place mutation. After 0011, `list-push` has
value semantics, so `publish` must store the list it gets back (it already
does this). `$bindings` and the suspended module record must not be
expected to alias. When the list is unshared, RC reuse makes the update
in place, and the replaced value is freed at once.

`$current-exports` gets the same rule: `provide` should not append a name that
is already there.

**Optional verb: `(unbind NAME)`.** It removes a top-level name from the
current module. Without it, a session can never get rid of a name it once
defined. That is a small cost and there is no request for it yet, so this
proposal does not require it.

### 2.2 `$session-macros`: upsert on redefine

This is the same change as §2.1, for macros.

### 2.3 `$events`: a bounded buffer with a drop counter

Today every firing of every trigger is kept until `(poll-events)` drains it.
That works only for a session that someone `follow`s, or one that drains
itself (the registry drains every minute; github-events drains in
`ge-output`). If nobody follows a session, its buffer grows without limit,
at ~28 bytes for each byte of each message.

Proposal:

1. **Keep less for each firing.** Record `(handler-name result)`. Keep the
   event only when it was **not handled**: there was no handler, or the
   handler returned a failure. A firing that succeeded has delivered its event
   already. `theater-repl read` (text mode) already throws the event away.
   For a message, this cuts what each firing keeps from ~28n bytes to the
   size of the result.
2. **Limit the buffer to 256 firings** by default. When it is full, **drop
   the oldest** and increment `$events-dropped`. The newest firings are the
   ones a person who just ran `read` wants to see. Refusing new firings is not
   an option, because the runtime has already delivered them.
3. **Report the drops.** `(poll-events "text")` prints
   `"(N earlier firings dropped)"` as its first line when N > 0, then resets
   the counter. `(poll-events)` returns the count as the first element.
4. **A verb to change the limit: `(event-buffer N)`.** `N = 0` turns off
   buffering. A service like the registry or github-events can then stop
   draining on a timer (its handlers still run). The verb returns the old
   limit.

The 256 cap is ~4 minutes of a 1 Hz ticker. Without the event payload, each
firing is about the size of its result, so at a typical small result the
full buffer is tens of KB.

### 2.4 Modules and `(reload NAME)`

What exists today, and what this proposal keeps:

- The registry holds **one entry for each module name**. `reload` replaces it.
  No list of old versions is kept.
- Importers' copies are rebound and the old bindings are removed.
- A closure keeps its home module by **name**. So a closure from the old
  version that someone copied by hand (`(define h registry-handle)`, a value in
  a list, an instance table) runs its **old body**, but its top-level names
  resolve in the **new** env. It does not keep the old env alive: closures
  never point at a module env. The only thing it keeps from the old version is
  its own body (and its lexical env, which is empty for a top-level lambda).
- After a reload, the old version is **released when unreachable** (relies on
  0011). Normally it is unreachable at once.

**Should an old closure keep working after reload?** Yes. Keep the current
"old body, current names" behavior, for these reasons:

- Retention stays bounded. Nothing pins a version, so chains of old versions
  cannot build up. The worst case is one old body for each value copied out
  by hand.
- Erlang-style "two versions, kill holders of the third" needs process
  identity, and Wisp values do not have it.
- Making a stale closure fail ("module version changed") would need a version
  stamp in every closure. It would also break handlers that someone stored
  before a reload.
- The mixed case (an old body calls a helper that the new version deleted)
  fails with the normal `unbound symbol` error. That is the right failure.

No unload verb is needed. Module count is bounded by the code a session
loads.

**Problem found:** `reload` fails for any module that declares a `record`,
`variant` or `trait` (`error: invalid or already defined type name`),
because `$types` and `$traits` are session-wide. This is outside this ticket
and is listed in the PR.

### 2.5 `$types`, `$traits`, `$instances`, `$session-globals`: never shrink, by design

These are bounded by distinct names in the program text, plus distinct host
record shapes. Values refer to types by **index** (`aggregate name id ...`),
so removing a type would invalidate every value of that type. **No change.**
If per-module reload of types is fixed later (§2.4), it must append a new id
and leave the old one in place, so values of the old type keep working.

### 2.6 Host resources: freed explicitly

A collector cannot release these (ids are strings), and they must not be
released silently. A timer or listener that nothing in the heap refers to is
the normal case for a service. The existing close verbs stay the policy. Add
one thing:

- **`(handles)`**: list the open timers, listeners, connections, processes
  and channels this session opened. To do this, the interpreter keeps a small
  table that the open and close verbs maintain. The table is bounded by the
  number of open handles: entries are removed on close, and on the
  `on-close`/`on-exit` events for that id.

Content stores outlive the session, so they are not a heap question. Old
content left behind when a label moves is a Theater store issue, listed in
the PR.

### 2.7 User-level state

A handler cannot `define`, but it can grow state through `global.set`, a
store, or (today only) a `define`d typed list it `list-push`es to. That last
way stops working after 0011, because `list-push` will no longer mutate
the list. That
growth is the program's own and is the program's responsibility. To make it
visible, add:

- **`(memory-stats)`**: returns heap bytes in use (the bump pointer today;
  the allocator's live total after 0011), and the number of entries in
  `$bindings`, `$modules`, `$types`, `$session-macros` and `$events`, plus
  the drop count. It costs almost nothing and makes the next measurement a
  one-liner from a live session.

### 2.8 Policy table

| Item | Policy | Limit, and what happens at it |
|---|---|---|
| `$bindings` | **Bounded**: upsert | distinct names; optional `(unbind NAME)` |
| `$current-exports` | **Bounded**: no duplicates | distinct names |
| `$session-macros` | **Bounded**: upsert | distinct names |
| `$events` | **Bounded**: ring buffer, keep event only if unhandled | 256 firings (`(event-buffer N)`); drop oldest, count, report |
| `$modules` | **Bounded**: one entry for each name | distinct module names |
| Old module versions | **Released when unreachable** (0011) | at most one old body for each hand-copied value |
| `$types`, `$traits`, `$instances`, `$session-globals` | **Never shrink**: values index into them | distinct declarations |
| Per-evaluation scratch | **Released when unreachable** (0011) | — |
| Host handles | **Freed explicitly** (existing verbs) + `(handles)` | — |
| User data | The program's responsibility; `(memory-stats)` to see it | — |

## 3. Cycles

The question for 0011: can Wisp values form a cycle in the Granite heap?
**Closures and environments: no. User data: yes, through `list-push`.**

### 3.1 Every place an env or a value changes after creation

| Site | What it changes | Can it close a cycle? |
|---|---|---|
| `eval-lambda` | Builds `(closure params body env home)` and captures the env list **itself**, not a copy | Only if something later pushes into that list. Nothing does (next rows) |
| `extend-env` (`let`) | Copies the env, then pushes: a new list | No. The captured list is never touched |
| `apply-value` / `bind-args` | Copies the captured env, then pushes the arguments. **This runs before the body**, so any closure the body creates sees a finished list | No |
| `typed-bindings`, generic and trait application | Build fresh envs the same way | No |
| `let` | Not recursive: the value is evaluated in the **outer** env | No |
| `letrec`, named let, `set!` | Do not exist | — |
| `define` / `publish` | Pushes onto `$bindings` **in place** (the module record shares the list) | No value points at a module env. Closures keep only the module **name**, and top-level names, recursion included, resolve by string lookup. So a closure in `$bindings` does not point back at `$bindings` |
| `fn`, `record`, `variant`, `trait`, `instance` | Publish new values; append to `$types`/`$traits`/`$instances` | No. Type references are ids and names |
| `require` (`bind-exports`) | Publishes the exporter's values into the importer's env | Values are shared, not cyclic. No value points at either env |
| `reload` (`rebind-*`) | Builds new env lists by copying | No |
| `switch-module` | Rebuilds `$modules`, swaps `$bindings` | No |
| `global.set` | Rebuilds `$session-globals` with a one-value contents list | No. A value cannot refer to a global cell |
| `record-event` | Appends to `$events` | No. Nothing points at `$events` |
| `cons`, `cdr`, `car` | Copy | No |
| **`list-push` on a typed list** (`apply-collection`) | Calls Granite `list-push` on the **existing** list, which **mutates its header in place** | **Yes** (§3.2) |

So the coordinator's understanding holds for closures. A closure captures its
env lexically, and top-level names resolve through the module name. Recursion
(`(define f (lambda (n) ... (f ...)))`) looks `f` up by string at call time,
so it creates no pointer.

### 3.2 The cycle: in-place `list-push` and a recursive nominal type

```lisp
(variant tree (leaf s32) (kids (list tree)))
(define ks (list-new tree))
(define n (kids ks))     ; n -> ks
(list-push ks n)         ; ks -> n   : cycle
(match (list-get ks 0) ((kids inner) (list-len inner)) ((leaf x) -1))  ; => 1
```

I checked this in a live session (it answers `1`: the list inside the first
element is `ks` itself). A record whose field is a list of that record works the
same way. Only **nominal** recursive types can do this. A structural type
cannot contain itself: `(list (option X))` cannot hold a `(some <itself>)`.
The printer is safe (`show` has a depth limit of 64). `marshal` of a cyclic
value is not guarded.

Any Granite program can build the same structure, not only Wisp:
`list-push` is the Granite builtin. So this is a finding about **Granite**,
and it refutes "`list-push` returns a new list, so the heap is acyclic" in
0011's ticket.

0011 considered three options:

- **(a) Make `list-push` functional.** It returns a new header and the old
  list is unchanged. That removes the only cycle source, and RC with Perceus
  reuse turns it back into an in-place push when the list is unshared. A
  Wisp program that relies on in-place push breaks: for example a handler
  that appends to a `define`d list, or `(define b a) (list-push a 1)` that
  expects `b` to change. I found no site in the interpreter or in
  `experiment/lib` that depends on the mutation: they all use the returned
  list, and the interpreter copies on purpose (`copy-env`) to avoid it.
- **(b) Keep in-place push, and add cycle collection** (a trial-deletion
  backup collector over lists whose element type is a recursive nominal type,
  or tracing).
- **(c) Keep in-place push, RC only, and document the leak.** Cycles need a
  user-defined recursive type plus a deliberate self-push. They are rare,
  and they leak only that structure.

**Decision (0011, `docs/proposals/MEMORY.md`): (a).** `list-push` gets value
semantics, and the compiler mutates in place only when it can prove the list
is unique. Every Granite heap, and so every Wisp heap, is then acyclic by
construction, and Perceus-style RC is complete. No tracing GC is needed.
This doc depends on that. The Rust `eval` back-end already gives
`list-push` value semantics, so the change also makes the two back-ends
agree. For Wisp users, `(list-push xs v)` stops changing `xs`: only the
returned list contains `v`.

### 3.3 What a future `set!` would do

- **`set!` on a local** (a binding in a captured env): binding records are
  **shared** between env copies (`copy-env` copies pointers to the same
  `binding` records). If `set!` writes `binding.item` in place, then
  `(let (f 0) (begin (set! f (lambda () (f))) f))` gives
  binding → closure → env → binding. This is the classic closure/env cycle,
  and `letrec` and named let need exactly this pattern. With `set!` on locals,
  RC alone is incomplete.
- **`set!` on top-level names only**, as an upsert into `$bindings` (§2.1):
  **no cycle**, because closures do not point at `$bindings`. If `set!` is
  ever added, the recommendation is to keep it top-level only. The same goes
  for a `letrec`: do it by top-level name, as `define` already does.

## 4. Measurements

Harness: `tests/retention_measure.rs` (ignored by default) and
`tests/fixtures/retention/probe.wisp`. The probe is the evaluator plus two
test-only exports. `heap-mark` returns the bump pointer (a 0-byte
`heap-alloc`). `fire-message` delivers text to `on-message` through
`dispatch-event`, as the actor's `handle-send` does. Host calls go to stubs
that return realistic success shapes. The registry is the real
`experiment/lib/registry.wisp`, loaded with `require`. Today nothing is freed,
so each figure is **bytes allocated**: garbage plus retained data.

```
cargo test --test retention_measure -- --ignored --nocapture
```

| Operation | Heap allocated |
|---|---|
| fresh session after first evaluation | 54 KB |
| evaluate `0` | 0.8 KB |
| evaluate `(+ 1 2)` | 4.4 KB |
| `(loop 100)`: 100 iterations of a tail-recursive countdown | 614 KB (~6 KB for each iteration) |
| `define`, with 100 / 500 / 1000 / 2000 bindings | 2.2 / 3.8 / 5.8 / 9.9 KB (linear in binding count: `list-push` copies) |
| redefine one name, with 2000 bindings | 10 KB, and the old value stays |
| host call: `msg-send` / `store-get-by-label` / `actor-status` | 33 / 23 / 22 KB (16 KB marshal buffer + unmarshal) |
| cross-module call `(id 1)` | 2.0 KB |
| inbound message, 64 B / 1 KB / 4 KB | 17 KB / **2.1 MB** / **34 MB** (quadratic: byte list built one push at a time) |
| draining 20 buffered 1 KB messages with `(poll-events)` (prints them) | 12.7 MB |
| **registry lookup**, one request, end to end | **~465 KB** (`registry-handle` alone: 454 KB) |
| registry register (no change) | ~414 KB |
| `(reload registry)` (9 KB source) | **~215 MB** (unmarshalling the file's bytes) |

### 4.1 How fast long-lived sessions grow today

- **The registry:** ~450 KB for each request. The actor's memory is fixed at
  2 GB (`(memory 32000 32000)`), so the registry fails after **about 4,500
  requests** of any kind. The ticket's "~1 KB per request" is about 450 times
  too low. Nearly all of it is garbage: eval steps (the handler runs a few
  hundred steps at ~1–6 KB each), three or four host calls at ~25 KB each,
  and the message byte list.
- **github-events** has the same per-step and per-host-call costs. Its own
  header estimates some 6,000 sends before it runs out of memory, which agrees
  with these figures.
- **Interactive REPLs:** each evaluation costs KB to MB. A `require` of a
  typical library file costs tens to hundreds of MB, because of the quadratic
  byte list.

### 4.2 What dominates after 0011 frees garbage

These figures come from the layout in §1.1, not from a collector run:

- **A service that drains its events (the registry, github-events): flat.**
  The live set is the loaded code plus the firings since the last drain. A
  registry firing keeps a ~60-byte message (~1.7 KB as a byte list) plus a
  ~80-byte result string, so ~2 KB. At one request per second that is
  ~120 KB before the 60-second drain. That is the largest item, and it is
  bounded.
- **A session nobody drains: `$events`, without limit.** ~28 bytes for each
  byte of message. 1,000 messages of 1 KB keep ~30 MB alive. This is **the
  dominant retained item**, and it is why §2.3 is the most important change.
  A busy `on-output` (a process writing to stdout) or a bare `monitor` fills
  it fastest.
- **An interactive REPL: `$bindings` through redefinition.** Each redefinition
  keeps the old value, which for a closure is its whole syntax tree: ~28 bytes
  for each token, so a 50-line function is roughly 10–20 KB. Someone who
  iterates on a function 200 times in a day keeps a few MB. §2.1 removes it.
- Everything else (modules, types, traits, globals, macros once upserted) is
  bounded by the code loaded and is KB-sized.

## 5. Problems found outside this ticket

These are for the PR description; I have not fixed them:

1. **`list-push` reallocates the whole data array on every push**
   (`codegen.rs`, "simplified version that reallocates every time"). Building
   an n-element list costs O(n²) bytes and time. This is the largest single
   source of garbage: 1 KB message → 2 MB, 9 KB module → 215 MB. Growing the
   capacity by doubling (the header already has a cap field) fixes it whatever
   0011 decides.
2. **`marshal` allocates a fixed 16,388-byte buffer on every host call**
   (`marshal.wisp`, TODO).
3. **`reload` fails for modules that declare `record`, `variant` or `trait`**:
   `$types` is session-wide.
4. **The evaluator allocates ~1–6 KB for each step** (failure strings for
   misses in the local env during `lookup`, env and argument copies). This is
   not broken down further here.
5. **Theater-side growth is not measured:** the actor's chain (every host
   call and message is recorded), and content-store data left behind when a
   label moves.
