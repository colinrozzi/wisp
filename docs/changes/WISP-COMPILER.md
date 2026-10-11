# Wisp Compiler (dual back-end: compile Wisp → wasm)

**Status**: B0 done (target proven). B1 (the emitter) next. The design below is
settled; the closure emit strategy (B2) is deferred to be decided with B0/B1
experience.
**Started**: 2026-10-11
**Related**: [WISP-MODULES.md](WISP-MODULES.md) (the module = compilation unit),
[WISP-DISTRIBUTION.md](WISP-DISTRIBUTION.md) (`require` boundary a compiled module
shares), the dual-back-end direction memory.

## Objective

Give **Wisp** a second back-end: compile a module to a **Pack package** (the same
artifact Granite emits), so a dynamic Lisp gets a stable, distributable binary. A
compiled module slots into the exact `require`/distribution boundary already built. The
strategic bet: nail the foundational compile capability, then let others build the
ecosystem (standard library included) on top of stable artifacts.

Mirror Granite's own shape: one shared front-end feeding two back-ends.
`analyze → {codegen, eval}` for Granite; **reader + macros → {tree-walker, wasm}** for
Wisp. Only the back-end differs; the reader (`reader.wisp`) and macro expander are
reused unchanged.

## Decision: pure AOT (not a bundled dynamic image)

A compiled artifact contains the **value representation, allocator, `apply` (dynamic
dispatch), the builtins used, and closure machinery** — but **drops the runtime reader
and evaluator**. Rationale:

- **It loses nothing today.** Wisp has no user-callable `eval`/`read` (only the host
  `evaluate` entry — the REPL's read-eval boundary). Macros are **compile-time**
  (expanded in the shared front-end). So functions + macros all compile; a compiled
  library is identical in power to the same library interpreted.
- **It's what makes compiling worth it.** Bundling the reader+evaluator would make an
  artifact ≈ *the interpreter + a few precompiled functions* — barely different from
  today's actor (interpreter + embedded source), forfeiting the lean-artifact payoff.
- **The dynamic story stays with the interpreter.** Interpret for dynamic/exploratory
  work (the actor we ship); compile for durable/distributable libraries. This is the
  Granite/Wisp philosophy applied *inside* Wisp.
- **Clean future line:** if a runtime `(eval …)` is ever added, modules needing it stay
  interpreted ("needs eval → interpret") — the Granite/Wisp split in miniature.

## Architecture: specialize the interpreter

The runtime already exists. The tagged `value` type (`values.wisp`), `apply-value`, the
builtins, and closures-as-data are all written in Granite in the interpreter. So
**compiling ≈ specializing the interpreter**: for each form, emit code that calls the
runtime with the AST baked in, instead of walking it at runtime. A compiled
`(define add1 (lambda (x) (+ x 1)))` becomes, in spirit, a function over `value` that
builds the argument and calls the runtime numeric primitive — exactly what the
interpreter does per node, minus the dispatch.

### Two grounded constraints (from recon, 2026-10-11)

1. **The runtime must be bundled in-module.** `run --dep` links packages via the
   wasmtime component model with the Graph ABI (CGRF) across the boundary — values are
   **serialized, not shared pointers**. So a `value` cannot be passed by reference to a
   separate "runtime" component. The value/apply/builtins runtime therefore lives in the
   **same module** as the compiled code (pure-AOT keeps that bundle minimal by dropping
   reader/eval). A shared-runtime dependency would require core-wasm module linking with
   shared memory — a possible far-future optimization, not the path.
2. **Closures need indirection Granite source lacks.** Granite's surface exposes no
   `call_indirect`/tables/funcref. First-order code (B0/B1) needs none, so the compiler
   emits **Granite source** there and Granite produces the Pack package. Closures (B2)
   need either a minimal Granite extension (function tables + indirect call — a
   foundational addition that keeps one pipeline and free Pack output) or direct WAT
   emission. **Decided at B2**, with B0/B1 experience.

### Emit strategy

- **B0/B1 — emit Granite source.** The Wisp front-end (reader+macros) yields the AST; a
  codegen walks it and emits a Granite program that `include`s the runtime and defines
  each `define`/first-order `lambda` as a `fn` over `value`, exporting the provided
  ones. Granite compiles it → Pack package. No new Granite features; reuses the real
  runtime and the whole codegen/Pack pipeline.
- **B2+ — closures.** Choose the indirection mechanism (Granite extension vs WAT) once
  first-order compilation is proven end to end.

### Artifact = Pack package

A `(module m (provide f g) …)` compiles to a Pack package exporting `f`, `g`; the
`provide` set is the interface, privates don't escape. It joins the same distribution
story as Granite packages and the `require` boundary (B5).

## Bricks

- **B0 — scaffold + value ABI. DONE.** Proved the target by hand (no emitter yet):
  `tests/fixtures/wisp-compiled-add1.wisp` expresses `(lambda (x) (+ x 1))` specialized
  over the real `value` type, `include`s `values.wisp`, compiles to a Pack package, and
  runs → **42** (`tests/wisp_compiler.rs`). Findings:
  - `values.wisp` **is includable standalone** — the value runtime is reusable without
    the evaluator (open question resolved).
  - The compile/run path works via `run-module` (core module), not `run` (component
    parser rejects the core-module Pack package).
  - **Boundary finding:** a *raw recursive* `value` return does **not** round-trip the
    plain CGRF decoder ("Root index out of range"); unwrapping to a scalar works. This is
    why the interpreter's `evaluate` export returns a *string*. Compiled modules will
    expose their provided functions through the actor's existing marshal/`any` bridge
    (`marshal.wisp`) rather than raw `value` — settle this boundary in B1/B5.
- **B1 — first-order functions.** Arithmetic, `if`, `let`, calls to other defs + runtime
  builtins, over `value`. No closures, no `eval`. The emitter covers the first-order
  subset of a `(module …)`.
- **B2 — closures.** Closure-conversion (code reference + captured env); decide the
  indirection mechanism (Granite function tables vs WAT). Dynamic `apply`.
- **B3 — tail calls** (`return_call`) so recursion/iteration don't grow the stack.
- **B4 — coverage** to the compilable subset: records/variants/match, traits-as-used,
  strings, the full builtin surface.
- **B5 — wire into distribution.** A compiled module is a Pack package; teach `require`
  to load compiled modules, not just source (and the compiled module's `provide` set is
  its interface). Shares the capability-bounded loader.

## Open questions (tracked, not blocking)

- **Runtime extraction.** Can `values.wisp` + the needed runtime fns be `include`d into
  a standalone Granite program cleanly, or does the runtime need factoring into an
  includable core? (Answered in B0.)
- **Closure indirection (B2):** extend Granite with function tables / indirect call
  (one pipeline, Pack output free) vs emit WAT directly (full control, hand-built
  linkage). Lean toward the Granite extension if it's small.
- **Compiler host:** written in Granite now (natural — the interpreter is); self-hosting
  in Wisp is a later possibility.
- **Driver shape:** where the Wisp front-end runs to produce the AST the emitter
  consumes (reuse the interpreter to read+expand, vs a Rust-side driver).
