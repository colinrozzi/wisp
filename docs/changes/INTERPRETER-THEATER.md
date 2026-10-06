# Interpreter → Theater integration

Status: engine boundary verified; actor lifecycle and RPC integration remain.
Checked 2026-10-05 against the local Theater checkout at `63cc4294` and the
published Packr 0.24.1 engine crates. This is a local development target, not a
deployment plan.

## What already works

The evaluator is one compiled Wasm module. Each live instance owns its bindings,
closures, types, trait instances, macros, globals, and allocator. Theater's
current in-module state model fits this directly: calls pass arguments and
results; the runtime does not thread an external state value through exports.

`tests/interpreter_engine.rs` uses the same capture-based interfaces exposed by
the local Theater `pack_bridge`: `HostImports`, `host_fn`, `WasmEngine`, and
`packr_core::call_with_value`, with `packr_wasmtime::WasmtimeEngine`. It verifies:

- The evaluator's embedded CGRF metadata is readable by the current metadata API.
- Theater's single-argument tuple convention reaches `evaluate` correctly.
- Definitions and closures survive successive calls to the same instance.
- Separate instances have separate sessions.
- Both source imports work with an in-memory bundle and ordinary result errors.
- Failed includes publish no preceding definitions; language errors leave the
  session usable.

Run from the repository root:

```sh
nix develop --offline -c cargo test --locked --test interpreter_engine
```

This test exercises the engine boundary. It does not instantiate a Theater
`ActorRuntime`, register a handler, replay a chain, or route an RPC.

## Keep the first actor small

The first vertical slice should expose a single typed evaluation operation:
`theater:simple/wisp.evaluate(source: string) -> string`. Retain the existing
printed-value/`error:` result convention for this slice. One actor owns one
evaluator instance, with calls serialized by its mailbox.

The existing exports are `evaluate(source: string) -> string` and
`evaluate-from(source: string, base: string) -> string`. A wrapper can give the
evaluation operation its Theater interface name. Add and test
`theater:simple/actor.init` against the runtime's real init arguments before
calling this a runnable actor. The local canonical example is Theater's
`test-actors/state-test`: it accepts config as `value` and returns
`result<_, string>`. Wisp does not yet expose the general Pack `value` type, and
the compiler's unit construction/code generation is incomplete. Resolve that
init boundary explicitly; do not assume an arbitrary string initializer is
compatible with the existing lifecycle contract.

Do not implement one generated Wasm export per interpreted function. The first
actor can evaluate calls such as `(add-two 40)` through the existing boundary.
Structured values and application-specific RPC exports can follow after the
actor session is proven.

## Host capabilities

The evaluator currently imports only:

| Interface | Function | Type |
| --- | --- | --- |
| `wisp-source` | `resolve-path` | `(base: string, path: string) -> result<string, string>` |
| `wisp-source` | `read-source` | `(path: string) -> result<string, string>` |

Use immutable actor-bundle paths or content-addressed source entries. The local
REPL's filesystem provider is not the actor implementation. These calls must go
through Theater's host-import registry/interceptor so recorded source responses
can be supplied during replay. Include normalization, deduplication, parsing,
and expansion remain in Wisp.

Keep the evaluator's source/file/step limits. The local Rust host additionally
supplies 20 million fuel per request and file-count/total-byte limits; the engine
preflight does not install those host policies. Add actor execution deadlines
and source budgets when building the actual handler. Interpreter allocations
currently last until the instance is discarded, so account for session lifetime
and memory limits in the actor acceptance test.

General Wisp imports and Theater effects (`self.log`, RPC, storage) are a later
slice. Route them through registered host capabilities and the same interceptor;
they do not require serializing the interpreter's environment between calls.

## Existing integration workspace

`crates/` is a separate, older integration workspace. An offline
`cargo check -p test-runtime` currently reaches compilation and fails with 27
API errors. Its manifest pins Theater **v0.3.0** and Pack **v0.2.0**, while its
working source uses the newer `HostImports`/`CachingPackRuntime` API and the
four-argument, in-module-state `ActorStore` construction. The pinned Theater has
neither those imports nor that constructor. This is a reproduced dependency/API
mismatch, not evidence that the new evaluator cannot run on Pack.

The local Theater checkout is newer (`theater` crate version 0.3.29) and uses
`packr-core` plus `packr-wasmtime`. Its full build was not tested here. The other
Wisp integration crates also retain older APIs, including a supervisor-handler
dependency absent from the local Theater workspace. Choose and pin a coherent
Theater/Pack release or revision before migrating this workspace. Keep the root
compiler/interpreter workspace independent. Preserve the existing uncommitted
`test-runtime` migration while doing that work.

## Acceptance checklist for the next slice

1. Align dependencies and build a minimal local Theater host plus interpreter
   actor wrapper; confirm metadata, init, and the namespaced evaluation export.
2. Spawn an actor and issue two RPC calls: define `add-two`, then evaluate
   `(add-two 40)` and receive `42` from the same instance.
3. Spawn another actor and verify the definition is absent there.
4. Load bundled source through the registered imports; verify include errors
   and evaluation errors leave the established session usable.
5. Record the call/import sequence and replay it into a fresh instance, checking
   outputs. Apply the source and execution limits on this actual actor path.

## Language compatibility baseline

Core computation, macros, traits/generics, byte values, unit values, and derived
record equality now have interpreter coverage. Shared fixtures compare supported
behavior with the compilers, with exceptions documented in
[`interpreter/README.md`](../../interpreter/README.md).

Keep separate checklist entries for full expected-type inference, declaration
ordering, static checks, resource handles, raw memory operations, and general
imports. Compiler byte/string conversions still use an older list layout; they
need focused repair/verification before byte-based actor transport relies on
them. None of those should prevent proving a string-based evaluation actor first,
once its lifecycle boundary is handled.
