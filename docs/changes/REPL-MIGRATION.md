# REPL build migration to the new packr API

**Status:** Migration ✅ complete and proven working (ran live, returned `42`, drove the
self-hosted compiler). Fresh builds are currently blocked by an **external** theater-checkout
dependency issue (not wisp code). Defining *generic* fns at the REPL is a follow-up.

## What changed (`crates/test-runtime/src/main.rs`)

The REPL/runtime was written against the old `theater::pack_bridge` API (`AsyncRuntime`,
`Ctx`/`AsyncCtx`, `PackInstance::new(name, &wasm, &rt, store, |builder| builder.interface().
func_typed())` + `.await`). The new packr-core 0.24 model is capture-based. Migrated to:

- imports: `AsyncRuntime`/`Ctx` → `host_fn, CachingPackRuntime, HostImports, PackInstance, WasmEngine`.
- a helper `make_pack_instance(name, wasm, imports)`: `CachingPackRuntime::new()` →
  `compile_cached(&wasm)` → `engine().instantiate(&module, imports)` → sync
  `PackInstance::new(name, instance, create_actor_store(), Arc::new(wasm))`. (The
  `WasmtimeInstance` owns its store, so the local runtime can be dropped.)
- host imports built with `HostImports::new()` + `.define(iface, fn, host_fn(|v| async {…}))`;
  helpers `log_imports(tag)` and `add_assembler(&mut imports)`.
- `create_actor_store`: `ActorStore::new` is now 4-arg with an **unbounded** `theater_tx`.
- `WasmEventData::WasmResult`: the `state` field was removed (5 sites).
- `--test-repl-actor`: its async `wisp:evaluator` host fn captured a `PackInstance` (not
  `Sync` under the new `host_fn` bounds). Stubbed — it's a demo; `--repl` is unaffected.

## CGRF ABI version bump (v2 → v3)

The runtime (packr-abi 0.24) uses CGRF `VERSION = 3`; the Rust compiler emitted `2`, so the
runtime rejected the compiler's output (`InvalidEncoding("Unsupported version")`). The header
and String-node formats are byte-identical between v2 and v3, so this was a pure version bump:
`CGRF_VERSION` in `src/compiler.rs` and the test harness's `write_cgrf_string` version in
`tests/self_hosted.rs`, then regenerate `examples/wisp-compiler.wasm`. Fixpoint stays green.

## Proven working

`cargo run -p test-runtime -- --repl` (with a prebuilt `theater.rlib`):
```
wisp> (i32.add (i32.const 40) (i32.const 2))
42
```

## Blocker for fresh builds (external)

theater's local checkout (`../theater`, v0.3.29) is mid-development against an unpublished
packr and does not compile cleanly against the crates.io packr:
- packr-core **0.24.1**: `theater_runtime.rs:882` — `setup_tx` oneshot type mismatch
  (`Result<(), _>` vs `Result<MetadataWithHashes, _>`), a packr-core version duplication.
- packr-core **0.24.0**: `MetadataWithHashes` not found.
This is theater's to resolve (align its packr dependency). Once it builds, the migrated REPL
works as shown.

## Follow-up: defining generic fns at the REPL

The REPL compiles each `(fn …)` standalone as `(export (fn …))` to validate it. A **generic**
fn is a template (`is-generic-fn` detects a `(where …)` or `(-> …)` param) — it can't be
compiled standalone (it has no concrete types yet), and `(export (fn …))` isn't detected as a
template by `monomorphize` (which only matches bare top-level `(fn …)`), so the `(where T)` is
mis-read as the body. Fix: in the DefineFn path, if the fn is generic, **store** it in the
accumulated `functions` without a standalone compile; it then materializes when a later
expression calls it (the full compile sees template + call → monomorphize specializes).
Non-generic fns and plain expressions already work.

## Note: workspace resolution

The Theater integrations now have a separate workspace in `crates/`. The root
workspace contains only `wisp` and `wisp-repl`, so `cargo test --workspace` no longer
resolves Theater dependencies. Use `--manifest-path crates/Cargo.toml` to select
the integrations; see [their build notes](../../crates/README.md).

The old root `.cargo/config.toml` was local and ignored by Git. Its overrides have
been preserved as inactive `.cargo/theater.local.toml`. They reference crates that
no longer exist in the sibling checkout, including `theater-handler-supervisor`
and `val-serde`; do not reactivate them without updating the paths and versions.

The root CLI and `wisp-repl` still pin `pack` v0.2.0 (CGRF v2), whereas compiler
return values use CGRF v3. The compiler tests pass via direct Wasmtime calls, but
the old Pack runners need a separate ABI migration.
