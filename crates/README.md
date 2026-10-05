# Theater integrations

This is a separate Cargo workspace for experimental Theater integrations. The
compiler and Rust-backed REPL live in the root workspace and can be built and
tested without resolving Theater dependencies.

| Crate | Purpose |
| --- | --- |
| `test-runtime` | Runtime experiments and self-hosted compiler REPL |
| `theater-repl` | Actor-based REPL using Theater handlers |
| `theater-handler-wisp` | Assembly, composition, and evaluation host functions |
| `assembler-handler` | Older handler, excluded pending API updates |

## Build status

These integrations are mid-migration between Theater/Pack APIs. `test-runtime`
uses the newer capture-based host API, while `theater-repl` and
`theater-handler-wisp` still use older APIs. Their manifests pin Theater v0.3.0;
the migrated runtime needs dependency alignment before a fresh build can succeed.
See [REPL-MIGRATION.md](../docs/changes/REPL-MIGRATION.md) for details.

After aligning dependencies, select this workspace explicitly:

```sh
cargo build --manifest-path crates/Cargo.toml -p test-runtime
cargo run --manifest-path crates/Cargo.toml -p test-runtime -- --repl
```

Keep local Theater overrides in `crates/.cargo/config.toml` and run Cargo from
`crates/` when using them. Cargo discovers configuration from the working
directory, not the manifest path. Machine-specific sibling paths do not belong
in root Cargo configuration: even unused patches can block compiler builds.

Previous local overrides, when present, are preserved at
`../.cargo/theater.local.toml` as an inactive reference. Verify paths and API
versions before reusing them; newer Theater checkouts may no longer contain
`theater-handler-supervisor` or `val-serde`.
