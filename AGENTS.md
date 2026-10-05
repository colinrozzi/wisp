# Repository Guidelines

## Project Structure & Module Organization
The root Cargo workspace contains the `wisp` compiler and `wisp-repl`. Rust compiler
code lives in `src/compiler.rs`, exposed by `src/lib.rs`; `src/main.rs` implements
the CLI. Keep additional compiler modules inside `src/`. The self-hosted compiler
is `examples/wisp-compiler.lisp`, and standard library sources live in `std/`.
Shared fixtures belong in `tests/fixtures/`. Theater integrations use a separate
workspace in `crates/`; see `crates/README.md` before working on them.

## Build, Test, and Development Commands
- `nix develop` supplies Rust and native build dependencies.
- `cargo build --workspace` builds the compiler and Rust-backed REPL.
- `cargo run -p wisp -- compile examples/prog.lisp` writes `examples/compiled/prog.wasm`.
- `cargo run -p wisp -- compile <source.lisp> <out-stem> --emit-wat --emit-pact`
  also writes readable WAT and interface text. Explicit output stems are relative
  to the current directory; use `target/` for temporary build products.
- `cargo test --workspace` runs compiler and REPL library tests.
- `cargo test -p wisp --test self_hosted test_bootstrap_fixpoint` verifies self-hosting.
- `cargo fmt --all -- --check` and
  `cargo clippy --workspace --all-targets --all-features -- -D warnings` check style.

The current output is a raw Wasm module using the Pack/Graph ABI, not a component.
Do not assume `wasmtime --invoke export module.wasm 5` matches source signatures.
The CLI and interactive REPL still have a CGRF v2/v3 migration gap; see README.md.

## Coding Style & Naming Conventions
Follow idiomatic Rust 2024 style in the root workspace: four-space indentation,
snake_case for functions/variables, and CamelCase for types/enums. Keep parser,
emitter, and analyzer helpers grouped by responsibility. Name S-expression fixtures
descriptively, such as `double_then_factorial.lisp`.

## Testing Guidelines
Use the existing integration suites in `tests/` for regression coverage. Tests
compile short deterministic programs and execute the generated modules through
Wasmtime; self-hosting tests also exercise the compiler written in Wisp. Name new
tests `test_<area>_<behavior>`. Run focused tests during development and the full
root workspace suite for compiler changes. Keep scratch artifacts under `target/`.
Preserve intentional WAT/WIT golden fixtures when cleaning generated files.

## Commit & Pull Request Guidelines
Use short imperative summaries, around 50 characters when possible. Describe the
compiler surface affected, new commands or fixtures, and validation results. Link
issues when applicable and include sample generated-WAT diffs when codegen semantics
change. Preserve existing uncommitted work when making unrelated edits.
