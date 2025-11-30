# Repository Guidelines

## Project Structure & Module Organization
Rust sources live in `src/main.rs`, which houses tokenizing, parsing, and Wasm/WIT emission. Sample programs go under the repo root (see `prog.lisp`) and compiled artifacts are written as `<stem>.wat/.wasm/.wit`. Keep additional compiler modules inside `src/` (e.g., `src/parser.rs`) and add fixtures under a future `tests/fixtures/` directory so they can be shared across regression tests. Temporary build products stay confined to `target/`.

## Build, Test, and Development Commands
- `cargo run -- prog.lisp tiny` compiles the sample program and emits `tiny.wat`, `tiny.wasm`, and `tiny.wit`.
- `cargo run -- <source.lisp> <out-stem>` is the general entry point; it tokenizes, builds the AST, emits WAT/WIT, and converts to Wasm via the `wat` crate.
- `wasmtime --invoke run tiny.wasm 5` executes the exported `run` function for quick sanity checks. Substitute the export (`double`, `factorial`, etc.) as needed.
- `cargo fmt && cargo clippy --all-targets --all-features` keeps the Rust codebase formatted and linted before opening a PR.

## Coding Style & Naming Conventions
Follow idiomatic Rust 2021 style: four-space indentation, snake_case for functions/variables, CamelCase for types/enums, and upper-case acronyms only when conventional (`CmpOp`). Parser, emitter, and analyzer helpers should be grouped in focused modules with clear prefixes (`tokenize_*`, `parse_*`, `generate_*`). Keep S-expression examples in test fixtures short and descriptive (`double_then_factorial.lisp`).

## Testing Guidelines
Automated tests are pending; prefer golden tests that run `cargo run -- tests/fixtures/foo.lisp foo` and diff the produced WAT/WIT to expected snapshots. Until those land, manually recompile `prog.lisp` plus any new fixture and run `wasmtime` against every exported function touched by the change. Name future tests `test_<area>_<behavior>` and keep fixtures deterministic (no randomness or external IO).

## Commit & Pull Request Guidelines
History favors short, imperative summaries (`multiple exports`, `conditionals`). Match that tone, reference the subsystem touched, and keep to ~50 characters when possible. Every PR should describe the compiler surface affected, list new commands or fixtures added, and note manual `wasmtime` output for confidence. Link issues when applicable and attach sample diffs (e.g., snippets of generated WAT) whenever the change alters codegen semantics.
