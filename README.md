# Granite + Wisp

This repo holds **two languages**, and the one-liner is: **Granite compiles Wisp.**

**Granite** is a statically typed, ahead-of-time language that compiles
S-expressions to WebAssembly — WASM instructions nearly 1:1, plus variants and
records, exhaustive `match`, hygienic macros, traits and generics, substructural
linearity, unforgeable capabilities, and type-state. It has S-expression *syntax* but
not Lisp *semantics* (no runtime `eval`, no closures). Granite is the compiler at the
repo root (the `granite` crate + `granite` CLI).

**Wisp** is a small dynamic **Lisp** (closures, a runtime reader, dynamic eval, runtime
macros) written *in* Granite and compiled by it. It lives under `wisp/` and runs as a
Theater actor. This is the actual Lisp. Both languages are first-class.

## Getting started

Enter the development environment with [Nix](https://nixos.org/):

```sh
nix develop
cargo build --workspace
cargo test --workspace
```

The Nix shell provides Rust, Wasmtime, and native build dependencies. With an
existing recent stable Rust toolchain, Cargo can also be used directly;
native dependencies include a C/C++ toolchain, pkg-config, and OpenSSL.

Compile the sample program:

```sh
cargo run -p granite -- compile examples/prog.wisp
```

This writes `examples/compiled/prog.wasm`. The compiler embeds interface metadata
in the module. Request readable output explicitly:

```sh
cargo run -p granite -- compile examples/prog.wisp target/prog --emit-wat --emit-pact
```

An explicit output stem is relative to the current working directory. Without
one, outputs go into `compiled/` beside the source.

## Language

Functions declare parameter and return types; Wasm instructions are directly
available as expressions:

```lisp
(export
  (fn double ((x s32)) s32
    (i32.mul x (i32.const 2))))

(export
  (fn factorial ((n s32)) s32
    (if (i32.eq n (i32.const 0))
      (i32.const 1)
      (i32.mul n (factorial (i32.sub n (i32.const 1)))))))
```

Granite supports:

- Numeric types (`s32`, `s64`, `f32`, `f64`, `u8`), strings, lists, tuples,
  records, variants, options, and results.
- Lexical bindings, conditionals, pattern matching, recursion, memory operations,
  globals, and function imports/exports.
- `defmacro`, hygienic `syntax-rules`, and procedural `syntax-case` macros.
- Traits, instances, generic specialization, and derived record equality.
- `(include "relative/path.wisp")` for source inclusion. The numeric standard
  library in `std/num.wisp` supplies operators such as `+` through traits.

See [examples/](examples/), [test fixtures](tests/fixtures/), and the
[standard library](std/) for executable examples. Features in the Rust and
self-hosted compilers are tested separately; support is not identical.

## Memory

Compiled code (`granite compile`) manages its heap itself: each object carries a
small header with a reference count, and an object is freed when its last
reference goes (`--rc`, or `GRANITE_RC=1`; see
[docs/proposals/MEMORY.md](docs/proposals/MEMORY.md)). The memory starts small
and grows as the heap needs it. For a program this means:

- **Values are immutable.** `(list-push xs v)` returns a list with `v` added and
  leaves `xs` unchanged; it writes in place only when nothing else can see `xs`
  (its count is 1), so threading an accumulator through a loop costs O(1) per
  push. `string-append` works the same way.
- **The heap has no cycles,** and that is what lets reference counting reclaim
  everything: an object only ever points at objects that existed when it was
  built.
- **No mutable cells without a cycle plan.** A feature that lets a value be
  changed after it is built (a `set!`, a mutable box or list, `letrec`) could
  make a cycle (closure → environment → box → closure), and reference counting
  never frees a cycle. Such a feature must refer back *by name* through a table a
  global owns (as top-level recursion does today), or bring its own cycle
  collector.
- **Raw memory is manual.** `heap-alloc` returns a block the program frees with
  `heap-free`; raw blocks are never counted. `string-addr`/`any-addr` give the
  address of a counted object, valid until the end of the enclosing `let` (or the
  function); `string-from-addr`/`any-from-addr` copy bytes into a new object.
- **Debugging:** `--debug-heap` (or `GRANITE_DEBUG_HEAP=1`) poisons freed blocks,
  traps on a use after free or a double free, and with `--rc` checks every count
  against the heap after each export call (`__granite_heap_verify`).
  `__granite_heap_stats` reports live bytes and blocks.

## Wisp — the interpreted Lisp

**Wisp** is a small dynamic Lisp written in Granite (`wisp/interpreter/`), running inside
one persistent Wasm instance:

```sh
cargo run --example interpreter
# wisp> (define add-two (lambda (x) (+ x 2)))
# #<closure>
# wisp> (add-two 40)
# 42
```

It supports lexical closures, persistent definitions, typed functions, records,
variants and pattern matching, s32/s64/f32/f64 arithmetic and casts, u8 values, unit, recursion,
typed lists/options/results/tuples, Lisp lists, typed globals, and recoverable errors.
Generic functions, trait instances, and higher-order arguments run directly in the
interpreter, including the algorithms in `std/list.wisp`. Record equality can be
derived with `(derive Eq Type)`.
Persistent `defmacro` templates support quasiquotation and splicing; hygienic
`syntax-rules` supports literal patterns and nested ellipses. Procedural
`syntax-case-lambda` adds guards and computation during expansion.
Source files can be loaded as command-line arguments, with relative `include`
directives resolved from each file's directory.

Wisp values are immutable like Granite's: `(list-push xs v)` returns the longer
list and leaves `xs` as it was, so use the result.

Wisp is deliberately *not* a full Scheme: no `set!`, `call/cc`, `letrec`/named-let,
varargs/`apply`, or numeric tower (fixed-width numerics only). It is closures + macros
+ runtime `eval` over a tagged dynamic value, shaped to echo Granite's surface.

The [Wisp Theater actor](wisp/actor/README.md) runs the same evaluator through a real
actor mailbox, with a local socket REPL and RPC discovery — the `theater-repl` daemon
drives it. See [wisp/interpreter/README.md](wisp/interpreter/README.md).

## Execution and ABI status

The current compiler emits **raw Wasm modules using the Pack/Graph ABI**.
Exported functions exchange encoded values through memory using four pointer/
length parameters; they do not expose their source-language signatures directly
to `wasmtime --invoke`.

The CLI retains two execution commands:

- `run` loads WebAssembly components from the older component pipeline. It cannot
  load the raw modules produced by the current `compile` command.
- `run-module` loads raw modules. Its Pack path supports no arguments or one
  string via `--input`; positional integers use the raw Wasm calling convention.

The compiler metadata encoder, `run-module`, and Rust-backed REPL use CGRF v3
through Packr v0.24.1 (imported as `pack`). The version is pinned once in the root
`Cargo.toml` under `[workspace.dependencies]`. Recompile older `.wasm` artifacts
before using them with these runners; changing an old artifact's header alone
does not update its generated encoding code.

For example, execute a string-returning export or start the REPL:

```sh
cargo run -p granite -- compile examples/string-return-test.wisp target/greet
cargo run -p granite -- run-module target/greet.wasm greet
# Hello from wisp!
cargo run -p wisp-repl
# wisp> (i32.add 40 2)
# S32(42)
```

Primitive lists use packed CGRF Array nodes; lists of compound values use graph
nodes. Packr map/set values are not yet Wisp language types. The Theater
integrations remain a separate migration; see the [runtime migration notes](docs/changes/REPL-MIGRATION.md).

## Project layout

| Path | Purpose |
| --- | --- |
| `src/compiler/` | Granite compiler pipeline (one file per stage) + the `eval` back-end |
| `src/lib.rs` | Public compiler library (`granite` crate) |
| `src/main.rs` | The `granite` CLI (`compile`, `eval`, `run`) |
| `examples/wisp-compiler.wisp` | Self-hosted Granite compiler (Granite in Granite) |
| `std/` | Granite standard library sources |
| `tests/` | Granite language + self-hosting integration tests |
| `wisp-repl/` | Granite's host-side typed REPL |
| `wisp/interpreter/` | **Wisp** — the dynamic Lisp, written in Granite |
| `wisp/actor/` | The Wisp Theater actor + the `theater-repl` daemon |
| `crates/`, `wisp-actor/` | Legacy/experimental, excluded from the workspace |
| `docs/changes/`, `docs/proposals/` | Implementation notes and design proposals |

The root workspace contains `granite` and `wisp-repl`. The Wisp Theater daemon
(`wisp/actor/legacy-host/`) is its own workspace, so its Theater dependencies do not
prevent building or testing Granite.

## Development checks

```sh
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace
```

Run a focused suite with `cargo test -p granite --test generics`. The self-hosting
suite includes `test_bootstrap_fixpoint`, which checks that two successive
generations of the self-hosted compiler produce byte-identical WAT:

```sh
cargo test -p granite --test self_hosted test_bootstrap_fixpoint -- --nocapture
```

Self-hosting tests need substantially more memory and stack than ordinary
language tests. Build and scratch outputs belong under `target/`; generated
`compiled/` directories and local `.direnv/` state are ignored. Keep intentional
WAT/WIT snapshots under `tests/fixtures/`.
