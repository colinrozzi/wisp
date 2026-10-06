# Wisp

Wisp is a typed Lisp that compiles to WebAssembly. It has a Rust reference
compiler, a compiler written in Wisp, hygienic macros, traits and generics,
and experimental REPL and Theater actor integrations.

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
cargo run -p wisp -- compile examples/prog.lisp
```

This writes `examples/compiled/prog.wasm`. The compiler embeds interface metadata
in the module. Request readable output explicitly:

```sh
cargo run -p wisp -- compile examples/prog.lisp target/prog --emit-wat --emit-pact
```

An explicit output stem is relative to the current working directory. Without
one, outputs go into `compiled/` beside the source. Both `.lisp` and `.wisp`
examples use the same compiler.

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

The Rust compiler supports:

- Numeric types (`s32`, `s64`, `f32`, `f64`, `u8`), strings, lists, tuples,
  records, variants, options, and results.
- Lexical bindings, conditionals, pattern matching, recursion, memory operations,
  globals, and function imports/exports.
- `defmacro`, hygienic `syntax-rules`, and procedural `syntax-case` macros.
- Traits, instances, generic specialization, and derived record equality.
- `(include "relative/path.lisp")` for source inclusion. The numeric standard
  library in `std/num.lisp` supplies operators such as `+` through traits.

See [examples/](examples/), [test fixtures](tests/fixtures/), and the
[standard library](std/) for executable examples. Features in the Rust and
self-hosted compilers are tested separately; support is not identical.

## Interpreted REPL

A small interpreter written in Wisp now runs inside one persistent Wasm instance:

```sh
cargo run --example interpreter
# wisp> (define add-two (lambda (x) (+ x 2)))
# #<closure>
# wisp> (add-two 40)
# 42
```

It supports lexical closures, persistent definitions, typed functions, records,
variants and pattern matching, s32/s64/f32/f64 arithmetic and casts, recursion,
typed lists/options/results/tuples, Lisp lists, typed globals, and recoverable errors.
Generic functions, trait instances, and higher-order arguments run directly in the
interpreter, including the algorithms in `std/list.lisp`.
Persistent `defmacro` templates support quasiquotation and splicing; hygienic
`syntax-rules` supports literal patterns and nested ellipses. Procedural
`syntax-case-lambda` adds guards and computation during expansion.
Source files can be loaded as command-line arguments, with relative `include`
directives resolved from each file's directory.
This is an initial subset; additional types, static checking, derived instances, and Theater
calls remain ahead. See [interpreter/README.md](interpreter/README.md).

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
cargo run -p wisp -- compile examples/string-return-test.wisp target/greet
cargo run -p wisp -- run-module target/greet.wasm greet
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
| `src/compiler.rs` | Rust compiler pipeline and Wasm/interface emission |
| `src/lib.rs` | Public compiler library |
| `src/interpreter.rs` | Local host for the Wisp-written interpreter |
| `src/main.rs` | Compile and execution CLI |
| `examples/wisp-compiler.lisp` | Self-hosted compiler |
| `interpreter/` | Interpreter, reader, and printer written in Wisp |
| `std/` | Wisp standard library sources |
| `tests/` | Compiler, language, and self-hosting integration tests |
| `wisp-repl/` | Rust-backed REPL library and experimental interactive runner |
| `crates/` | Separate workspace for experimental Theater integrations |
| `wisp-actor/` | Standalone guest actor experiment |
| `docs/changes/` | Implementation and migration notes |
| `docs/proposals/` | Design proposals |

The root workspace contains `wisp` and `wisp-repl`. Theater integrations have
their own [workspace and build notes](crates/README.md), so their dependencies
do not prevent building or testing the compiler.

## Development checks

```sh
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace
```

Run a focused suite with `cargo test -p wisp --test generics`. The self-hosting
suite includes `test_bootstrap_fixpoint`, which checks that two successive
generations of the self-hosted compiler produce byte-identical WAT:

```sh
cargo test -p wisp --test self_hosted test_bootstrap_fixpoint -- --nocapture
```

Self-hosting tests need substantially more memory and stack than ordinary
language tests. Build and scratch outputs belong under `target/`; generated
`compiled/` directories and local `.direnv/` state are ignored. Keep intentional
WAT/WIT snapshots under `tests/fixtures/`.
