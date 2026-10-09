# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repo holds **two languages**, and the one-liner is: **Granite compiles Wisp.**

- **Granite** — a statically typed, ahead-of-time language that compiles
  S-expressions to WebAssembly. It has S-expression *syntax* but not Lisp
  *semantics*: no runtime `eval`, no first-class closures, everything is checked and
  compiled ahead of time. It exposes WASM instructions nearly 1:1, plus scalar types
  (s32/s64/f32/f64/u8/u16/u32/u64/bool), variants and records, exhaustive `match`,
  generics, traits/instances, substructural linearity (`lin`/`aff`), unforgeable
  capabilities, and type-state. **Granite is the compiler at the repo root** (the
  `granite` crate + `granite` CLI). It is the foundation.
- **Wisp** — a dynamic **Scheme** (runtime reader, closures, dynamic values, runtime
  macros) that is *written in Granite and compiled by it*. It lives under `wisp/` and
  runs as a Theater actor. This is the actual Lisp.

Both are first-class, on purpose: Wisp for fast interactive/exploratory work and
short-lived actors; Granite for typed actors built to last. (The name "Granite" is for
New Hampshire, the Granite State.)

### Repository layout

- `src/` — the **Granite** compiler (Rust): the `granite` crate + `granite` CLI.
- `std/`, `examples/`, `tests/` — Granite's stdlib, examples, and test suite.
- `wisp-repl/` — Granite's host-side typed REPL (Rust).
- `wisp/interpreter/` — **Wisp**, the Scheme, implemented in Granite (reader, values,
  evaluator, printer, macros, …).
- `wisp/actor/` — the Wisp Theater actor (`actor.wisp` → `actor.wasm`, which
  `(include)`s `wisp/interpreter/evaluator.wisp`) and the `theater-repl` daemon
  (`wisp/actor/legacy-host/`, crate `wisp-interpreter-actor`, its own workspace).
- `crates/`, `wisp-actor/` — legacy/experimental, excluded from the workspace.

### Pack Packages (Not WASM Components)

Granite targets **Pack packages**, not standard WebAssembly Components:

| Aspect | Standard WASM Components | Pack Packages |
|--------|-------------------------|---------------|
| **ABI** | Canonical ABI | Graph ABI (CGRF) |
| **Types** | WIT | wit+ (recursive types) |
| **Runtime** | wasmtime component model | Pack/Theater |

**Why Pack?** Recursive types like `variant sexpr { sym(string), lst(list<sexpr>) }`
are essential for a Lisp (and for Granite's own ADTs). Pack's wit+ and Graph ABI
support this natively. See the [Pack crate](../pack) for the runtime.

## Common Commands

```bash
# --- Granite (the compiler, at the repo root) ---

# Compile a source file to a .wasm Pack package (-> <dir>/compiled/<stem>.wasm)
cargo run -- compile <source.wisp> [out-stem]
cargo run -- compile <source.wisp> --emit-wat --emit-pact   # also write readable views

# Evaluate a source file with the tree-walking interpreter (no compile step).
# Runs the full front/middle (so the type system still applies), then interprets a
# nullary entry (default `main`). Imported calls hit a built-in stdio host.
cargo run -- eval <source.wisp> [--entry NAME]

# Run an exported function from a compiled package
cargo run -- run <package.wasm> <function-name> <args...> [--dep <module>=<dep.wasm>]

# Granite's host-side typed REPL
cargo run -p wisp-repl

# --- Wisp (the Scheme, as a Theater actor) ---

# Rebuild the Wisp actor (wisp/actor/actor.wasm) after editing the interpreter
bash wisp/actor/build.sh

# Interactive Dynamic Wisp REPL (spawns the Wisp actor, evaluates the Scheme live)
cargo run --manifest-path wisp/actor/legacy-host/Cargo.toml -- repl

# --- Development ---
cargo fmt --all && cargo clippy --locked --workspace --all-targets --all-features -- -D warnings
cargo test --workspace            # Granite's suite (root crate + wisp-repl)
```

The installed binary is `granite`; `cargo run --` at the root invokes it (the one
root bin). The two interpreters are distinct: `granite eval` / `wisp-repl` run
**Granite** (typed, no closures); `theater-repl repl` runs **Wisp** (the dynamic
Scheme — `(define f (lambda (x) (+ x 2)))` → `#<closure>`).

## Architecture (Granite compiler)

### Pipeline

`src/main.rs` is the CLI (`compile`, `eval`, `run`, `run-module` subcommands) and the
wasmtime runtime for `run`.

`src/compiler/` is the pipeline, one file per stage (each submodule does
`use super::*` and keeps items `pub(crate)`):

- `mod.rs` — the AST / data model (`Type`, `Expr`, `SExpr`, `Program`, …), `compile()`,
  `analyze()` (the shared front/middle: tokenize → parse → expand → lower →
  type-check → typed `Program`), and shared helpers.
- `tokenizer.rs` — `tokenize()`: source → `Token` stream.
- `parser.rs` — `parse_sexpr()` → `SExpr`; `parse_program()`/`parse_expr()` → typed
  `Program`/`Expr`.
- `macros.rs` — defmacro, syntax-rules, syntax-case, pattern matching.
- `lower.rs` — `expand_generics()`/`Lowering`: monomorphize generics/traits, expand
  `derive`, resolve generic ADTs by name.
- `typecheck.rs` — `check_expr()` (inference with numeric widening) and
  `check_fn_linearity()`/`linear_uses()` (the substructural checker).
- `codegen.rs` — `generate_wat()`, `generate_wit()`, CGRF/Pack encode+decode glue.
- `eval.rs` — the tree-walking **back-end**: the dual of codegen. `analyze()` + eval.
  `Value`, `Host` (imported calls dispatch here), `eval_source*`, `eval_repl_expr*`.
- `repl.rs` — `ReplSession`: the shared REPL abstraction (accumulate `(fn)`/`(define)`,
  evaluate expressions, host supplied per-`feed`). Used by `wisp-repl`.

`compile()` = `analyze()` + codegen; `eval_source()` = `analyze()` + eval. So the
compiled and interpreted paths share the entire front/middle and type system; only the
back-end differs.

### Key data structures

- **Type** — `S32`/`S64`/`F32`/`F64`, `U8`/`U16`/`U32`/`U64`/`Bool`, `Str`,
  `Record(name)`, `Variant(name)`, `Option`/`Result`/`List`/`Tuple`, `Resource`,
  `Borrow`.
- **Expr** — the AST (literals, `Var`, `Call`, `WasmInstr`, `If`, `Let`, `Match`,
  record/variant construction + access, list/string ops, `WithCap`/`BorrowCap`, …).
- **Program** — functions, imports, exports, globals, records, variants, resources,
  capabilities, data segments.
- **Parameter** carries a `Multiplicity` (`Un`/`Aff`/`Lin`) for the substructural axis.

## Language Features (Granite)

Functions: `(fn name ((param type) ...) return-type body)`
Generics: a `fn` with a `(where (Trait T) ...)` clause is a template, monomorphized per concrete type. A bare `(where T)` is an unconstrained type parameter; multiple allowed (`(where T U)`, `(where (Ord T) (Convert T U))`).
Higher-order functions: a parameter typed `(-> arg... ret)` takes a function *name*, resolved at compile time — `(map f xs)` specializes to `map--f--T` with `f` inlined (defunctionalization; no runtime closures).
Imports/Exports: `(import module func ((param type) ...) return-type)`; `(export name)` or `(export (fn ...))`.
**Variants/records:** `(variant name (case payload...) ...)`, `(record name (field type) ...)`; both may be generic (`(variant (Name T ...) ...)`), monomorphized by name in lowering.
**Match:** `(match e ((case binding...) body) ...)` — exhaustive, `_` wildcard supported.
**Traits/instances:** `(trait (Name T ...) (fn method (params) : ret) ...)` + `(instance (Name Type ...) (fn method ... body) ...)`; resolve at compile time, expected type disambiguates return-typed methods.
**Deriving:** `(derive Trait Type)` generates an instance at compile time (`Eq` on records and variants).
**Substructural:** `(lin T)` = used exactly once, `(aff T)` = at most once; checker-only (erases to `T`). A variant case payload may be `(lin T)`/`(aff T)` — type-state (a linear value whose structural type narrows on `match`).
**Capabilities:** `(capability Name)` declares an unforgeable linear resource; `(with-cap (c Name) body)` mints it (RAII, released strictly last), `(& c)` borrows without consuming, `(release-cap c)` is the terminal consumer.
**Macros:** `(defmacro name (params...) template)`; quasiquote `` `expr ``, unquote `,expr`, splice `,@expr`.
**Globals:** `(global $name type mut|const init-value)`, `(global.get $name)`, `(global.set $name value)`.
**WASM ops (explicit, exact-type):** arithmetic `(i32.add a b)`; comparisons `(i32.lt_s a b)` (→ s32 0/1); constants `(i32.const 42)`; conversions `(i64.extend_i32_s x)`; memory `(i32.load addr)`, `(memory.grow pages)`.
Conditionals: `(if cond then else)` — cond must be s32. Let: `(let (name value) body)`.
Casts: `(s32 expr)`, `(f64 expr)`, … Includes: `(include "path.wisp")` (relative to the including file; e.g. `(include "std/num.wisp")`). Comments: `; to end of line`.

### Type system notes
- Numeric literal defaults: integers `s32`, floats `f64`. Suffixes: `42s64`, `3.14f32`.
- WASM instructions require exact type matches — no implicit unification. `(i32.add x y)` needs both exactly `s32`.
- Type checking (and the substructural checker) run before codegen *and* before eval.

## Wisp (the Scheme)

`wisp/interpreter/` is a complete dynamic Scheme written in Granite:
- `reader.wisp` parses S-expressions from a string **at runtime** (what makes it dynamic),
- `values.wisp` defines the dynamic `(variant value …)` with `closure`/`symbol`/`builtin`/…,
- `evaluator.wisp` is an environment-based tree-walking `eval`/`apply` with closures.

`wisp/actor/actor.wisp` wraps it as a Theater actor: it exports `evaluate(source)` plus
inbound-event callbacks (`handle-tick`, `handle-send`, tcp `on-data`/`on-close`,
lifecycle `handle-actor-event`) that dispatch to user-defined handlers in the live
session (`interpreter/rpc.wisp`). The `theater-repl` daemon (`wisp/actor/legacy-host/`)
spawns this actor and drives it; because the session *is* a Theater actor, inbound
events and host calls are native.

Granite's `Host`/`EvalSession`/`TheaterHost` infra (in the daemon crate and
`src/compiler/eval.rs`) is dormant groundwork for a possible future "live Granite in a
Theater actor" — not currently on the `theater-repl` path.

## Testing

`cargo test --workspace` runs Granite's suite from `tests/*.rs` (tokenizer, parser,
pattern_match, codegen, string_ops, eval, repl, type_state, linearity, capabilities,
generics, derive, cgrf, the `interpreter_*` tests that compile the Wisp Scheme, …).
Each compiles source to `.wasm` and runs it under wasmtime (or evaluates it), checking
the result. The `theater-repl` daemon has its own suite (`cargo test --manifest-path
wisp/actor/legacy-host/Cargo.toml --test theater_host`).

- **Output ABI**: an exported function is called as `(in_ptr, in_len, out_ptr_ptr,
  out_len_ptr)`. The callee allocates the output buffer, writes its address into
  `out_ptr_ptr` and length into `out_len_ptr`. The result is CGRF-encoded: after the
  pointer, an s32 payload sits at `+24` (16-byte CGRF header + 8-byte node header); a
  string has length at `+24`, bytes at `+28`.
- **Big stack**: the Wisp interpreter recurses deeply (the reader recurses per
  character), so the `interpreter_*`/`self_hosted` tests run on a 1 GiB thread via
  `run_big_stack` rather than the default 2 MiB test stack.

Test fixtures live in `tests/fixtures/` for manual `cargo run -- compile` checks.

## Coding Style

- Rust 2024 edition, standard formatting (4-space indent).
- snake_case for functions/variables, CamelCase for types.
- Helper grouping: `tokenize_*`, `parse_*`, `gen_*`, `check_*`.
- Short imperative commit messages (~50 chars): "add s64 support", "fix type widening".
- Keep compiler stages clearly separated: tokenize → parse → type-check → codegen/eval.

## Output Files

By default, compiling `examples/prog.wisp` writes one file, in a `compiled/` subfolder
next to the source (Racket-style), so source directories stay clean:
- `examples/compiled/prog.wasm` — the Pack package. It embeds the interface metadata
  (CGRF), so it is the one true artifact.

Two optional human-readable **views** (off by default; derivable from the wasm):
- `--emit-wat` → `examples/compiled/prog.wat` — readable WAT disassembly.
- `--emit-pact` → `examples/compiled/prog.pact` — text interface (also embedded as CGRF).

An explicit out-stem (`compile prog.wisp path/name`) is used verbatim (relative to the
current directory), bypassing the `compiled/` default. The output directory is created
if missing. `compiled/` is gitignored.
