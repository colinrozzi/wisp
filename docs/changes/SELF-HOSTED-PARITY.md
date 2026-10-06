# Self-Hosted Compiler — Feature Parity Audit

**Status:** Audit complete (2026-09-22). No code changes yet.

**Goal:** Know *exactly* what the self-hosted compiler (`examples/wisp-compiler.lisp`,
compiled to `examples/wisp-compiler.wasm`) is missing relative to the Rust reference
compiler (`src/compiler.rs`), so we can choose parity work deliberately.

## Why this matters

The REPL (`crates/test-runtime`) compiles **every** line a user types by calling the
self-hosted `wisp-compiler.wasm`'s `compile-source` export under wasmtime
(`crates/test-runtime/src/main.rs:2957`, `:3596`). It has **no dependency on the `wisp`
crate** and never touches `src/compiler.rs`. So a gap in the self-hosted compiler *is* a
gap in what the REPL can do — parity is the whole game.

## Method (why this is "tested", not just "read")

- **Absence is reliable:** a feature with no dispatch arm in the self-hosted compiler is
  genuinely unsupported. All "missing" rows below are confirmed by `grep` finding zero
  head-symbol dispatch.
- **Presence needs care:** a dispatch arm can be a stub. Every "present" row below was
  checked against its actual implementation, not just the arm. This corrected two
  read-only mis-scores: `begin` is fully implemented, and `variant`/`record`/`import`/
  `data` return `""` in `compile-by-name` *by design* (they are metadata-only forms
  gathered in earlier passes at `wisp-compiler.lisp:422,443`), not stubs.

## At parity — works in the REPL today

Literals (int/float/string), variables, function calls, `if`, `let`, `begin`, `match`;
all WASM instructions (arith / bitwise / compare / convert / memory); `global` +
`global.get` / `global.set`; `fn`, `export`, `import`, `variant`, `record` + field
access, `data`; lists (`list-new` / `list-push` / `list-get` / `list-len`); string
builtins (`string-len` / `string-ref` / `string-append` / `string=?` / `substring`).

## Done since the audit

- **Option/Result** (`some`/`none`/`ok`/`err`) — ✅ 2026-09-24. Implemented as built-in
  parametric *variants* injected into the compile context (`add-builtin-variants`), so
  construction and `match` reuse the existing variant machinery. Tags/layout match Rust
  (option `[none=0, some=1]`, result `[ok=0, err=1]`, payload at +4). The Rust surface
  syntax carries type annotations — `(some T v)`, `(none T)`, `(ok T E v)`, `(err T E v)`
  — which the self-hosted codegen ignores by taking the payload as the **last** arg (also
  accepts the un-annotated `(some v)`). Guarded by `test_selfhosted_option_result`.
- **Tuples** — ✅ 2026-09-24. `(tuple v...)` → `$__make_record_N` heap record (field i at
  +4*i). **Construct-only**, matching Rust: no element-access form and no tuple match
  pattern exist in either compiler. Arity 1–5 (the record makers the runtime provides).
  Guarded by `test_selfhosted_tuple`.
- **Macros** (unhygienic + quasiquotation) — ✅ 2026-10-04. `defmacro` + `` ` ``/`,`/`,@`.
  Reader sugar desugars in the parser (`` `x `` → `(quasiquote x)`, etc. via reserved-name
  symbol tokens — no new `token`/`sexpr` variants). A pre-codegen pass (`collect-macros` →
  `expand-all`) collects defmacros, drops them, and expands every other form top-down with
  recursive re-expansion (depth cap 100) and quasiquote eval (`eval-qq`/`qq-value`: unquote
  substitutes a param, unquote-splice splices a param bound to a list). Unhygienic, matching
  Rust's PHASE-1-MACROS. Guarded by `test_selfhosted_macros` (substitution, control-flow,
  splice, macro-calling-macro). Fixpoint preserved (the compiler's own source uses no macros,
  so the pass is identity over it).

## Missing — grouped by effort

| Effort | Feature | Rust ref (src/compiler.rs) | Notes |
|---|---|---|---|
| **S** | `string-from-bytes` / `string-to-bytes` | 6791–6816 | Simple builtins; likely needed for self-hosting closure only if the compiler uses them (it does not today). |
| **M** | Type ascription / cast `(s32 expr)`, `expr : type` | 6292–6326 | `s32`/`s64`/… currently only recognized in *type* position, not as an expression head. |
| **M** | Higher-order fn params (`-> arg… ret`) | 3945–3948 | Defunctionalized in Rust (monomorphize per fn name). |
| **M** | Quasiquote / unquote / unquote-splice | 2398–2399, 3122 | Prereq for ergonomic macros. |
| **L** | Macros — `defmacro` / `define-syntax` / `syntax-case` + hygiene | 2430–2634 | Biggest single lever for a Lisp; largest effort. |
| **L** | Traits + `instance` + `derive` | 4893–5227 | |
| **L** | Generics / `where`-clauses (monomorphization) | 4984–5363 | |
| **L** | `include` (file splicing) | 315–370 | |
| **L** | `resource` / borrow types | 890–892 | Least-used; defer. |

## Two different finish lines

These are **not** the same target — decide which we're chasing:

1. **REPL parity** — everything a user might type. Wants Option/Result, tuples, macros,
   traits/generics. Large surface.
2. **Self-hosting closure** — the self-hosted compiler compiles *its own* source
   (`wisp-compiler.lisp`) unaided, so the Rust compiler can be retired (see the
   self-hosting campaign). This needs only the features `wisp-compiler.lisp` *itself*
   uses — which is core language + lists + strings + variants/records/match. It uses
   **no** macros, traits, generics, Option/Result, or tuples. So this finish line is
   much closer; the remaining blocker is the M8 memory/stack limit (recursive tokenizer
   on the ~42 KB self-source), **not** missing features.

## Roadmap (all of these are wanted — this is the order)

1. **Self-hosting closure** — ✅ **DONE (2026-09-24)**. The M8 "recursion/stack limit"
   framing was stale; the real blocker was string-escape decoding in the tokenizer.
   gen-2 == gen-3 (byte-identical) is now guarded by `test_bootstrap_fixpoint`. See
   [SELF-HOSTED-COMPILER.md](SELF-HOSTED-COMPILER.md) M8.
2. **Option/Result + tuples** — ✅ **DONE (2026-09-24)**. See "Done since the audit".
3. **Macros** — ✅ **DONE (2026-10-04)**. `defmacro` + quasiquotation, unhygienic.
4. **Traits + generics + `derive`** — monomorphization. **← next (largest remaining)**
5. Remainder: `->` params, `include`, `string-from/to-bytes`, resource/borrow types.
