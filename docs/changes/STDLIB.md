# A Minimal Numeric Standard Library

**Status**: First slice working (compiles end to end)
**Started**: 2026-08-11
**Related**: [GENERICS-AND-TRAITS.md](GENERICS-AND-TRAITS.md) (the trait machinery this
builds on), [../proposals/ELABORATION.md](../proposals/ELABORATION.md) (the type flow it
exercises).

## Objective

Turn the operator machinery into an actual library a program can pull in, so `+ - * /`,
`<`, `>`, `=`, and the typed constants `zero`/`one` work out of the box across the
scalar types. This is the "real generic code" that will later show which elaboration
machinery (structural unification, postponement) is genuinely needed.

## Two pieces

### 1. `(include "path")` — a way to reach the library

There was no include or prelude mechanism; `compile()` read exactly one file. A prelude
prepended to every program would collide with fixtures that define their own `Add`, so
inclusion is **explicit** instead:

```lisp
(include "std/num.lisp")   ; path is relative to the including file
```

`expand_includes` (in `src/compiler.rs`) runs *before* macro and generic expansion, so
included traits, macros, and functions are all visible. It splices the referenced file's
top-level forms in place, resolving the path relative to the including file's directory,
and includes each file **at most once** (keyed by canonical path), which also breaks
cycles. A bad path is a clear error.

### 2. `std/num.lisp` — the library

Traits with one instance per scalar type (`s32`, `s64`, `f32`, `f64`):

- **Arithmetic** — `Add` (`+`), `Sub` (`-`), `Mul` (`*`), `Div` (`/`).
- **Comparison** — `Ord` (`<`, `>`), `Eq` (`=`), each returning `s32` (1/0).
- **Typed constants** — `Zero` (`zero`), `One` (`one`), which dispatch on the return
  type.

These are ordinary traits and instances — there is no separate "builtin operator"
path. `+` is just the `Add` method, resolved to `i32.add`/`f64.add`/… at the concrete
type after monomorphization.

## Instances are emitted on demand

A stdlib is only practical if bringing it in does not bloat every binary. Instances are
now emitted **lazily**, exactly like generic specializations: an instance method is kept
in a table and emitted only when a method call resolves to it (transitively — a generic
that uses `+` pulls in the `Add` instances for the types it is used at). Unused
instances are never emitted.

Measured on `tests/fixtures/use_std.lisp` (uses `+ - * <`, `zero`/`one`, and a generic
`double` at two types): **9** instance functions emitted out of **36** defined; total
module functions **22** instead of **49**. An included-but-unused instance costs zero.

Implementation: `Lowering` gained `instance_defs` (name → form), `used_instances`, and
`instance_worklist`; `walk` calls `mark_instance` when it resolves a method; Pass 3
drains the generic and instance worklists together until both are empty.

## Proof

`tests/fixtures/use_std.lisp` compiles and each operator lowers to the right
instruction:

- `add3` → `Add--c43--s32` (`i32.add`)
- `poly` → `Mul--c42--f64`, `One--one--f64`, `Sub-----f64`
- `d32` / `d64` → `double--s32` / `double--f64` (the generic reused via the stdlib's
  `Add` trait)
- `clamp-sign` → `Ord--c60--s32` (`i32.lt_s`), `Zero`/`One`/`Sub` at `s32`

## `std/list.lisp` — generic algorithms over `(list T)` (2026-08-13)

Built on structural unification (see GENERICS-AND-TRAITS.md), a second stdlib module
provides generic list functions, monomorphized per element type when used:

- `length` — fully parametric (`(where T)`, no constraint).
- `fold` / `map` / `filter` — higher-order; take a function argument.
- `sum` — `(where (Add T) (Zero T))`, defined as `(fold + (zero) xs)`.
- `contains` — membership by `(where (Eq T))`.
- `reverse` — parametric; builds a new list.

The higher-order functions are **compile-time specialized**: `(map f xs)` monomorphizes
`map` for that specific `f` and element type, inlining `f` — no runtime function values.
So `sum` really is `fold` plus `+`/`zero`, with zero indirection after specialization.

They recurse by index (`list-len` + `list-get`); Wisp lists have no nil/cons pattern
matching. `std/list.lisp` starts with `(include "num.lisp")` for the numeric traits;
the include-once rule makes a double include of `num.lisp` harmless. Tests:
`tests/list.rs` (length, sum at s32 and f64, contains present/absent, reverse).

Two small compiler enablers landed with this:

- **Unconstrained generics** — a bare `(where T)` declares a type parameter with no
  trait bound, so `length`/`reverse` need no fake constraint.
- **Body substitution** — `specialize` now substitutes the type parameter inside the
  body too, so a type position like `(list-new T)` becomes `(list-new s32)`.

## Known limits / next steps

- **No `map`/`filter` with arbitrary functions** — Wisp has no first-class functions
  yet, so higher-order list ops need either function values or a macro. Present list
  ops apply fixed operations (via trait methods) instead.
- **No `include` path search** — the path is resolved only relative to the including
  file. No standard search root (e.g. a `WISP_PATH`) yet, so programs outside the repo
  reference `std/num.lisp` by a relative path.
- **Error spans in included files** render against the top file's source text (the
  include is spliced before a single shared `CompileContext`). Fine while the stdlib is
  correct; worth revisiting if libraries grow.
