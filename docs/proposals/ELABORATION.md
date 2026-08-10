# Proposal: Elaboration — Bidirectional Types and the Road to Type-Aware Macros

**Status:** Vision (blue-sky). No code committed to this beyond the bottom-up half.
**Related:** [changes/GENERICS-AND-TRAITS.md](../changes/GENERICS-AND-TRAITS.md) (what
is built), [proposals/METAPROGRAMMING.md](METAPROGRAMMING.md) (the macro vision),
[proposals/TYPE-SYSTEM.md](TYPE-SYSTEM.md).

## The one irreducible complexity

An ergonomic typed Lisp wants `(+ 1 2)` and `(+ 1.0 2.0)` to both just work. WASM has
no single `add` — `i32.add` and `f64.add` are distinct machine ops — so *something*
must choose the instruction by type. This couples **meaning to type**: the meaning of
`+` depends on the types of its arguments. That coupling is not a design smell; it is
the price of a tight, multi-width WASM mapping. Every mechanism below exists to manage
that one coupling.

## Two directions of type flow

Type information can travel two ways through a program:

- **Bottom-up (synthesis):** types flow *up* from the leaves. A literal `5` is `s32`;
  `(+ a b)` takes its type from `a` and `b`. **This is built today.** Generics
  monomorphize, so by the time an operator is resolved every type is concrete; a
  simple bottom-up `infer_type` (literals, parameters, known returns, wasm-instr
  results, and now *through generic calls*) is enough. No metavariables, no retry
  loop. See GENERICS-AND-TRAITS.md.

- **Top-down (checking):** types flow *down* from the context. The place that *uses*
  a result tells the sub-expression what type is expected. **This is not built yet.**
  It is a different mechanism, not a deeper version of bottom-up inference.

The project deliberately took the bottom-up path first and got a long way on it:
generics, traits + instances, a trait checker, top-level operators, signature-driven
dispatch, and inference through nested generic calls. The next frontier is top-down.

## Return-type dispatch: the first top-down feature

Return-type dispatch means the implementation is chosen by the type the context wants,
because the type parameter appears only in the **return**, not in the arguments.

```lisp
(trait (Zero T) (fn zero () : T))
(instance (Zero s32) (fn zero () : s32 (i32.const 0)))
(instance (Zero f64) (fn zero () : f64 (f64.const 0.0)))
```

`(zero)` has no arguments, so bottom-up inference cannot decide which instance to use.
The expected type must flow *down* into the call.

### What it unlocks

1. **Typed constants** — `zero`, `one`, `empty`, `default`, `min`, `max`.
2. **Generic algorithms that need a starting value** — the big one:
   ```lisp
   (fn sum ((xs : (list T))) : T
     (where (Add T) (Zero T))
     (fold + (zero) xs))
   ```
   `(zero)` must match the element type, which comes from the return. Same shape:
   `product`, `concat`, `average`, any `fold` over a monoid.
3. **Parsing / decoding into the expected type** — `(decode bytes) : Config`. Central
   to a WASM/actor world: a message arrives as bytes and is decoded into the type the
   handler expects, driven by the expected type. (Ties into the Pack/Graph ABI.)
4. **Lighter numeric literals** — with a type flowing down, a bare `5` can adopt the
   expected type; fewer suffixes, fewer casts.

## Why it is also a foundation

Building return-type dispatch requires exactly one new capability: **an expected type
that flows down through the walk**. That is the top-down half of *bidirectional type
checking*. Once it exists, much becomes cheap:

- **Fewer annotations everywhere** (types flow down, so you write less).
- **Literals that fit their context.**
- **The door to type-aware macros.** The elaborator is precisely the place where a
  macro can ask *"what type is expected here?"* — the original goal of a strong,
  type-aware macro system (the "Design C" from the founding discussion; see
  METAPROGRAMMING.md).

## The destination: an elaborator (Lean-shaped, trimmed)

The mature form of all this is a single **elaboration** pass that expands and types at
the same time, using three classic tools:

1. **Bidirectional checking** — expected types flow down; inferred types flow up.
2. **Metavariables** — placeholders for not-yet-known types, filled in later.
3. **Postponement** — stuck sub-problems are set aside and retried when their inputs
   arrive; only a genuinely unsolvable one is an error.

Wisp sits in a sweet spot for this. It has **no subtyping** (so we unify, like Lean,
rather than solve subtype bounds, like Scala) and it is **not dependent** (so
elaboration never runs code to check a type, avoiding Lean's hardest part). And
monomorphization already removes most of the pressure for the heavy machinery — we can
adopt the pieces incrementally, starting with just the expected-type flow needed for
return-type dispatch.

A likely staging:

- [x] Thread an optional expected type through expression elaboration (bidirectional).
      *Done 2026-08-10* — `walk` in the generics pre-pass carries `expected`, seeded
      from return position, `if`/`let` tails, ascription/cast, and sibling arguments.
- [x] Return-type dispatch for nullary/return-typed trait methods (`zero`, `empty`, …).
      *Done 2026-08-10* — the expected type is a resolution source for trait methods
      whose type parameter is only in the return. See
      [changes/GENERICS-AND-TRAITS.md](../changes/GENERICS-AND-TRAITS.md) and
      `tests/fixtures/return_dispatch.lisp`.
- [x] Literals adopt the expected type.
      *Done 2026-08-10* — a default integer literal (type `s32`; there is no `s32`
      suffix, so it is provably a default) adopts the expected type: it widens to
      `s64` or promotes to a float. Explicit suffixes are respected. So
      `(fn f () : s64 5)`, `(i64.add 1 2)`, and `(f64.add 1 2)` now work.
      Fixture: `tests/fixtures/literal_adoption.lisp`.
- [ ] Metavariables + postponement, only where a case genuinely needs them.
- [ ] Type-aware macros / elaborators (Design C): macros that may query the expected
      type — the unified metaprogramming mechanism envisioned in METAPROGRAMMING.md.

## One-line summary

Return-type dispatch gives typed constants and real generic algorithms today, and it
lays the first stone of the elaborator — the same top-down machinery that eventually
powers a type-aware macro system.
