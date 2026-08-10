# Generics and Traits: An Ergonomic Typed Core

**Status**: First slice working (compiles end to end)
**Started**: 2026-08-09
**Supersedes vision**: The Theater Shell direction is set aside. The new goal is a
powerful, ergonomic, *typed* Lisp that compiles directly and tightly to WebAssembly.

## Objective

Close the gap between the language surface and the machine. Today the surface is
almost raw WASM:

```lisp
(i32.add (i32.const 40) (i32.const 2))
```

We want an ergonomic surface that keeps full static typing and a tight WASM mapping:

```lisp
(+ 40 2)
```

The rich type machinery already exists in `src/compiler.rs` (records, variants,
options, results, lists, tuples, strings via the Pack/Graph ABI). The missing piece
is the *surface*: generic functions plus overloaded operators.

## The core problem, and why our design dissolves it

An ergonomic `+` means "meaning depends on type": `+` must become `i32.add` for
integers and `f64.add` for floats. WASM has no single `add`, so *some* choice by
type is forced. This coupling of meaning to type is the essential complexity — the
price of a tight, multi-width WASM mapping. It cannot be removed while the feature
stays.

Normally this coupling forces heavy machinery (interleaved expansion + type
checking, metavariables, a postpone-and-retry loop — the Lean/Scala "elaboration"
story). **Monomorphization avoids nearly all of it.** We make a concrete copy of
each generic function per type it is used with. After copying, every type is
concrete, so every operator resolves by a simple lookup. No metavariables, no retry
loop.

## Design decisions

1. **Generics compile by monomorphization.** One copy per concrete type. Tight WASM,
   zero runtime cost. (Boxing is rejected — it breaks the tight mapping.)
2. **Overloading is done with traits + instances** (type classes), resolved at
   concrete types after monomorphization.
3. **Built-in operators are just the first instances.** `+` for `s32` and `f64` are
   ordinary instances shipped with the standard library. There is no separate
   "builtin operator" mechanism.
4. **`trait` declares an interface; `instance` implements it for a type.** The word
   `interface` stays reserved for the Pack/WIT component-model sense, to avoid an
   overload. A trait is a *constraint over types*, not a type.
5. **Keep the trait system minimal to start:** one type parameter per trait, no
   overlapping instances (one instance per (trait, type); a clash is an error),
   whole-program instance search, no superclasses.
6. **One small piece of machinery remains:** methods that dispatch on their *return*
   type (e.g. `zero : T`) need the expected type to flow down (bidirectional
   checking) or an explicit annotation at the call site. The heavy retry loop is
   still not needed.

## Target syntax (sketch)

```lisp
; declare a trait: a named set of method signatures over a type parameter T
(trait (Add T)
  (fn + ((a T) (b T)) T))

; ship the built-in instances — these ARE the + operator
(instance (Add s32)
  (fn + ((a s32) (b s32)) s32 (i32.add a b)))

(instance (Add f64)
  (fn + ((a f64) (b f64)) f64 (f64.add a b)))

; a generic function constrained by a trait
(fn double ((x T)) T
  (where (Add T))
  (+ x x))
```

Uses monomorphize to concrete copies:

```lisp
(double 5)    ; makes double@s32 ; + resolves to i32.add
(double 2.5)  ; makes double@f64 ; + resolves to f64.add
```

## Semantics / rules

- **Trait**: introduces a constraint name and a set of method signatures, each
  mentioning the trait's type parameter. A trait is not a type; no value has a trait
  as its type (no dynamic dispatch, no vtables).
- **Instance**: provides, for one concrete type, a body for every method of the
  trait. Exactly one instance per (trait, type). Two matching instances is an error.
- **Constraint (`where`)**: a generic function lists the traits its type parameters
  must satisfy. The body may call any method of those traits on those parameters.
- **Call-site inference**: for `(f a b ...)`, infer the type arguments from the
  argument types by one-directional unification (bidirectional checking supplies the
  expected type where arguments do not determine it).
- **Monomorphization**: for each distinct set of concrete type arguments, emit one
  specialized copy. Inside a copy, all types are concrete.
- **Method resolution**: inside a concrete copy, each method call looks up the unique
  instance for the concrete type and inlines its body. The "dictionary" is a
  compile-time artifact and is fully erased; no record exists at run time.

## Resolved: primitives, with dictionary lowering (2026-08-09)

`trait` and `instance` are their own **core forms**, not sugar over records. A trait
is a *constraint over types*; a record is a *type*. They live at different levels, so
baking in traits does not duplicate records. Reasons:

- The one genuinely new mechanism is **resolution** ("find the instance for a type"),
  and it exists in both designs — so the sugar path saves nothing hard.
- The sugar path forces records to grow type parameters and function-typed fields
  first (extra scope, extra risk, coupling to the record system).
- Native forms give native, clear errors ("`s32` does not implement `Add`") instead
  of record-field errors ("missing field `+`").

The record/dictionary view survives as the **internal lowering**: the compiler lowers
a trait to a dictionary of functions (as Haskell and Rust do), and monomorphization
erases that dictionary. Native surface on top, dictionaries underneath.

## Implementation plan

Implemented as a pre-pass `expand_generics` in `src/compiler.rs` (runs after macro
expansion, before `parse_program`; wired in `compile()`). It lowers everything to
plain monomorphic `fn`s, so the rest of the pipeline is untouched.

- [x] Parse `trait` top-level form (name + type param + method signatures).
- [x] Parse `instance` top-level form (trait + type + method bodies).
- [x] Parse generic `fn` with a `where` constraint clause and type parameters.
- [x] Collect a whole-program instance table keyed by (trait, type).
- [x] Infer type arguments at generic call sites (from literals, params, known calls).
- [x] Monomorphize: emit one specialized function copy per concrete type-argument set.
- [x] Resolve method calls to instance bodies inside each copy; erase dictionaries.
- [x] Ship `Add` instances for `s32` and `f64` (fixture `tests/fixtures/traits_add.lisp`).
- [x] Prove the slice: `(double 5)` → `i32.add`, `(double 2.5)` → `f64.add`.

### Type-annotation syntax: `(name : type)` (Form A) — everywhere

The colon is a separate symbol (Typed Racket style), so the reader is unchanged and
there is no clash with package names like `colin:math/ops` (whose colon is mid-symbol,
not space-delimited). Both the old form and the colon form are accepted everywhere:

- **Parameters:** `(x : s32)` or `(x s32)` — `parse_typed_params`.
- **Function return:** `(fn f (params) : ret body)` or `(fn f (params) ret body)` —
  handled by `fn_shape`, a shared structural view of a `fn` form that tolerates an
  optional `:` before the return type and an optional `(where ...)` clause. Used by
  `parse_fn_form` and the generics pre-pass, so arity is never assumed.
- **Import return:** `(import m n (params) : ret)` — `parse_import_form`.
- **Global type:** `(global $g : type mut init)` — `parse_global_form`.
- **Ascription:** `(expr : type)` alongside the head form `(type expr)` — in
  `parse_expr` (scalar types, matching the existing cast).
- **Let binding:** `(let (name : type value) body)` alongside `(let (name value) body)`.
  The annotation ascribes the value to the declared type (so `(let (big : s64 n) ...)`
  with `n : s32` emits `i64.extend_i32_s`). Scalar types, matching ascription.

Fixtures: `tests/fixtures/colon_everywhere.lisp`, plus a typed-let example verified
(`(let (big : s64 n) ...)` → widen then bind). Every existing type position now takes
the colon; non-scalar annotations (records/lists in `let`/ascription) remain future work.

### Trait methods resolve everywhere (not only inside generics)

`walk` resolves a trait-method call to its concrete instance function in **any**
context. It picks the dispatch type from the first argument whose inferred type has a
matching instance; inside a generic body it falls back to the type-parameter binding.
So `(+ a b)` and `(+ (i32.const 40) (i32.const 2))` work in plain functions —
`+` is a true top-level operator, not only usable inside a generic wrapper.
Fixture: `tests/fixtures/top_level_plus.lisp`.

### Trait checker (Pass 0 + instance checks in the pre-pass)

Traits are now collected first (Pass 0) with their method signatures kept, so
instances and `where` clauses are checked regardless of source order. The checker
rejects, with clear messages:

- an instance of an **undeclared trait**;
- a **duplicate instance** for the same (trait, type);
- an instance method the trait **does not declare**;
- an instance **missing** a trait method;
- an instance method whose **parameter or return types do not match** the trait's
  signature (with the type parameter substituted by the instance type — structural
  equality via `type_expr_eq`, so non-scalar types like `(list T)` compare correctly);
- a `where` clause naming an **undeclared trait**;
- a **duplicate trait** declaration.

Verified against positive fixtures and five negative cases (see commit/testing notes).

### Deeper inference (bottom-up)

`infer_type` now reaches through **generic calls**: a call to a generic returns its
declared return type with the type parameter replaced by the inferred type argument
(so `(double 5)` is known to be `s32`, and `(+ (double 5) 1)` resolves). Recursion is
bounded by expression-nesting depth.

Method **dispatch is signature-driven**: Pass 0 records, per method, which parameter
carries the trait's type parameter (`method_dispatch`). `walk` dispatches on that
argument first (falling back to any argument with a matching instance, then to the
generic type-parameter binding). This picks the right instance even when the type
parameter is not the first argument and a competing instance exists.
Fixtures: `tests/fixtures/inference_nested.lisp`, `tests/fixtures/inference_dispatch.lisp`.

### Return-type dispatch (top-down, first slice — 2026-08-10)

`walk` now threads an **expected type** downward (`expected: Option<String>`), the
top-down half of bidirectional checking. It is a fourth resolution source for a trait
method, after the two argument-based ones and before the generic-body fallback: if no
argument fixes the type, the expected type from context does. This resolves methods
whose type parameter is only in the **return** (e.g. `zero : T`), which no argument can
decide.

The expected type is seeded from four places:

- **Return position** — a `fn` body is expected at the declared return type
  (`process_fn_form`, and `specialize` for a monomorphized copy).
- **`if` / `let` tails** — both `if` branches and a `let` body inherit the expected
  type of the whole form; a `let` value uses its own `: type` annotation.
- **Ascription / cast** — `(e : s32)` and `(s32 e)` push `s32` into `e`.
- **Sibling argument** — a resolved call expects each argument at its parameter type
  (via a new `fn_params` table over instance methods, plain fns, exported fns, and
  monomorphized copies). So `(+ x (one))` gives `(one)` the type of `x`, and
  `(+ (one) (one))` in an `s32` function resolves both.
- **Raw wasm instruction** — an instruction expects each argument at its operand type
  (from `lookup_wasm_instr`), so `(i32.add (zero) ...)` gives `(zero)` type `s32` and
  `(f64.mul (one) ...)` gives `(one)` type `f64`.

A generic whose result *is* its type parameter also takes its type argument from the
expected type when no argument determines it.

Fixture: `tests/fixtures/return_dispatch.lisp` — return position, ascription, sibling
argument, both-constants, `if` branches, and raw wasm-instruction arguments, each
picking the right instruction (`Zero--zero--s32` vs `Zero--zero--f64`, etc.). When no
context exists at all (a truly unconstrained `(zero)`), the error is clear: "cannot
resolve trait method 'zero': no instance matches the argument types or the expected
type".

See [proposals/ELABORATION.md](../proposals/ELABORATION.md) for the wider arc this
seeds (typed constants, `fold`/`sum` over a monoid, decode-into-expected-type, and the
road to type-aware macros).

### Literals adopt the expected type (2026-08-10)

The same expected type retypes a **default integer literal**. There is no `s32`
suffix, so an integer literal whose type is `s32` is *provably* a default and safe to
change; an explicit `s64`/`f32`/`f64` suffix is left untouched. When an expected type
reaches such a literal it adopts it: `s64` widens the integer, and `f32`/`f64` promote
it to a float. So these now compile that previously did not:

- `(fn f () : s64 5)` — the body literal takes the return type.
- `(i64.add 1 2)`, `(f32.add 1 2)` — literals take the operand type.
- `(5 : s64)` — ascription retypes the literal.
- `(let (big : s64 100) ...)` — the value takes the binding type.
- `(+ x 1)` with `x : f64` — dispatch fixes the type on `x`, then the sibling `1`
  adopts `f64`.

Float literals are **not** adopted: a default `f64` and an explicit `3.14f64` are
indistinguishable from the type alone, so respecting the suffix wins. Use integer
literals (which promote) or a `f32`/`f64` suffix where a specific float width is needed.
Fixture: `tests/fixtures/literal_adoption.lisp`.

### Known limits (next steps)

- **Generic return-type inference is shallow** — it fires only when the generic's
  return *is* the bare type parameter, not when it is nested (e.g. `(list T)`).
- **No cross-argument structural unification** (e.g. param `(list T)` vs arg
  `(list s32)` to solve `T`); inference matches a type parameter only where a parameter
  type *is* the bare type parameter.
- **Single type parameter per generic/trait**; no multi-param traits, no superclasses.
- Colon `let`/ascription are scalar-only.
- **Float literals do not adopt the expected type** (the default-vs-explicit `f64`
  ambiguity). Integer literals promote to floats, so this is rarely felt.
- **Standard library** (`-`, `*`, `<`, `=`, comparisons, more numeric types) — planned
  as a separate session/project.
- **Name mangling** uses `Trait--method--type` and `gen--type` (readable, `-` is a
  legal symbol char here). Dictionaries are fully erased — no runtime cost.

## Breaking changes

Expected. We are the only consumer and may break the working compiler freely. Raw
WASM instructions (`i32.add`, ...) remain available; the trait-based operators are a
new, higher layer on top of them.

## Success criteria

- `(+ 1 2)` and `(+ 1.0 2.0)` both compile and run, with correct WASM instructions.
- A generic `(fn double ...)` with `(where (Add T))` works for at least two types.
- Generated WAT for a monomorphized copy contains the concrete instruction directly
  (e.g. `i32.add`), with no runtime dictionary.
```
