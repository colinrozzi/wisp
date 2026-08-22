# Deriving: the first type-aware macro

**Status**: First slice working (`derive Eq` for records)
**Started**: 2026-08-15
**Related**: [GENERICS-AND-TRAITS.md](GENERICS-AND-TRAITS.md) (the trait system this
generates instances for), [../proposals/ELABORATION.md](../proposals/ELABORATION.md)
(the type-aware macro vision).

## Objective

The original metaprogramming vision includes *type-aware* macros — macros that see
types and generate code accordingly. Most **type-directed dispatch** ("do something
different based on type") we already have via traits + return-type dispatch. The thing
traits *cannot* do is reflect on a type's **structure**. That is what deriving adds:
inspect a record's fields (or, later, a variant's cases) and generate a trait instance.

```lisp
(record point (x s32) (y s32))
(derive Eq point)
; generates, by reading point's fields:
; (instance (Eq point)
;   (fn = ((a : point) (b : point)) : s32
;     (i32.and (i32.eq (point.x a) (point.x b))
;              (i32.eq (point.y a) (point.y b)))))
```

This is Rust's `#[derive(Eq)]` / Haskell's `deriving`, and it composes with the trait
machinery: after deriving, `(= p1 p2)` on two `point`s resolves to the generated
instance like any other.

## Where it runs, and why that is enough

The key architectural fact: the existing macro system expands **before** any type
information exists. But deriving does not need the *type checker* — it only needs a
type's **structure**, and a `(record ...)` form is already present as raw syntax at
expansion time. So `expand_derives` runs right after macro expansion and before the
generics pre-pass:

```
parse -> includes -> macros -> [expand_derives] -> expand_generics -> parse_program -> type_check
```

It scans the forms for record definitions (name -> `[(field, type)]`), then rewrites
each `(derive Trait Type)` into a generated `(instance ...)`. Because it emits an
ordinary instance, everything downstream — the trait checker, monomorphization,
on-demand emission — treats it exactly like a hand-written instance.

## The type-aware part

Deriving reads each field's **type** and picks the right primitive per field:
`i32.eq` for `s32`/`u8`, `i64.eq` for `s64`, `f32.eq`/`f64.eq` for floats. A mixed-width
record (`(rec (a s32) (b s64) (c f64))`) derives a comparison that uses `i32.eq`,
`i64.eq`, and `f64.eq` respectively — code generation driven by structure and type.

## Implementation

- `expand_derives(forms)` — collect record shapes, then replace each `(derive Trait
  Type)` with a generated instance. Wired into `compile()` before `expand_generics`.
- `derive_eq_record` — builds the `(instance (Eq Type) (fn = ...))` form, folding the
  per-field comparisons with `i32.and` (an empty record is always equal).
- `scalar_eq_instr` — maps a scalar type to its equality instruction.
- Per-trait generators (like Rust's derive macros): `derive` supports a known set of
  traits. `Eq` is the first.

Tests: `tests/derive.rs` (equal / differing fields, mixed-width records, and rejection
of unknown traits and non-record types).

## Known limits / next steps

- **Records only** — variant deriving (match on both, compare tags then payloads) is
  not done yet.
- **Scalar fields only** — a field of record type would need the generated code to call
  `=` (the trait method) recursively rather than a primitive; a natural next step that
  makes deriving compose through nested records.
- **`Eq` only** — `Ord`, `Show` (needs a string builder), `Default`/`Zero`,
  `Convert` follow the same per-trait-generator pattern.
- **`derive` requires the trait to be declared** (e.g. `(trait (Eq T) ...)`, as in
  `std/num.lisp`).
