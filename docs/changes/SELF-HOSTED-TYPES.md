# Self-Hosted Compiler — Type System

**Status:** Started 2026-10-04. Increments 1 (scalar inference + casts), 2 (variable type
environment + signature table), 2.5 (typed locals), 3 (generics / monomorphization),
4 (traits + instances), 5 (`derive Eq`), and 6 (higher-order fn params) ✅ COMPLETE.

**Why:** The self-hosted compiler (`examples/wisp-compiler.wisp`) does **zero** type
inference today — it is pure codegen, treating everything as `i32`/pointer. The remaining
parity features all need types known at each use site:

- **numeric casts** `(s32 expr)` — pick the conversion op from the *source* type
  (`i64`→`i32` is `i32.wrap_i64`; `f64`→`i32` is `i32.trunc_f64_s`; …);
- **generics / monomorphization** — specialize `(fn id ((x T)) T …)` from the call-site
  argument type;
- **traits** — resolve a method to an instance by the receiver/return type;
- **`derive`** — reflect over a record's field types.

So type inference is the shared foundation. This doc tracks building it incrementally.

## Representation

Types are represented as **name strings**, matching the existing `type-to-wat` convention
(`"s32"`, `"s64"`, `"f32"`, `"f64"`, `"string"`, and `"other"` for pointer-shaped values —
lists/records/variants/option/result/tuple, all `i32` at the WAT level). This avoids a new
`sexpr`-like variant type for now; a richer structured `wtype` (with `list<T>`, type
variables for generics, function types) comes when generics land (Increment 3).

## Inference rules (Increment 1)

`infer-type (e, ctx) -> type-name`:

- `(num _)` → `s32`; `(fnum _)` → `f64`; `(str _)` → `string`.
- wasm instruction call: comparisons (`eq ne lt gt le ge`, detected by the two chars after
  the `iNN.`/`fNN.` prefix) → `s32`; otherwise the prefix determines it (`i32`→`s32`,
  `i64`→`s64`, `f32`→`f32`, `f64`→`f64`).
- cast head `(s32|s64|f32|f64 e)` → the target type.
- `(if c t e)` → type of `t`.
- everything else (bare vars, calls, constructors) → `s32` **default** for now.

### Increment 2 (done): variable type environment + signature table

`compile-ctx` gained `ctx-vartypes` (a `(list vartype)` lexical env) and `ctx-sigs` (a
`(list fnsig)` return-type table built once by `collect-sigs`). The env is **seeded from
`fn` params** (`ctx-with-params` in `compile-fn-def`) and **extended at each `let`**
(`with-var`, in both the normal and match-arm `let` handlers). `infer-type` now resolves a
bare symbol via `vartype-of` and a call via `sig-ret-of`, so `(s32 x)` / `(s32 (g …))` pick
the right conversion from the variable's / callee's real type. `with-var` copies the env
(self-hosted `list-push` mutates in place, so sharing would leak a binding into sibling
scopes). Guarded by `test_selfhosted_casts_inferred` (s64/f64 params, s64-returning call).

### Increment 2.5 (done): typed locals

`gen-locals` now declares each local with the WAT type inferred from its `let` binding's
value (`find-local-type` searches the body for the binding and runs `infer-type`), instead
of hardcoding `i32`. So `(let (y (i64.const 7)) (s32 y))` declares `(local $y i64)` and
round-trips. Fixpoint-safe: every local in the compiler's *own* source infers to an
i32-mapped type (`s32`/string/pointer), so its emitted locals are unchanged. Guarded by
`test_selfhosted_typed_locals` (i64 + f64 locals, i32 regression).

Match-arm pattern bindings still default to `s32` (variant payload types aren't tracked
yet); a value that references an inner `let` var infers via the fn-level env (→ `s32`
fallback) rather than the fully-nested env. Both are refined in Increment 3.

## Numeric casts (Increment 1, first consumer)

`(s32 e)` / `(s64 e)` / `(f32 e)` / `(f64 e)` in expression position:
infer `e`'s type, emit the matching conversion op (or the bare value when source == target).
Covers all 12 scalar source→target conversions. Guarded by `test_selfhosted_casts`.

## Increment plan

1. **Scalar inference + casts** — ✅ DONE 2026-10-04. `infer-type` over literals/wasm-instrs/
   casts/if; `(sNN|fNN expr)` casts (all 12 scalar conversions + same-type no-op). No codegen
   threading; fixpoint intact. Test: `test_selfhosted_casts`.
2. **Variable type environment** — ✅ DONE 2026-10-04. `ctx-vartypes` + `ctx-sigs` on
   `compile-ctx`, seeded from `fn` params, extended at `let`; `collect-sigs` signature table.
   Inference now correct for params and call results. Test: `test_selfhosted_casts_inferred`.
2.5. **Typed locals** — ✅ DONE 2026-10-04. `gen-locals` declares each local from its
   binding's inferred type (`find-local-type`); non-i32 `let` bindings round-trip. Test:
   `test_selfhosted_typed_locals`.
3. **Generics / monomorphization** — ✅ DONE 2026-10-04 (unconstrained). A source-to-source
   pass (`monomorphize`) after macros, before codegen: collect `(where …)` templates, drop
   them, infer concrete type args at each call (via `infer-type` on bare type-param params),
   specialize into mangled monomorphic fns (`base--t1--t2`), rewrite calls, transitively +
   deduped. **Identity fast-path** when no templates → generic-free self-source untouched →
   fixpoint safe. Test: `test_selfhosted_generics` (identity, 2-param projection, dedup,
   transitive). Limits (→ later): trait-constrained `(where (Trait T))` needs Increment 4;
   compound-type-param inference `(xs (list T))` needs structured types; type args from
   let-bound args fall back to s32 (param/call/literal args are exact).
   NOTE: adding this grew the self-source past ~130 KB, whose non-freeing bump-heap
   self-compile exceeded 1 GB → bumped the Rust-emitted memory and the gen-2 test harness to
   2 GB (`(memory 32000 32000)`).
4. **Traits + instances** — ✅ DONE 2026-10-04. `(trait (Name T) (fn m (ps) ret) …)` +
   `(instance (Name Type) (fn m (ps) ret body) …)`. Instance methods emit as monomorphic
   fns named `trait--method--type`; a trait-method call resolves by **the first argument's
   inferred type** — which works directly *and* inside a specialized generic body (its args
   are concretely typed after substitution), so **no constraint/binding threading is needed**.
   `(where (Trait T))` now contributes `T` as a type param. Syntax: where-clause goes **before**
   the body — `(fn double ((x T)) T (where (Add T)) (add x x))`. Fast-path widened to "no
   templates AND no instances". Test: `test_selfhosted_traits` (direct dispatch + constrained
   generic). Limits (→ later): single dispatch type per trait (multi-param traits take the
   first); return-type-only dispatch not handled; instance bodies that call generics aren't
   seeded into the spec worklist; `derive` not yet ported; operator method names not sanitized.
6. **Higher-order fn params (`->`)** — ✅ DONE 2026-10-04. A `(name (-> arg… ret))` param
   makes a fn a template; at a call the arg is a function *name*. The specializer inlines it
   (body substitution: param name → fn name), drops the param from the signature, and mangles
   it in before the type args (`apply--inc`, `apply2--inc--s32`). `spec` gained `sp-funcargs`;
   `is-generic-fn`/`parse-template` now accept a where-less template. Test: `test_selfhosted_hof`.
7. **Compound type-param inference** — ✅ DONE 2026-10-04. `infer-typeargs` now *unifies* each
   param's type pattern against the argument's **structured** type (`infer-type-sexpr`) via
   `find-binding`, so `(xs (list T))` + a `(list s32)` arg binds `T=s32`. Types are canonicalized
   to strings (`type-str`) for mangling; the var env stores compound type strings; `list-get`
   now yields the list's **element type** so trait dispatch on elements works (`(list point)` →
   `Eq--eq--point`). Test: `test_selfhosted_compound_generics` (incl. a case that fails under the
   old s32 default). Fixpoint-safe: the self-source's compound types still map to i32 locals.
   Limits (→ later): type args inferable only from a `(-> T T)` func-arg signature; multi-param
   traits; return-type dispatch.

5. **`derive`** — ✅ DONE 2026-10-04 (`Eq` for records). `expand-derives` runs first inside
   `monomorphize`: for `(derive Eq Type)` it reflects the record's fields and builds
   `(instance (Eq Type) (fn eq ((a Type)(b Type)) s32 <and of per-field (i32.eq (Type.f a) (Type.f b))>))`,
   which the instance pipeline then handles. Fields are 4-byte i32 slots so `i32.eq` fits every
   field. Required making `infer-type` return the record name for a constructor call `(point …)`
   (so trait dispatch picks `Eq--eq--point`); fixpoint-safe since record types still map to i32
   locals. Test: `test_selfhosted_derive`. Limits (→ later): only `Eq`, only records (no variants),
   string/nested-record fields compared by pointer not value, the `Eq` trait must be declared.

Fixpoint (`test_bootstrap_fixpoint`) must stay green after every increment.
