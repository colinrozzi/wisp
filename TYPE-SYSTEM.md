# Type System Design: Explicit Typing for Wisp

**Status:** Design Proposal
**Author:** Design discussion with user
**Date:** 2025-12-01

## Overview

This document outlines a proposal to extend Wisp beyond its current single-type (`s32`) design to support multiple WebAssembly primitive types with explicit type annotations.

## Motivation

Currently, Wisp supports only 32-bit signed integers (`s32`). All values, parameters, and return types are implicitly `s32`. While this simplicity is valuable for initial learning, it limits:

- Numeric precision (no 64-bit integers or floating point)
- Direct mapping to WebAssembly's full type system
- Interoperability with components expecting other types
- Realistic computational tasks (scientific computing, graphics, etc.)

## Goals

1. **Explicit over implicit** - All types are annotated, no inference needed
2. **WebAssembly alignment** - Map directly to WASM primitive types
3. **Backward compatible** - Existing code should have a clear migration path
4. **Educational value** - Demonstrate type checking in a compiler pipeline
5. **Simplicity maintained** - Avoid complex type system features

## Non-Goals

- Type inference (Hindley-Milner, etc.)
- Generics or parametric polymorphism
- Subtyping or type hierarchies
- Automatic numeric conversions/coercions
- User-defined types (structs, enums, etc.)

## Proposed Type System

### Supported Types

Match WebAssembly's primitive types:

| Wisp Type | WebAssembly Type | Description |
|-----------|------------------|-------------|
| `s32` | `i32` | 32-bit signed integer |
| `s64` | `i64` | 64-bit signed integer |
| `f32` | `f32` | 32-bit floating point |
| `f64` | `f64` | 64-bit floating point |

Optional future additions:
- `bool` - Sugar for `i32` with 0/1 values and boolean operations

### Syntax Design

#### Function Definitions with Types

```lisp
;; Current syntax (implicit s32):
(fn factorial (n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

;; Proposed syntax (explicit types):
(fn factorial ((n s32)) s32
  (if (= n 0) 1 (* n (factorial (- n 1)))))

;; Multiple parameters:
(fn area ((width f64) (height f64)) f64
  (* width height))

;; Zero parameters:
(fn get-pi () f64
  3.14159)
```

**Format:** `(fn name ((param1 type1) (param2 type2) ...) return-type body)`

#### Type-Specific Literals

Numeric literals need type disambiguation:

```lisp
;; Integer literals
42      ;; Defaults to s32
42s64   ;; Explicit s64
-10     ;; s32 negative
-10s64  ;; s64 negative

;; Float literals
3.14    ;; Defaults to f64
3.14f32 ;; Explicit f32
2.0f64  ;; Explicit f64
```

**Default rules:**
- Integer literals without suffix → `s32`
- Float literals without suffix → `f64`

#### Explicit Type Conversions

No implicit conversions - require explicit conversion functions:

```lisp
;; Conversion functions (matches WebAssembly instruction names):
(s64.extend-s32 x)    ;; s32 → s64 (sign extend)
(s32.wrap-s64 x)      ;; s64 → s32 (truncate)
(f32.demote-f64 x)    ;; f64 → f32
(f64.promote-f32 x)   ;; f32 → f64
(s32.trunc-f64 x)     ;; f64 → s32 (truncate toward zero)
(f64.convert-s32 x)   ;; s32 → f64
;; ... etc for all WASM conversion instructions
```

Example with conversions:

```lisp
(fn mixed-math ((x s32)) f64
  (let (y (f64.convert-s32 x))
    (* y 1.5)))
```

#### Imports and Exports

```lisp
;; Import with explicit types
(import math sqrt ((x f64)) f64)

;; Export remains similar
(export factorial)
(export (fn public-api ((x s32)) s32 ...))
```

### Example Programs

#### Basic Typed Program

```lisp
;; Math module with different types
(fn square ((x s32)) s32
  (* x x))

(fn square-f64 ((x f64)) f64
  (* x x))

(fn hypotenuse ((a f64) (b f64)) f64
  (let (a2 (* a a))
  (let (b2 (* b b))
    (sqrt (+ a2 b2)))))

(export square)
(export square-f64)
```

#### With Conversions

```lisp
(fn int-to-float-calc ((n s32)) f64
  (let (x (f64.convert-s32 n))
    (* x 2.5)))

(fn mixed ((a s32) (b f64)) f64
  (+ (f64.convert-s32 a) b))

(export int-to-float-calc)
```

## Implementation Plan

### Phase 1: Type Representation

**Changes:**
- Add `Type` enum in `compiler.rs`
- Update `Function` struct to include parameter types and return type
- Update `Import` struct similarly
- Modify `Expr` to carry type information (needed for type checking)

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
    S32,
    S64,
    F32,
    F64,
}

struct Function {
    name: String,
    params: Vec<(String, Type)>,  // Changed from Vec<String>
    return_type: Type,
    body: Expr,
}
```

### Phase 2: Parser Updates

**Changes:**
- Update `parse_fn_form` to extract type annotations
- Update `parse_import_form` to parse typed parameters
- Add type literal parsing for suffixed numbers
- Update `tokenize` if needed for type suffixes

### Phase 3: Type Checking Pass

**New component:** Type checker runs after parsing, before codegen

**Responsibilities:**
- Verify all operations are type-consistent
  - `(+ e1 e2)` requires both operands have same numeric type
  - `(if cond then else)` requires `cond` is `s32` (as boolean), branches match
- Check function calls match signatures (arity and types)
- Annotate `Expr` nodes with resolved types
- Report clear type errors with source context

**Example checks:**
```lisp
;; Error: type mismatch
(fn bad ((x s32) (y f64)) s32
  (+ x y))  ;; ERROR: cannot add s32 and f64

;; OK: explicit conversion
(fn good ((x s32) (y f64)) f64
  (+ (f64.convert-s32 x) y))
```

### Phase 4: Codegen Updates

**Changes:**
- Branch on type when generating arithmetic/comparison instructions
  - `Add` with `S32` → `i32.add`
  - `Add` with `F64` → `f64.add`
- Update constant generation
  - `Int(42, S32)` → `i32.const 42`
  - `Float(3.14, F64)` → `f64.const 3.14`
- Handle conversion expressions as WASM conversion instructions
- Update function signatures in WAT output

### Phase 5: WIT Generation Updates

**Changes:**
- Map Wisp types to WIT types in interface definitions
- Update import/export declarations with correct types

### Phase 6: Migration Path for Existing Code

**Option A: Default to s32 (backward compatible)**
- If no type annotations, assume all `s32`
- Parsing falls back to old behavior

**Option B: Require explicit migration**
- All code must have type annotations
- Provide migration tool to add `s32` everywhere

Recommend **Option B** for clarity, with a simple script to auto-annotate old code.

## Open Questions

1. **Boolean type?**
   - Option A: Keep using `s32` with 0/1 convention
   - Option B: Add `bool` as sugar over `s32`

2. **Type aliases?**
   - Allow `(type my-number f64)` definitions?

3. **String type?**
   - Current WIT supports strings, but requires memory management
   - Defer until Wisp has memory model?

4. **Error messages?**
   - How detailed should type errors be?
   - Show expected vs actual types?
   - Source location tracking needed?

5. **Literal ambiguity?**
   - Should `0` work in both int and float contexts?
   - Or require `0` (int) vs `0.0` (float)?

6. **Operator overloading?**
   - Keep `+`, `-`, `*` working on all numeric types (type-checked)?
   - Or require explicit operators like `+.i32`, `+.f64`?
   - **Recommendation:** Keep simple operators, type-check to disambiguate

## Future Extensions

Potential additions after base type system is working:

1. **User-defined types**
   - Structs/records
   - Enums/tagged unions

2. **References and Memory**
   - Pointer types
   - Reference semantics

3. **Type inference for locals**
   - Function signatures stay explicit
   - Let bindings could infer from value

4. **Generic functions**
   - Parametric polymorphism
   - Type variables

## References

- [WebAssembly Type System](https://webassembly.github.io/spec/core/syntax/types.html)
- [WIT Type System](https://component-model.bytecodealliance.org/design/wit.html)
- WebAssembly numeric conversion instructions
- Existing typed Lisps: Typed Racket, Shen, etc.

## Decision Log

- **2025-12-01:** Decided on Option 1 (Explicit Types) over inference or gradual typing
- **2025-12-01:** Chose to align type names with WebAssembly (`s32`, `f64`, etc.)
