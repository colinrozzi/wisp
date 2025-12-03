# Phase 0: Expose Core WASM Instructions

**Status**: Complete
**Started**: 2025-12-02
**Completed**: 2026-01-10
**Related Proposal**: [docs/proposals/METAPROGRAMMING.md](../proposals/METAPROGRAMMING.md)

## Objective

Transform Wisp from using polymorphic operators (`+`, `-`, `*`, `=`) to exposing explicit WebAssembly instructions (`i32.add`, `i64.mul`, `f32.eq`, etc.). This is the foundation for the metaprogramming system - the compiler should map almost 1:1 to WASM instructions.

## Motivation

Currently, Wisp has "convenient" operators that automatically unify types:
- `(+ 1 2s64)` → automatically widens to s64 addition
- `(= x y)` → figures out the right comparison based on types

**Why change?**
1. The macro system should provide conveniences, not the compiler
2. Explicit instructions make it clear what WASM code is generated
3. Simpler compiler = easier to understand and extend
4. Educational value: users see exactly what WASM does

## Current State (Before)

### What Works
```lisp
(fn factorial ((n s32)) s32
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

### Implementation
- Polymorphic operators: `+`, `-`, `*`
- Polymorphic comparisons: `=`, `==`, `<`, `<=`, `>`, `>=`
- Type unification logic in `unify_numeric()`
- Special AST variants: `Expr::Add`, `Expr::Sub`, `Expr::Mul`, `Expr::Cmp`
- Complex codegen in `gen_numeric_binop()`

### What's Missing
- Division operations (completely absent!)
- Memory operations (load/store)
- Global variables (get/set)
- Explicit constant instructions
- Not-equal comparisons

## Target State (After)

### What Will Work
```lisp
(fn factorial ((n s32)) s32
  (if (i32.eq n (i32.const 0))
      (i32.const 1)
      (i32.mul n (factorial (i32.sub n (i32.const 1))))))
```

### Implementation
- All WASM instructions exposed as regular expressions
- Uniform representation (e.g., all instructions look like function calls)
- Simple type checking: `i32.add` requires two `i32` arguments, returns `i32`
- Direct codegen: `(i32.add x y)` → `i32.add`

## Implementation Plan

### Phase 1: Core Arithmetic & Comparisons
- [x] Add division operations to AST and type checker
  - [x] `Expr::Div` or extend to `Expr::WasmInstr` (chose WasmInstr!)
  - [x] Support signed/unsigned: `i32.div_s`, `i32.div_u`, etc.
- [x] Create instruction registry mapping names → type signatures
  - [x] Arithmetic: `i32.add`, `i64.sub`, `f32.mul`, `f64.div`, etc.
  - [x] Comparisons: `i32.eq`, `i32.ne`, `i32.lt_s`, `f64.ge`, etc.
- [x] Update parser to recognize dotted instruction names
  - [x] `i32.add` should parse as a symbol, not error
  - [x] Parse `(i32.add a b)` like a special form or function call
- [x] Update type checker to validate WASM instructions
  - [x] Look up instruction signature
  - [x] Check argument count and types match exactly (no unification!)
  - [x] Return result type
- [x] Update codegen to emit WASM instructions directly
  - [x] `(i32.add x y)` → emit `i32.add` instruction
  - [x] Special handling for constants (immediates, not stack values)
- [x] Remove old polymorphic operators
  - [x] Remove `Expr::Add`, `Expr::Sub`, `Expr::Mul`
  - [x] Remove `unify_numeric()` function
  - [x] Remove `gen_numeric_binop()` function
  - [x] Remove `CmpOp` enum
  - [x] Remove old operator parsing from parse_expr

### Phase 2: Constants
- [x] Add explicit constant instructions
  - [x] `i32.const`, `i64.const`, `f32.const`, `f64.const`
  - [x] Parse `(i32.const 42)` as instruction
  - [x] Keep numeric literals working for convenience (decided: YES, keep both)

### Phase 3: Type Conversions
- [x] Expose conversion instructions
  - [x] `i32.wrap_i64`, `i64.extend_i32_s`, `i64.extend_i32_u`
  - [x] `f32.demote_f64`, `f64.promote_f32`
  - [x] `f32.convert_i32_s`, `f64.convert_i64_s`, etc.
  - [x] `i32.trunc_f32_s`, `i64.trunc_f64_s`, etc.
- [x] Decide: keep `(s32 expr)` syntax or require explicit instructions?
  - **DECISION**: Keep both! Type cast syntax stays for convenience, explicit instructions available too

### Phase 4: Memory Operations
- [x] Add memory instruction support to AST
  - [x] `memory.size`, `memory.grow`
  - [x] Load: `i32.load`, `i64.load`, `f32.load`, `f64.load`
  - [x] Store: `i32.store`, `i64.store`, `f32.store`, `f64.store`
  - [x] Byte-level: `i32.load8_s`, `i32.load8_u`, `i32.store8`, etc.
  - [x] All 16-bit variants: `i32.load16_s`, `i64.store32`, etc.
- [x] Add memory declaration to WAT generation
  - [x] Default memory: 1 page initial, 100 pages max
- [x] Special codegen handling for store instructions
  - [x] Stores "return" the value that was stored (for composability)
  - [x] Uses local variables to preserve value on stack
- [x] Type check memory operations
  - [x] Address must be `i32`
  - [x] Value type matches load/store instruction
- [x] Test with examples
  - [x] Basic store and load
  - [x] Memory size and grow
  - [x] Byte-level operations

### Phase 5: Global Variables
- [x] Add global support to AST
  - [x] Parse global declarations at top level
  - [x] `(global $name type mutability init-value)`
- [x] Add global operations
  - [x] `global.get $name`
  - [x] `global.set $name` (for mutable globals)
- [x] Generate WAT global declarations
- [x] Type check global operations
- [x] Test with example (`examples/global-test.lisp`)

### Phase 6: Testing & Documentation
- [x] Update all examples to use new syntax
  - [x] `examples/prog.lisp`
  - [x] `examples/math.lisp`
  - [x] `examples/user.lisp`
  - [x] `examples/typed.lisp`
  - [x] `tests/fixtures/s64_factorial.lisp`
  - [x] `tests/fixtures/f64_math.lisp`
- [x] Verify all examples compile and run correctly
- [x] Update CLAUDE.md with new language reference
- [x] Mark this change document as complete
- [ ] Consider: create simple macros in a library to restore `+`, `-`, `*` convenience (future work)

## Examples: Before → After

### Arithmetic
```lisp
# Before
(* x 2)

# After
(i32.mul x (i32.const 2))
```

### Comparison
```lisp
# Before
(if (>= x 10) a b)

# After
(if (i32.ge_s x (i32.const 10)) a b)
```

### Type Conversion
```lisp
# Before
(s64 x)

# After
(i64.extend_i32_s x)
```

### Mixed Types (currently auto-unifies)
```lisp
# Before (auto-widens to s64)
(+ x y)  ; where x:s32, y:s64

# After (explicit conversion required)
(i64.add (i64.extend_i32_s x) y)
```

## Breaking Changes

This is a **complete rewrite** of the language syntax. All existing `.lisp` files will need updating.

**Migration strategy:**
1. Implement new instructions alongside old operators
2. Add deprecation warnings for old syntax
3. Update all examples and tests
4. Remove old syntax in final commit

Alternatively: Just rip the bandaid off since this is an educational project with no users yet.

## Open Questions

1. **Numeric literals**: Should `42` still work, or require `(i32.const 42)`?
   - Proposal: Keep literals for ergonomics, but support explicit form too

2. **Type cast syntax**: Keep `(s32 expr)` or require `(i32.wrap_i64 expr)`?
   - Proposal: Remove cast syntax, use explicit conversion instructions

3. **Backward compatibility**: Worth keeping old operators temporarily?
   - Proposal: No, clean break. Can add back as macros later.

4. **Instruction registry**: Hardcode all instructions or data-driven approach?
   - Proposal: Start with hardcoded, refactor to table later if needed

## Success Criteria

Phase 0 is complete when:
- [x] All WASM arithmetic, comparison, and conversion instructions work
- [x] Memory operations (load/store/grow) work
- [x] Global variables work
- [x] All old polymorphic operators are removed
- [x] Type checker requires exact type matches (no auto-unification)
- [x] All examples compile and run with new syntax
- [x] CLAUDE.md documents the new instruction set

## Future Work (Post Phase 0)

After Phase 0, we can:
- Implement Phase 1 of macros (unhygienic + quasiquote)
- Create a macro library that provides `+`, `-`, `*` as conveniences
- Add more WASM instructions as needed (bitwise ops, etc.)
- Build example allocators using memory operations
