# Phase 3: Automatic Hygiene

**Status**: Complete
**Started**: 2026-01-10
**Completed**: 2026-01-10
**Related Proposal**: [docs/proposals/METAPROGRAMMING.md](../proposals/METAPROGRAMMING.md)

## Objective

Add automatic hygiene to the macro system to prevent variable capture bugs.

## Motivation

Without hygiene, macros can accidentally capture or shadow user variables:

```lisp
; Without hygiene - BUG!
(defmacro swap (a b)
  `(let ((tmp ,a))
     (let ((_ (set! ,a ,b)))
       (set! ,b tmp))))

; User code:
(let ((tmp 5))
  (swap tmp x))  ; BUG! The macro's 'tmp' shadows the user's 'tmp'
```

With hygiene, the compiler automatically renames macro-introduced variables:

```lisp
; With hygiene - works correctly
; The macro's 'tmp' becomes 'tmp#1' internally
(let ((tmp 5))
  (swap tmp x))  ; Works! User's tmp is distinct from macro's tmp#1
```

## Design

### Scope Sets Algorithm (Simplified)

We use a simplified version of Racket's scope sets:

1. Each identifier carries a set of **scopes** (unique IDs)
2. When a macro expands, we add a fresh scope to all identifiers in the template
3. When resolving a variable reference:
   - Find all bindings with that name
   - The binding whose scopes are a subset of the reference's scopes wins
   - If no subset relationship exists, it's an ambiguous reference error

### Data Structures

```rust
/// A unique scope identifier
type ScopeId = u64;

/// Set of scopes attached to an identifier
#[derive(Debug, Clone, Default)]
struct ScopeSet {
    scopes: HashSet<ScopeId>,
}

/// Extended span with hygiene information
#[derive(Debug, Clone)]
struct Span {
    line: usize,
    column: usize,
    length: usize,
    scopes: ScopeSet,  // NEW: hygiene tracking
}
```

### Example: How It Works

```lisp
; Original code (before any expansion)
; All identifiers start with scope set {0} (the "base" scope)

(defmacro inc (x)
  `(let ((tmp (i32.const 1)))
     (i32.add ,x tmp)))

; When macro expands, fresh scope #1 is added to template identifiers:
; - 'let' gets scope {0, 1}
; - 'tmp' (binding) gets scope {0, 1}
; - 'i32.const', 'i32.add' get scope {0, 1}
; - 'tmp' (reference) gets scope {0, 1}
; - BUT: ,x substitutes the user's 'x' which keeps its original scopes

; User code:
(let ((tmp 10))     ; tmp has scope {0}
  (inc tmp))        ; tmp reference has scope {0}

; After expansion:
(let ((tmp 10))                      ; tmp{0}
  (let ((tmp (i32.const 1)))         ; tmp{0,1} - different binding!
    (i32.add tmp tmp)))              ; First tmp has {0}, second has {0,1}

; Resolution:
; - First 'tmp' reference has scopes {0}
;   - Binding tmp{0} matches (subset)
;   - Binding tmp{0,1} does NOT match ({0,1} is not subset of {0})
; - Second 'tmp' reference has scopes {0,1}
;   - Binding tmp{0,1} matches exactly
```

## Implementation Plan

### Phase 3.1: Add Scope Infrastructure
- [x] Add `ScopeId` type and global counter
- [x] Create `ScopeSet` struct with basic operations
- [x] Add `scopes` field to `Span`
- [x] Update all span creation to include empty scope set

### Phase 3.2: Track Scopes Through Parsing
- [x] Add base scope (scope 0) during initial parsing
- [x] Propagate scopes through all SExpr operations

### Phase 3.3: Hygiene in Macro Expansion
- [x] Generate fresh scope for each macro expansion
- [x] Add scope to all template identifiers (except unquoted expressions)
- [x] Track which identifiers were introduced by macros

### Phase 3.4: Hygienic Variable Resolution
- [x] Update `parse_expr` to use scope-aware lookup
- [x] Implement subset matching for binding resolution
- [x] Name mangling to preserve scope distinction in codegen

### Phase 3.5: Testing
- [x] Create test cases for common hygiene scenarios
- [x] Test macro-introduced bindings don't capture user variables
- [x] Test user variables don't capture macro internals
- [x] Verify all existing examples still work

## Test Cases

### Case 1: Macro Binding Doesn't Capture User Variable

```lisp
; User has a variable 'tmp'
; Macro introduces its own 'tmp'
; They should be distinct

(defmacro with-temp (body)
  `(let ((tmp (i32.const 0)))
     ,body))

(export (fn test-hygiene () s32
  (let ((tmp (i32.const 42)))
    (with-temp
      tmp))))  ; Should return 42, not 0

; Expected: returns 42
```

### Case 2: Macro Can Still Reference Its Own Bindings

```lisp
(defmacro make-counter ()
  `(let ((count (i32.const 0)))
     (i32.add count (i32.const 1))))

(export (fn test-self-ref () s32
  (make-counter)))

; Expected: returns 1
```

### Case 3: Nested Macros Work Correctly

```lisp
(defmacro outer (x)
  `(let ((tmp (i32.const 1)))
     (inner ,x)))

(defmacro inner (y)
  `(let ((tmp (i32.const 2)))
     (i32.add tmp ,y)))

(export (fn test-nested () s32
  (let ((tmp (i32.const 100)))
    (outer tmp))))

; Each 'tmp' is distinct:
; - User's tmp = 100
; - outer's tmp = 1
; - inner's tmp = 2
; Result: inner's tmp + user's tmp = 2 + 100 = 102
```

## Breaking Changes

This is largely backwards compatible. Macros that relied on capturing user variables will now behave differently (correctly).

Macros that intentionally capture must be rewritten to use explicit breaking of hygiene (future feature).

## Success Criteria

Phase 3 is complete when:
- [x] Each identifier carries a scope set
- [x] Macro expansion adds fresh scopes to introduced identifiers
- [x] Variable resolution uses scope-based lookup
- [x] Test Case 1 (no capture) passes - returns 42
- [x] Test Case 2 (self reference) passes - returns 100
- [x] Test Case 3 (nested macros) passes - returns 102
- [x] All existing examples still compile and run correctly

## Future Work (Phase 4)

With hygiene in place, Phase 4 can add pattern matching:
```lisp
(define-syntax when
  (syntax-rules ()
    [(when cond body ...)
     (if cond (begin body ...) (i32.const 0))]))
```

## References

- [Binding as Sets of Scopes](https://www.cs.utah.edu/~mflatt/scope-sets-5/) - Matthew Flatt
- [Fear of Macros: Hygiene](https://www.greghendershott.com/fear-of-macros/all.html#%28part._hygiene%29)
- [Beautiful Racket: Hygiene](https://beautifulracket.com/explainer/hygiene.html)
