# Phase 2: Syntax Objects

**Status**: Complete
**Started**: 2026-01-10
**Related Proposal**: [docs/proposals/METAPROGRAMMING.md](../proposals/METAPROGRAMMING.md)

## Objective

Add source location tracking to S-expressions so error messages can show exactly where problems occur - including tracking code through macro expansion.

## Motivation

Currently, errors look like:
```
thread 'main' panicked at src/compiler.rs:1373:29:
Unknown operator or function: foo
```

With syntax objects, errors will show:
```
error: Unknown operator or function: foo
  --> examples/test.lisp:15:5
   |
15 |     (foo x y)
   |      ^^^
```

This is especially important for macro-generated code, where we need to know both where the macro was called and where the problematic template code came from.

## Design

### Core Data Structure

```rust
#[derive(Debug, Clone)]
struct Span {
    file: String,
    line: usize,
    column: usize,
    length: usize,
}

#[derive(Debug, Clone)]
enum SExpr {
    Sym(String, Span),
    Int { value: i64, ty: Type, span: Span },
    Float { value: f64, ty: Type, span: Span },
    List(Vec<SExpr>, Span),
    Quasiquote(Box<SExpr>, Span),
    Unquote(Box<SExpr>, Span),
    UnquoteSplice(Box<SExpr>, Span),
}
```

### Alternative: Wrapper Approach

Instead of adding Span to every variant, wrap SExpr:

```rust
#[derive(Debug, Clone)]
struct Syntax {
    datum: SExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum SExprKind {
    Sym(String),
    Int { value: i64, ty: Type },
    Float { value: f64, ty: Type },
    List(Vec<Syntax>),
    Quasiquote(Box<Syntax>),
    Unquote(Box<Syntax>),
    UnquoteSplice(Box<Syntax>),
}
```

**Decision**: Use the wrapper approach - cleaner separation of concerns.

## Implementation Plan

### Phase 2.1: Add Span Type
- [x] Create `Span` struct with file, line, column, length
- [x] Add `Span::dummy()` for generated code
- [x] Implement `Display` for nice error formatting

### Phase 2.2: Update Tokenizer
- [x] Track line and column during tokenization
- [x] Store span information in tokens
- [x] Update Token enum to include spans

### Phase 2.3: Update Parser
- [x] Create `Syntax` wrapper type (or add spans to SExpr)
- [x] Propagate spans through `parse_sexpr`
- [x] Spans for lists include opening paren to closing paren

### Phase 2.4: Update Macro Expansion
- [x] Preserve spans through macro expansion where possible
- [ ] Mark macro-generated code with special spans (future work)
- [ ] Track expansion origin for debugging (future work)

### Phase 2.5: Update Error Handling
- [x] Change panics to Result returns where feasible
- [x] Create structured error type with span
- [x] Format errors with source context

### Phase 2.6: Integration
- [x] Update `parse_program` to use new types
- [x] Update `parse_expr` to use new types
- [ ] Update type checker to report spans (future work)
- [x] Test with intentional errors

## Error Format

```
error[E001]: type mismatch
  --> examples/test.lisp:10:5
   |
10 |     (i32.add x 3.14)
   |              ^^^^^
   |
   = expected s32, found f64

error[E002]: undefined function
  --> examples/test.lisp:15:5
   |
15 |     (foo x)
   |      ^^^
   |
   = note: expanded from macro at examples/test.lisp:3:1
```

## Breaking Changes

This is an internal refactor. The source language syntax is unchanged.

## Success Criteria

Phase 2 is complete when:
- [x] All S-expressions carry source location information
- [x] Error messages show file, line, and column
- [x] Error messages show the problematic source code snippet
- [ ] Macro-generated code tracks its origin (future work)
- [x] All existing examples still compile
- [ ] Type errors show precise locations (future work)

## Future Work (Phase 3)

With syntax objects in place, Phase 3 (Hygiene) can add scope tracking:
```rust
struct Syntax {
    datum: SExprKind,
    span: Span,
    scopes: ScopeSet,  // Added in Phase 3
}
```
