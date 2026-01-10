# Phase 5: Procedural Macros (syntax-case)

**Status**: Complete
**Started**: 2026-01-10
**Completed**: 2026-01-10
**Related Proposal**: [docs/proposals/METAPROGRAMMING.md](../proposals/METAPROGRAMMING.md)

## Objective

Add `syntax-case` for procedural macros that can run arbitrary code during expansion, while maintaining hygiene.

## Motivation

`syntax-rules` is declarative - you can only match and substitute. `syntax-case` adds procedural power:

```lisp
; With syntax-rules - can only substitute
(define-syntax swap
  (syntax-rules ()
    ((swap a b)
     (let (tmp a) (let (_ (set! a b)) (set! b tmp))))))

; With syntax-case - can compute, validate, transform
(define-syntax assert-positive
  (syntax-case-lambda (stx)
    ((assert-positive expr)
     (if (negative-literal? #'expr)
         (macro-error "Expression is negative!")
         #'(if (i32.gt_s expr (i32.const 0))
               expr
               (i32.const 0))))))
```

## Design

### Syntax

```lisp
; Define a procedural macro
(define-syntax macro-name
  (syntax-case-lambda (stx)
    (pattern template)
    (pattern guard template)
    ...))

; Or with explicit syntax-case
(define-syntax macro-name
  (syntax-case-lambda (stx)
    (syntax-case stx (literals...)
      (pattern template)
      (pattern guard template)
      ...)))
```

### New Forms

| Form | Description |
|------|-------------|
| `(syntax-case-lambda (stx) clauses...)` | Procedural macro definition |
| `(syntax expr)` or `#'expr` | Create syntax object from template |
| `(syntax-case stx (lits) clauses...)` | Pattern match on syntax |
| `(quasisyntax template)` or `` #`template `` | Quasiquoted syntax template |
| `(unsyntax expr)` or `#,expr` | Unquote in quasisyntax |
| `(syntax->datum stx)` | Extract datum from syntax (loses hygiene) |
| `(datum->syntax ctx datum)` | Create syntax with context's scopes |

### Pattern Guards

Guards allow conditional pattern matching:

```lisp
(syntax-case stx ()
  ((my-mac x)
   (identifier? #'x)      ; Guard - must be true to match
   #'(process-identifier x))
  ((my-mac x)
   #'(process-other x)))
```

### Compile-Time Helpers

Available during macro expansion:

```lisp
(identifier? stx)        ; Is it an identifier?
(syntax->datum stx)      ; Get the raw datum
(datum->syntax ctx stx)  ; Create syntax with ctx's scopes
(syntax-error msg stx)   ; Raise compile-time error
(generate-temporaries stxs) ; Generate fresh hygienic names
```

### Example: Computed Macro

```lisp
(define-syntax repeat-n
  (syntax-case-lambda (stx)
    ((repeat-n n body)
     ; n must be a literal number - check at compile time
     (let ((count (syntax->datum #'n)))
       (if (not (integer? count))
           (syntax-error "repeat-n requires literal count" #'n)
           ; Generate n copies of body
           (let ((bodies (make-list count #'body)))
             #`(my-begin #,@bodies)))))))

; Usage:
(repeat-n 3 (print "hello"))
; Expands to:
(my-begin (print "hello") (print "hello") (print "hello"))
```

### Example: Validation Macro

```lisp
(define-syntax define-checked
  (syntax-case-lambda (stx)
    ((define-checked (name args ...) body ...)
     (if (not (identifier? #'name))
         (syntax-error "name must be identifier" #'name)
         #'(fn name (args ...)
             (my-begin body ...))))))
```

## Simplified Implementation

For wisp, we'll implement a simplified version:

1. **syntax-case-lambda** - Receives the full macro call as input
2. **Pattern matching** - Same as syntax-rules but with guards
3. **syntax/#'** - Creates syntax object with current hygiene scope
4. **quasisyntax/#`** - Like quasiquote but for syntax
5. **Compile-time predicates** - `identifier?`, `number?`, etc.

### Data Structures

```rust
/// Extended macro types
enum MacroKind {
    DefMacro(Macro),
    SyntaxRules(SyntaxRulesMacro),
    SyntaxCase(SyntaxCaseMacro),
}

/// A syntax-case macro
struct SyntaxCaseMacro {
    name: String,
    param: String,  // The stx parameter name
    literals: Vec<String>,
    clauses: Vec<SyntaxCaseClause>,
}

/// A clause with optional guard
struct SyntaxCaseClause {
    pattern: Pattern,
    guard: Option<CompileTimeExpr>,
    template: Template,
}

/// Expressions evaluated at compile time
enum CompileTimeExpr {
    Identifier(String),
    App(String, Vec<CompileTimeExpr>),
    Syntax(SExpr),
    SyntaxCase { ... },
    If { cond, then, else_ },
    Let { name, value, body },
}
```

## Implementation Plan

### Phase 5.1: Syntax Object Primitives
- [x] Add `(syntax expr)` form that creates syntax with current scope
- [x] Add `(quasisyntax template)` with `(unsyntax e)`
- [x] Preserve hygiene through syntax construction

### Phase 5.2: syntax-case-lambda Parsing
- [x] Parse `(syntax-case-lambda (param) clauses...)`
- [x] Parse clauses with optional guards
- [x] Store as `SyntaxCaseMacro`

### Phase 5.3: Compile-Time Evaluation
- [x] Implement mini-interpreter for compile-time expressions
- [x] Add `identifier?`, `number?`, `syntax->datum`
- [x] Add `syntax-error` for compile-time errors

### Phase 5.4: Pattern Matching with Guards
- [x] Extend pattern matching to evaluate guards
- [x] Guards have access to pattern bindings
- [x] First matching pattern+guard wins

### Phase 5.5: Template Expansion
- [x] Expand `#'expr` to syntax object
- [x] Expand `#`template` with `#,` and `#,@`
- [x] Maintain hygiene through expansion

### Phase 5.6: Integration & Testing
- [x] Update `expand_macros` for syntax-case
- [x] Test computed macros
- [x] Test validation macros
- [x] Test hygiene preservation

## Test Cases

### Case 1: Simple syntax-case

```lisp
(define-syntax my-when
  (syntax-case-lambda (stx)
    ((my-when test body)
     #'(if test body (i32.const 0)))))

(export (fn test-when ((x s32)) s32
  (my-when (i32.gt_s x (i32.const 0))
    (i32.mul x x))))
```

### Case 2: With Guard

```lisp
(define-syntax literal-add
  (syntax-case-lambda (stx)
    ((literal-add a b)
     (and (number? #'a) (number? #'b))  ; Guard
     ; Compute at compile time!
     (let ((sum (+ (syntax->datum #'a) (syntax->datum #'b))))
       (datum->syntax #'a sum)))
    ((literal-add a b)
     #'(i32.add a b))))

(literal-add 3 5)  ; Expands to 8 at compile time!
(literal-add x 5)  ; Expands to (i32.add x 5)
```

### Case 3: Computed Expansion

```lisp
(define-syntax make-adders
  (syntax-case-lambda (stx)
    ((make-adders name n)
     (let ((count (syntax->datum #'n)))
       #`(my-begin
           #,@(map (lambda (i)
                    #`(fn #,(format-id #'name "~a-~a" #'name i) ((x s32)) s32
                        (i32.add x (i32.const #,i))))
                  (range count)))))))

(make-adders add 3)
; Generates: add-0, add-1, add-2 functions
```

## Breaking Changes

None - this adds new functionality.

## Success Criteria

Phase 5 is complete when:
- [x] `syntax-case-lambda` macros can be defined
- [x] Pattern matching with guards works
- [x] `#'` creates hygienic syntax objects
- [x] `#`` with `#,` expands correctly
- [x] Compile-time computation works
- [x] `syntax-error` produces good errors
- [x] Hygiene is preserved throughout
- [x] Test cases pass

## Future Work (Phase 6+)

- Full `syntax-case` as nestable expression
- `with-syntax` for binding syntax patterns
- `syntax-parse` with better error messages
- Custom syntax classes

## References

- [Syntactic Abstraction in Scheme](https://www.cs.indiana.edu/~dyb/pubs/tr356.pdf) - Dybvig et al.
- [Writing Hygienic Macros in Scheme with Syntax-Case](https://www.cs.indiana.edu/~dyb/pubs/tr356.pdf)
- [Racket syntax-case](https://docs.racket-lang.org/reference/stx-patterns.html)
