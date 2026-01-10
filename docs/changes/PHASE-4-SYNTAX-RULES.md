# Phase 4: Pattern-Matching Macros (syntax-rules)

**Status**: Complete
**Started**: 2026-01-10
**Completed**: 2026-01-10
**Related Proposal**: [docs/proposals/METAPROGRAMMING.md](../proposals/METAPROGRAMMING.md)

## Objective

Add `define-syntax` with `syntax-rules` for pattern-matching macros, enabling more expressive and safer macro definitions.

## Motivation

While `defmacro` is powerful, it requires manual argument handling:

```lisp
; With defmacro - manual destructuring
(defmacro my-if (cond then else)
  `(if ,cond ,then ,else))

; Can't easily handle variable number of arguments
; Can't pattern match on structure
```

With `syntax-rules`, macros use declarative pattern matching:

```lisp
; With syntax-rules - pattern matching
(define-syntax my-if
  (syntax-rules ()
    [(my-if cond then else)
     (if cond then else)]))

; Multiple patterns for different cases
(define-syntax my-cond
  (syntax-rules (else)
    [(my-cond (else result))
     result]
    [(my-cond (test result))
     (if test result (i32.const 0))]
    [(my-cond (test result) clause ...)
     (if test result (my-cond clause ...))]))
```

## Design

### Syntax

```lisp
(define-syntax macro-name
  (syntax-rules (literal ...)
    [pattern template]
    [pattern template]
    ...))
```

- `literal ...` - Symbols that match literally (not as pattern variables)
- `pattern` - S-expression with pattern variables and `...`
- `template` - S-expression with pattern variables and `...`

### Pattern Language

| Pattern | Matches |
|---------|---------|
| `symbol` | Any expression, binds to symbol (pattern variable) |
| `literal` | Exactly that symbol (if in literals list) |
| `(p1 p2 ...)` | List matching each sub-pattern |
| `(p1 p2 ... . rest)` | List with rest pattern |
| `(p ...)` | Zero or more expressions matching p |
| `(p1 p2 ... p3)` | p1, then zero+ p2, then p3 |

### Template Language

| Template | Expands to |
|----------|------------|
| `symbol` | The bound value (if pattern variable) or literal symbol |
| `(t1 t2 ...)` | List with each sub-template expanded |
| `(t ...)` | Repeated for each match of the ellipsis pattern |

### Data Structures

```rust
/// A syntax-rules macro definition
struct SyntaxRulesMacro {
    name: String,
    literals: Vec<String>,
    rules: Vec<SyntaxRule>,
}

/// A single pattern/template rule
struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

/// Pattern for matching
enum Pattern {
    /// Pattern variable (matches anything, binds)
    Variable(String),
    /// Literal symbol (matches exactly)
    Literal(String),
    /// Wildcard _ (matches anything, doesn't bind)
    Wildcard,
    /// List pattern
    List(Vec<Pattern>),
    /// List with ellipsis: (p1 p2 p3 ...)
    ListWithEllipsis {
        before: Vec<Pattern>,
        repeated: Box<Pattern>,
        after: Vec<Pattern>,
    },
}

/// Template for expansion
enum Template {
    /// Pattern variable reference
    Variable(String),
    /// Literal symbol
    Symbol(String),
    /// Literal value
    Literal(SExpr),
    /// List template
    List(Vec<Template>),
    /// Repeated template: t ...
    Ellipsis(Box<Template>),
}

/// Bindings from pattern matching
enum Binding {
    /// Single value
    Single(SExpr),
    /// List of values (from ellipsis match)
    List(Vec<SExpr>),
}
```

### Example: How It Works

```lisp
(define-syntax or
  (syntax-rules ()
    [(or) (i32.const 0)]
    [(or e) e]
    [(or e1 e2 ...)
     (let (tmp e1)
       (if tmp tmp (or e2 ...)))]))

; Usage:
(or a b c)

; Step 1: Try pattern (or) - doesn't match (3 args)
; Step 2: Try pattern (or e) - doesn't match (3 args)
; Step 3: Try pattern (or e1 e2 ...) - matches!
;   e1 = a
;   e2 ... = [b, c]

; Step 4: Expand template (let (tmp e1) (if tmp tmp (or e2 ...)))
;   e1 -> a
;   e2 ... -> b c (spliced into recursive or)
; Result: (let (tmp a) (if tmp tmp (or b c)))

; Step 5: Recursive expansion of (or b c)
;   e1 = b, e2 ... = [c]
; Result: (let (tmp b) (if tmp tmp (or c)))

; Step 6: Recursive expansion of (or c)
;   Matches (or e), e = c
; Result: c

; Final expanded code:
(let (tmp a)
  (if tmp tmp
    (let (tmp b)
      (if tmp tmp c))))

; Note: Each 'tmp' gets different hygiene scopes automatically!
```

## Implementation Plan

### Phase 4.1: Parse define-syntax
- [x] Add `SyntaxRulesMacro` struct
- [x] Parse `(define-syntax name (syntax-rules ...))` form
- [x] Store in separate map from defmacro macros

### Phase 4.2: Parse Patterns
- [x] Implement `Pattern` enum
- [x] Parse pattern from S-expression
- [x] Handle ellipsis in patterns
- [x] Track which symbols are literals vs variables

### Phase 4.3: Pattern Matching
- [x] Implement `match_pattern(pattern, sexpr) -> Option<Bindings>`
- [x] Handle literal matching
- [x] Handle variable binding
- [x] Handle list matching
- [x] Handle ellipsis matching (zero or more)

### Phase 4.4: Parse Templates
- [x] Implement `Template` enum
- [x] Parse template from S-expression
- [x] Track ellipsis in templates
- [x] Validate template variables exist in pattern

### Phase 4.5: Template Expansion
- [x] Implement `expand_template(template, bindings) -> SExpr`
- [x] Handle variable substitution
- [x] Handle ellipsis expansion
- [x] Integrate with hygiene (fresh scopes)

### Phase 4.6: Integration
- [x] Update `expand_macros` to handle syntax-rules
- [x] Try rules in order, use first match
- [x] Recursive expansion of result

### Phase 4.7: Testing
- [x] Simple single-pattern macros
- [x] Multiple-pattern macros
- [x] Ellipsis patterns and templates
- [ ] Nested ellipsis (future work)
- [x] Literal keywords
- [x] Hygiene with syntax-rules

## Test Cases

### Case 1: Simple Pattern Matching

```lisp
(define-syntax swap
  (syntax-rules ()
    [(swap a b)
     (let (tmp a)
       (let (_ (set! a b))
         (set! b tmp)))]))
```

### Case 2: Multiple Patterns

```lisp
(define-syntax my-and
  (syntax-rules ()
    [(my-and) (i32.const 1)]
    [(my-and e) e]
    [(my-and e1 e2)
     (if e1 e2 (i32.const 0))]))
```

### Case 3: Ellipsis

```lisp
(define-syntax begin
  (syntax-rules ()
    [(begin e) e]
    [(begin e1 e2 ...)
     (let (_ e1) (begin e2 ...))]))

(begin
  (f 1)
  (f 2)
  (f 3))
; Expands to: (let (_ (f 1)) (let (_ (f 2)) (f 3)))
```

### Case 4: Literal Keywords

```lisp
(define-syntax my-cond
  (syntax-rules (else)
    [(my-cond (else result))
     result]
    [(my-cond (test result))
     (if test result (i32.const 0))]
    [(my-cond (test result) rest ...)
     (if test result (my-cond rest ...))]))

(my-cond
  ((i32.eq x 0) (i32.const 100))
  ((i32.eq x 1) (i32.const 200))
  (else (i32.const 0)))
```

## Breaking Changes

None - this adds new functionality without changing existing behavior.

## Success Criteria

Phase 4 is complete when:
- [x] `define-syntax` with `syntax-rules` is parsed
- [x] Pattern matching works for simple patterns
- [x] Ellipsis patterns match zero or more elements
- [x] Templates expand with variable substitution
- [x] Ellipsis templates expand correctly
- [x] Literal keywords match exactly
- [x] Hygiene works with syntax-rules
- [x] Test cases 1-4 all pass
- [x] All existing examples still work

## Future Work (Phase 5)

With syntax-rules in place, future enhancements could include:
- `syntax-case` for procedural macros with pattern matching
- `with-syntax` for building syntax objects
- `syntax->datum` and `datum->syntax` for escaping hygiene
- Reader macros for custom syntax

## References

- [R5RS syntax-rules](https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_12.html)
- [Scheme Macros](https://docs.scheme.org/guide/macros/)
- [Fear of Macros](https://www.greghendershott.com/fear-of-macros/)
