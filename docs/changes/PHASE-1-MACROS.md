# Phase 1: Unhygienic Macros with Quasiquotation

**Status**: Complete
**Started**: 2026-01-10
**Completed**: 2026-01-10
**Related Proposal**: [docs/proposals/METAPROGRAMMING.md](../proposals/METAPROGRAMMING.md)

## Objective

Add basic macro support to Wisp - the first step toward the metaprogramming vision. This phase implements unhygienic macros with quasiquotation, allowing users to define syntactic abstractions.

## Motivation

With Phase 0 complete, users must write verbose explicit WASM instructions:
```lisp
(i32.add x (i32.const 1))
```

With macros, we can restore convenience while keeping the core minimal:
```lisp
(defmacro + (a b) `(i32.add ,a ,b))
(+ x 1)  ; expands to (i32.add x (i32.const 1))
```

## Target Syntax

### Macro Definition
```lisp
(defmacro name (param1 param2 ...)
  template)
```

### Quasiquotation
- **Quasiquote** `` `expr `` - quote an expression, allowing unquotes inside
- **Unquote** `,expr` - evaluate expr and splice result into template
- **Unquote-splicing** `,@expr` - splice a list into the surrounding list

### Examples

```lisp
; Simple substitution macro
(defmacro inc (x)
  `(i32.add ,x (i32.const 1)))

(inc y)  ; → (i32.add y (i32.const 1))

; Multi-expression macro
(defmacro when (cond body)
  `(if ,cond ,body (i32.const 0)))

(when (i32.gt x 0)
  (i32.mul x x))
; → (if (i32.gt x 0) (i32.mul x x) (i32.const 0))

; Convenience operators
(defmacro + (a b) `(i32.add ,a ,b))
(defmacro - (a b) `(i32.sub ,a ,b))
(defmacro * (a b) `(i32.mul ,a ,b))
(defmacro = (a b) `(i32.eq ,a ,b))
```

## Implementation Plan

### Phase 1.1: Tokenizer Updates
- [x] Add quasiquote token (backtick `` ` ``)
- [x] Add unquote token (comma `,`)
- [x] Add unquote-splicing token (`,@`)
- [x] Handle these in symbol parsing
- [x] Add comment support (`;` to end of line)

### Phase 1.2: Parser Updates
- [x] Add `SExpr::Quasiquote`, `SExpr::Unquote`, `SExpr::UnquoteSplice` variants
- [x] Parse quasiquoted expressions
- [x] Parse `defmacro` as a top-level form

### Phase 1.3: Macro Data Structures
- [x] Create `Macro` struct (name, params, template)
- [x] Build macro registry during expansion phase

### Phase 1.4: Macro Expansion Pass
- [x] Implement `expand_macros()` function
- [x] Walk S-expressions looking for macro calls
- [x] Substitute parameters into template
- [x] Evaluate quasiquoted templates
- [x] Handle recursive expansion (with depth limit of 100)
- [x] Run expansion before parsing to Expr AST

### Phase 1.5: Integration
- [x] Update compilation pipeline: tokenize → parse → **expand** → type-check → codegen
- [x] Test with example macros (`examples/macro-test.lisp`)

### Phase 1.6: Standard Macro Library
- [ ] Create `std/arithmetic.wisp` with `+`, `-`, `*`, `/`, `%` (future work)
- [ ] Create `std/comparison.wisp` with `=`, `!=`, `<`, `>`, `<=`, `>=` (future work)
- [ ] Create `std/control.wisp` with `when`, `unless`, `cond` (future work)

## Data Structures

```rust
// New SExpr variants for quasiquotation
enum SExpr {
    // ... existing variants ...
    Quote(Box<SExpr>),
    Quasiquote(Box<SExpr>),
    Unquote(Box<SExpr>),
    UnquoteSplice(Box<SExpr>),
}

// Macro definition
struct Macro {
    name: String,
    params: Vec<String>,
    template: SExpr,
}

// Updated Program
struct Program {
    functions: Vec<Function>,
    imports: Vec<Import>,
    exports: Vec<String>,
    globals: Vec<Global>,
    macros: Vec<Macro>,  // NEW
}
```

## Expansion Algorithm

```
expand(sexpr, macros):
  match sexpr:
    List([Sym(name), ...args]) if name in macros:
      macro = macros[name]
      expanded = substitute(macro.template, zip(macro.params, args))
      return expand(expanded, macros)  // recursive expansion

    Quasiquote(template):
      return eval_quasiquote(template, depth=1)

    List(items):
      return List(items.map(|item| expand(item, macros)))

    other:
      return other

eval_quasiquote(template, depth):
  match template:
    Unquote(expr) if depth == 1:
      return expr  // splice in the expression

    Quasiquote(inner):
      return Quasiquote(eval_quasiquote(inner, depth + 1))

    Unquote(inner):
      return Unquote(eval_quasiquote(inner, depth - 1))

    List(items):
      return List(items.flat_map(|item|
        match item:
          UnquoteSplice(list) if depth == 1: list.items
          other: [eval_quasiquote(other, depth)]
      ))

    other:
      return other
```

## Breaking Changes

None - this is purely additive.

## Open Questions

1. **Macro scope**: Should macros be module-local or require explicit export?
   - Start with: all macros visible within the file they're defined

2. **Expansion order**: Top-down or bottom-up?
   - Start with: top-down (expand outer macros first)

3. **Recursive macros**: Allow macros to call themselves?
   - Start with: yes, but with depth limit (e.g., 100)

4. **Numeric literal handling**: Should `1` in macro output become `(i32.const 1)`?
   - Start with: no, keep current behavior (literals work as-is)

## Success Criteria

Phase 1 is complete when:
- [x] `defmacro` syntax works
- [x] Quasiquote, unquote, and unquote-splicing work
- [x] Macros expand correctly before type checking
- [x] Can implement `+`, `-`, `*` as macros
- [x] Can implement `when` control flow macro
- [x] All existing examples still compile
- [x] New macro examples demonstrate the feature (`examples/macro-test.lisp`)

## Example: Full Workflow

```lisp
; File: examples/macro-test.lisp

; Define convenience macros
(defmacro + (a b) `(i32.add ,a ,b))
(defmacro - (a b) `(i32.sub ,a ,b))
(defmacro * (a b) `(i32.mul ,a ,b))
(defmacro = (a b) `(i32.eq ,a ,b))

(defmacro when (cond body)
  `(if ,cond ,body (i32.const 0)))

; Use macros in functions
(export (fn factorial ((n s32)) s32
  (if (= n 0)
      1
      (* n (factorial (- n 1))))))

; After expansion, becomes:
; (export (fn factorial ((n s32)) s32
;   (if (i32.eq n (i32.const 0))
;       (i32.const 1)
;       (i32.mul n (factorial (i32.sub n (i32.const 1)))))))
```

## Future Work (Post Phase 1)

- Phase 2: Syntax objects for source location tracking
- Phase 3: Automatic hygiene
- Phase 4: Pattern matching (syntax-rules)
- Phase 5: Full syntax-parse
