# Phase 7: Macro Components

**Status**: Design
**Started**: 2025-01-12
**Prerequisites**: Phase 6 (WIT types) complete

## Objective

Unify the module system by making macros WASM components. Macros implement a standard interface, receive S-expressions as WIT variant values, and return transformed S-expressions. The compiler loads and calls these components during expansion.

## Motivation

With Phase 6 providing WIT types, macros can naturally work with structured data:

- `sexpr` is a WIT variant (not tagged s32 hacks)
- `result<sexpr, macro-error>` for error handling
- `list<sexpr>` for syntax lists
- `string` for symbols and messages

This means:
- Macros are regular wisp programs compiled to components
- The "compile-time language" is just wisp
- Macros can be distributed as `.wasm` files
- Any language targeting WASM can provide macros

## Design

### The S-Expression Type

Using Phase 6's WIT types:

```lisp
(record span
  (line s32)
  (column s32)
  (length s32)
  (scopes (list s32)))  ; for hygiene

(record macro-error
  (message string)
  (location (option span)))

(variant sexpr
  (sym string span)         ; symbol
  (int s64 span)            ; integer literal
  (flt f64 span)            ; float literal
  (str string span)         ; string literal
  (lst (list sexpr) span))  ; list
```

### The Macro Interface

```wit
package wisp:macro@0.1.0;

interface transformer {
  use sexpr.{sexpr, span, macro-error};

  expand: func(input: sexpr) -> result<sexpr, macro-error>;
}

world macro {
  export transformer;
}
```

### Writing a Macro

A macro is a wisp program that exports `expand`:

```lisp
;; list-macro.wisp
;; Transforms (list a b c) -> (cons a (cons b (cons c nil)))

(export expand)

(fn expand ((input sexpr)) (result sexpr macro-error)
  (match input
    ((lst items span)
     (match items
       ;; Skip the 'list' symbol, process the rest
       ((cons _ args) (ok (fold-right make-cons (make-nil span) args)))
       (_ (err (macro-error "list requires arguments" (some span))))))
    (_ (err (macro-error "expected list" (none))))))

(fn make-cons ((elem sexpr) (acc sexpr)) sexpr
  (let-record elem (_ span)
    (lst (list-of (sym "cons" span) elem acc) span)))

(fn make-nil ((span span)) sexpr
  (sym "nil" span))
```

Compile it:
```bash
cargo run -- compile list-macro.wisp std/list-macro
```

### Using a Macro

```lisp
;; user.wisp
(import-macro list "std/list-macro.wasm")

(list 1 2 3)
;; Compiler calls list-macro.wasm's expand function
;; Expands to: (cons 1 (cons 2 (cons 3 nil)))
```

### Compiler Behavior

When the compiler encounters `(import-macro name "path.wasm")`:

1. **Load**: Instantiate the component (cache for reuse)
2. **Register**: Associate the macro name with the component

When the compiler encounters a call to a macro:

1. **Serialize**: Convert internal AST to `sexpr` WIT value
2. **Call**: Invoke the component's `expand` function
3. **Handle result**:
   - `ok(sexpr)`: Deserialize and continue compilation
   - `err(macro-error)`: Report compile error with location
4. **Recurse**: Expand any macros in the result

### Hygiene

Scope information travels in the `span`:

```lisp
(record span
  (line s32)
  (column s32)
  (length s32)
  (scopes (list s32)))  ; scope set for hygiene
```

The compiler:
1. Assigns a fresh scope ID before calling the macro
2. Includes current scopes in the input `sexpr`
3. Macro can add/inspect scopes as needed
4. Compiler resolves variables using scope sets

### Standard Library Structure

```
std/
  list-macro.wasm       ; (list ...) macro
  cond-macro.wasm       ; (cond ...) macro
  and-macro.wasm        ; (and ...) short-circuit
  or-macro.wasm         ; (or ...) short-circuit
  let*-macro.wasm       ; (let* ...) sequential bindings
  types/
    typed-fn.wasm       ; optional type checking macro
```

### Example: Type Checking Macro

Type safety as a library, not a language feature:

```lisp
;; std/types/typed-fn.wisp
;; Implements compile-time type checking

(export expand)

;; (typed-fn name ((param : type) ...) : return-type body)
;; Checks types at compile time, emits plain (fn ...) on success

(fn expand ((input sexpr)) (result sexpr macro-error)
  (match input
    ((lst items span)
     (let (checked (check-types items))
       (match checked
         ((ok untyped) (ok untyped))
         ((err msg) (err (macro-error msg (some span)))))))
    (_ (err (macro-error "typed-fn expects a list" (none))))))
```

Usage:
```lisp
(import-macro typed-fn "std/types/typed-fn.wasm")

(typed-fn car ((p : pair)) : value
  ...)

(car my-symbol)  ; Compile error from typed-fn macro!
```

## Implementation Plan

### Phase 7.1: Macro Loading
- [ ] Parse `(import-macro name "path.wasm")` syntax
- [ ] Load macro components using wasmtime
- [ ] Cache loaded components
- [ ] Build registry of macro name -> component

### Phase 7.2: Serialization
- [ ] Convert internal `SExpr` to WIT `sexpr` variant
- [ ] Include span and scope information
- [ ] Handle all syntax types (symbols, numbers, lists, etc.)

### Phase 7.3: Expansion
- [ ] Call macro component's `expand` function
- [ ] Deserialize result back to internal `SExpr`
- [ ] Handle `err` results as compile errors
- [ ] Recursively expand results

### Phase 7.4: Hygiene Integration
- [ ] Pass scope sets through serialization
- [ ] Generate fresh scopes for macro calls
- [ ] Preserve hygiene across component boundary

### Phase 7.5: Bootstrap Macros
- [ ] Write `list` macro as component
- [ ] Write `cond` macro as component
- [ ] Write `and`/`or` macros as components
- [ ] Verify hygiene works correctly

### Phase 7.6: Migration
- [ ] Deprecate inline `defmacro`/`define-syntax`
- [ ] Or: keep for convenience, compile to in-memory components
- [ ] Update documentation
- [ ] Provide migration guide

## Open Questions

1. **Keep inline macros?**
   - Pro: Convenient for small projects
   - Con: Two systems
   - Option: Inline macros compile to temporary components

2. **Macro-providing macros?**
   - A macro that generates another macro
   - Would need compile-time component generation
   - Maybe not needed for v1

3. **Macro debugging?**
   - How to debug macro expansion?
   - Source maps through expansion?
   - Expansion tracing?

## Success Criteria

Phase 7 is complete when:
- [ ] `import-macro` loads macro components
- [ ] Macro expansion calls the component's `expand` function
- [ ] `list` and `cond` work as external components
- [ ] Hygiene is preserved across component boundaries
- [ ] Meaningful error messages from macro errors
- [ ] Example type-checking macro demonstrates the approach

## References

- [WebAssembly Component Model](https://component-model.bytecodealliance.org/)
- [Racket's Syntax System](https://docs.racket-lang.org/reference/syntax-model.html)
- [Rust Procedural Macros](https://doc.rust-lang.org/reference/procedural-macros.html)
