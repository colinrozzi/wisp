# wisp Compiler Progress

**A Lisp-to-WebAssembly Compiler with Hygienic Macros**

## Overview

wisp is an educational Lisp compiler that targets WebAssembly Components. It demonstrates how to build a macro system with automatic hygiene using the scope sets algorithm.

## Completed Phases

### Phase 0: WASM Instructions ✅
**Status**: Complete

Exposed WebAssembly instructions directly in the language, providing a nearly 1:1 mapping to WASM.

**Features:**
- Integer operations: `i32.add`, `i32.sub`, `i32.mul`, `i32.div_s`, `i64.add`, etc.
- Float operations: `f32.add`, `f64.mul`, `f64.div`, etc.
- Comparisons: `i32.eq`, `i32.lt_s`, `i32.ge_s`, `f64.ne`, etc.
- Constants: `i32.const`, `i64.const`, `f32.const`, `f64.const`
- Conversions: `i64.extend_i32_s`, `f32.demote_f64`, `i32.trunc_f64_s`
- Memory: `i32.load`, `i32.store`, `memory.size`, `memory.grow`
- Globals: `global.get`, `global.set`

**Example:**
```lisp
(export (fn add ((a s32) (b s32)) s32
  (i32.add a b)))
```

---

### Phase 1: Quasiquote Macros ✅
**Status**: Complete

Added `defmacro` with quasiquotation for template-based macro expansion.

**Features:**
- `defmacro` for defining macros
- Quasiquote (`` ` ``) for templates
- Unquote (`,`) for inserting values
- Unquote-splicing (`,@`) for splicing lists

**Example:**
```lisp
(defmacro + (a b) `(i32.add ,a ,b))
(defmacro when (cond body)
  `(if ,cond ,body (i32.const 0)))

(+ 3 5)           ; Expands to (i32.add 3 5)
(when (> x 0) x)  ; Expands to (if (> x 0) x (i32.const 0))
```

---

### Phase 2: Syntax Objects ✅
**Status**: Complete

Added source location tracking to all S-expressions for better error messages.

**Features:**
- `Span` struct with line, column, length
- Source locations preserved through parsing
- Beautiful error messages with context

**Example Error:**
```
error: unknown function or operator 'foo'
  --> examples/test.lisp:15:5
   |
15 |     (foo x y)
   |      ^^^
```

---

### Phase 3: Automatic Hygiene ✅
**Status**: Complete

Implemented the scope sets algorithm for automatic macro hygiene.

**Features:**
- Each identifier carries a `ScopeSet`
- Fresh scope generated for each macro expansion
- Template identifiers get macro scope added
- Unquoted expressions keep original scopes
- Scope-aware variable resolution
- Name mangling for codegen

**How it works:**
```lisp
(defmacro with-temp (body)
  `(let (tmp (i32.const 0))
     ,body))

; User code:
(let (tmp (i32.const 42))
  (with-temp tmp))  ; Returns 42, NOT 0!

; After expansion:
; - User's tmp has scope {0}
; - Macro's tmp has scope {0, 1}
; - Reference in body has scope {0}
; - Resolution: {0} ⊆ {0} but {0,1} ⊄ {0}
; - User's tmp is correctly selected!
```

**Test Results:**
- `test-no-capture`: 42 ✓ (user variable not captured)
- `test-self-ref`: 100 ✓ (macro can reference own bindings)
- `test-nested`: 102 ✓ (nested macros work correctly)

---

### Phase 4: Pattern-Matching Macros ✅
**Status**: Complete

Added `define-syntax` with `syntax-rules` for Scheme-style pattern matching macros.

**Features:**
- `define-syntax` for defining pattern-matching macros
- `syntax-rules` with literal keywords
- Pattern variables that bind to matched expressions
- Ellipsis (`...`) for matching zero or more elements
- Multiple patterns tried in order
- Full hygiene integration

**Example:**
```lisp
; Simple pattern
(define-syntax my-add
  (syntax-rules ()
    ((my-add a b)
     (i32.add a b))))

; Multiple patterns
(define-syntax my-inc
  (syntax-rules ()
    ((my-inc x)
     (i32.add x (i32.const 1)))
    ((my-inc x n)
     (i32.add x n))))

; Ellipsis for variadic macros
(define-syntax my-begin
  (syntax-rules ()
    ((my-begin e) e)
    ((my-begin e1 e2 ...)
     (let (_ e1) (my-begin e2 ...)))))

; Literal keywords
(define-syntax my-cond
  (syntax-rules (else)
    ((my-cond (else result))
     result)
    ((my-cond (test result))
     (if test result (i32.const 0)))))
```

**Test Results:**
- `test-simple 3 5`: 8 ✓ (basic pattern matching)
- `test-inc 10`: 11 ✓ (multiple patterns)
- `test-inc-n 10 5`: 15 ✓ (multiple patterns)
- `test-begin 7`: 17 ✓ (ellipsis expansion)
- `test-else`: 42 ✓ (literal keywords)
- `test-cond 5`: 100 ✓ (conditional patterns)

---

### Phase 5: Procedural Macros (syntax-case) ✅
**Status**: Complete

Added `syntax-case-lambda` for procedural macros that can run arbitrary code during expansion.

**Features:**
- `syntax-case-lambda` for procedural macro definitions
- Pattern matching with optional guards
- `#'` (syntax quote) for creating syntax objects with hygiene
- `#`` (quasisyntax) with `#,` (unsyntax) and `#,@` (unsyntax-splice)
- Compile-time evaluation with predicates: `identifier?`, `number?`, `integer?`
- Compile-time operations: `let`, `if`, `and`, `or`, `not`, `+`, `-`
- `syntax->datum` for extracting values from syntax
- `syntax-error` for compile-time error reporting

**Example:**
```lisp
; Macro with guard - only matches identifiers
(define-syntax double-if-id
  (syntax-case-lambda (stx)
    ((double-if-id x)
     (identifier? #'x)
     #'(i32.add x x))
    ((double-if-id x)
     #'x)))

; Macro using quasisyntax
(define-syntax add-one
  (syntax-case-lambda (stx)
    ((add-one x)
     #`(i32.add #,x (i32.const 1)))))

; Macro with compile-time let
(define-syntax triple
  (syntax-case-lambda (stx)
    ((triple x)
     (let (doubled #'(i32.add x x))
       #`(i32.add #,doubled x)))))
```

**Test Results:**
- `test-simple 3 5`: 8 ✓ (basic syntax-case)
- `test-guard-id 10`: 20 ✓ (pattern guard with identifier?)
- `test-guard-nonid`: 21 ✓ (guard fallthrough)
- `test-quasisyntax 41`: 42 ✓ (quasisyntax with unsyntax)
- `test-ct-let 5`: 15 ✓ (compile-time let binding)
- `test-when 7`: 49 ✓ (simple when macro)

---

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                     Compilation Pipeline                      │
├──────────────────────────────────────────────────────────────┤
│                                                              │
│   Source Code (.lisp)                                        │
│        │                                                     │
│        ▼                                                     │
│   ┌─────────────┐                                           │
│   │  Tokenizer  │  Converts text to tokens                  │
│   └─────────────┘                                           │
│        │                                                     │
│        ▼                                                     │
│   ┌─────────────┐                                           │
│   │   Parser    │  Builds S-expressions with spans          │
│   └─────────────┘                                           │
│        │                                                     │
│        ▼                                                     │
│   ┌─────────────┐                                           │
│   │   Macro     │  Expands defmacro, syntax-rules, and      │
│   │  Expansion  │  syntax-case with automatic hygiene       │
│   └─────────────┘                                           │
│        │                                                     │
│        ▼                                                     │
│   ┌─────────────┐                                           │
│   │  parse_expr │  Converts S-expr to typed AST             │
│   └─────────────┘  with hygienic variable resolution        │
│        │                                                     │
│        ▼                                                     │
│   ┌─────────────┐                                           │
│   │ Type Check  │  Validates types match WASM requirements  │
│   └─────────────┘                                           │
│        │                                                     │
│        ▼                                                     │
│   ┌─────────────┐                                           │
│   │  Codegen    │  Generates WAT text format                │
│   └─────────────┘                                           │
│        │                                                     │
│        ▼                                                     │
│   ┌─────────────┐                                           │
│   │ Component   │  Encodes WASM component with WIT          │
│   │  Encoder    │                                           │
│   └─────────────┘                                           │
│        │                                                     │
│        ▼                                                     │
│   Output: .wat, .wasm, .wit                                 │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

## Key Data Structures

### Scope Sets (Hygiene)
```rust
type ScopeId = u64;

struct ScopeSet {
    scopes: HashSet<ScopeId>,
}

struct Span {
    line: usize,
    column: usize,
    length: usize,
    scopes: ScopeSet,  // Hygiene tracking
}

struct Binding {
    name: String,
    scopes: ScopeSet,
}
```

### Pattern Matching
```rust
enum Pattern {
    Variable(String),
    Literal(String),
    Wildcard,
    List(Vec<Pattern>),
    ListWithEllipsis {
        before: Vec<Pattern>,
        repeated: Box<Pattern>,
        after: Vec<Pattern>,
    },
}

enum Template {
    Variable(String),
    Symbol(String),
    Atom(SExpr),
    List(Vec<Template>),
    Ellipsis(Box<Template>),
}
```

## Usage

```bash
# Compile a source file
cargo run -- compile examples/prog.lisp examples/prog

# Run an exported function
cargo run -- run examples/prog.wasm function-name arg1 arg2

# Run with dependencies
cargo run -- run examples/user.wasm run 5 --dep math=examples/math.wasm
```

## Example Programs

### Factorial with Macros
```lisp
(defmacro * (a b) `(i32.mul ,a ,b))
(defmacro - (a b) `(i32.sub ,a ,b))
(defmacro = (a b) `(i32.eq ,a ,b))

(export (fn factorial ((n s32)) s32
  (if (= n 0)
      1
      (* n (factorial (- n 1))))))
```

### Begin with syntax-rules
```lisp
(define-syntax begin
  (syntax-rules ()
    ((begin e) e)
    ((begin e1 e2 ...)
     (let (_ e1) (begin e2 ...)))))

(export (fn example () s32
  (begin
    (global.set $counter (i32.const 0))
    (global.set $counter (i32.add (global.get $counter) (i32.const 1)))
    (global.get $counter))))
```

## References

- [Binding as Sets of Scopes](https://www.cs.utah.edu/~mflatt/scope-sets-5/) - Matthew Flatt
- [R5RS syntax-rules](https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_12.html)
- [Fear of Macros](https://www.greghendershott.com/fear-of-macros/)
- [Beautiful Racket: Hygiene](https://beautifulracket.com/explainer/hygiene.html)

## Future Work

Potential Phase 6+ enhancements:
- Full `syntax-case` as nestable expression (not just at top level)
- `with-syntax` for binding syntax patterns
- `syntax-parse` with better error messages
- Custom syntax classes
- `datum->syntax` for controlled hygiene breaking
- Reader macros for custom syntax
- Nested ellipsis support in syntax-rules
- Module system with macro imports/exports

---

*Built with Rust, targeting WebAssembly Components*
