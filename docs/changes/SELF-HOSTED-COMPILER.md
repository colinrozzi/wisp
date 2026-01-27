# Self-Hosted Wisp Compiler

**Status**: In Progress
**Started**: 2026-01-25
**Target**: Write a Wisp compiler in Wisp itself

## Vision

Write a Wisp compiler in Wisp. Compile it to WASM. Run it as a Theater actor. Use it to compile REPL expressions into eval actors.

```
┌─────────────────────────────────────────────────────────┐
│                    REPL Flow                            │
│                                                         │
│  User: "(i32.add x 10)"                                │
│           │                                             │
│           ▼                                             │
│  ┌─────────────────┐                                   │
│  │ Compiler Actor  │  (Wisp compiler written in Wisp)  │
│  │ compile(src) →  │  → WASM bytes                     │
│  └─────────────────┘                                   │
│           │                                             │
│           ▼                                             │
│  ┌─────────────────┐                                   │
│  │  Eval Actor     │  (init → eval → result)           │
│  │  (ephemeral)    │                                   │
│  └─────────────────┘                                   │
│           │                                             │
│           ▼                                             │
│  Result: 52                                            │
└─────────────────────────────────────────────────────────┘
```

## Why Self-Hosted?

- **Pure actor model**: Everything is an actor, including the compiler
- **No std dependency**: Self-hosted compiler runs in WASM sandbox
- **Homoiconic**: Wisp compiling Wisp - code as data
- **Portable**: Compiler runs anywhere WASM runs

## Milestones

### M1: String Operations ✓
**Status**: Complete

Add string manipulation operations needed to write a tokenizer.

- [x] `(string-len s)` - get string length
- [x] `(string-ref s idx)` - get byte at index
- [x] `(substring s start end)` - extract substring
- [x] `(string-append s1 s2)` - concatenate strings
- [x] `(string=? s1 s2)` - string equality

**Documentation**: [STRING-OPERATIONS.md](STRING-OPERATIONS.md)

---

### M2: Pattern Matching ✓
**Status**: Complete

The existing variant `match` already supports the patterns needed for compiler writing.

**Implemented**:
```lisp
; Define AST as a variant
(variant sexpr
  (sym string)           ; symbol with name
  (num s32)              ; number
  (lst (list sexpr)))    ; list of s-expressions

; Match on it
(match expr
  ((sym name) ...)        ; match symbol, bind name
  ((num n) ...)           ; match number, bind n
  ((lst items) ...))      ; match list, bind items
```

**Bug Fixed**: `list-new` with variant types was incorrectly treating them as records. Fixed by passing variant names to `parse_type_expr`.

**Tests**: 13 tests in `tests/pattern_match.rs`

---

### M3: Tokenizer in Wisp ✓
**Status**: Complete

Write a tokenizer that converts source string to list of tokens.

**Token Type**:
```lisp
(variant token
  (lparen)                ; (
  (rparen)                ; )
  (number s32)            ; integer literal
  (symbol string)         ; identifier/symbol
  (str-lit string))       ; string literal "..."
```

**Implementation** (`examples/wisp-tokenizer.lisp`):

The tokenizer includes:
- Character classification: `is-whitespace`, `is-digit`, `is-delimiter`
- Tokenizer state record with `src`, `pos`, `len`
- Skip helpers: `skip-ws`, `skip-eol`, `skip-ignored-at` (whitespace + comments)
- Token readers: `read-number`, `read-symbol`, `read-string-lit`, `read-token`
- Main function: `tokenize` returns `(list token)`

**Example**:
```lisp
(tokenize "(i32.add 1 2)")
; → [lparen, symbol "i32.add", number 1, number 2, rparen]

(tokenize "(fn add ((x s32)) s32 x)")
; → 12 tokens: ( fn add ( ( x s32 ) ) s32 x )
```

**Features**:
- Handles whitespace (space, tab, newline, CR)
- Handles comments (`;` to end of line)
- Parses negative numbers (`-42`)
- Parses string literals (`"hello"`) with escape sequences
- Handles all Wisp delimiters

**Tests**: 32 tests in `tests/tokenizer.rs`

---

### M4: Parser in Wisp
**Status**: Complete

Convert token list to S-expression AST.

**SExpr Type**:
```lisp
(variant sexpr
  (sym string)            ; symbol/identifier
  (num s32)               ; number literal
  (str string)            ; string literal
  (lst (list sexpr)))     ; list of s-expressions
```

**Implementation** (`examples/wisp-parser.lisp`):

The parser includes:
- Token type (same as tokenizer)
- Parser result record with expr and new-pos fields
- Recursive descent parsing: `parse-list-items`, `parse-one`, `parse-atom`
- Top-level functions: `parse`, `parse-all`, `read-sexpr`, `read-all`
- Sexpr utilities: `is-sym`, `is-num`, `is-str`, `is-lst`, `get-sym`, `get-num`, `get-str`, `get-lst`

**Example**:
```lisp
(read-sexpr "(add 1 2)")
; → (lst [(sym "add"), (num 1), (num 2)])

(read-all "(fn add ((x s32)) s32 x)")
; → [(lst [(sym "fn"), (sym "add"), ...])]
```

**Bug Fixed**: During development, discovered that `list-push` was not copying old data when reallocating. Fixed by adding `memory.copy` to copy existing elements before appending new one.

**Tests**: 52 tests in `tests/parser.rs`

---

### M5: Code Generator for Simple Expressions
**Status**: Complete

Generate WAT string for basic expressions.

**Subset**:
- Integer literals → `(i32.const N)`
- WASM instructions → `(i32.add ...)`
- Variable references → `(local.get $name)`
- Function calls → `(call $name ...)`

**Implementation** (`examples/wisp-codegen.lisp`):

The codegen includes:
- `i32-to-string`: Convert integers to strings (needed for code generation)
- `is-wasm-instr`: Detect WASM instructions (i32., i64., f32., f64. prefixes)
- `compile-number`: `42` → `(i32.const 42)`
- `compile-var`: `x` → `(local.get $x)`
- `compile-wasm-call`: `(i32.add a b)` → `(i32.add <compile a> <compile b>)`
- `compile-fn-call`: `(foo a b)` → `(call $foo <compile a> <compile b>)`
- `compile-expr`: Main dispatcher that handles all expression types

**Example**:
```lisp
(compile-expr (num 42))
; → "(i32.const 42)"

(compile-expr (sym "x"))
; → "(local.get $x)"

; For (i32.add 1 2):
(compile-expr (lst [(sym "i32.add"), (num 1), (num 2)]))
; → "(i32.add (i32.const 1) (i32.const 2))"

; For (foo 1 2):
(compile-expr (lst [(sym "foo"), (num 1), (num 2)]))
; → "(call $foo (i32.const 1) (i32.const 2))"
```

**Tests**: 21 tests in `tests/codegen.rs`

---

### M6: Code Generator for Functions
**Status**: Complete

Extend codegen to handle function definitions.

**Subset**:
- `fn` definitions with parameters
- `let` bindings
- `if` conditionals
- Function calls

**Implementation** (extended `examples/wisp-codegen.lisp`):

Added to M5 codegen:
- `compile-if`: `(if c t e)` → `(if (result i32) <c> (then <t>) (else <e>))`
- `compile-let`: `(let (x v) b)` → `(local.tee $x <v>) <b>`
- `type-to-wat`: `s32` → `i32`, `s64` → `i64`, etc.
- `compile-param`: `(x s32)` → `(param $x i32)`
- `compile-params`: Compile list of parameters
- `compile-fn`: `(fn name ((x s32)) s32 body)` → `(func $name (param $x i32) (result i32) <body>)`

**Example**:
```lisp
; Compile (if x y z)
(compile-expr (lst [(sym "if"), (sym "x"), (sym "y"), (sym "z")]))
; → "(if (result i32) (local.get $x) (then (local.get $y)) (else (local.get $z)))"

; Compile (let (x 42) x)
(compile-expr (lst [(sym "let"), (lst [(sym "x"), (num 42)]), (sym "x")]))
; → "(local.tee $x (i32.const 42)) (local.get $x)"

; Compile (fn identity ((x s32)) s32 x)
(compile-fn [(sym "fn"), (sym "identity"), (lst [(lst [(sym "x"), (sym "s32")])]), (sym "s32"), (sym "x")])
; → "(func $identity (param $x i32) (result i32) (local.get $x))"
```

**Tests**: 28 tests in `tests/codegen.rs`

---

### M7: Self-Compile Simple Program
**Status**: Complete

Complete self-hosted Wisp compiler that can compile Wisp source to WAT.

**Implementation** (`examples/wisp-compiler.lisp`):

The full compiler integrates all previous milestones:
- Tokenizer from M3
- Parser from M4
- Code generator from M5/M6
- Top-level compilation (`compile-toplevel`, `compile-export`, `compile-fn-def`)
- Module wrapper generation (`compile` function)

**Main Entry Point**:
```lisp
(fn compile ((src string)) string
  (let (forms (read-all src))
    (let (body (compile-toplevels forms (i32.const 0) (list-len forms) ""))
      (string-append "(module\n  (memory 1)\n" (string-append body "\n)")))))
```

**Exported Functions**:
- `get-identity-wat`: Compiles identity function and returns WAT
- `get-factorial-wat`: Compiles factorial function and returns WAT
- `test-compile-identity`: Tests identity compilation
- `test-compile-factorial`: Tests factorial compilation

**Example Output**:
```lisp
(compile "(fn identity ((x s32)) s32 x)")
; → "(module
;      (memory 1)
;      (func $identity (param $x i32) (result i32)
;        (local.get $x))
;    )"
```

**Tests**: `tests/self_hosted.rs`
- `test_self_hosted_compiles`: Verifies compiler compiles
- `test_self_hosted_identity_wat`: Verifies identity function compilation
- `test_self_hosted_factorial_wat`: Verifies factorial function compilation

---

### REPL Integration ✓
**Status**: Complete

The self-hosted compiler now powers an interactive REPL.

**Usage**:
```bash
cargo run -p test-runtime -- --repl
```

**Example Session**:
```
wisp> (i32.add (i32.const 40) (i32.const 2))
42
wisp> (define x 10)
defined x = 10
wisp> (i32.mul x (i32.const 5))
50
wisp> (fn factorial ((n s32)) s32 (if (i32.le_s n (i32.const 1)) (i32.const 1) (i32.mul n (factorial (i32.sub n (i32.const 1))))))
defined function factorial
wisp> (factorial (i32.const 5))
120
```

**Pipeline**:
1. REPL generates source with inlined variables and accumulated functions
2. Self-hosted compiler (`wisp-compiler.wasm`) compiles source → WAT
3. `wat` crate assembles WAT → WASM
4. Wasmtime executes WASM and returns result

**Documentation**: [WISP-REPL-ARCHITECTURE.md](WISP-REPL-ARCHITECTURE.md)

---

### M8: Full Bootstrap
**Status**: Partial (blocked by tokenizer recursion)

Compile the Wisp compiler with itself.

**Goal**:
1. Use Rust compiler to compile Wisp compiler → `wisp-v1.wasm`
2. Use `wisp-v1.wasm` to compile Wisp compiler → `wisp-v2.wasm`
3. Use `wisp-v2.wasm` to compile Wisp compiler → `wisp-v3.wasm`
4. Verify: `wisp-v2.wasm` == `wisp-v3.wasm` (fixed point)

**Current State**:

The self-hosted compiler can successfully compile programs up to ~5KB. Full bootstrap
of the 42KB compiler source is blocked by the recursive tokenizer design:
- The tokenizer calls `tokenize-acc` once per character position
- A 42KB file requires ~40,000+ recursive calls
- Each call allocates tokens via `list-push`
- This exhausts the module's 6.4MB memory limit

**Verified Working**:
- ✓ `test_bootstrap_compile_simple`: Compiles identity function
- ✓ `test_bootstrap_compile_medium`: Compiles factorial, fibonacci, is-even/is-odd
- ✓ `test_bootstrap_compile_large`: Compiles ~3KB of tokenizer helpers + math functions

**Features Implemented**:

The self-hosted compiler currently supports:
- ✓ `fn` definitions
- ✓ `export`
- ✓ `if`, `let`
- ✓ WASM instructions (i32.*, i64.*, etc.)
- ✓ Function calls
- ✓ `match` expressions
- ✓ Built-in operations: `string-len`, `string-ref`, `list-len`, `list-get` (inlined)
- ✓ Built-in operations: `string-append`, `string=?`, `substring` (runtime helpers)
- ✓ Built-in operations: `list-new`, `list-push` (runtime helpers)
- ✓ Variant constructors: `sym`, `num`, `str`, `lst`, `lparen`, `rparen`, `number`, `symbol`, `str-lit`
- ✓ Record constructors: `token-result`, `parse-result`
- ✓ Record field access: `token-result.tok`, `token-result.new-pos`, `parse-result.expr`, `parse-result.new-pos`
- ✓ Variant/record definitions (skipped in compilation - constructors are hardcoded)

**To Achieve Full Bootstrap**:
1. Rewrite tokenizer to use iteration instead of recursion, OR
2. Implement WASM tail call optimization in the Wisp compiler, OR
3. Increase WASM module memory limits significantly

**Implementation Notes**:
- String/list operations are inlined as simple WAT expressions or use runtime helpers
- Tests require larger stack: `RUST_MIN_STACK=16777216` due to deeply nested if-else chains
- WASM stack configured to 128MB for bootstrap tests

---

## Feature Status

| Feature | Status | Notes |
|---------|--------|-------|
| Strings | ✓ Complete | Create, length, ref, substring, append, compare |
| String ops | ✓ Complete | M1 complete |
| Pattern match | ✓ Complete | M2 complete - variant match works for AST |
| Tokenizer | ✓ Complete | M3 complete - `tokenize` function in Wisp |
| Parser | ✓ Complete | M4 complete - `parse-all`, `read-all` functions |
| Codegen | ✓ Complete | M5/M6 complete - expressions and functions |
| Self-hosted compiler | ✓ Complete | M7 complete - `compile` function |
| Records | ✓ Exists | Already in Wisp |
| Variants | ✓ Exists | Already in Wisp |
| Lists | ✓ Exists | Already in Wisp |
| Recursive variants | ✓ Fixed | Bug fixed - list-new now handles variant types |
| Recursion | ✓ Works | Already in Wisp |

## Design Decisions

### WAT vs WASM Output

**Decision**: Start with WAT (text) output.

**Rationale**:
- Simpler to implement (string concatenation)
- Human-readable for debugging
- Can use host function `wat2wasm` for assembly
- WASM binary encoding can be added later

### Scope of Self-Hosted Compiler

**Initial subset**:
- Scalars: s32, s64, f32, f64
- WASM instructions
- let bindings
- if conditionals
- fn definitions
- Function calls

**Later additions**:
- Strings
- Records
- Variants
- match
- Macros (bootstrap the macro system!)

### Testing Strategy

**Approach**: Comparison testing

1. Compile test program with Rust compiler → expected output
2. Compile test program with Wisp compiler → actual output
3. Compare: WAT text, WASM bytes, or execution results

## Open Questions

1. **How to handle imports?** The Wisp compiler will need to import `wat2wasm` or similar. How does this affect the actor model?

2. **Memory management**: The self-hosted compiler will allocate strings, AST nodes, etc. Should we implement a GC, or rely on arena allocation per compilation?

3. **Error messages**: How sophisticated should error reporting be in the self-hosted compiler?

## Related Files

- `docs/changes/STRING-OPERATIONS.md` - M1 documentation
- `docs/changes/WISP-REPL-ARCHITECTURE.md` - REPL architecture and usage
- `examples/string-test.lisp` - String operation examples
- `tests/string_ops.rs` - String operation tests
- `tests/pattern_match.rs` - M2 pattern matching tests
- `examples/wisp-tokenizer.lisp` - M3 tokenizer implementation
- `tests/tokenizer.rs` - M3 tokenizer tests
- `examples/wisp-parser.lisp` - M4 parser implementation (includes tokenizer)
- `tests/parser.rs` - M4 parser tests
- `examples/wisp-codegen.lisp` - M5/M6 code generator implementation
- `tests/codegen.rs` - M5/M6 codegen tests
- `examples/wisp-compiler.lisp` - M7 complete self-hosted compiler
- `examples/wisp-compiler.wasm` - Compiled self-hosted compiler
- `tests/self_hosted.rs` - M7 self-hosted compiler tests
- `crates/test-runtime/` - REPL and test runtime implementation
- `crates/assembler-handler/` - WAT-to-WASM Theater handler

## Success Criteria

1. ✓ String operations for source code manipulation (M1)
2. ✓ Pattern matching for AST traversal (M2)
3. ✓ Write a tokenizer in Wisp that tokenizes Wisp code (M3)
4. ✓ Write a parser in Wisp that parses Wisp code (M4)
5. ✓ Write a codegen for simple expressions (M5)
6. ✓ Write a codegen for functions (M6)
7. ✓ Compile a simple Wisp program using the Wisp compiler (M7)
8. ✓ Interactive REPL powered by self-hosted compiler
9. Eventually: compile the Wisp compiler with itself (M8)
