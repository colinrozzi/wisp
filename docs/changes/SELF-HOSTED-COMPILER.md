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
**Status**: Not Started

Generate WAT string for basic expressions.

**Subset**:
- Integer literals
- WASM instructions (i32.add, etc.)
- Variable references

**Interface**:
```lisp
(fn compile-expr ((expr sexpr)) string
  (match expr
    ((sym s) (compile-var s))
    ((num n) (format "(i32.const ~a)" n))
    ((lst items) (compile-call items))
    ...))
```

---

### M6: Code Generator for Functions
**Status**: Not Started

Extend codegen to handle function definitions.

**Subset**:
- `fn` definitions with parameters
- `let` bindings
- `if` conditionals
- Function calls

---

### M7: Self-Compile Simple Program
**Status**: Not Started

Compile a simple Wisp program using the Wisp compiler.

**Test Case**:
```lisp
; factorial.wisp
(fn factorial ((n s32)) s32
  (if (i32.le_s n (i32.const 1))
    (i32.const 1)
    (i32.mul n (factorial (i32.sub n (i32.const 1))))))
```

**Success Criteria**:
1. Rust compiler compiles `factorial.wisp` → `expected.wasm`
2. Wisp compiler compiles `factorial.wisp` → `actual.wasm`
3. Both produce identical results when run

---

### M8: Full Bootstrap
**Status**: Not Started

Compile the Wisp compiler with itself.

**Steps**:
1. Use Rust compiler to compile Wisp compiler → `wisp-v1.wasm`
2. Use `wisp-v1.wasm` to compile Wisp compiler → `wisp-v2.wasm`
3. Use `wisp-v2.wasm` to compile Wisp compiler → `wisp-v3.wasm`
4. Verify: `wisp-v2.wasm` == `wisp-v3.wasm` (fixed point)

---

## Feature Status

| Feature | Status | Notes |
|---------|--------|-------|
| Strings | ✓ Complete | Create, length, ref, substring, append, compare |
| String ops | ✓ Complete | M1 complete |
| Pattern match | ✓ Complete | M2 complete - variant match works for AST |
| Tokenizer | ✓ Complete | M3 complete - `tokenize` function in Wisp |
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
- `examples/string-test.lisp` - String operation examples
- `tests/string_ops.rs` - String operation tests
- `tests/pattern_match.rs` - M2 pattern matching tests
- `examples/wisp-tokenizer.lisp` - M3 tokenizer implementation
- `tests/tokenizer.rs` - M3 tokenizer tests
- `examples/wisp-parser.lisp` - M4 parser implementation (includes tokenizer)
- `tests/parser.rs` - M4 parser tests

## Success Criteria

1. ✓ String operations for source code manipulation (M1)
2. ✓ Pattern matching for AST traversal (M2)
3. ✓ Write a tokenizer in Wisp that tokenizes Wisp code (M3)
4. ✓ Write a parser in Wisp that parses Wisp code (M4)
5. Write a codegen in Wisp that generates valid WASM (M5-M6)
6. Compile a simple Wisp program using the Wisp compiler (M7)
7. Eventually: compile the Wisp compiler with itself (M8)
