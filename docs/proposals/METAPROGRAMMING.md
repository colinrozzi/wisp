# Metaprogramming Framework Design

## Vision

Transform Wisp from a simple Lisp-to-WASM compiler into a **minimal core + powerful metaprogramming** system that serves as a playground for exploring language design concepts.

### Core Philosophy

1. **Minimal instruction set**: The core language maps almost 1:1 to WebAssembly instructions
2. **Unbounded extensions**: A macro system allows building arbitrary abstractions on top
3. **Learning through building**: Experiment with type systems, memory management strategies, and other language features as user-space libraries

### Why This Approach?

- **Educational**: Learn both language implementation and PL theory by building features as macros
- **Experimental**: Try different approaches (gradual typing, borrow checking, effect systems) without committing to any
- **Minimal core complexity**: The compiler stays simple; complexity lives in macro libraries
- **Real power**: Can implement production-level features (Typed Racket is proof of concept)

## The Core Language

### What Stays in Core

**Structural forms** (these handle scoping and program structure):
- `fn` - Function definitions
- `export` - Export declarations
- `import` - Import declarations
- `let` - Local bindings
- `if` - Conditionals

### What Becomes WASM Instructions

**Arithmetic operations** (type-specific):
```lisp
i32.add, i32.sub, i32.mul, i32.div_s, i32.div_u
i64.add, i64.sub, i64.mul, i64.div_s, i64.div_u
f32.add, f32.sub, f32.mul, f32.div
f64.add, f64.sub, f64.mul, f64.div
```

**Comparison operations**:
```lisp
i32.eq, i32.ne, i32.lt_s, i32.gt_s, i32.le_s, i32.ge_s
i64.eq, i64.ne, i64.lt_s, i64.gt_s, i64.le_s, i64.ge_s
f32.eq, f32.ne, f32.lt, f32.gt, f32.le, f32.ge
f64.eq, f64.ne, f64.lt, f64.gt, f64.le, f64.ge
```

**Memory operations** (NEW - critical for implementing allocators/GC):
```lisp
; Memory management
(memory.size)           ; Get current memory size in pages
(memory.grow pages)     ; Grow memory by pages

; Loads (with optional offset/alignment)
(i32.load addr)
(i64.load addr)
(f32.load addr)
(f64.load addr)

; Stores
(i32.store addr val)
(i64.store addr val)
(f32.store addr val)
(f64.store addr val)

; Byte-level operations
(i32.load8_s addr)   ; Load 8-bit signed
(i32.load8_u addr)   ; Load 8-bit unsigned
(i32.store8 addr val)
; ... similar for 16-bit
```

**Global variables** (for heap pointers, etc.):
```lisp
(global.get $name)
(global.set $name val)
```

**Constants**:
```lisp
(i32.const 42)
(i64.const 100)
(f32.const 3.14)
(f64.const 2.718)
```

**Type conversions**:
```lisp
(i32.wrap_i64 x)      ; i64 -> i32
(i64.extend_i32_s x)  ; i32 -> i64 (signed)
(f32.demote_f64 x)    ; f64 -> f32
(f64.promote_f32 x)   ; f32 -> f64
; ... etc
```

### Example: Before and After

**Current Wisp:**
```lisp
(fn factorial ((n s32)) s32
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

**Core Wisp (WASM instructions):**
```lisp
(fn factorial ((n s32)) s32
  (if (i32.eq n (i32.const 0))
      (i32.const 1)
      (i32.mul n (factorial (i32.sub n (i32.const 1))))))
```

**With macros (user-space):**
```lisp
; Define conveniences as macros
(defmacro = (a b) (i32.eq a b))
(defmacro * (a b) (i32.mul a b))
(defmacro - (a b) (i32.sub a b))

; Now you can write:
(fn factorial ((n s32)) s32
  (if (= n 0) 1 (* n (factorial (- n 1)))))
```

## Macro System Design

### Approach: Racket-Inspired, Incrementally Built

We're building a Racket-style macro system because:
- **Automatic hygiene**: Prevents variable capture bugs
- **Great errors**: Source location tracking for debugging
- **Pattern matching**: Declarative, powerful syntax
- **Production-proven**: Used in real languages (Typed Racket, Scribble, etc.)

### Implementation Phases

#### Phase 1: Unhygienic Macros with Quasiquotation (~1 week)
**Goal**: Get basic macro functionality working

```lisp
(defmacro when (condition body)
  `(if ,condition ,body 0))

; Usage:
(when (i32.gt x 10)
  (i32.add x 1))

; Expands to:
(if (i32.gt x 10)
    (i32.add x 1)
    0)
```

**Components**:
- Parse `defmacro` forms
- Store macro definitions (name, params, template)
- Implement quasiquote (`` ` ``), unquote (`,`), unquote-splicing (`,@`)
- Macro expansion pass (before type checking)
- Handle recursive expansion

**Data structures**:
```rust
struct Macro {
    name: String,
    params: Vec<String>,
    template: Sexpr,
}

// Macro registry
HashMap<String, Macro>
```

#### Phase 2: Syntax Objects (~3-4 days)
**Goal**: Add metadata for better error messages

Instead of `Sexpr`, use:
```rust
struct SyntaxObject {
    datum: Sexpr,           // The actual S-expression
    source_loc: SourceLoc,  // File, line, column
    scopes: ScopeSet,       // For hygiene (added in Phase 3)
}

struct SourceLoc {
    file: String,
    line: usize,
    column: usize,
    span: usize,
}
```

**Benefits**:
- Error messages show exact source location
- Track where code came from (user vs macro-generated)
- Foundation for hygiene

#### Phase 3: Automatic Hygiene (~3-4 days)
**Goal**: Prevent variable capture automatically

```lisp
; Without hygiene - BUG!
(defmacro bad-swap (a b)
  `(let ((tmp ,a))
     (set! ,a ,b)
     (set! ,b tmp)))

(let ((tmp 5))
  (bad-swap tmp x))  ; Breaks! tmp conflicts

; With hygiene - works automatically
; Compiler renames macro-introduced 'tmp' to tmp#123
```

**Implementation**: Scope sets algorithm
- Track which scope each identifier came from
- Rename bindings introduced by macros
- Preserve user-written names

#### Phase 4: Pattern Matching (syntax-rules level) (~1 week)
**Goal**: Declarative macro definitions

```lisp
(define-syntax when
  (syntax-rules ()
    [(when condition body ...)
     (if condition (begin body ...) 0)]))
```

**Features**:
- Pattern matching on syntax structure
- Ellipsis (`...`) for repetition
- Multiple patterns with guards
- Cleaner than manual quasiquoting

#### Phase 5: Full syntax-parse (~1-2 weeks)
**Goal**: Production-grade macro system

```lisp
(define-syntax-parser when
  [(when condition:expr body:expr ...)
   #'(if condition (begin body ...) 0)])
```

**Features**:
- Syntax classes (`:expr`, `:id`, `:keyword`)
- Custom syntax classes for domain-specific patterns
- Precise error messages
- Syntax properties and metadata

### Incremental Value

Each phase is independently useful:
- **Phase 1**: Can already build most abstractions
- **Phase 2**: Professional error messages
- **Phase 3**: Macro safety
- **Phase 4**: Macro elegance
- **Phase 5**: Language-building power

## What Becomes Possible

### Type Systems (as macro libraries!)

```lisp
; Hindley-Milner type inference
(import "types/hindley-milner.wisp")

(defn-typed map ((f (-> a b)) (xs (List a))) (List b)
  ...)

; Gradual typing (TypeScript-style)
(import "types/gradual.wisp")

(defn-gradual add (x y)
  (+ x y))  ; Works with or without type annotations

; Refinement types (Liquid Haskell-style)
(import "types/refinement.wisp")

(defn-refined div ((x Int) (y (Int != 0))) Int
  (i32.div_s x y))  ; Compiler proves y != 0
```

### Memory Management Strategies

```lisp
; Bump allocator
(import "memory/bump.wisp")

(defmacro malloc (size)
  `(let ((ptr (global.get $heap-ptr)))
     (global.set $heap-ptr (i32.add ptr ,size))
     ptr))

; Arena allocator
(import "memory/arena.wisp")

(with-arena arena
  (let ((x (arena-alloc arena 100))
        (y (arena-alloc arena 200)))
    ...))  ; All deallocated when exiting scope

; Reference counting GC
(import "memory/refcount.wisp")

(let ((obj (rc-alloc MyStruct)))
  (rc-incref obj)
  (rc-decref obj))  ; Freed when count hits 0

; Mark-sweep GC
(import "memory/gc.wisp")

(let ((obj (gc-alloc MyStruct)))
  ...)  ; Automatically collected

; Rust-style borrow checker
(import "memory/borrow.wisp")

(let ((vec (vec-new)))
  (with-borrow (slice 'a vec)
    (process slice))
  ; Compile error if slice used here
  )
```

### Effect Systems

```lisp
(import "effects/simple.wisp")

(defn-pure factorial ((n Int)) Int
  (if (= n 0) 1 (* n (factorial (- n 1)))))  ; OK

(defn-io read-file ((path String)) String
  (file-read path))  ; Marked as I/O

(defn-pure bad ()
  (read-file "foo.txt"))  ; COMPILE ERROR: I/O in pure function
```

### Pattern Matching & ADTs

```lisp
(import "data/adt.wisp")

(deftype Option
  (Some value)
  (None))

(deftype Result
  (Ok value)
  (Err error))

(defn unwrap-or ((opt Option) (default Int)) Int
  (match opt
    [(Some x) x]
    [(None) default]))
```

### Async/Await

```lisp
(import "async/runtime.wisp")

(defn-async fetch-user ((id Int)) User
  (let ((response (await (http-get (user-url id)))))
    (parse-user response)))

; Expands to state machine
```

### Domain-Specific Languages

```lisp
; SQL-like query language
(import "dsl/query.wisp")

(query
  (from users)
  (where (> age 18))
  (select name email))

; Expands to low-level data structure traversal
```

## Why This Works

### Multiple Incompatible Systems Can Coexist

Different projects can use different macro libraries:

```lisp
; Project A: Static types + manual memory
(import "types/static.wisp")
(import "memory/manual.wisp")

; Project B: Gradual types + GC
(import "types/gradual.wisp")
(import "memory/gc.wisp")

; Project C: No types + arena
(import "memory/arena.wisp")
```

The core compiler doesn't know or care!

### The Racket Precedent

This isn't theoretical - Racket proves it works:
- **Typed Racket**: Full static type system as macros
- **Lazy Racket**: Lazy evaluation as macros
- **Scribble**: Entire documentation language as macros
- **Datalog**: Logic programming as macros

### What Macros Can't Do

**Limitations**:
- Can't change WASM's evaluation order (strict)
- Can't add features WASM doesn't support (continuations, etc.)
- Can't optimize at codegen level (that's WASM's job)
- Can't change the runtime model

**But**: These are WASM limitations, not macro limitations.

## Development Strategy

### Minimal Viable Product

1. **Expose WASM instructions** in core language
2. **Add memory operations** (load, store, grow)
3. **Implement Phase 1 macros** (unhygienic + quasiquote)
4. **Build example macro libraries**:
   - Arithmetic operators (`+`, `-`, `*`, `/`)
   - Simple allocator
   - Basic type assertions

**Time estimate**: 1-2 weeks

### First Real Test

Implement a simple type checker as a macro library:
```lisp
(defmacro defn-typed (name params ret body)
  ; Walk body, check types, insert runtime assertions
  ...)
```

This will reveal what's missing from Phase 1.

### Iterate

Based on pain points:
- Add syntax objects if error messages are confusing
- Add hygiene if variable capture becomes a problem
- Add pattern matching if macros are too verbose

## Open Questions

1. **Module system**: How do macro libraries import/export macros?
2. **Compilation model**: Separate compilation or whole-program?
3. **Macro debugging**: How do users debug macro expansion?
4. **Performance**: Is macro expansion fast enough for large programs?
5. **Documentation**: How do we document what macros do?

## Success Metrics

We'll know this is working when:

1. The core compiler code stops growing
2. New features are added as macro libraries
3. Multiple incompatible type systems coexist
4. We can implement a non-trivial language feature (GC, borrow checker) in a weekend
5. The system is fun to experiment with!

## References

- [Racket Macro Guide](https://docs.racket-lang.org/guide/macros.html)
- [Fear of Macros Tutorial](https://www.greghendershott.com/fear-of-macros/all.html)
- [Beautiful Racket: Hygiene](https://beautifulracket.com/explainer/hygiene.html)
- [Typed Racket](https://docs.racket-lang.org/ts-guide/) - proof that type systems as macros work
- [Matthew Flatt's Macro Papers](https://www.cs.utah.edu/~mflatt/) - deep theory

## Next Steps

1. Create a concrete plan for Phase 1 implementation
2. Design the macro definition syntax
3. Implement quasiquote expansion
4. Build macro expansion pass
5. Test with simple macros (`when`, `unless`, `+`)
6. Add WASM memory operations
7. Implement a bump allocator as a macro library
