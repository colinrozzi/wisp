# Component-Based Macro System

## The Big Idea

**Unify compile-time and runtime into a single component model.**

Instead of having two packaging systems (one for macros, one for runtime code), macros themselves are WASM components. The build process loads macro components and calls their `expand` function until the program reaches bedrock (pure WASM instructions).

### Why This Is Awesome

1. **Single packaging system** - Distribute macros as `.wasm` files, just like any other library
2. **Component model handles everything** - Versioning, linking, sandboxing all built-in
3. **Macros are Wisp programs** - Once bootstrapped, you write macros in Wisp itself
4. **No special cases** - The compiler doesn't distinguish "macro imports" from "runtime imports" at the syntax level
5. **Hybrid libraries possible** - A library can export both macros (compile-time) and functions (runtime)

## The WIT Interface

### Core Expansion Interface

```wit
// wit/expander.wit

package wisp:macros@0.1.0;

/// An S-expression node
variant sexpr {
    symbol(string),
    number(s64),
    float(f64),
    list(list<sexpr>),
}

/// Result of expansion
variant expand-result {
    /// Expansion succeeded, here's the new code
    expanded(sexpr),
    /// Nothing to expand (already at bedrock)
    unchanged,
    /// Expansion error
    error(string),
}

interface expander {
    /// Expand an S-expression one step
    /// Returns the expanded form, or unchanged if no expansion needed
    expand: func(expr: sexpr) -> expand-result;

    /// Check if this expander handles a given form
    /// e.g., a `+` macro would return true for (+ 1 2)
    handles: func(expr: sexpr) -> bool;
}
```

### Extended Interface with Metadata

```wit
/// Source location information (for error messages)
record source-loc {
    file: string,
    line: u32,
    column: u32,
}

/// S-expression with source location
record syntax {
    expr: sexpr,
    loc: option<source-loc>,
}

interface expander-v2 {
    /// Expand with source location tracking
    expand-syntax: func(expr: syntax) -> result<syntax, string>;

    /// Multi-step expansion to fixed point
    expand-all: func(expr: syntax) -> result<syntax, string>;

    /// Get macro documentation
    describe: func(name: string) -> option<string>;
}
```

## Build Process

### How Compilation Works

```
┌─────────────┐
│ Source Code │
│  prog.wisp  │
└──────┬──────┘
       │
       ▼
┌─────────────────┐
│  Parse to AST   │
└──────┬──────────┘
       │
       ▼
┌──────────────────────────┐
│ Load Macro Libraries     │ ◄─── math-macros.wasm
│ (from imports)           │ ◄─── types.wasm
└──────┬───────────────────┘ ◄─── gc-macros.wasm
       │
       ▼
┌─────────────────────────────┐
│ Expansion Loop:             │
│ 1. Walk AST                 │
│ 2. For each node, ask each  │
│    macro lib: handles(node)?│
│ 3. If yes, call expand(node)│
│ 4. Replace node with result │
│ 5. Repeat until fixed point │
└──────┬──────────────────────┘
       │
       ▼
┌─────────────────┐
│  Type Check     │
└──────┬──────────┘
       │
       ▼
┌─────────────────┐
│  Code Gen       │
└──────┬──────────┘
       │
       ▼
┌─────────────────┐
│  prog.wasm      │
└─────────────────┘
```

### Distinguishing Macro Imports from Runtime Imports

**Option 1: Metadata in import**
```lisp
(import "math-macros" :phase compile)  ; Only used during compilation
(import "math-runtime" :phase runtime) ; Linked into final binary
```

**Option 2: Convention - check WIT exports**
```lisp
(import "math-macros")  ; Compiler checks: does it export 'expander' interface?
                        ; If yes → macro library
                        ; If no → runtime library
```

**Option 3: Separate import forms**
```lisp
(import-macros "math-macros")  ; Compile-time only
(import "math-runtime")        ; Runtime
```

I prefer **Option 2** - it's implicit and reduces syntax, but Option 3 is clearer.

## Example: Simple Macro Library

### Arithmetic Operators Library

```lisp
; File: math-macros.wisp
; Exports macro expansion for +, -, *, /

(export expand)
(export handles)

(fn handles ((expr ???)) bool
  ; Check if expr is (+ ...) or (- ...) or (* ...) or (/ ...)
  (if (list? expr)
      (let ((head (car expr)))
        (or (eq? head '+)
            (eq? head '-)
            (eq? head '*)
            (eq? head '/)))
      false))

(fn expand ((expr ???)) ???
  (let ((op (car expr))
        (a (cadr expr))
        (b (caddr expr)))
    (match op
      ['+ `(i32.add ,a ,b)]
      ['- `(i32.sub ,a ,b)]
      ['* `(i32.mul ,a ,b)]
      ['/ `(i32.div_s ,a ,b)])))
```

**Compiled to:** `math-macros.wasm` with WIT:
```wit
world math-macros {
  export expander;
}
```

### Using the Macro Library

```lisp
; File: user.wisp
(import "math-macros")

(fn double ((x s32)) s32
  (* x 2))  ; Expands to (i32.mul x (i32.const 2))

(fn add-three ((a s32) (b s32) (c s32)) s32
  (+ (+ a b) c))  ; Expands to (i32.add (i32.add a b) c)
```

**Build process:**
1. Parse `user.wisp`
2. See `(import "math-macros")` → load `math-macros.wasm`
3. Walk AST, find `(* x 2)`
4. Ask math-macros: `handles((* x 2))` → `true`
5. Call math-macros: `expand((* x 2))` → `(i32.mul x (i32.const 2))`
6. Replace in AST
7. Repeat until no more macros
8. Final AST is pure WASM instructions → typecheck & codegen

## Hybrid Libraries: Macros + Runtime

Some libraries export both macros and runtime functions!

```lisp
; File: gc.wisp
; A garbage collector with both macros and runtime support

; === MACRO EXPORTS ===
(export expand)   ; For expander interface
(export handles)

; === RUNTIME EXPORTS ===
(export gc-init)
(export gc-collect)
(export gc-alloc-internal)

; Macro: (gc-alloc size) → expands to allocation with metadata
(fn expand ((expr ???)) ???
  (if (eq? (car expr) 'gc-alloc)
      (let ((size (cadr expr)))
        `(let ((ptr (gc-alloc-internal ,size)))
           (i32.store ptr (i32.const 0))  ; ref count = 0
           (i32.add ptr 4)))  ; return data pointer
      expr))

; Runtime: actual allocation function
(fn gc-alloc-internal ((size s32)) s32
  (let ((ptr (global.get $heap-ptr)))
    (global.set $heap-ptr (i32.add ptr (i32.add size 4)))
    ptr))

; Runtime: garbage collector
(fn gc-collect () s32
  ; ... mark and sweep implementation ...
  0)
```

**Usage:**
```lisp
(import "gc")

(fn main () s32
  (gc-init)
  (let ((obj (gc-alloc 100)))  ; Uses MACRO to expand
    (gc-collect)               ; Calls RUNTIME function
    obj))
```

The build process:
- Loads `gc.wasm` as macro library (because it exports `expander`)
- Expands `(gc-alloc 100)` using the macro
- Links `gc-collect` and `gc-alloc-internal` as runtime imports
- Final binary imports `gc.wasm` for the runtime functions only

## Bootstrapping Challenge

### The Chicken and Egg Problem

To write macros in Wisp, you need:
- S-expression manipulation functions (`car`, `cdr`, `cons`, `list?`, etc.)
- Pattern matching
- Quasiquote/unquote

But these are usually implemented via macros!

### Solution: Three-Stage Bootstrap

**Stage 0: Rust-based Core Expander**
```rust
// Built into the compiler
fn core_expand(expr: &Sexpr) -> Sexpr {
    match expr {
        Sexpr::List(items) if items[0] == "+" => {
            // Hard-coded expansion for +
            list![symbol("i32.add"), items[1].clone(), items[2].clone()]
        }
        // ... more hard-coded expansions
        _ => expr.clone()
    }
}
```

Ships with compiler, handles minimal set: `+`, `-`, `*`, `/`, `defmacro`, quasiquote.

**Stage 1: Wisp-based Expander (using only core instructions)**
```lisp
; expander.wisp
; Written using ONLY core WASM instructions + Stage 0 macros

(fn expand ((expr ???)) ???
  ; Can now use +, -, pattern matching from Stage 0
  (if (list? expr)
      (let ((head (car expr)))
        (match head
          ['+ (expand-plus expr)]
          ['- (expand-minus expr)]
          ...))
      expr))

(export expand)
```

Compile this with Stage 0 → `expander.wasm`

**Stage 2: Feature-Rich Expander (self-hosting)**
```lisp
; expander-v2.wisp
(import "expander")  ; Import Stage 1!

; Now we can use the expander's own macros to implement itself
(defmacro defmacro (name params body)
  `(fn ,name ,params ???
     ,body))

; Re-export
(export expand)
```

Compile with Stage 1 → `expander-v2.wasm`

Now `expander-v2.wasm` can expand itself! 🎉

## S-Expression Serialization

For passing S-expressions across component boundaries, we need a serialization format.

### Option 1: WIT Variant (shown above)
```wit
variant sexpr {
    symbol(string),
    number(s64),
    float(f64),
    list(list<sexpr>),
}
```

**Pros:** Type-safe, canonical
**Cons:** Recursive types might be tricky in WIT, limited to what WIT supports

### Option 2: Canonical S-Expression Format (bytes)
```wit
interface expander {
    expand: func(program: list<u8>) -> list<u8>;
}
```

Use a standard format like:
- [Canonical S-expressions](https://people.csail.mit.edu/rivest/Sexp.txt) (Rivest)
- Custom binary format
- JSON (human-readable, easy debugging)

**Pros:** Flexible, can add new types without WIT changes
**Cons:** Need parser/serializer, less type-safe

### Recommendation

Start with **Option 1 (WIT variant)** for type safety and simplicity. If it becomes limiting, switch to **Option 2** later.

## Expansion Algorithm

### Fixed-Point Expansion

```rust
fn expand_all(expr: Sexpr, macro_libs: &[Component]) -> Sexpr {
    let mut current = expr;
    let mut changed = true;

    while changed {
        changed = false;
        current = expand_once(current, macro_libs, &mut changed);
    }

    current
}

fn expand_once(expr: Sexpr, macro_libs: &[Component], changed: &mut bool) -> Sexpr {
    match expr {
        Sexpr::List(items) => {
            // Try to expand this form
            for lib in macro_libs {
                if lib.call_handles(&expr) {
                    let expanded = lib.call_expand(&expr);
                    if expanded != expr {
                        *changed = true;
                        return expanded;
                    }
                }
            }

            // Recursively expand children
            Sexpr::List(
                items.into_iter()
                     .map(|item| expand_once(item, macro_libs, changed))
                     .collect()
            )
        }
        _ => expr
    }
}
```

### Expansion Order

**Inside-out vs Outside-in?**

```lisp
(+ (* 2 3) 4)
```

- **Inside-out**: Expand `(* 2 3)` first → `(i32.mul 2 3)`, then `(+ (i32.mul 2 3) 4)` → `(i32.add (i32.mul 2 3) 4)`
- **Outside-in**: Expand `(+)` first → `(i32.add (* 2 3) 4)`, then `(*)` → `(i32.add (i32.mul 2 3) 4)`

For simple macros it doesn't matter, but for complex ones:
- **Inside-out** = Scheme/Racket default, better for most cases
- **Outside-in** = Allows macros to control sub-form expansion

**Recommendation**: Start with inside-out (easier), add outside-in control later if needed.

## Open Design Questions

### 1. Macro Composition

If multiple macro libraries define the same operator (e.g., two different `+` implementations), which wins?

**Options:**
- First import wins
- Last import wins
- Explicit precedence/priority
- Error (disallow conflicts)

### 2. Expansion Errors

When a macro fails to expand, how do we report it?

```wit
variant expand-result {
    expanded(sexpr),
    unchanged,
    error(string),  // ← Need source location!
}
```

Need to thread source locations through expansion.

### 3. Macro Debugging

How do users debug macro expansion?

- `--trace-expansion` flag shows each step?
- REPL command: `(macroexpand '(+ 1 2))` → `(i32.add 1 2)`?
- Debugger that steps through expansion?

### 4. Performance

Calling across component boundaries has overhead. For large programs with heavy macro use, will expansion be too slow?

**Mitigations:**
- Cache expanded forms
- Batch expansion calls
- Eventually: compile hot macros into the compiler itself

### 5. Security/Sandboxing

Macros run arbitrary WASM code during compilation. This is:
- **Good**: Powerful, flexible
- **Bad**: Could be malicious, slow, or non-deterministic

**Mitigations:**
- WASI sandboxing (limited I/O)
- Timeouts
- Resource limits
- Trusted macro registries?

## Implementation Roadmap

### MVP (Week 1-2)

1. **Define WIT interface** for `expander`
2. **Implement Stage 0** hard-coded Rust expander for `+`, `-`, `*`, `/`
3. **Add expansion pass** to compiler (before type checking)
4. **Test**: Write a program using `+`, verify it expands to `i32.add`

### Phase 1 (Week 3-4)

5. **Add `defmacro` support** to Stage 0
6. **Write Stage 1 expander** in Wisp (quasiquote, pattern matching)
7. **Bootstrap**: Compile Stage 1 using Stage 0
8. **Test**: Write user macros using `defmacro`

### Phase 2 (Week 5-6)

9. **Add source location tracking** to S-expressions
10. **Improve error messages** with source locations
11. **Add `macroexpand` introspection**
12. **Write macro library examples**: allocator, type checker

### Phase 3 (Week 7-8)

13. **Hygiene**: Add scope sets to syntax objects
14. **Pattern matching**: Implement `syntax-rules` or `syntax-parse`
15. **Self-hosting**: Stage 2 expander that compiles itself
16. **Documentation**: Write guide on writing macros

## Success Metrics

We'll know this works when:

1. ✅ The core compiler doesn't handle `+` - it's a macro
2. ✅ Macros are distributed as `.wasm` files on a registry
3. ✅ A user can write a type checker as a macro library in a weekend
4. ✅ The expander compiles itself
5. ✅ Multiple incompatible macro sets coexist in different projects
6. ✅ Expansion is fast enough for real codebases (< 1s for 10K LOC)

## Comparison to Other Approaches

### Racket
- **Racket**: Macros are Racket code, interpreted at compile-time
- **Wisp**: Macros are WASM components, run in VM at compile-time
- **Advantage**: Distribution/versioning via component model
- **Disadvantage**: Component boundary overhead

### Rust
- **Rust**: Procedural macros are compiled Rust code (separate crate)
- **Wisp**: Macros are compiled Wisp code (components)
- **Similar**: Both compile macros to native/bytecode
- **Advantage**: We can use component model sandboxing

### Common Lisp
- **CL**: Macros are Lisp functions, run in same image as compiler
- **Wisp**: Macros are separate components
- **Advantage**: Clear separation, safer
- **Disadvantage**: Can't share compiler state with macros

## Next Steps

1. ✅ Document this design (this file!)
2. ⏳ Design concrete WIT interface
3. ⏳ Prototype S-expression serialization
4. ⏳ Sketch Stage 0 Rust expander
5. ⏳ Add expansion pass to compiler
6. Write first macro library: `math-macros.wisp`
7. Test end-to-end: write program, expand, compile, run

---

**This is a novel approach** - I haven't seen macros-as-components done quite this way before. It unifies compile-time and runtime in an elegant way while leveraging WASM's sandboxing and component model's distribution. Worth experimenting with!
