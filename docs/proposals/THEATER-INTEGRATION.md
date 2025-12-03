# Wisp + Theater Integration

This document describes the path to making Wisp a first-class language for writing Theater actors and the target language for the Theater REPL.

## Goal

Wisp becomes a fully-featured language that can:
1. Write complete Theater actors (replacing Rust for simple cases)
2. Serve as the command language for an interactive Theater REPL
3. Support all WIT types needed for Theater interfaces

## Current State

Wisp currently supports:
- Scalar types: `s32`, `s64`, `f32`, `f64`
- Functions with explicit WASM instructions
- Basic imports/exports
- Memory operations (load, store, grow)
- Let bindings, if expressions

## Target: WIT Type Support

### Strings

**Syntax:**
```lisp
;; String literals
"hello world"

;; String operations
(string.concat "hello" " " "world")  ;; -> "hello world"
(string.length "hello")               ;; -> 5
(string.slice "hello" 1 3)            ;; -> "el"
```

**Implementation:**
- UTF-8 encoding in linear memory
- Pointer + length representation (WIT canonical ABI)
- Built-in allocator for string operations

### Lists

**Syntax:**
```lisp
;; List literals
(list 1 2 3 4 5)

;; List operations
(list.length xs)           ;; -> s32
(list.get xs 0)            ;; -> element or trap
(list.push xs elem)        ;; -> new list
(list.map xs fn)           ;; -> new list
(list.filter xs pred)      ;; -> new list
(list.fold xs init fn)     ;; -> accumulated value
```

**Implementation:**
- Pointer + length in memory
- Element type determines layout
- Copy-on-write or immutable semantics

### Records

**Syntax:**
```lisp
;; Define a record type
(record point
  (x f64)
  (y f64))

;; Construct a record
(point (x 1.0) (y 2.0))

;; Field access
(point.x p)   ;; -> 1.0
(point.y p)   ;; -> 2.0

;; Functional update
(point.with-x p 3.0)  ;; -> new point with x=3.0
```

**Implementation:**
- Compile to WIT record type
- Memory layout follows canonical ABI
- Field accessors as generated functions

### Variants

**Syntax:**
```lisp
;; Define a variant type
(variant shape
  (circle (radius f64))
  (rectangle (width f64) (height f64))
  (point))

;; Construct variants
(shape.circle (radius 5.0))
(shape.rectangle (width 10.0) (height 20.0))
(shape.point)

;; Pattern match
(match s
  ((circle r) (f64.mul 3.14159 (f64.mul r r)))
  ((rectangle w h) (f64.mul w h))
  ((point) (f64.const 0.0)))
```

### Option and Result

**Syntax:**
```lisp
;; Option type
(some 42)
(none)

(match opt
  ((some x) x)
  ((none) (i32.const 0)))

;; Result type
(ok 42)
(err "something went wrong")

(match result
  ((ok x) x)
  ((err e) (panic e)))

;; Combinators
(option.map opt fn)
(option.unwrap-or opt default)
(result.map res fn)
(result.map-err res fn)
```

### Tuples

**Syntax:**
```lisp
;; Tuple construction
(tuple 1 "hello" 3.14)

;; Tuple access
(tuple.0 t)  ;; -> 1
(tuple.1 t)  ;; -> "hello"
(tuple.2 t)  ;; -> 3.14

;; Destructuring
(let (((tuple a b c) t))
  (string.concat b (to-string a)))
```

## Target: WIT Interface Support

### Importing Interfaces

```lisp
;; Import an entire interface
(import theater:simple/supervisor)

;; Import specific functions
(import theater:simple/supervisor
  spawn
  stop
  list-children)

;; Use imported functions (automatically namespaced)
(supervisor.spawn manifest none)
```

**Implementation:**
- Parse WIT files to understand available interfaces
- Generate import declarations in compiled WASM
- Type-check calls against WIT signatures

### Exporting Interfaces

```lisp
;; Declare we're implementing an interface
(implement theater:simple/actor)

;; Define the required functions
(fn init ((state (option (list u8))) (params (tuple string)))
    (result (option (list u8)) string)
  (ok (some (list))))

;; Export marks it as the interface implementation
(export init)
```

## Theater REPL Integration

### Core Principle: Everything is Eval

The REPL is fundamentally simple:

```lisp
(loop (print (eval (read))))
```

The entire REPL reduces to implementing one primitive: `eval`.

### How It Works

1. User sends Wisp source as a string
2. REPL actor parses the string
3. Compiles to WASM component (with caching)
4. Instantiates with environment imports linked
5. Executes and returns result as Wisp expression

### Host-Managed Environment

The REPL actor maintains an environment - a map from names to values. Compiled commands access it through imports:

```wit
interface repl-env {
  variant value {
    nil,
    bool(bool),
    i32(s32),
    i64(s64),
    f32(float32),
    f64(float64),
    str(string),
    list(list<value>),
    func(func-value),
    actor-id(string),
  }

  record func-value {
    source: string,
    captured: list<tuple<string, value>>,
  }

  get: func(name: string) -> option<value>;
  set: func(name: string, val: value);
  keys: func() -> list<string>;
  eval: func(code: string) -> result<value, string>;
}
```

Variable references compile to `repl-env.get` calls. Definitions compile to `repl-env.set` calls.

### Functions Are Strings

Key insight: **functions are stored as their source code**.

```lisp
> (fn square ((n s32)) s32 (i32.mul n n))
```

Stored in environment as:
```
"square" → Value::Func {
  source: "(fn square ((n s32)) s32 (i32.mul n n))",
  captured: {},
}
```

When `(square 5)` is called:
1. Look up "square" → get source string
2. Compile to WASM (or use cached)
3. Instantiate with env imports + argument binding
4. Execute → return result

This is homoiconic: **code is data, data is code**.

### Closures Capture Environment

When a function references free variables, they're captured at definition time:

```lisp
> (def x 10)
> (fn add-x ((y s32)) s32 (i32.add x y))
```

Stored as:
```
"add-x" → Value::Func {
  source: "(fn add-x ((y s32)) s32 (i32.add x y))",
  captured: {"x": Value::I32(10)},
}
```

Even if `x` changes, `add-x` remembers its captured value.

### Eval as a Primitive

Since functions are strings, we expose `eval` to user code:

```lisp
> (eval "(i32.add 1 2)")
3

> (def code "(i32.mul 6 7)")
> (eval code)
42

;; Dynamic code generation
> (eval (string.concat "(i32.add " (to-string x) " 5)"))
15
```

### Macros Are Just Functions

Functions that return code strings, combined with eval:

```lisp
> (fn unless ((cond string) (body string)) string
    (string.concat "(if " cond " nil " body ")"))

> (eval (unless "false" "(i32.const 42)"))
42
```

No special macro system needed - it emerges from eval + functions-as-strings.

### Session State

The REPL maintains state between commands:

```lisp
;; Define a value (persists in session)
> (def my-actor (spawn "echo/manifest.toml"))
(actor-id "abc123")

;; Use it later
> (request my-actor "ping")
"pong"

;; Define a function (persists in session)
> (fn greet ((name string)) string
    (string.concat "Hello, " name "!"))
greet

> (greet "Theater")
"Hello, Theater!"

;; Inspect the environment
> (env)
((x . 10) (my-actor . (actor-id "abc123")) (greet . (func ...)))
```

### Implicit Imports

The REPL automatically provides these imports to all commands:

```lisp
;; Always available in REPL context
repl-env      ;; environment access (get, set, eval)
theater:simple/supervisor
theater:simple/message-server-host
theater:simple/runtime
theater:simple/store
```

### REPL-Specific Forms

```lisp
;; Evaluate code string
(eval code-string)

;; Get function source
(get-source fn-name)

;; Show session bindings
(env)

;; Clear session
(clear)

;; Load a wisp file
(load "path/to/file.lisp")

;; Time an expression
(time expr)

;; Show type of expression
(type-of expr)
```

## Implementation Roadmap

### Phase 1: Strings
1. Add string token type to lexer
2. Add string type to type system
3. Implement memory allocation for strings
4. Add string operations (concat, length, slice)
5. Generate proper WIT for string exports/imports

### Phase 2: Lists
1. Add list literal syntax
2. Add parameterized list type
3. Implement memory layout for lists
4. Add list operations
5. Handle nested types (list of strings, etc.)

### Phase 3: Records and Variants
1. Add record/variant definition syntax
2. Implement type definitions
3. Generate constructors and accessors
4. Implement pattern matching
5. Memory layout per canonical ABI

### Phase 4: WIT Integration
1. Parse WIT files
2. Generate import stubs from WIT
3. Type-check against WIT signatures
4. Validate export compatibility
5. Handle complex WIT types (resources, handles)

### Phase 5: REPL Backend
1. In-memory compilation API
2. Component instantiation with imports
3. Result serialization
4. Session state management
5. Error recovery and reporting

## Example: Complete Theater Actor in Wisp

```lisp
;; echo-actor.lisp
;; A simple echo actor written entirely in Wisp

(import theater:simple/runtime)
(import theater:simple/message-server-client)

(record state
  (echo-count s32)
  (last-message (option string)))

(fn init ((existing (option (list u8))) (params (tuple string)))
    (result (option (list u8)) string)
  (runtime.log "Echo actor starting")
  (match existing
    ((some bytes) (ok (some bytes)))  ;; restore existing state
    ((none) (ok (some (serialize (state
                                   (echo-count 0)
                                   (last-message none))))))))

(fn handle-request ((state-bytes (list u8)) (msg (list u8)))
    (result (tuple (list u8) (list u8)) string)
  (let ((state (deserialize state-bytes))
        (message (deserialize msg)))
    (runtime.log (string.concat "Echoing: " message))
    (let ((new-state (state
                       (echo-count (i32.add (state.echo-count state) 1))
                       (last-message (some message)))))
      (ok (tuple (serialize new-state) (serialize message))))))

(export init)
(export handle-request)
```

## Open Questions

1. **Garbage Collection**: How to manage memory for complex types? Reference counting? Arena allocation?

2. **Generics**: Should Wisp support parameterized types beyond list<T>?

3. **Error Handling**: Should we have exceptions or stick with Result types?

4. **Macros**: With eval + functions-as-strings, do we need a separate macro system? The current design suggests macros emerge naturally.

5. **Debugging**: How to provide good error messages and debugging for REPL users?

6. **Compilation Caching**: Cache by source hash? Invalidate when environment changes?

7. **Closure Semantics**: Capture by value (current design) or by reference? Value is simpler and matches immutable style.

8. **Recursion in Eval**: How does a function call itself? The function name needs to be in scope during its own compilation.

## References

- [REPL Actor Design](/Users/colinrozzi/work/actor-registry/repl-actor/DESIGN.md)
- [WebAssembly Component Model](https://component-model.bytecodealliance.org/)
- [WIT Specification](https://component-model.bytecodealliance.org/design/wit.html)
- [Canonical ABI](https://component-model.bytecodealliance.org/design/canonical-abi.html)
