# Wisp + Theater Integration

This document describes the path to making Wisp a first-class language for writing Theater actors and the target language for the Theater REPL.

## Goal

Wisp becomes a fully-featured language that can:
1. Write complete Theater actors (replacing Rust for simple cases)
2. Serve as the command language for an interactive Theater REPL
3. Support all WIT types needed for Theater interfaces

## Current State

Wisp already supports most of what's needed for Theater actors:

### Core Types (Implemented)
- Scalar types: `s32`, `s64`, `f32`, `f64`, `u8`
- Strings: literals `"hello"` and operations
- Lists: `(list-new type)`, `(list-push lst elem)`, `(list-get lst idx)`, `(list-len lst)`
- Options: `(some value)`, `(none type)`, pattern matching
- Results: `(ok value)`, `(err value)`, pattern matching
- Tuples: `(tuple a b c)`, construction and access
- Records: `(record name (field type) ...)`, construction and field access
- Variants: `(variant name (case payload) ...)`, pattern matching

### Theater Integration (Implemented)
- Import theater interfaces: `(import theater:simple/runtime log ((msg string)) s32)`
- Pack/CGRF encoding for complex types
- External interface declarations in world config

### String Operations (Implemented)
```lisp
"hello world"                        ; String literal
(string-len s)                       ; Get length
(string-ref s idx)                   ; Get byte at index
(substring s start end)              ; Slice
(string-append s1 s2)                ; Concatenate
(string=? s1 s2)                     ; Equality check
```

### List Operations (Implemented)
```lisp
(list-new u8)                        ; Create empty list of u8
(list-push lst elem)                 ; Add element, returns new list
(list-get lst idx)                   ; Get element at index
(list-len lst)                       ; Get length
```

### Option and Result (Implemented)
```lisp
; Option
(some 42)
(none s32)

(match opt
  ((some x) x)
  ((none) 0))

; Result
(ok 42)
(err "something went wrong")

(match result
  ((ok x) x)
  ((err e) (handle-error e)))
```

### Records and Variants (Implemented)
```lisp
; Define a record type
(record point
  (x f64)
  (y f64))

; Construct a record
(point (x 1.0) (y 2.0))

; Field access
(point.x p)

; Define a variant type
(variant shape
  (circle (radius f64))
  (rectangle (width f64) (height f64))
  (point))

; Construct variants
(shape.circle (radius 5.0))

; Pattern match
(match s
  ((circle r) (f64.mul 3.14159 (f64.mul r r)))
  ((rectangle w h) (f64.mul w h))
  ((point) 0.0))
```

### Theater Import Example (Working)
```lisp
; Import log from theater runtime
(import theater:simple/runtime log ((msg string)) s32)

; Function that logs and returns a value
(fn hello () s32
  (let (_ (log "Hello from wisp!"))
    42))

(export hello)
```

## What's Still Needed

### Phase 1: Complete Theater Actor Support
1. **State serialization helpers** - convenient way to serialize/deserialize actor state
2. **Export signature syntax** - cleaner way to declare complex export types
3. **SHA-256 or import mechanism** - for content-addressed stores
4. **Byte array literals** - `#[1 2 3 4]` syntax for `list<u8>`

### Phase 2: REPL Backend
1. In-memory compilation API
2. Component instantiation with imports
3. Result serialization
4. Session state management
5. Error recovery and reporting

## Example: Content Store Actor in Wisp

This is close to what's possible today:

```lisp
; Content Store Actor
; A content-addressed store using SHA-256 hashes

; Import theater interfaces
(import theater:simple/runtime log ((msg string)) s32)
(import theater:simple/store new () (result string string))
(import theater:simple/store store
  ((store-id string) (content (list u8)))
  (result string string))
(import theater:simple/store get
  ((store-id string) (content-ref string))
  (result (list u8) string))
(import theater:simple/store label
  ((store-id string) (label string) (content-ref string))
  (result (tuple) string))
(import theater:simple/store get-by-label
  ((store-id string) (label string))
  (result (option string) string))

; State is serialized as bytes - store-id length + store-id bytes
(fn serialize-state ((store-id string)) (list u8)
  ; Convert string to bytes with length prefix
  (let ((len (string-len store-id)))
    (let ((result (list-new u8)))
      ; Write length as 4 bytes (little endian)
      (let ((result (list-push result (i32.and len 255))))
        (let ((result (list-push result (i32.and (i32.shr_u len 8) 255))))
          (let ((result (list-push result (i32.and (i32.shr_u len 16) 255))))
            (let ((result (list-push result (i32.shr_u len 24))))
              ; Write string bytes
              (write-string-bytes result store-id 0 len))))))))

(fn write-string-bytes ((result (list u8)) (s string) (idx s32) (len s32)) (list u8)
  (if (i32.ge_s idx len)
    result
    (write-string-bytes
      (list-push result (string-ref s idx))
      s
      (i32.add idx 1)
      len)))

; SHA-256 hash (would need to import or implement)
; For now, placeholder that uses the store's content-ref
(fn sha256-hex ((data (list u8))) string
  ; TODO: implement SHA-256 or import from host
  "placeholder-hash")

; Actor init
(fn init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
  (let (_ (log "Content store actor starting..."))
    (match state
      ((some bytes)
        ; Resuming with existing state
        (ok (tuple (some bytes))))
      ((none)
        ; Create new store
        (match (new)
          ((ok store-id)
            (let (_ (log (string-append "Created store: " store-id)))
              (ok (tuple (some (serialize-state store-id))))))
          ((err e) (err e)))))))

; Put content - returns hash
(fn put ((state (list u8)) (data (list u8)))
    (result (tuple (list u8) string) string)
  (let ((store-id (deserialize-state state)))
    (let ((hash (sha256-hex data)))
      (match (store store-id data)
        ((ok content-ref)
          (match (label store-id hash content-ref)
            ((ok _) (ok (tuple state hash)))
            ((err e) (err e))))
        ((err e) (err e))))))

; Get content by hash
(fn get-content ((state (list u8)) (hash string))
    (result (tuple (list u8) (option (list u8))) string)
  (let ((store-id (deserialize-state state)))
    (match (get-by-label store-id hash)
      ((ok maybe-ref)
        (match maybe-ref
          ((some content-ref)
            (match (get store-id content-ref)
              ((ok data) (ok (tuple state (some data))))
              ((err e) (ok (tuple state (none (list u8)))))))
          ((none) (ok (tuple state (none (list u8)))))))
      ((err e) (err e)))))

; Check if hash exists
(fn has ((state (list u8)) (hash string))
    (result (tuple (list u8) s32) string)
  (let ((store-id (deserialize-state state)))
    (match (get-by-label store-id hash)
      ((ok maybe-ref)
        (match maybe-ref
          ((some _) (ok (tuple state 1)))
          ((none) (ok (tuple state 0)))))
      ((err _) (ok (tuple state 0))))))

(export init)
(export put)
(export get-content)
(export has)
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
"square" -> Value::Func {
  source: "(fn square ((n s32)) s32 (i32.mul n n))",
  captured: {},
}
```

When `(square 5)` is called:
1. Look up "square" -> get source string
2. Compile to WASM (or use cached)
3. Instantiate with env imports + argument binding
4. Execute -> return result

This is homoiconic: **code is data, data is code**.

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
    (string-append "Hello, " name "!"))
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

## Open Questions

1. **SHA-256**: Import from host or implement in Wisp? Host import is simpler.

2. **State Serialization**: Should we have automatic serialization for records, or require manual byte manipulation?

3. **Garbage Collection**: How to manage memory for complex types? Reference counting? Arena allocation?

4. **Error Handling**: Current Result types work well. Should we add `?` operator for propagation?

5. **Debugging**: How to provide good error messages and debugging for REPL users?

## References

- [Pack crate](../../../pack) - Runtime for CGRF/Graph ABI
- [Theater](../../../theater) - Actor runtime
- [WebAssembly Component Model](https://component-model.bytecodealliance.org/)
- [WIT Specification](https://component-model.bytecodealliance.org/design/wit.html)
