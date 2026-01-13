# Phase 6: WIT Types

**Status**: Design
**Started**: 2025-01-12
**Prerequisites**: Phase 5 (syntax-case) complete

## Objective

Add first-class support for WebAssembly Component Model types: records, variants, lists, options, results, and strings. This enables compound data structures, proper component interop, and lays the foundation for macro components.

## Motivation

Currently wisp only has WASM scalar types (s32, s64, f32, f64). To work with structured data, users must manually manage memory with i32.load/i32.store. This is:

- Error-prone (no type safety on memory layout)
- Tedious (manual offset calculations)
- Incompatible with the component model (can't export/import rich types)

The component model already defines rich types. By supporting them natively, wisp gets:

1. **Compound data** - records, variants, lists without manual memory management
2. **Type safety** - the compiler enforces correct usage
3. **Component interop** - export/import functions with rich signatures
4. **Foundation for macros** - S-expressions become a natural WIT variant

## Design

### Type Declarations

Types are declared at the top level:

```lisp
;; Records - product types with named fields
(record point
  (x s32)
  (y s32))

(record span
  (line s32)
  (column s32)
  (length s32))

;; Variants - sum types with named cases
(variant shape
  (circle s32)           ; radius
  (rectangle s32 s32)    ; width, height
  (point))               ; no payload

;; Recursive types
(variant sexpr
  (symbol string)
  (integer s64)
  (float f64)
  (list (list sexpr)))   ; list of sexpr
```

### Constructing Values

**Records** - use the type name as a constructor:

```lisp
(point 10 20)              ; point with x=10, y=20
(span 1 5 10)              ; span with line=1, col=5, len=10
```

**Variants** - use the case name as a constructor:

```lisp
(circle 5)                 ; a circle with radius 5
(rectangle 10 20)          ; a rectangle 10x20
(point)                    ; the point case (no payload)

(symbol "hello")           ; sexpr: a symbol
(integer 42)               ; sexpr: an integer
(list (list (integer 1) (integer 2)))  ; sexpr: a list
```

### Destructuring / Pattern Matching

**Records** - bind fields by name:

```lisp
(fn distance ((p point)) f64
  (let-record p (x y)
    (f64.sqrt (f64.add
      (f64.mul (f64 x) (f64 x))
      (f64.mul (f64 y) (f64 y))))))
```

**Variants** - match on cases:

```lisp
(fn area ((s shape)) s32
  (match s
    ((circle r) (i32.mul (i32.mul r r) (i32.const 3)))
    ((rectangle w h) (i32.mul w h))
    ((point) (i32.const 0))))

(fn sexpr-to-string ((e sexpr)) string
  (match e
    ((symbol s) s)
    ((integer n) (i64-to-string n))
    ((float f) (f64-to-string f))
    ((list items) (format-list items))))
```

### Built-in Parameterized Types

These mirror WIT's built-in types:

```lisp
;; Lists - dynamic length sequences
(list s32)                 ; list of s32
(list point)               ; list of points
(list (list s32))          ; nested lists

;; Options - nullable values
(option s32)               ; maybe an s32
(option point)             ; maybe a point

;; Results - success or error
(result s32 string)        ; s32 on success, string on error
(result point span)        ; point on success, span on error

;; Strings - UTF-8 text
string                     ; built-in string type
```

**Constructing built-ins:**

```lisp
;; Lists
(list-new)                        ; empty list
(list-push my-list value)         ; add to list
(list-get my-list index)          ; get by index
(list-len my-list)                ; length

;; Options
(some 42)                         ; option with value
(none)                            ; no value

;; Results
(ok 42)                           ; success
(err "something went wrong")      ; failure

;; Strings
"hello world"                     ; string literal
(string-len s)                    ; length
(string-concat a b)               ; concatenation
```

**Matching built-ins:**

```lisp
(fn unwrap-or ((o (option s32)) (default s32)) s32
  (match o
    ((some x) x)
    ((none) default)))

(fn handle-result ((r (result s32 string))) s32
  (match r
    ((ok x) x)
    ((err msg) (i32.const -1))))
```

### Function Signatures with Rich Types

```lisp
(fn make-point ((x s32) (y s32)) point
  (point x y))

(fn parse-number ((s string)) (result s64 string)
  ...)

(fn map-list ((items (list s32)) (f (func (s32) s32))) (list s32)
  ...)

;; Exports work naturally
(export make-point)
(export parse-number)
```

### WIT Generation

The compiler generates corresponding WIT:

```lisp
;; wisp source
(record point (x s32) (y s32))
(variant shape (circle s32) (rectangle s32 s32))
(export (fn area ((s shape)) s32 ...))
```

```wit
// generated WIT
package example:wisp;

world component {
  record point {
    x: s32,
    y: s32,
  }

  variant shape {
    circle(s32),
    rectangle(tuple<s32, s32>),
  }

  export area: func(s: shape) -> s32;
}
```

### Memory Representation

The compiler handles memory layout automatically:

- **Records**: Fields laid out sequentially, properly aligned
- **Variants**: Discriminant byte/word + payload
- **Lists**: Length + pointer to data (or component model representation)
- **Strings**: UTF-8 bytes with length

Users don't manage this directly. The compiler generates appropriate load/store sequences.

### Interaction with Macros

Macros still operate on S-expression syntax at compile time. The new types exist at runtime:

```lisp
;; Macro that generates variant construction
(define-syntax make-some
  (syntax-rules ()
    ((make-some x) (some x))))

;; Works fine - macro expands to (some 42), compiler handles the rest
(make-some 42)
```

For macro components (future Phase 7), `sexpr` becomes a real WIT variant that can be passed across component boundaries.

## Implementation Plan

### Phase 6.1: Records
- [ ] Add `record` declaration parsing
- [ ] Add record type to Type enum
- [ ] Implement record construction
- [ ] Implement `let-record` destructuring
- [ ] Generate correct WAT (memory layout, load/store)
- [ ] Generate WIT record declarations
- [ ] Test with simple point/span examples

### Phase 6.2: Variants
- [ ] Add `variant` declaration parsing
- [ ] Add variant type to Type enum
- [ ] Implement variant case construction
- [ ] Implement `match` expression
- [ ] Generate correct WAT (discriminant + payload)
- [ ] Generate WIT variant declarations
- [ ] Test with shape example

### Phase 6.3: Built-in Parameterized Types
- [ ] Add `list<T>` type support
- [ ] Add `option<T>` type support
- [ ] Add `result<T, E>` type support
- [ ] Implement constructors (some, none, ok, err)
- [ ] Implement list operations (list-new, list-push, etc.)
- [ ] Pattern matching for options/results

### Phase 6.4: Strings
- [ ] Add `string` as built-in type
- [ ] String literals create string values (not raw memory)
- [ ] Basic string operations
- [ ] Proper WIT string handling

### Phase 6.5: Component ABI
- [ ] Implement canonical ABI lowering for records
- [ ] Implement canonical ABI lowering for variants
- [ ] Implement canonical ABI for lists/strings
- [ ] Test cross-component calls with rich types

### Phase 6.6: Recursive Types
- [ ] Support recursive type definitions (like sexpr)
- [ ] Handle recursive memory layout
- [ ] Test with actual sexpr variant

## Examples

### Before (manual memory management)

```lisp
;; Manually managing a "point" as two s32s at a memory location
(fn make-point ((x s32) (y s32)) s32
  (let (ptr (alloc (i32.const 8)))
    (let (_ (i32.store ptr x))
      (let (_ (i32.store (i32.add ptr (i32.const 4)) y))
        ptr))))

(fn point-x ((p s32)) s32
  (i32.load p))

(fn point-y ((p s32)) s32
  (i32.load (i32.add p (i32.const 4))))
```

### After (WIT types)

```lisp
(record point (x s32) (y s32))

(fn make-point ((x s32) (y s32)) point
  (point x y))

(fn point-x ((p point)) s32
  (let-record p (x y) x))

(fn point-y ((p point)) s32
  (let-record p (x y) y))
```

### S-expression Type (enables macro components)

```lisp
(record span
  (line s32)
  (column s32)
  (length s32))

(variant sexpr
  (sym string span)
  (int s64 span)
  (flt f64 span)
  (lst (list sexpr) span))

;; Now a macro component can receive/return sexpr values
(fn expand ((input sexpr)) (result sexpr string)
  (match input
    ((lst items span)
     (ok (transform-list items span)))
    (_ (err "expected list"))))
```

## Success Criteria

Phase 6 is complete when:
- [ ] Records can be declared, constructed, and destructured
- [ ] Variants can be declared, constructed, and matched
- [ ] `option` and `result` types work
- [ ] `list` type works with basic operations
- [ ] `string` type works
- [ ] Exported functions can have rich type signatures
- [ ] Generated WIT correctly reflects the types
- [ ] Cross-component calls work with these types

## Future Extensions

1. **Generics**: User-defined parameterized types
2. **Type aliases**: `(type-alias coordinate point)`
3. **Methods**: Functions associated with types
4. **Derive**: Auto-generate common operations (equality, debug, etc.)

## Relationship to Future Phases

**Phase 7 (Macro Components)** depends on this:
- The `sexpr` variant type is how macros receive/return syntax
- The `result` type is how macros report errors
- Lists and strings are essential for syntax manipulation

With WIT types in place, macro components become natural - no hacks needed.

## References

- [WIT Specification](https://component-model.bytecodealliance.org/design/wit.html)
- [Canonical ABI](https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md)
- [wit-bindgen](https://github.com/bytecodealliance/wit-bindgen)
