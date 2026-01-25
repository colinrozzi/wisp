# Phase 7: Macro Packages

**Status**: Design Phase
**Started**: 2025-01-12
**Updated**: 2025-01-15
**Prerequisites**: Phase 6 (WIT types) complete, Runtime resource support needed

## Objective

Enable macros to be implemented as separate WebAssembly packages that receive and return syntax trees via resource handles. This allows:
- Macros written in any language that compiles to WASM packages
- Sandboxed macro execution
- Potential for distributed/cached macro compilation

## Design Change: Resources Instead of Recursive Variants

**Important**: WIT MVP does not support recursive type definitions. The original design used:

```lisp
;; This WON'T work - WIT doesn't allow recursive types
(variant sexpr
  (sym string span)
  (lst (list sexpr) span))  ; ERROR: recursive reference
```

Instead, we use **resources** with a handle-based API. The host (compiler) owns all syntax tree data, and macro packages manipulate it through opaque handles.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    Wisp Compiler (Host)                 │
│                                                         │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐ │
│  │ Source Code │───▶│   Parser    │───▶│  SExpr AST  │ │
│  └─────────────┘    └─────────────┘    └──────┬──────┘ │
│                                               │        │
│                     ┌─────────────────────────┼────┐   │
│                     │    Resource Table       │    │   │
│                     │  ┌───┬───┬───┬───┬───┐ │    │   │
│                     │  │ 0 │ 1 │ 2 │ 3 │...│◀┘    │   │
│                     │  └─┬─┴─┬─┴───┴───┴───┘      │   │
│                     │    │   │                    │   │
│                     │    ▼   ▼                    │   │
│                     │  SExpr nodes (owned by host)│   │
│                     └─────────────────────────────┘   │
│                              │                        │
│                              │ i32 handles            │
│                              ▼                        │
│  ┌──────────────────────────────────────────────────┐ │
│  │              Macro Package (Guest)               │ │
│  │                                                  │ │
│  │  import syntax: interface { ... }                │ │
│  │  export expand: func(stx: borrow<sexpr>) ->      │ │
│  │                       result<sexpr, string>      │ │
│  └──────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
```

## WIT Interface Design

```wit
package wisp:syntax@0.1.0;

interface types {
  /// Source location information
  record span {
    line: u32,
    column: u32,
    length: u32,
  }

  /// The kind of an S-expression
  enum sexpr-kind {
    symbol,
    integer,
    float,
    string-lit,
    list,
  }
}

interface syntax {
  use types.{span, sexpr-kind};

  /// Opaque handle to an S-expression node (host-owned)
  resource sexpr;

  // === Inspection ===

  /// Get the kind of an S-expression
  get-kind: func(s: borrow<sexpr>) -> sexpr-kind;

  /// Get the span of an S-expression
  get-span: func(s: borrow<sexpr>) -> span;

  /// Get symbol value (traps if not a symbol)
  get-symbol: func(s: borrow<sexpr>) -> string;

  /// Get integer value (traps if not an integer)
  get-integer: func(s: borrow<sexpr>) -> s64;

  /// Get float value (traps if not a float)
  get-float: func(s: borrow<sexpr>) -> f64;

  /// Get string literal value (traps if not a string)
  get-string: func(s: borrow<sexpr>) -> string;

  /// Get list length (traps if not a list)
  list-len: func(s: borrow<sexpr>) -> u32;

  /// Get list item by index (traps if not a list or out of bounds)
  list-get: func(s: borrow<sexpr>, index: u32) -> sexpr;

  // === Construction ===

  /// Create a symbol
  make-symbol: func(name: string, span: span) -> sexpr;

  /// Create an integer literal
  make-integer: func(value: s64, span: span) -> sexpr;

  /// Create a float literal
  make-float: func(value: f64, span: span) -> sexpr;

  /// Create a string literal
  make-string: func(value: string, span: span) -> sexpr;

  /// Create a list from items
  make-list: func(items: list<sexpr>, span: span) -> sexpr;

  // === Utilities ===

  /// Deep clone an sexpr
  clone: func(s: borrow<sexpr>) -> sexpr;

  /// Pretty-print for debugging
  to-string: func(s: borrow<sexpr>) -> string;
}

interface macro {
  use syntax.{sexpr};

  /// Expand a macro invocation
  /// Input: the full macro call s-expression (e.g., (my-macro arg1 arg2))
  /// Output: the expanded form, or an error message
  expand: func(stx: borrow<sexpr>) -> result<sexpr, string>;
}

world macro-package {
  import syntax;
  export macro;
}
```

## Runtime Prerequisites

Before implementing macro packages, the runtime (`src/main.rs`) needs:

### 1. Resource Table Implementation

```rust
use std::collections::HashMap;

/// Manages sexpr handles for package interaction
pub struct ResourceTable {
    /// Maps handle (u32) -> SExpr
    entries: HashMap<u32, SExpr>,
    /// Next handle to allocate
    next_handle: u32,
}

impl ResourceTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            next_handle: 1, // 0 reserved for null/invalid
        }
    }

    /// Allocate a handle for an SExpr (takes ownership)
    pub fn insert(&mut self, sexpr: SExpr) -> u32 {
        let handle = self.next_handle;
        self.next_handle += 1;
        self.entries.insert(handle, sexpr);
        handle
    }

    /// Get an SExpr by handle (borrowed)
    pub fn get(&self, handle: u32) -> Option<&SExpr> {
        self.entries.get(&handle)
    }

    /// Get an SExpr by handle (mutable)
    pub fn get_mut(&mut self, handle: u32) -> Option<&mut SExpr> {
        self.entries.get_mut(&handle)
    }

    /// Remove and return an SExpr (for transferring ownership)
    pub fn remove(&mut self, handle: u32) -> Option<SExpr> {
        self.entries.remove(&handle)
    }

    /// Clear all entries (e.g., after macro expansion completes)
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}
```

### 2. Host State for Wasmtime

```rust
/// State passed to host functions
pub struct MacroHostState {
    pub resource_table: ResourceTable,
}

impl MacroHostState {
    pub fn new() -> Self {
        Self {
            resource_table: ResourceTable::new(),
        }
    }
}
```

### 3. Host Function Implementations

```rust
use wasmtime::*;

// Example: get-kind implementation
fn syntax_get_kind(
    mut caller: Caller<'_, MacroHostState>,
    handle: u32,
) -> Result<u32, Trap> {
    let state = caller.data();
    let sexpr = state.resource_table.get(handle)
        .ok_or_else(|| Trap::new("invalid sexpr handle"))?;

    Ok(match sexpr {
        SExpr::Sym(..) => 0,       // symbol
        SExpr::Int { .. } => 1,    // integer
        SExpr::Float { .. } => 2,  // float
        SExpr::Str(..) => 3,       // string
        SExpr::List(..) => 4,      // list
        _ => return Err(Trap::new("unexpected sexpr variant")),
    })
}

// Example: get-symbol implementation
fn syntax_get_symbol(
    mut caller: Caller<'_, MacroHostState>,
    handle: u32,
) -> Result<(u32, u32), Trap> {  // returns (ptr, len) for string
    let state = caller.data();
    let sexpr = state.resource_table.get(handle)
        .ok_or_else(|| Trap::new("invalid sexpr handle"))?;

    match sexpr {
        SExpr::Sym(name, _) => {
            // Allocate string in package memory and return ptr/len
            // (Details depend on canonical ABI string handling)
            todo!("implement string return")
        }
        _ => Err(Trap::new("get-symbol called on non-symbol")),
    }
}

// Example: make-list implementation
fn syntax_make_list(
    mut caller: Caller<'_, MacroHostState>,
    items_ptr: u32,
    items_len: u32,
    span_line: u32,
    span_col: u32,
    span_len: u32,
) -> Result<u32, Trap> {
    let state = caller.data_mut();

    // Read item handles from component memory
    let mut items = Vec::new();
    for i in 0..items_len {
        let item_handle = /* read from memory at items_ptr + i*4 */;
        let item = state.resource_table.get(item_handle)
            .ok_or_else(|| Trap::new("invalid item handle"))?
            .clone();
        items.push(item);
    }

    let span = Span {
        line: span_line,
        column: span_col,
        length: span_len,
        ..Default::default()
    };

    let new_sexpr = SExpr::List(items, span);
    let handle = state.resource_table.insert(new_sexpr);
    Ok(handle)
}
```

### 4. Component Linker Setup

```rust
fn setup_macro_linker(engine: &Engine) -> Result<Linker<MacroHostState>> {
    let mut linker = Linker::new(engine);

    // Inspection functions
    linker.func_wrap("wisp:syntax/syntax", "[resource-drop]sexpr", |_, _: u32| {
        // Handle cleanup when package drops a handle
        Ok(())
    })?;
    linker.func_wrap("wisp:syntax/syntax", "get-kind", syntax_get_kind)?;
    linker.func_wrap("wisp:syntax/syntax", "get-span", syntax_get_span)?;
    linker.func_wrap("wisp:syntax/syntax", "get-symbol", syntax_get_symbol)?;
    linker.func_wrap("wisp:syntax/syntax", "get-integer", syntax_get_integer)?;
    linker.func_wrap("wisp:syntax/syntax", "get-float", syntax_get_float)?;
    linker.func_wrap("wisp:syntax/syntax", "get-string", syntax_get_string)?;
    linker.func_wrap("wisp:syntax/syntax", "list-len", syntax_list_len)?;
    linker.func_wrap("wisp:syntax/syntax", "list-get", syntax_list_get)?;

    // Construction functions
    linker.func_wrap("wisp:syntax/syntax", "make-symbol", syntax_make_symbol)?;
    linker.func_wrap("wisp:syntax/syntax", "make-integer", syntax_make_integer)?;
    linker.func_wrap("wisp:syntax/syntax", "make-float", syntax_make_float)?;
    linker.func_wrap("wisp:syntax/syntax", "make-string", syntax_make_string)?;
    linker.func_wrap("wisp:syntax/syntax", "make-list", syntax_make_list)?;

    // Utilities
    linker.func_wrap("wisp:syntax/syntax", "clone", syntax_clone)?;
    linker.func_wrap("wisp:syntax/syntax", "to-string", syntax_to_string)?;

    Ok(linker)
}
```

### 5. Macro Expansion Integration

```rust
pub fn expand_macro(
    engine: &Engine,
    linker: &Linker<MacroHostState>,
    macro_component: &Component,
    input: SExpr,
) -> Result<SExpr> {
    // Create fresh state for this expansion
    let mut state = MacroHostState::new();

    // Insert input into resource table
    let input_handle = state.resource_table.insert(input);

    // Create store with state
    let mut store = Store::new(engine, state);

    // Instantiate component
    let instance = linker.instantiate(&mut store, macro_component)?;

    // Get the expand function
    let expand = instance
        .get_func(&mut store, "wisp:syntax/macro#expand")
        .ok_or_else(|| anyhow!("macro missing expand function"))?;

    // Call expand(input_handle) -> result<sexpr, string>
    let mut results = [Val::I32(0), Val::I32(0)]; // discriminant + payload
    expand.call(&mut store, &[Val::I32(input_handle as i32)], &mut results)?;

    // Parse result
    let discriminant = results[0].unwrap_i32();
    if discriminant == 0 {
        // Ok case: payload is output handle
        let output_handle = results[1].unwrap_i32() as u32;
        store.data_mut().resource_table.remove(output_handle)
            .ok_or_else(|| anyhow!("macro returned invalid handle"))
    } else {
        // Err case: payload is error string (ptr, len)
        let error_msg = /* read string from component memory */;
        Err(anyhow!("macro error: {}", error_msg))
    }
}
```

## Implementation Plan

### Phase 7.1: Runtime Resource Table (PREREQUISITE)
- [ ] Add `ResourceTable` struct to main.rs
- [ ] Add `MacroHostState` struct
- [ ] Test resource allocation/deallocation

### Phase 7.2: Host Function Implementation (PREREQUISITE)
- [ ] Implement inspection functions (get-kind, get-symbol, etc.)
- [ ] Implement construction functions (make-symbol, make-list, etc.)
- [ ] Handle canonical ABI string/list marshaling
- [ ] Handle `borrow<sexpr>` vs owned `sexpr` semantics

### Phase 7.3: Package Linker Setup (PREREQUISITE)
- [ ] Set up Wasmtime linker with syntax interface
- [ ] Handle resource drop callbacks
- [ ] Test with a hand-written WAT macro package

### Phase 7.4: Macro Loading in Compiler
- [ ] Parse `(import-macro name "path.wasm")` syntax
- [ ] Load and cache macro packages
- [ ] Build registry of macro name -> package

### Phase 7.5: Expansion Integration
- [ ] Call macro package's expand function
- [ ] Handle result (ok/err)
- [ ] Recursively expand results
- [ ] Report errors with source locations

### Phase 7.6: Wisp Macro Authoring
- [ ] Add imports for syntax interface to wisp
- [ ] Write example macro in wisp
- [ ] Test full round-trip: wisp -> package -> used as macro

## Example: A Simple Macro in Wisp

```lisp
;; double-macro.lisp
;; Doubles a numeric literal: (double 21) -> 42

(import syntax get-kind ((s (borrow sexpr))) s32)
(import syntax get-integer ((s (borrow sexpr))) s64)
(import syntax list-get ((s (borrow sexpr)) (i s32)) sexpr)
(import syntax list-len ((s (borrow sexpr))) s32)
(import syntax make-integer ((v s64) (line s32) (col s32) (len s32)) sexpr)
(import syntax get-span ((s (borrow sexpr))) span)

(record span (line s32) (column s32) (length s32))

(export (fn expand ((stx (borrow sexpr))) (result sexpr string)
  ;; Expect: (double <number>)
  (if (i32.ne (list-len stx) (i32.const 2))
    (err (result sexpr string) "double expects exactly one argument")
    (let (num-expr (list-get stx (i32.const 1)))
      (let (value (get-integer num-expr))
        (let (sp (get-span num-expr))
          (ok (result sexpr string)
            (make-integer
              (i64.mul value (i64.const 2))
              (span.line sp)
              (span.column sp)
              (span.length sp)))))))))
```

Usage:
```lisp
(import-macro double "macros/double.wasm")

(fn main () s32
  (double 21))  ; expands to 42
```

## Open Questions

1. **Hygiene**: How do we handle identifier hygiene? Options:
   - Extend span to include scope information
   - Separate scope tracking resource
   - Defer to Phase 8

2. **Error Locations**: How to get good source locations in macro errors?
   - Macros return span in error?
   - Track "expansion stack"?

3. **Caching**: Should compiled macro components be cached? Where?

4. **Dependencies**: Can macros import other interfaces? (e.g., file I/O for include)

## Success Criteria

Phase 7 is complete when:
- [ ] Runtime implements resource table and host functions
- [ ] Macro packages can be loaded and instantiated
- [ ] A macro written in wisp compiles to a package and works
- [ ] Error messages include source locations
- [ ] At least `double` and one list-manipulating macro work

## References

- [WIT Resources](https://component-model.bytecodealliance.org/design/wit.html#resources)
- [Wasmtime Component Model](https://docs.wasmtime.dev/api/wasmtime/component/index.html)
- [Canonical ABI](https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md)
