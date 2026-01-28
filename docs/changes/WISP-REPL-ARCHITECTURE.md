# Wisp REPL Architecture

## Overview

A Theater-native REPL for Wisp that compiles S-expressions to WebAssembly components and evaluates them as ephemeral actors. The REPL uses a "compile-and-run" model where each expression is compiled along with all accumulated definitions into a self-contained component.

## Status

- [x] Design finalized
- [x] Wisp compiler updated for REPL mode
- [x] Theater wrapper component implemented
- [x] REPL Actor implemented with compile-and-run loop
- [x] Component composition (wrapper + compiled wisp)
- [x] End-to-end testing
- [x] **Component loading with wit+ imports**
- [ ] Theater spawn/await integration (future)
- [ ] State persistence via Theater event chain (future)
- [ ] Remote Theater connections (future)

## Quick Start

```bash
# Run the REPL
cargo run -p test-runtime -- --repl
```

**Example session:**
```
wisp> (i32.add (i32.const 40) (i32.const 2))
42
wisp> (define x 10)
defined x = 10
wisp> (define y 5)
defined y = 5
wisp> (i32.add x y)
15
wisp> (fn square ((n s32)) s32 (i32.mul n n))
defined function square
wisp> (square (i32.const 7))
49
wisp> (fn factorial ((n s32)) s32 (if (i32.le_s n (i32.const 1)) (i32.const 1) (i32.mul n (factorial (i32.sub n (i32.const 1))))))
defined function factorial
wisp> (factorial (i32.const 5))
120
```

## Component Loading

Load WASM components and call their exported functions from REPL expressions.

**Syntax:**
```lisp
; Import an interface from a WASM component
(import <interface> from "<path.wasm>")

; Import from host runtime
(import <interface> from host)
```

**Example session:**
```
wisp> (import colin:math/ops from "examples/math-lib-raw.wasm")
loaded interface colin:math/ops from examples/math-lib-raw.wasm
  exports: add, multiply, square

wisp> (list)
imports: 1 loaded
  colin:math/ops from examples/math-lib-raw.wasm (3 exports)
    add(s32, s32) -> s32
    multiply(s32, s32) -> s32
    square(s32) -> s32

wisp> (add (i32.const 3) (i32.const 4))
7

wisp> (multiply (i32.const 6) (i32.const 7))
42

wisp> (square (i32.const 5))
25

wisp> (define x 8)
wisp> (add (square x) (i32.const 1))
65
```

**How it works:**
1. Parse the import syntax and load the WASM component
2. Extract function signatures using Wasmtime's type introspection
3. When an expression uses an imported function, generate a stub for the compiler
4. Post-process the generated WAT to inject proper import declarations
5. Link the compiled expression to the imported component at instantiation

### Host Imports

Built-in host functions for debugging and development:

```
wisp> (import wisp:repl/debug from host)
loaded interface wisp:repl/debug from host
  exports: print-i32, print-i64, print-f32, print-f64

wisp> (print-i32 (i32.const 42))
[debug] 42
42

wisp> (import colin:math/ops from "examples/math-lib-raw.wasm")
wisp> (print-i32 (add (i32.const 3) (i32.const 4)))
[debug] 7
7
```

The debug functions return the value they print, enabling easy chaining.

**Current limitations:**
- Only basic WASM types (i32, i64, f32, f64) are supported
- Complex types (strings, lists, records) need CGRF encoding (not yet integrated)

## Key Insight

Wisp is a compiled language that produces WebAssembly components. Rather than building an interpreter or complex actor communication for variable/function resolution, we embrace compilation:

- **Every eval compiles a fresh component** containing all definitions + the expression
- **Variables are inlined** as constants at compile time
- **Functions are regular WASM functions** that call each other directly
- **No runtime environment lookups** - everything is baked in

This is similar to how Swift REPL, Rust's evcxr, or Julia work - compile-and-run loops that feel interactive because compilation is fast.

## Architecture

### Two Modes

The REPL supports two modes of operation:

1. **Eval Mode**: Compile expression, wrap with Theater interface, spawn, get result
2. **Build Mode**: Accumulate definitions, export as pure component (no Theater wrapping)

This separation keeps the Wisp compiler Theater-agnostic while enabling interactive development.

### Eval Mode Flow

```
                User Input: (i32.add x (double y))
                              │
                              ▼
                    ┌─────────────────┐
                    │   REPL Actor    │
                    │                 │
                    │  state:         │
                    │    bindings:    │
                    │      x → 42     │
                    │      y → 10     │
                    │    functions:   │
                    │      double → ...│
                    │                 │
                    │  1. Parse       │
                    │  2. Compile     │─────────┐
                    │  3. Compose     │         │
                    │  4. Spawn       │         │
                    │  5. Await       │         │
                    └────────▲────────┘         │
                             │                  ▼
                             │    ┌──────────────────────────────────┐
                             │    │        Composed Actor            │
                             │    │                                  │
                             │    │  ┌────────────┐  ┌────────────┐ │
                      result │    │  │  Theater   │  │  Compiled  │ │
                             │    │  │  Wrapper   │  │  Wisp      │ │
                             │    │  │            │  │            │ │
                             │    │  │ init(): ───┼─►│ eval() ────┼─┤
                             │    │  │   result = │  │  returns 62│ │
                             └────┼──│   eval()   │  │            │ │
                                  │  │   reply(r) │  │ (pure wasm)│ │
                                  │  └────────────┘  └────────────┘ │
                                  └──────────────────────────────────┘
```

### Build Mode Flow

```
wisp> (fn double ((n s32)) s32 ...)    ; accumulate definition
wisp> (fn quadruple ((n s32)) s32 ...) ; accumulate definition
wisp> (compile "math" (double quadruple))
                              │
                              ▼
                    ┌─────────────────┐
                    │   REPL Actor    │
                    │                 │
                    │  Compile all    │
                    │  requested fns  │
                    │  (NO wrapper)   │
                    └────────┬────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │   math.wasm     │
                    │                 │
                    │  exports:       │
                    │    double(s32)  │
                    │    quadruple(s32)│
                    │                 │
                    │  (pure component│
                    │   no Theater)   │
                    └─────────────────┘
```

### Component Separation

The key insight is that the **Theater wrapper is eval-specific**:

- **Wisp compiler**: Always produces pure components with exported functions
- **Theater wrapper**: Generic component (written once) that calls eval and replies
- **Composition**: Only happens for eval, not for build/export

This means components built in the REPL can be deployed anywhere - they have no Theater dependencies unless you explicitly add them.

## Theater Wrapper Component

A generic, reusable component that bridges pure Wisp components to Theater's actor model.

**What it does:**
- Imports `eval` from the compiled Wisp component
- Imports `reply` from Theater runtime
- Exports `init` (Theater actor interface)
- On init: calls eval, sends result via reply

**Pseudo-WIT:**
```wit
// What the wrapper imports from Theater
interface theater-runtime {
    reply: func(result: value);
}

// What the wrapper imports from compiled Wisp
interface wisp-eval {
    eval: func() -> value;
}

// What the wrapper exports (Theater actor interface)
interface actor {
    init: func(state: option<list<u8>>) -> result<tuple<option<list<u8>>>, string>;
}
```

**Implementation (conceptual):**
```rust
#[import(wit = "wisp:eval/eval")]
fn eval() -> Value;

#[import(wit = "theater:runtime/reply")]
fn reply(result: Value);

#[export(wit = "theater:actor/init")]
fn init(state: Option<Vec<u8>>) -> Result<(Option<Vec<u8>>,), String> {
    let result = eval();
    reply(result);
    Ok((state,))
}
```

The wrapper is written once and composed with each compiled expression.

## Compiled Output

Given REPL state:
```
bindings: {x: 42, y: 10}
functions: {double: (fn double ((n s32)) s32 (i32.mul n (i32.const 2)))}
```

And expression: `(i32.add x (double y))`

The compiled component contains:
```wat
(module
  ;; All accumulated function definitions
  (func $double (param $n i32) (result i32)
    (i32.mul (local.get $n) (i32.const 2)))

  ;; The expression to evaluate, with variables inlined
  (func (export "eval") (result i32)
    (i32.add
      (i32.const 42)                    ;; x inlined
      (call $double (i32.const 10))))   ;; y inlined
)
```

## REPL Actor

### State

```rust
struct ReplState {
    // Variable bindings: name -> value
    bindings: HashMap<String, Value>,

    // Function definitions: name -> AST
    functions: HashMap<String, FunctionDef>,
}
```

### Message Protocol

```
// Evaluate an expression
{eval: "(i32.add x (double y))"}
  → {ok: 62}
  → {err: "compile error: unknown variable 'z'"}

// Define a variable
{define: {name: "x", value: 42}}
  → {ok: ()}

// Define a function
{defn: "(fn double ((n s32)) s32 (i32.mul n (i32.const 2)))"}
  → {ok: ()}

// List current definitions
{list: ()}
  → {bindings: [["x", 42], ["y", 10]],
     functions: ["double", "quadruple"]}

// Export current state as a compiled component
{compile: {name: "math", exports: ["double", "quadruple"]}}
  → {ok: <wasm-bytes>}

// Clear all definitions
{clear: ()}
  → {ok: ()}
```

### Eval Flow

1. **Parse** input string to AST
2. **Validate** - check all referenced variables/functions exist
3. **Compile** - generate WAT with:
   - All function definitions from state
   - Variables inlined as constants
   - Expression wrapped in `(export "eval")`
4. **Encode** - WAT → WASM component
5. **Spawn** - start as ephemeral actor
6. **Await** - wait for result
7. **Return** - send result to caller

### Special Forms

The REPL actor handles these specially (not compiled):

- `(define name value)` - evaluate value, store in bindings
- `(fn name ...)` - parse and store in functions
- `(list)` - show current definitions
- `(clear)` - reset state
- `(compile name)` - export as component

Everything else goes through compile-and-run.

## Wisp Compiler Changes

### REPL Mode

Add a REPL compilation mode that:

1. **Accepts a context** - existing bindings and function definitions
2. **Inlines variables** - replaces variable references with constant values
3. **Includes all functions** - even if not directly called (they might call each other)
4. **Wraps expression** - in an exported `eval` function

```rust
fn compile_repl(
    expr: &Expr,
    bindings: &HashMap<String, Value>,
    functions: &HashMap<String, FunctionDef>,
) -> Result<Vec<u8>, CompileError>
```

### Variable Resolution

During compilation, when encountering an identifier:

1. Check if it's a local (function parameter, let binding) → use `local.get`
2. Check if it's a function name → will be a `call`
3. Check if it's in bindings → inline as `i32.const`/`f64.const`/etc.
4. Otherwise → compile error "unknown variable"

### Type Handling

Variables in bindings need type information:

```rust
enum Value {
    S32(i32),
    S64(i64),
    F32(f32),
    F64(f64),
}
```

When inlining, emit the appropriate const instruction.

## State Persistence

The REPL actor's state (bindings + functions) persists via Theater's event chain:

- Each `define`/`fn` is recorded as an event
- On actor restart, state is reconstructed from event history
- User's session survives REPL restarts

## Example Session

```
wisp> (define x 42)
ok

wisp> (define y 10)
ok

wisp> (fn double ((n s32)) s32 (i32.mul n (i32.const 2)))
ok

wisp> (fn quadruple ((n s32)) s32 (double (double n)))
ok

wisp> (double x)
84

wisp> (quadruple y)
40

wisp> (i32.add (quadruple x) y)
178

wisp> (list)
bindings: x=42, y=10
functions: double, quadruple

wisp> (compile "math" (double quadruple))
compiled: math.wasm (exports: double, quadruple)
```

## Implementation Plan

### Phase 1: Wisp Compiler REPL Mode ✓
- [x] Add `compile_repl()` function
- [x] Implement variable inlining
- [x] Implement context (bindings + functions) passing
- [x] Generate component with `eval` export
- [x] Test compilation with context

### Phase 2: Theater Wrapper Component ✓
- [x] Create wrapper component project (`examples/eval-wrapper.wisp`)
- [x] Import Theater reply primitive
- [x] Import `eval` from composed component
- [x] Implement `init` that calls eval and replies
- [x] Test wrapper in isolation

### Phase 3: REPL Actor Core ✓
- [x] Create `test-runtime` project (`crates/test-runtime`)
- [x] Implement state management (bindings, functions)
- [x] Implement `define` and `fn` handling
- [x] Implement component composition (wrapper + compiled)
- [x] Implement eval flow (compile → assemble → execute)

### Phase 4: Component Loading ✓
- [x] Parse wit+ style import syntax: `(import <interface> from <source>)`
- [x] Handle `host` source - register as host-provided import
- [x] Handle `"file.wasm"` source - load component, extract exports
- [x] Extract function signatures via Wasmtime type introspection
- [x] Track loaded interfaces in REPL state
- [x] Link compiled expressions to loaded dependencies
- [ ] Integrate with Composite's Graph ABI for complex types

### Phase 5: Theater Integration (Future)
- [ ] Add spawn capability to REPL actor
- [ ] Implement await for eval result
- [ ] Wire up message protocol
- [ ] Actor listing and inspection

### Phase 6: Build Mode (Future)
- [ ] Implement `compile` command (unwrapped export)
- [ ] Support selecting which functions to export
- [ ] Test produced components work standalone

### Phase 7: Distributed Theater (Future)
- [ ] Connect to remote Theater runtimes
- [ ] Cross-runtime actor references
- [ ] Remote actor messaging

### Phase 8: Persistence & Polish ✓ (Partial)
- [ ] Implement state serialization
- [ ] Integrate with Theater event chain
- [ ] Test restart recovery
- [x] `list` command
- [x] `clear` command
- [x] Error messages and UX

## Open Questions

1. **Type annotations for define**: Should `(define x 42)` infer s32, or require `(define x 42s32)`?
   - Leaning toward: infer from literal, allow suffix for explicit

2. **Mutable bindings**: Should `(define x 10)` after `(define x 42)` update or error?
   - Leaning toward: update (shadow previous)

3. **Function redefinition**: Same question for functions
   - Leaning toward: allow redefinition

4. **Memory/tables**: If we need memory operations, how does that work across evals?
   - Each eval is fresh, no persistent memory
   - Could add `(define-memory ...)` that gets included in compilation

5. **Package resolution**: How do we locate packages for imports?
   - Direct file paths: `(import foo:bar/baz from "path/to/component.wasm")`
   - Convention-based: `foo/bar.wasm` for `foo:bar/interface`
   - Registry: Future package registry support

## Test Runtime

The REPL is implemented in `crates/test-runtime`. It provides multiple modes:

### Usage

```bash
# Basic: Run a function from a WASM file
cargo run -p test-runtime -- <wasm> <func> [arg]

# Compile: Source → WAT → WASM (validates output)
cargo run -p test-runtime -- --compile "(fn foo () s32 (i32.const 42))"

# Compile and run: Compile, then execute a function
cargo run -p test-runtime -- --compile-run "(export (fn foo () s32 (i32.const 42)))" foo

# With arguments:
cargo run -p test-runtime -- --compile-run "(export (fn factorial ((n s32)) s32 ...))" factorial 5

# REPL: Interactive mode
cargo run -p test-runtime -- --repl
```

### REPL Commands

| Command | Description |
|---------|-------------|
| `(define x 42)` | Define a variable (inlined into expressions) |
| `(fn name ...)` | Define a function (included in compilation) |
| `(import iface from src)` | Load a component and import its functions |
| `(list)` | Show bindings, functions, and imports with signatures |
| `(clear)` | Clear all bindings, functions, and imports |
| `quit` / `exit` | Exit the REPL |
| *expression* | Compile and evaluate, print result |

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    REPL Pipeline                        │
│                                                         │
│  1. User enters expression                             │
│           │                                             │
│           ▼                                             │
│  2. Generate source with inlined variables             │
│     and accumulated function definitions               │
│           │                                             │
│           ▼                                             │
│  3. Self-hosted compiler (wisp-compiler.wasm)          │
│     compiles source → WAT string                       │
│           │                                             │
│           ▼                                             │
│  4. wat crate assembles WAT → WASM bytes               │
│           │                                             │
│           ▼                                             │
│  5. Wasmtime executes WASM, returns result             │
│           │                                             │
│           ▼                                             │
│  6. Print result                                        │
└─────────────────────────────────────────────────────────┘
```

---

## Future Vision: Theater Shell

The REPL is designed to evolve into a **Theater Shell** - a full interactive environment for actor development and system management.

### The Big Picture

More than just a language REPL, the Theater Shell becomes the command center for:
- **Developing** actors (write Wisp code, compile to WASM)
- **Deploying** actors (spawn them into Theater)
- **Monitoring** actors (see what's running, inspect state)
- **Interacting** with actors (send messages, get responses)
- **Managing** distributed systems (connect to remote Theaters)

### Distributed Architecture

```
┌─────────────────────────────────────────────────┐
│              REPL Process                        │
│  ┌───────────────────────────────────────────┐  │
│  │         Local Theater Runtime              │  │
│  │  ┌──────┐  ┌────────┐  ┌────────┐        │  │
│  │  │ Shell│  │ Dev    │  │ Test   │        │  │
│  │  │ Actor│  │ Actor  │  │ Actor  │        │  │
│  │  └──┬───┘  └────────┘  └────────┘        │  │
│  └─────┼─────────────────────────────────────┘  │
└────────┼────────────────────────────────────────┘
         │ (Theater protocol)
         ▼
┌─────────────────────────────────────────────────┐
│           Remote Theater Runtime                 │
│  ┌────────┐  ┌────────┐  ┌────────┐            │
│  │Service │  │Database│  │ Worker │            │
│  └────────┘  └────────┘  └────────┘            │
└─────────────────────────────────────────────────┘
```

**Key insight**: The REPL runs its **own Theater runtime** (lightweight, in-process). Through actor interfaces, it can connect to and interact with actors on remote Theaters. This embraces a fully distributed model where the REPL is a first-class Theater citizen.

### Component Loading (Next Step)

Before full actor management, we need the ability to load and use components:

```lisp
; Import interface from host runtime
(import theater:simple/runtime from host)

; Import interface from a WASM component
(import colin:math/division from "math.wasm")

; Import specific function from interface
(import colin:math/division.fraction from "math.wasm")

; Use imported functions
(log "Hello Theater!")
(fraction (i32.const 1) (i32.const 3))
```

This syntax aligns with **Composite's wit+** - separating *what* interface to import from *where* to import it.

### Future Commands

```lisp
; Actor lifecycle
(spawn "path/to/actor.wisp")           ; Compile and spawn
(spawn "path/to/actor.wasm")           ; Spawn pre-compiled
(kill actor-id)                         ; Stop actor
(actors)                                ; List running actors

; Messaging
(send actor-id '(message data))        ; Send message
(ask actor-id '(request data))         ; Request/response

; Inspection
(actor-info actor-id)                  ; State and metadata
(actor-events actor-id)                ; Event chain history
(actor-state actor-id)                 ; Current state

; Remote Theater connections
(connect "theater://remote:8080")      ; Connect to remote Theater
(disconnect remote-id)                  ; Disconnect
(remotes)                               ; List connections
```

### Roadmap

| Phase | Focus | Status |
|-------|-------|--------|
| 1 | Self-hosted compiler | ✅ Complete |
| 2 | Basic REPL (eval, define, fn) | ✅ Complete |
| 3 | Component loading (import from) | 🔜 Next |
| 4 | Actor spawning & messaging | Future |
| 5 | State inspection | Future |
| 6 | Remote Theater connections | Future |

---

## Related Documents

- `docs/proposals/METAPROGRAMMING.md` - Macro system vision
- `docs/changes/PHASE-1-MACROS.md` - Macro implementation (completed)
- `docs/changes/SELF-HOSTED-COMPILER.md` - Self-hosted compiler (M1-M7)
- Composite project - wit+ and Graph ABI for recursive types
