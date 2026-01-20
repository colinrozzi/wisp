# Wisp REPL Architecture

## Overview

A Theater-native REPL for Wisp that compiles S-expressions to WebAssembly components and evaluates them as ephemeral actors. The REPL uses a "compile-and-run" model where each expression is compiled along with all accumulated definitions into a self-contained component.

## Status

- [x] Design finalized
- [ ] Wisp compiler updated for REPL mode
- [ ] Theater wrapper component implemented
- [ ] REPL Actor implemented with compile-and-run loop
- [ ] Component composition (wrapper + compiled wisp)
- [ ] Theater spawn/await integration
- [ ] State persistence via Theater event chain
- [ ] End-to-end testing

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

### Phase 1: Wisp Compiler REPL Mode
- [ ] Add `compile_repl()` function
- [ ] Implement variable inlining
- [ ] Implement context (bindings + functions) passing
- [ ] Generate component with `eval` export
- [ ] Test compilation with context

### Phase 2: Theater Wrapper Component
- [ ] Create wrapper component project
- [ ] Import Theater reply primitive
- [ ] Import `eval` from composed component
- [ ] Implement `init` that calls eval and replies
- [ ] Test wrapper in isolation

### Phase 3: REPL Actor Core
- [ ] Create `wisp-repl-actor` project
- [ ] Implement state management (bindings, functions)
- [ ] Implement `define` and `fn` handling
- [ ] Implement component composition (wrapper + compiled)
- [ ] Implement eval flow (compile → compose → spawn → await)

### Phase 4: Theater Integration
- [ ] Add spawn capability to REPL actor
- [ ] Implement await for eval result
- [ ] Wire up message protocol

### Phase 5: Build Mode
- [ ] Implement `compile` command (unwrapped export)
- [ ] Support selecting which functions to export
- [ ] Test produced components work standalone

### Phase 6: Persistence & Polish
- [ ] Implement state serialization
- [ ] Integrate with Theater event chain
- [ ] Test restart recovery
- [ ] `list` command
- [ ] `clear` command
- [ ] Error messages and UX

## Open Questions

1. **Type annotations for define**: Should `(define x 42)` infer s32, or require `(define x 42s32)`?
   - Leaning toward: infer from literal, allow suffix for explicit

2. **Mutable bindings**: Should `(define x 10)` after `(define x 42)` update or error?
   - Leaning toward: update (shadow previous)

3. **Function redefinition**: Same question for functions
   - Leaning toward: allow redefinition

4. **Imports**: How do we handle components that import from other components?
   - Future work: `(import "math" double)` could load from compiled component

5. **Memory/tables**: If we need memory operations, how does that work across evals?
   - Each eval is fresh, no persistent memory
   - Could add `(define-memory ...)` that gets included in compilation

## Related Documents

- `docs/proposals/METAPROGRAMMING.md` - Macro system vision
- `docs/changes/PHASE-1-MACROS.md` - Macro implementation (completed)
