# Theater REPL

Interactive Wisp shell powered by Theater actors.

## Overview

Theater REPL is a fully actor-based interactive environment for Wisp, a Lisp-like language that compiles to WebAssembly. Each expression you type is compiled by the self-hosted Wisp compiler (running as WASM), assembled to WASM bytecode, and evaluated - all orchestrated by Theater actors.

## Quick Start

```bash
# From the wisp directory
cargo run -p theater-repl

# Or with release build (faster)
cargo run --release -p theater-repl
```

## Usage

```
theater-repl                  Start interactive REPL
theater-repl --actor <path>   Use custom REPL actor WASM
theater-repl --help           Show help
```

## Example Session

```
Theater REPL
Type 'quit' to exit

theater> (i32.add (i32.const 40) (i32.const 2))
42

theater> (i32.mul (i32.const 6) (i32.const 7))
42

theater> (if (i32.gt_s (i32.const 5) (i32.const 3))
           (i32.const 1)
           (i32.const 0))
1

theater> quit
Goodbye!
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Theater Runtime                          │
│                                                             │
│  User Input                                                 │
│      ↓                                                      │
│  MessageRouter → REPL Actor (spawn-repl-actor.wasm)         │
│                      ↓                                      │
│              Wisp Compiler (wisp-compiler.wasm)             │
│                      ↓ WAT                                  │
│              Assembler (wat-to-wasm)                        │
│                      ↓ WASM                                 │
│              Evaluator (instantiate + run)                  │
│                      ↓                                      │
│                   Result                                    │
│                                                             │
│  Handlers: runtime, store, supervisor, message-server, wisp │
└─────────────────────────────────────────────────────────────┘
```

### Components

1. **Theater Runtime** - Manages actor lifecycle and message routing
2. **REPL Actor** (`spawn-repl-actor.wasm`) - Handles the eval loop
3. **Wisp Compiler** (`wisp-compiler.wasm`) - Self-hosted compiler, compiles Wisp to WAT
4. **StaticComposer** - Merges REPL actor + compiler into single WASM at startup
5. **Handlers** - Provide host functions:
   - `runtime` - Logging, actor info
   - `store` - Key-value storage
   - `supervisor` - Actor spawning
   - `message-server` - Inter-actor messaging
   - `wisp` - Assembler, evaluator, import helpers

### Eval Loop

When you enter an expression:

1. **Parse & Wrap** - Expression wrapped as `(export (fn eval () <type> <expr>))`
2. **Compile** - WASM-to-WASM call to self-hosted compiler produces WAT
3. **Assemble** - WAT converted to WASM bytecode
4. **Compose** - If imports present, composed with dependency modules
5. **Evaluate** - WASM instantiated, `eval()` called, result returned

## Wisp Language

Wisp exposes WebAssembly instructions directly:

### Arithmetic
```lisp
(i32.add (i32.const 1) (i32.const 2))   ; 3
(i32.mul (i32.const 6) (i32.const 7))   ; 42
(i64.sub (i64.const 100) (i64.const 1)) ; 99
(f64.div (f64.const 22.0) (f64.const 7.0)) ; 3.142857...
```

### Comparisons
```lisp
(i32.eq (i32.const 5) (i32.const 5))    ; 1 (true)
(i32.lt_s (i32.const 3) (i32.const 5))  ; 1 (true)
(i32.gt_s (i32.const 3) (i32.const 5))  ; 0 (false)
```

### Conditionals
```lisp
(if (i32.const 1)
    (i32.const 42)
    (i32.const 0))
; Returns 42
```

### Types
- `s32` / `i32` - 32-bit signed integer
- `s64` / `i64` - 64-bit signed integer
- `f32` - 32-bit float
- `f64` - 64-bit float

## Performance

| Build   | Per Expression | Notes                    |
|---------|----------------|--------------------------|
| Debug   | ~600ms         | Good for development     |
| Release | ~30-35ms       | Production-ready speed   |

The compile-and-run cycle includes full WASM-to-WASM compilation through the self-hosted compiler.

## Files

- `src/main.rs` - Theater runtime setup and REPL loop
- `examples/actors/spawn-repl-actor.wasm` - The REPL actor
- `examples/wisp-compiler.wasm` - Self-hosted Wisp compiler

## Requirements

The following files must exist relative to the working directory:
- `examples/actors/spawn-repl-actor.wasm`
- `examples/wisp-compiler.wasm`

Run from the `wisp` repository root, or use `--actor` to specify a custom path.
