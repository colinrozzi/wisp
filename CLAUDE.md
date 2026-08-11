# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`wisp` is a Lisp-like compiler that compiles S-expressions to WebAssembly. It generates WAT (WebAssembly text) and WASM (binary) files. The compiler exposes WebAssembly instructions directly - nearly 1:1 mapping to WASM. Supports scalar types (s32, s64, f32, f64), explicit WASM instructions, conditionals, let bindings, function calls, and imports/exports.

**The compiler is self-hosted** - a Wisp compiler written in Wisp, compiled to WASM, powers the interactive REPL.

### Pack Packages (Not WASM Components)

Wisp targets **Pack packages**, not standard WebAssembly Components:

| Aspect | Standard WASM Components | Pack Packages |
|--------|-------------------------|---------------|
| **ABI** | Canonical ABI | Graph ABI (CGRF) |
| **Types** | WIT | wit+ (recursive types) |
| **Runtime** | wasmtime component model | Pack/Theater |

**Why Pack?** Recursive types like `variant sexpr { sym(string), lst(list<sexpr>) }` are essential for a Lisp. Pack's wit+ and Graph ABI support this natively.

See the [Pack crate](../pack) for the runtime.

### Vision: Theater Shell

The REPL is evolving into a **Theater Shell** - an interactive environment for developing and managing actors in the Theater runtime:

- **Develop** actors (write Wisp code, compile to WASM)
- **Deploy** actors (spawn them into Theater)
- **Monitor** actors (inspect state, view event chains)
- **Interact** with actors (send messages, receive responses)
- **Connect** to remote Theaters (distributed actor management)

The REPL runs its own lightweight Theater runtime and can communicate with actors on remote Theaters through standard actor interfaces. See [WISP-REPL-ARCHITECTURE.md](docs/changes/WISP-REPL-ARCHITECTURE.md) for details.

## Documentation Structure

The project uses a two-tier documentation system:

1. **`docs/proposals/`** - High-level vision documents describing aspirational features and future directions
   - `METAPROGRAMMING.md` - Vision for macro system and minimal core language
   - `TYPE-SYSTEM.md` - Type system explorations
   - `COMPONENT-MACROS.md` - Component-level macro ideas
   - These are "blue sky" documents that inspire long-term direction

2. **`docs/changes/`** - Concrete, actionable implementation plans with progress tracking
   - Each change document describes a specific set of modifications being actively worked on
   - Includes implementation plan with checkboxes that get updated as work progresses
   - Tracks status, breaking changes, examples, and success criteria
   - These are "on the ground" documents that drive current development

**Recently completed**:
- [SELF-HOSTED-COMPILER.md](docs/changes/SELF-HOSTED-COMPILER.md) - Self-hosted Wisp compiler (M1-M7 complete)
- [WISP-REPL-ARCHITECTURE.md](docs/changes/WISP-REPL-ARCHITECTURE.md) - Interactive REPL powered by self-hosted compiler
- [PHASE-1-MACROS.md](docs/changes/PHASE-1-MACROS.md) - Unhygienic macros with quasiquotation (complete)
- [PHASE-0-WASM-INSTRUCTIONS.md](docs/changes/PHASE-0-WASM-INSTRUCTIONS.md) - Exposing core WASM instructions (complete)

**Current focus**: Theater integration - actor spawning and messaging (see Theater Shell vision above)

## Common Commands

### Build and Run
```bash
# Compile a Lisp source file to a .wasm package (written to <dir>/compiled/<stem>.wasm)
cargo run -- compile <source.lisp> [out-stem]
# Example: cargo run -- compile examples/prog.lisp    # -> examples/compiled/prog.wasm
# Also write the readable views (off by default; both are derivable from the wasm):
cargo run -- compile <source.lisp> --emit-wat --emit-pact

# Run an exported function from a compiled component
cargo run -- run <component.wasm> <function-name> <args...>
# Example: cargo run -- run examples/prog.wasm factorial 5

# Run with a dependency component
cargo run -- run <component.wasm> <function-name> <args...> --dep <module>=<dep.wasm>
# Example: cargo run -- run examples/user.wasm run 5 --dep math=examples/math.wasm
```

### Self-Hosted REPL
```bash
# Interactive REPL powered by the self-hosted Wisp compiler
cargo run -p test-runtime -- --repl
```

**Example session:**
```
wisp> (i32.add (i32.const 40) (i32.const 2))
42
wisp> (define x 10)
defined x = 10
wisp> (i32.mul x (i32.const 5))
50
wisp> (fn factorial ((n s32)) s32 (if (i32.le_s n (i32.const 1)) (i32.const 1) (i32.mul n (factorial (i32.sub n (i32.const 1))))))
defined function factorial
wisp> (factorial (i32.const 5))
120
wisp> (import colin:math/ops from "examples/math-lib-raw.wasm")
loaded interface colin:math/ops from examples/math-lib-raw.wasm
  exports: add, multiply, square
wisp> (square (i32.const 7))
49
wisp> (import wisp:repl/debug from host)
loaded interface wisp:repl/debug from host
  exports: print-i32, print-i64, print-f32, print-f64
wisp> (print-i32 (add (i32.const 3) (i32.const 4)))
[debug] 7
7
```

**REPL commands:**
- `(define x 42)` - define variable (inlined into expressions)
- `(fn name ...)` - define function (included in compilation)
- `(import iface from "path.wasm")` - load component, import its functions
- `(import iface from host)` - import host-provided interface
- `(list)` - show bindings, functions, and imports with signatures
- `(clear)` - clear all definitions and imports
- `quit` - exit REPL

### Development
```bash
# Format and lint code before commits
cargo fmt && cargo clippy --all-targets --all-features

# Build the project
cargo build

# Run in debug mode
cargo run -- compile examples/prog.lisp
```

## Architecture

### Two-Module Structure

1. **src/main.rs** - CLI entry point and runtime
   - Defines CLI using clap with `compile` and `run` subcommands
   - `run_compile()`: orchestrates compilation pipeline
   - `run_component()`: WebAssembly runtime using Wasmtime, handles component instantiation, dependency linking, and function execution
   - `encode_params()`: converts string arguments to typed WebAssembly values
   - `parse_dep_arg()`: parses `module=path.wasm` format for dependencies

2. **src/compiler.rs** - Complete compilation pipeline
   - **Tokenization**: `tokenize()` converts source to Token stream (LParen, RParen, Symbol, Number)
   - **Parsing**: `parse_sexpr()` builds SExpr tree, `parse_program()` creates top-level Program AST with functions, imports, and exports
   - **Type Checking**: `collect_signatures()` builds function signature map, `check_expr()` performs type inference with numeric widening (s32→s64, f32→f64)
   - **Code Generation**: `generate_wat()` emits WebAssembly text format, `generate_wit()` creates WIT world with imports/exports, `encode_component()` uses wit-component to create final WebAssembly component

### Key Data Structures

- **Type**: Enum of S32, S64, F32, F64
- **Expr**: AST nodes (Int, Float, Var, Call, WasmInstr, If, Let, Ascribe, GlobalGet, GlobalSet)
- **Function**: Name, parameters with types, return type, and body expression
- **Import**: Module, function name, parameters, return type (used for component imports)
- **Global**: Name, type, mutability, and initial value (for module-level state)
- **Program**: Functions, imports, exports, and globals
- **CodegenEnv**: Tracks parameter bindings and local variables during WAT emission

### Type System

- Numeric literal defaults: integers to `s32`, floats to `f64`
- Suffixes allowed: `42s64`, `3.14f32`
- Type ascription/casting: `(s32 expr)`, `(f64 expr)` - triggers conversion instructions
- **WASM instructions require exact type matches** - no automatic type unification
- Comparisons require matching types and return `s32` (0 or 1)
- Type checking happens before codegen with full expression tree validation
- Example: `(i32.add x y)` requires both `x` and `y` to be exactly `s32`

### Component Model

- Exports are explicitly marked via `(export name)` or `(export (fn ...))`
- Imports declared as `(import module-name func-name ((param type) ...) result-type)`
- WIT world groups imports by module into interfaces
- Component encoding uses wit-component crate with embedded metadata
- Runtime dependency linking via `--dep module=path.wasm` creates namespace with exported functions

### Pack Integration

Wisp aligns with [Pack's](../pack) **wit+** - an extended WIT dialect that supports recursive types. This is essential for representing S-expressions and ASTs.

**REPL import syntax** (planned):
```lisp
; Import interface from host
(import theater:simple/runtime from host)

; Import from WASM component
(import colin:math/ops from "math.wasm")

; Import specific function
(import colin:math/ops.factorial from "math.wasm")
```

Key: Separate *what* to import (interface) from *where* (source).

## Language Features

Functions: `(fn name ((param type) ...) return-type body)`
Imports: `(import module func ((param type) ...) return-type)`
Exports: `(export name)` or `(export (fn ...))`
**Macros:** `(defmacro name (params...) template)` - define syntactic abstractions
**Quasiquote:** `` `expr `` - quote template, `,expr` - unquote, `,@expr` - splice
**Globals:** `(global $name type mut|const init-value)` - module-level mutable/immutable state
**Global ops:** `(global.get $name)`, `(global.set $name value)`
**WASM arithmetic (explicit):** `(i32.add a b)`, `(i64.sub a b)`, `(f32.mul a b)`, `(f64.div a b)`
**WASM comparisons:** `(i32.eq a b)`, `(i32.lt_s a b)`, `(i64.ge_s a b)`, `(f64.ne a b)`
**WASM constants:** `(i32.const 42)`, `(i64.const 100)`, `(f32.const 3.14)`, `(f64.const 2.718)`
**WASM conversions:** `(i64.extend_i32_s x)`, `(f32.demote_f64 x)`, `(i32.trunc_f64_s x)`
**Memory ops:** `(i32.load addr)`, `(i32.store addr val)`, `(memory.size)`, `(memory.grow pages)`
Conditionals: `(if cond then else)` - condition must be s32 (0=false, 1=true)
Let bindings: `(let (name value) body)` - introduces lexically scoped local
Type casts: `(s32 expr)`, `(s64 expr)`, `(f32 expr)`, `(f64 expr)`
Includes: `(include "path.lisp")` - splice another file's forms (path relative to this file); pulls in the stdlib, e.g. `(include "std/num.lisp")`
Comments: `; comment to end of line`

## Testing

Currently no automated tests. Test fixtures in `tests/fixtures/`:
- `s64_factorial.lisp` - recursive factorial with s64
- `f64_math.lisp` - f64 arithmetic and typed casts

Manual testing workflow:
1. Compile fixture: `cargo run -- compile tests/fixtures/foo.lisp tests/fixtures/foo`
2. Run exports: `cargo run -- run tests/fixtures/foo.wasm function-name args...`
3. Verify output matches expected behavior

Future: Golden tests that diff generated WAT/WIT against expected snapshots.

## Coding Style

- Rust 2021 edition with standard formatting (4-space indent)
- snake_case for functions/variables, CamelCase for types
- Helper grouping: `tokenize_*`, `parse_*`, `gen_*`, `check_*`
- Short imperative commit messages (~50 chars): "add s64 support", "fix type widening"
- Keep compiler stages clearly separated: tokenize → parse → type-check → codegen

## Output Files

By default, compiling `examples/prog.lisp` writes just one file, in a `compiled/`
subfolder next to the source (Racket-style), so source directories stay clean:
- `examples/compiled/prog.wasm` - the Pack package. It embeds the interface metadata
  (CGRF), so it is the one true artifact.

The other two are optional, human-readable **views** of information the wasm already
holds; nothing consumes them, so they are off by default:
- `--emit-wat` -> `examples/compiled/prog.wat` - readable WAT disassembly (the wasm is
  built from this; any wasm tool regenerates it).
- `--emit-pact` -> `examples/compiled/prog.pact` - text interface (also embedded in the
  wasm as CGRF metadata).

An explicit out-stem (`compile prog.lisp path/name`) is used verbatim (relative to the
current directory), bypassing the `compiled/` default. The output directory is created
if missing. `compiled/` is gitignored.
