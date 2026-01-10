# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`wisp` is an educational Lisp-like compiler that compiles S-expressions to WebAssembly components. It generates WAT (WebAssembly text), WASM (binary), and WIT (WebAssembly Interface Types) files. The compiler exposes WebAssembly instructions directly - nearly 1:1 mapping to WASM. Supports scalar types (s32, s64, f32, f64), explicit WASM instructions, conditionals, let bindings, function calls, and component imports/exports.

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
- [PHASE-1-MACROS.md](docs/changes/PHASE-1-MACROS.md) - Unhygienic macros with quasiquotation (complete)
- [PHASE-0-WASM-INSTRUCTIONS.md](docs/changes/PHASE-0-WASM-INSTRUCTIONS.md) - Exposing core WASM instructions (complete)

## Common Commands

### Build and Run
```bash
# Compile a Lisp source file to WAT/WASM/WIT
cargo run -- compile <source.lisp> [out-stem]
# Example: cargo run -- compile examples/prog.lisp examples/prog

# Run an exported function from a compiled component
cargo run -- run <component.wasm> <function-name> <args...>
# Example: cargo run -- run examples/prog.wasm factorial 5

# Run with a dependency component
cargo run -- run <component.wasm> <function-name> <args...> --dep <module>=<dep.wasm>
# Example: cargo run -- run examples/user.wasm run 5 --dep math=examples/math.wasm
```

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

For input `examples/prog.lisp` with out-stem `examples/prog`:
- `examples/prog.wat` - WebAssembly text format (module with imports, functions, exports)
- `examples/prog.wasm` - WebAssembly component binary (encoded with embedded WIT)
- `examples/prog.wit` - WIT world declaration (package example:wisp with imports/exports)
