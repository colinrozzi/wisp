# wisp

`wisp` is a deliberately tiny Lisp-like compiler that lowers a handful of S-expression forms to WebAssembly text/binary plus a matching WIT world. It is meant as an educational scaffold for experimenting with WebAssembly components and compiler pipelines.

## Workflow

```
$ cargo run -- compile examples/prog.lisp examples/prog
Wrote:
  examples/prog.wat
  examples/prog.wasm
  examples/prog.wit

$ cargo run -- run examples/prog.wasm double 7
14
```

1. **Compile** – `cargo run -- compile <source.lisp> [out-stem]` tokenizes/parses the input, builds an AST, emits `out-stem.wat/.wit`, and encodes `out-stem.wasm` as a WebAssembly component (with embedded WIT). If `out-stem` is omitted, it defaults to the source filename stem (e.g., `examples/prog` for `examples/prog.lisp`), and artifacts are placed next to the source.
2. **Run** – `cargo run -- run out-stem.wasm <export> <args…> [--dep mod=dep.wasm]` instantiates the component via Wasmtime and calls the chosen export. The optional `--dep` registers a single dependency component under the given module name to satisfy imports.

## Language Features

Everything is an `s32`. The current surface includes:

| Form | Description |
| ---- | ----------- |
| `(fn name (params...) body)` | Function definition. All functions return `s32`. |
| `(import mod fn (params…) s32)` | Declare an imported function; params/results are `s32` only. |
| `(export name)` / `(export (fn ...))` | Mark a function for export; list form defines and exports in one go. |
| Literals (`42`) | Signed 32-bit integers. |
| Variables | Function parameters and `let` bindings (lexically scoped). |
| Arithmetic | `(+ a b)`, `(- a b)`, `(* a b)` |
| Comparisons | `(= a b)` (also `==`), `(< a b)`, `(<= a b)`, `(> a b)`, `(>= a b)` – all return `0`/`1`. |
| Conditionals | `(if cond then else)` – both branches must evaluate to `s32`. |
| Let bindings | `(let (name value) body)` – introduces a new local binding. |
| Function calls | `(foo arg1 arg2 …)` with arity checked at compile time. Recursion is supported. |

## Example

`examples/prog.lisp` exercises most forms:

```lisp
(export
  (fn double (x)
    (* x 2)))

(export factorial)

(fn factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(fn main (x)
  (factorial (double x)))
```

Compiling it yields WAT/WIT with `double` and `factorial` exported (based on the explicit `export` forms). You can then call any export with the built-in runner:

```
$ cargo run -- run examples/prog.wasm factorial 5
120
```

To link two components, declare imports and supply a dependency at runtime:

```lisp
; examples/math.lisp
(export (fn double (x) (* x 2)))

; examples/user.lisp
(import math double (x) s32)
(export (fn run (x) (double x)))
```

```
$ cargo run -- compile examples/math.lisp examples/math
$ cargo run -- compile examples/user.lisp examples/user
$ cargo run -- run examples/user.wasm run 5 --dep math=examples/math.wasm
10
```

Typed scalars beyond `s32` are supported; build and run `typed.lisp` with:

```
$ cargo run -- compile examples/typed.lisp examples/typed
$ cargo run -- run examples/typed.wasm add64 40 2
42
$ cargo run -- run examples/typed.wasm mul-f64 3.5
8.75
```

Typed fixtures live under `tests/fixtures/`:

- `s64_factorial.lisp` – recursive factorial using `s64`
- `f64_math.lisp` – `f64` addition/scaling plus a small `f32` dot product

## Next Ideas

- Broaden the surface language (multiple `let` bindings, boolean ops, structured types).
- Add regression tests (e.g., compile golden programs and diff the output) so new features don’t regress existing codegen.
