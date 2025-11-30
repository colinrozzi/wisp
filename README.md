# tinyc

`tinyc` is a deliberately tiny Lisp-like compiler that lowers a handful of S-expression forms to WebAssembly text/binary plus a matching WIT world. It is meant as an educational scaffold for experimenting with WebAssembly components and compiler pipelines.

## Workflow

```
$ cargo run -- prog.lisp tiny
Wrote:
  tiny.wat
  tiny.wasm
  tiny.wit

$ wasmtime --invoke run tiny.wasm 5
120
```

1. **Compile** – `cargo run -- <source.lisp> <out-stem>` tokenizes/parses the input, builds an AST, emits `out-stem.wat/.wit`, and then converts the WAT to `out-stem.wasm` via the `wat` crate.
2. **Run** – use the Wasmtime CLI (`wasmtime --invoke run out-stem.wasm <args…>`) to call the exported `run` function, or import the WIT world into your own component host.

## Language Features

Everything is an `s32`. The current surface includes:

| Form | Description |
| ---- | ----------- |
| `(fn name (params...) body)` | Function definition. All functions return `s32`. |
| `(export name)` / `(export (fn ...))` | Mark a function for export. `main` is always exported as `run`. |
| Literals (`42`) | Signed 32-bit integers. |
| Variables | Function parameters and `let` bindings (lexically scoped). |
| Arithmetic | `(+ a b)`, `(- a b)`, `(* a b)` |
| Comparisons | `(= a b)` (also `==`), `(< a b)`, `(<= a b)`, `(> a b)`, `(>= a b)` – all return `0`/`1`. |
| Conditionals | `(if cond then else)` – both branches must evaluate to `s32`. |
| Let bindings | `(let (name value) body)` – introduces a new local binding. |
| Function calls | `(foo arg1 arg2 …)` with arity checked at compile time. Recursion is supported. |

## Example

`prog.lisp` exercises most forms:

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

Compiling it yields WAT/WIT with both `double` and `factorial` exported alongside the default `run` entry point. You can then call any export via Wasmtime:

```
$ wasmtime --invoke double tiny.wasm 7
14
$ wasmtime --invoke factorial tiny.wasm 5
120
```

## Next Ideas

- Broaden the surface language (multiple `let` bindings, boolean ops, structured types).
- Emit true components via `wasm-tools component new …` so hosts can bind through the component model directly.
- Add regression tests (e.g., compile golden programs and diff the output) so new features don’t regress existing codegen.
