# wisp

`wisp` is a deliberately tiny Lisp-like compiler that lowers a handful of S-expression forms to WebAssembly text/binary plus a matching WIT world. It is meant as an educational scaffold for experimenting with WebAssembly components and compiler pipelines.

## Workflow

```
$ cargo run -- compile prog.lisp wisp
Wrote:
  wisp.wat
  wisp.wasm
  wisp.wit

$ cargo run -- run wisp.wasm double 7
14
```

1. **Compile** – `cargo run -- compile <source.lisp> [out-stem]` tokenizes/parses the input, builds an AST, emits `out-stem.wat/.wit`, and encodes `out-stem.wasm` as a WebAssembly component (with embedded WIT). If `out-stem` is omitted, it defaults to the source filename stem (e.g., `prog` for `prog.lisp`).
2. **Run** – `cargo run -- run out-stem.wasm <export> <args…>` instantiates the component via Wasmtime and calls the chosen export. You can also import the WIT world into your own host.

## Language Features

Everything is an `s32`. The current surface includes:

| Form | Description |
| ---- | ----------- |
| `(fn name (params...) body)` | Function definition. All functions return `s32`. |
| `(export name)` / `(export (fn ...))` | Mark a function for export; list form defines and exports in one go. |
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

Compiling it yields WAT/WIT with `double` and `factorial` exported (based on the explicit `export` forms). You can then call any export with the built-in runner:

```
$ cargo run -- run wisp.wasm factorial 5
120
```

## Next Ideas

- Broaden the surface language (multiple `let` bindings, boolean ops, structured types).
- Add regression tests (e.g., compile golden programs and diff the output) so new features don’t regress existing codegen.
