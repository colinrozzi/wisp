# Interpreted Wisp

A small Lisp interpreter written in Wisp. The Rust compiler builds the evaluator
once; one Wasm instance then reads and evaluates every input, retaining its own
environment and closures. Rust only supplies the local terminal/ABI adapter.

From the repository root, inside `nix develop`:

```sh
cargo run --example interpreter
```

```lisp
(define make-adder (lambda (x) (lambda (y) (+ x y))))
(define add-two (make-adder 2))
(add-two 40)                          ; 42
(let (x 1000) (add-two 40))            ; 42: lexical capture
(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(fact 6)                             ; 720
(/ 1 0)                              ; error: division by zero
(add-two 40)                          ; 42: session still works
```

Enter one input per line; an input can contain several expressions. Use `begin`
for a sequence inside a function body. `:quit`, `:q`, or EOF exits. Piped input
works too. The reader itself accepts multiline source through `evaluate`.

The initial language has s32 integers, strings, symbols, lists, closures, and
built-in functions. Forms are `define`, `lambda`, `let`, `if`, `begin`, and
`quote` (also `'`). `let` uses Wisp's `(let (name expression) body)` syntax.
Arithmetic/comparisons `+`, `-`, `*`, `/`, `=`, `<` take two integers; `list`
takes any number of values, with `cons`, `car`, and `cdr` for list operations.
Zero and the empty list (`nil` or `'()`) are false; other values are true.
Arithmetic wraps at 32 bits except division errors, which produce diagnostics.

Local bindings are lexical. Closures see current top-level definitions, so
recursive functions and top-level redefinition work. Each call extends a copy
of its captured environment. `define` is currently top-level only and publishes
its binding after the expression succeeds. A whole input is read before any
evaluation; successful preceding definitions remain if a later expression fails.

`evaluator.lisp` exports `evaluate(source: string) -> string`: a printed value or
an `error:` diagnostic. Interpreter values stay in the session. This is a local
REPL text boundary, not a structured value transport. The module can also be
compiled directly for a host that keeps its Wasm instance alive:

```sh
cargo run -- compile interpreter/evaluator.lisp target/interpreter/evaluator
```

This is a feasibility implementation, not full compiled-Wisp parity: typed `fn`,
other numeric widths, records, macros, mutation, and Theater RPC built-ins are
not implemented yet. There is no garbage collection; the existing bump allocator
retains allocations until the session is discarded. Inputs are limited to 4096
bytes, reader nesting to 64, evaluator nesting to 128, and evaluation to 10,000
steps. The local host also applies Wasmtime fuel to reader/printer work. These
are fixed implementation limits, not an expanded request protocol.

Run the behavioral checks with `cargo test --test interpreter`.
