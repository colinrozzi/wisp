# Interpreted Wisp

A small Lisp interpreter written in Wisp. The Rust compiler builds the evaluator
once; one Wasm instance then reads and evaluates every input, retaining its own
environment and closures. Rust only supplies the local terminal/ABI adapter.

From the repository root, inside `nix develop`:

```sh
cargo run --example interpreter
# Or load an existing source file into the session first:
cargo run --example interpreter -- examples/factorial-test.lisp
# wisp> (factorial 6)
# 720
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

The initial language has s32/s64 integers, f32/f64 floats, strings, symbols, lists, closures, named
records and variants, and built-in functions. Forms include `define`, `lambda`,
`let`, `if`, `begin`, `quote` (also `'`), typed `fn`, `record`, `variant`, `match`,
and `export`. Both `(x s32)` and `(x : s32)` parameter/field declarations work.
`let` supports `(let (name expression) body)` and `(let (name : type expression) body)`.
Arithmetic/comparisons `+`, `-`, `*`, `/`, `=`, `<` take two numbers of the same type; `list`
takes any number of values, with `cons`, `car`, and `cdr` for list operations.
Zero and the empty list (`nil` or `'()`) are false; other values are true.
Integer arithmetic wraps at the operand width except division errors, which produce diagnostics.
The compiler's `i32` and `i64` constants, arithmetic, bitwise, shift/rotate, and comparison
operations are available. Comparisons return s32. String operations are `string-len`, `string-ref`,
`string-append`, `string=?`, and `substring`; positions and lengths count bytes.

Use an `s64` suffix for an explicit wide integer; the printer preserves that suffix.
Unsuffixed integers default to s32 and must fit its range. A literal can instead
adopt s64, f32, or f64 from a typed return, parameter, field, annotated let, cast, or instruction
operand. Expected types flow through `if` branches and `let` bodies. Stored and
quoted values keep their types; they do not implicitly widen. Use `(s64 expr)` or
`(expr : s64)` to convert an s32 value, and `(s32 expr)` to keep the low 32 bits of
an s64. `i64.extend_i32_s`, `i64.extend_i32_u`, and `i32.wrap_i64` are also available.

```lisp
(i64.add 4294967296 2)                ; 4294967298s64
(define wide 9223372036854775807s64)
(+ wide 1s64)                        ; -9223372036854775808s64
(i64.div_s -9223372036854775808 -1)    ; error: division overflow
(s32 4294967297s64)                   ; 1
```

Decimals default to f64; use `f32` or `f64` suffixes to select an explicit type.
Scientific notation works with a decimal point or a float suffix (`1.0e6`,
`1e6f32`). Float literals are read using exact integer arithmetic and rounded to
nearest, ties to even. As in the Rust compiler, f32 literals are parsed as f64
then demoted. A decimal f64 literal does not implicitly become f32 in a typed
context; use a suffix or cast. All scalar casts and the compiler's float arithmetic,
comparisons, promotion/demotion, and signed/unsigned conversion instructions work.

```lisp
(f32.add 1.5f32 2.25f32)             ; 3.75f32
(+ 0.1 0.2)                         ; 0.30000000000000004f64
(f64.div 1 0)                        ; inff64
(f64.div 0 0)                        ; nanf64
(s32 -2.75)                         ; -2
(s64 inff64)                        ; error: float-to-integer conversion out of range
```

Floating-point operations preserve IEEE behavior, including subnormals, signed
zero, infinity, and NaN. Numeric zero is false; NaN is true. Conversion to an
integer truncates toward zero; NaN, infinity, and out-of-range conversions produce
recoverable diagnostics. The printer uses up to 9 significant digits for f32 and
17 for f64, with a type suffix: output reads back to the same numeric value and
preserves signed zero. This is not a shortest-decimal formatter (`0.1` may print
as `0.10000000000000001f64`). NaN sign and payload are not preserved by text I/O.
The evaluator, including decimal conversion, remains entirely in Wisp.

Typed functions check arguments and return values at runtime. Constructors check
field/payload types, field access checks record identity, and `match` checks cases
and binding counts against the declared variant. Named types cannot currently be
redefined. Function bodies are checked as they execute; compile-time rejection of
invalid unexecuted branches and other static checks remain parity work.

```lisp
(record point (x s32) (y s32))
(fn sum-point ((p point)) s32 (i32.add (point.x p) (point.y p)))
(sum-point (point 20 22))              ; 42
(variant shape (circle s32) (rectangle s32 s32))
(match (rectangle 6 7) ((circle r) r) ((rectangle w h) (i32.mul w h))) ; 42
```

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

This is a feasibility implementation, not full compiled-Wisp parity: u8 values,
typed lists/options/results/tuples, macros, traits/generics, globals,
`include`, and Theater RPC built-ins are not implemented yet. `export` accepts
compiled source declarations; the module's external entry point remains `evaluate`.
There is no garbage collection; the existing bump allocator
retains allocations until the session is discarded. Inputs are limited to 4096
bytes, reader nesting to 64, evaluator nesting to 128, and evaluation to 10,000
steps. The local host also applies Wasmtime fuel to reader/printer work. These
are fixed implementation limits, not an expanded request protocol.

Run the behavioral checks with `cargo test --test interpreter --test interpreter_parity`.
The parity suite compares existing factorial and record examples, single-payload
variants, and arithmetic/string fixtures across the interpreter and both compilers.
The existing multi-payload variant example is compared against the Rust compiler;
the self-hosted compiler currently emits an undefined local for its second payload.
The self-hosted compiler also misprints the minimum s32 literal; the shared fixture
constructs that value by arithmetic to test operations independently of that bug.
Full-width s64 literals, typed payloads, and all integer operations at boundary
values are compared against the Rust compiler. The self-hosted reader currently
truncates integer literals to 32 bits, so its i64 fixture builds values with Wasm
instructions. The Rust compiler currently needs explicit suffixes/casts for ordinary
function and constructor arguments where the interpreter can adopt their expected
type. Expected-type propagation through `begin` and `match` remains incomplete;
use explicit suffixes in those result positions.
Float arithmetic and every conversion instruction are compared with compiled
Wasm. Reader/printer tests cover rounding boundaries, subnormals, signed zero,
non-finite values, and samples across binary exponents. A shared float fixture
also runs through both compilers.
