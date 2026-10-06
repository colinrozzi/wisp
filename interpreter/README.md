# Interpreted Wisp

A small Lisp interpreter written in Wisp. The Rust compiler builds the evaluator
once; one Wasm instance then reads and evaluates every input, retaining its own
environment and closures. Rust supplies the local terminal/ABI adapter and file I/O.

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

The initial language has s32/s64 integers, f32/f64 floats, strings, symbols, lists, closures,
named records and variants, typed lists, options, results, tuples, and built-in functions.
Forms include `define`, `lambda`,
`let`, `if`, `begin`, `quote` (also `'`), typed `fn`, `record`, `variant`, `match`,
`global`, `global.get`, `global.set`, `include`, `defmacro`, `quasiquote`, and
`export`. Both `(x s32)` and
`(x : s32)` parameter/field declarations work.
`let` supports `(let (name expression) body)` and `(let (name : type expression) body)`.
Arithmetic/comparisons `+`, `-`, `*`, `/`, `=`, `<` take two numbers of the same type; `list`
takes any number of values, with `cons`, `car`, and `cdr` for list operations.
Zero and the empty Lisp list (`nil` or `'()`) are false; other values are true,
including typed lists and option/result values.
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
and binding counts against the declared variant, option, or result. Named types cannot currently be
redefined. Function bodies are checked as they execute; compile-time rejection of
invalid unexecuted branches and other static checks remain parity work.

```lisp
(record point (x s32) (y s32))
(fn sum-point ((p point)) s32 (i32.add (point.x p) (point.y p)))
(sum-point (point 20 22))              ; 42
(variant shape (circle s32) (rectangle s32 s32))
(match (rectangle 6 7) ((circle r) r) ((rectangle w h) (i32.mul w h))) ; 42
```

Compound type declarations can nest `(list T)`, `(option T)`, `(result T E)`,
and `(tuple T1 T2 ...)`, including named record/variant types. Empty lists and
absent option/result branches retain their declared types. Tuple construction
infers each field's type; values can be passed, returned, and stored in other
containers. Like the compiler, `(tuple ...)` requires at least one element and
there is currently no tuple projection form.

```lisp
(define xs (list-new s64))
(list-push xs 4294967296)             ; #<list s64 (4294967296s64)>
(list-get xs 0)                      ; 4294967296s64
(list-len xs)                        ; 1
(some s32 42)                        ; (some s32 42)
(none (list s32))                    ; (none (list s32))
(err s32 string "oops")              ; (err s32 string "oops")
(match (some s32 42) ((some n) n) ((none) 0)) ; 42
(tuple 42 "x" (none s32))             ; (tuple 42 "x" (none s32))
```

`list-push` mutates a typed list and returns it, matching compiled Wisp. Aliases,
closures, and containers holding that list observe the update. Both operands
are evaluated before the push; type checks complete before that push mutates the
list. Earlier successful side effects remain if a later operation fails.
`list-get` checks its s32 index and reports out-of-bounds access as a diagnostic.
The Lisp `list`/`cons`/`car`/`cdr` operations remain separate from typed lists.
Typed lists display as `#<list TYPE (...)>`; this is a display format, not source
syntax. Recursive printing stops at depth 64 with `#<depth-limit>`, allowing
cyclic records and lists to be inspected without unbounded recursion.

Local bindings are lexical. Closures see current top-level definitions, so
recursive functions and top-level redefinition work. Each call extends a copy
of its captured environment. `define` is currently top-level only and publishes
its binding after the expression succeeds. A whole input is read before any
evaluation; successful preceding definitions remain if a later expression fails.

Explicit globals have their own namespace, separate from lexical variables and
`define`. Declarations are top-level, with a type, `mut` or `const`, and an integer
constant initializer. An optional colon before the type is accepted. Assignments
check the declared type and return the assigned value; immutable globals reject
assignment before evaluating its expression. Duplicate declarations are errors.

```lisp
(global $counter s32 mut 0)
(fn next () s32
  (global.set $counter (i32.add (global.get $counter) 1)))
(next)                              ; 1
(next)                              ; 2
(global $items (list s32) mut 0)
(global.set $items (list-new s32))
(list-push (global.get $items) 42)
```

Numeric initializers convert to the declared scalar type. Other types accept only
the compiler's zero placeholder; reading one before assigning a typed value is a
diagnostic. Globals persist across inputs and are isolated between sessions.

Top-level `(include "path.lisp")` expands source before evaluation. File paths are
relative to the including file's directory; interactive includes start at the
host's working directory. Canonical paths are included once per input graph,
including cycles and aliases. Loading again reads and evaluates the files again;
there is no session-wide include cache. Every file is read and parsed before any
form executes, so file/reader failures leave language state unchanged. Evaluation
errors retain earlier successful definitions and side effects. Includes inside
functions or `begin` are rejected; quoted forms and comments do not load files.
The Rust host's `Interpreter::load_file(path)` and command-line file arguments use
this file-relative loading behavior.

`defmacro` defines a persistent syntax template. Arguments are unevaluated forms;
commas substitute parameters and comma-at splices a list of forms. As in the
existing compiler surface, this is template substitution, not an arbitrary
compile-time Lisp function. Parameters must be distinct symbols and arity is
checked before expansion. A template can call other macros recursively.

```lisp
(defmacro when (condition body) `(if ,condition ,body 0))
(when 1 (+ 20 22))                   ; 42
(when 0 (/ 1 0))                     ; 0: body is not evaluated
(defmacro sumargs (xs) `(i32.add ,@xs))
(sumargs (15 27))                    ; 42
(define x 42)
`(answer ,x ,@(list 1 2))             ; (answer 42 1 2)
```

Includes expand first. Then all direct top-level macro declarations are collected
and all remaining forms are expanded before evaluation, allowing a function to
use a macro declared later in the same load graph. Definitions persist between
inputs; the latest definition wins. Existing functions and closures retain their
already expanded bodies. Quote protects data from expansion; quasiquote expands
only the expressions in active unquotes. Backquote, comma, and comma-at also work
for ordinary runtime list construction, with nested quasiquotes tracking their
own commas. Splicing requires a Lisp list, and standalone comma forms are errors.

Invalid macro declarations or expansion failures publish neither new macros nor
ordinary definitions. After successful expansion the macro table is published;
a subsequent evaluation error retains it along with earlier successful effects.
Macros cannot generate new include directives or macro declarations for another
collection pass. Quotation prefixes, `quote`, `include`, `defmacro`, and
`define-syntax` are reserved macro names.

These are classic, name-based macros, matching the self-hosted compiler's capture
behavior. Template-introduced local names can capture names in substituted code;
the Rust compiler instead tracks hygiene scopes. `define-syntax`, `syntax-rules`,
`syntax-case`, and hygienic expansion remain parity work. The Rust compiler also
currently fails to substitute a bare parameter in comma-at; the shared fixture
uses a spliced literal list containing unquotes to compare all three paths.

`evaluator.lisp` exports `evaluate(source: string) -> string`: a printed value or
an `error:` diagnostic. Interpreter values stay in the session. This is a local
REPL text boundary, not a structured value transport. The module can also be
compiled directly for a host that keeps its Wasm instance alive. It also exports
`evaluate-from(source: string, base: string) -> string`, where `base` is the
canonical source filename or empty for interactive input. Both use the Pack/Graph
ABI. Hosts must supply two imports in the `wisp-source` module:

- `resolve-path(base: string, path: string) -> (result string string)` returns a canonical filename.
- `read-source(path: string) -> (result string string)` returns UTF-8 source.

Ordinary I/O failures use the result's error string. Reading Lisp syntax, expanding
includes, and evaluating forms remain in Wisp. The local host implements these
imports and bounds file reads.

```sh
cargo run -- compile interpreter/evaluator.lisp target/interpreter/evaluator
```

This is a feasibility implementation, not full compiled-Wisp parity: u8/unit values,
hygienic macros, traits/generics, and Theater RPC built-ins are not implemented yet.
`export` accepts compiled source declarations; interpreted functions remain inside
the session rather than becoming new Wasm exports.
There is no garbage collection; the existing bump allocator
retains allocations until the session is discarded. Interactive inputs are limited
to 4096 bytes; files to 64 KiB each, with at most 256 files and 1 MiB of source per
load graph. Include nesting and reader nesting are limited to 64, evaluator
nesting to 128, and evaluation to 10,000 steps. Macro expansion allows at most
100 nesting levels and 10,000 syntax visits per input. The local host also applies
Wasmtime fuel to reader/printer and expansion work, which can be exhausted before
the file limits are reached. These
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
function and constructor arguments and typed list elements where the interpreter can
adopt their expected type. The interpreter also accepts compound let annotations;
the Rust compiler currently accepts only scalar let annotations.
Expected-type propagation through `begin` and `match` remains incomplete;
use explicit suffixes in those result positions.
Float arithmetic and every conversion instruction are compared with compiled
Wasm. Reader/printer tests cover rounding boundaries, subnormals, signed zero,
non-finite values, and samples across binary exponents. A shared float fixture
also runs through both compilers.
Compound-operation fixtures compare nested containers, option/result matching,
tuple arguments, aliases, and nested list mutations across all three paths.
Structured outputs are also compared with the Rust compiler's CGRF values.
Globals are compared across all three paths using declarations without a colon;
the self-hosted compiler does not currently parse the optional colon there.
Relative includes, canonical path deduplication, and cycles are compared against
the Rust compiler. Loading tests cover preflight failures and session recovery.
Macro fixtures compare the existing macro example, recursive expansion, lazy
branches, spliced templates, and repeated side effects across both compilers and
the interpreter. Interpreter tests also cover persistence, redefinition, quoted
data, nested quasiquotes, invalid declarations, and expansion recovery.
