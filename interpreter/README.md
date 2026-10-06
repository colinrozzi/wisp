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
`global`, `global.get`, `global.set`, `include`, `trait`, `instance`, `defmacro`, `define-syntax`, `quasiquote`, and
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
operand. Expected types flow through `if` and `match` branches, `let` bodies, and
the final expression of `begin`. Stored and
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

`defmacro` uses classic, name-based macros, matching the self-hosted compiler's capture
behavior. Template-introduced local names can capture names in substituted code;
the Rust compiler instead tracks hygiene scopes. The Rust compiler also
currently fails to substitute a bare parameter in comma-at; the shared fixture
uses a spliced literal list containing unquotes to compare all three paths.

For hygienic templates, use `define-syntax` with `syntax-rules`. Rules are tried
in order. `_` matches anything without binding; listed literal keywords match by
name, and numeric/string patterns match literal values and their printed type.
Other pattern identifiers bind syntax. Each pattern list supports one repeated
group, with fixed elements before and after it. Repeated groups can contain
compound patterns and nest; zero repetitions preserve empty captures. Templates
can repeat several groups, but variables used together in one repeated template
must have matching lengths. Pattern variables must be unique and used under
enough template ellipses; malformed rules are rejected before publication.

```lisp
(define-syntax with-temp
  (syntax-rules () ((_ body) (let (tmp 0) body))))
(let (tmp 42) (with-temp tmp))        ; 42: caller's tmp stays distinct
(define-syntax rows
  (syntax-rules () ((_ ((x ...) ...)) (list (list x ...) ...))))
(rows ((1 2) () (3)))                ; ((1 2) () (3))
```

Introduced identifiers have a fresh binding identity for each expansion. This
applies to `let`, lambda and typed function parameters, and match bindings.
Substituted syntax preserves its caller identity. Introduced free identifiers
resolve in the top-level session environment, so a caller's local binding cannot
capture a macro's helper reference. Top-level redefinition still affects those
helpers, like ordinary functions. Quotation prints/materializes ordinary symbols;
private identities do not become user-visible names. Record/variant type names,
field names, explicit globals, and literal keyword matching remain name-based.

All macro forms share a persistent namespace: the latest declaration wins,
including across forms. The existing expansion/publication and recovery rules
apply to each. Local macro declarations are not supported.
The self-hosted compiler does not implement `syntax-rules`; the Rust
compiler currently has incomplete nested/compound repetition capture and numeric
literal pattern matching, so those cases have interpreter-specific tests.

Procedural transformers use the compiler's `syntax-case-lambda` form. Each clause
has a pattern, an optional guard, and an expression that must produce syntax.
Use `#'` for syntax quotation, or quasisyntax with unsyntax to insert computed
syntax. Pattern variables substitute automatically inside either kind of syntax
template. The transformer's single parameter holds its entire input form.

```lisp
(define-syntax fold-add
  (syntax-case-lambda (stx)
    ((_ a b)
     (and (integer? #'a) (integer? #'b))
     (let (sum (+ (syntax->datum #'a) (syntax->datum #'b)))
       #`(i32.const #,sum)))
    ((_ a b) #'(i32.add a b))))
(fold-add 20 22)                     ; 42
(define-syntax gather
  (syntax-case-lambda (stx) ((_ x ...) #`(list #,@x))))
(gather 1 2 3)                       ; (1 2 3)
```

An optional `(syntax-case stx (literals...) clauses...)` wrapper selects literal
keywords, using the same pattern matcher as `syntax-rules`. The wrapper's input
must be the transformer parameter. `syntax-case` is not a general runtime form.
Syntax prefixes read as `syntax`, `quasisyntax`, `unsyntax`, and `unsyntax-splice`
forms; they are reserved for transformer expressions. Repeated captures can be
spliced with `#,@`, including empty and compound-pattern captures. A repeated
capture cannot be substituted as a single syntax value. Nested quasisyntax tracks
which unsyntax expressions are active. Introduced syntax retains the same hygiene
and top-level helper resolution as `syntax-rules`.

The transformer language provides `if`, lexical `let`, `identifier?`, `number?`,
`integer?`, `syntax->datum`, `not`, `and`, `or`, integer `+`/`-`, and `syntax-error`.
Integer computation uses wrapping signed 64-bit arithmetic; inserted results are
unsuffixed syntax literals whose type is determined when evaluated. Predicates
produce transformer booleans. As in the compiler, only a boolean false rejects a
guard; `if` also treats a computed integer zero as false. Syntax objects, including
quoted zero, are truthy. `and`/`or` evaluate all operands. `syntax->datum` extracts an
integer or removes an identifier's context; other values pass through unchanged.
An unbound transformer name denotes introduced syntax, and an unrecognized call
constructs a syntax application. Neither executes ordinary session code. A clause
returning a computed number/boolean rather than syntax is an error.

Transformer failures use the normal expansion diagnostics and publish no session
changes. This implementation additionally binds the whole-input parameter,
evaluates compound unsyntax expressions, and splices list-shaped syntax objects;
those paths are incomplete in the Rust compiler. It rejects invalid builtin
arguments instead of silently treating them as zero. Procedure bodies and helper
functions from the ordinary session are not callable during expansion.

Generic functions use the compiler's `where` syntax. Type parameters are inferred
from argument values and a known scalar return type. Compound signatures such as
`(list T)`, `(option T)`, `(result T U)`, tuples, and `(-> T U)` are matched
structurally. The interpreter substitutes concrete types into type annotations
and constructors, then evaluates the function body with ordinary local bindings.
It does not generate additional Wasm.

```lisp
(trait (Add T) (fn add ((a T) (b T)) T))
(instance (Add s32) (fn add ((a s32) (b s32)) s32 (i32.add a b)))
(fn twice ((x T)) T (where (Add T)) (add x x))
(twice 21)                          ; 42
(fn singleton ((x T)) (list T) (where T) (list-push (list-new T) x))
(list-get (singleton 42s64) 0)       ; 42s64
```

Traits can have several type parameters. Instance signatures must match all of
the trait's methods; a malformed declaration publishes nothing. Trait and instance
definitions are immutable within a session, while generic functions can be
redefined like ordinary functions. Declare a trait before its instances and
constrained functions. A constrained call checks for the required instances and
binds their methods locally. These bindings survive in captured closures.
Direct method calls select an instance from arguments and the expected scalar
return type; an ambiguous call is a diagnostic.

Function parameters use `(-> argument-types... result-type)`. Typed functions,
generic functions, trait methods, and lexical closures can be passed as values.
Function contracts check argument and return types when called. An untyped lambda
does not itself supply type-inference information: infer its type from other
arguments or give it a concrete annotated binding. Returned functions and typed
container/record fields retain their function contracts.

The existing standard library can be loaded unchanged in a fresh session:

```sh
cargo run --example interpreter -- std/list.lisp
```

```lisp
(define xs (list-push (list-push (list-new s32) 20) 21))
(sum (map (lambda (n) (i32.add n 1)) xs)) ; 43
(fold + (zero) xs)                       ; 41
(contains xs 21)                         ; 1
```

Loading `std/num.lisp` publishes its trait operators, including `+` and `=`.
Signature lookahead can use a later typed argument to resolve an earlier literal
or method call, as in `(fold + (zero) xs)`. It only reads type metadata; argument
expressions still execute once, from left to right. All type parameters must be
resolved before entering the body. Expected compound return types and arbitrary
expression analysis are not implemented; provide typed arguments when inference
has insufficient information. Quoted data keeps its symbols unchanged.

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
derived instances and Theater RPC built-ins are not implemented yet. Type checks
run during evaluation; unexecuted branches and function bodies are not checked.
`export` accepts compiled source declarations; interpreted functions remain inside
the session rather than becoming new Wasm exports.
There is no garbage collection; the existing bump allocator
retains allocations until the session is discarded. Interactive inputs are limited
to 4096 bytes; files to 64 KiB each, with at most 256 files and 1 MiB of source per
load graph. Include nesting and reader nesting are limited to 64, evaluator
nesting to 128, and evaluation to 10,000 steps. Macro expansion allows at most
100 nesting levels and 10,000 syntax/transformer visits per input. The local host also applies
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
Syntax-rules tests compare the existing example and introduced-binding hygiene
with the Rust compiler, and cover nested/empty repetitions, free identifier
resolution, typed bindings, redefinition, and invalid-rule recovery.
Procedural macro fixtures compare the existing example, guards, integer folding,
literal clauses, hygiene, and splicing against the Rust compiler. Additional tests
cover phase separation, nested quasisyntax, includes, overflow, invalid results,
and bounded expansion with recovery.
Generic functions, trait dispatch, nested specialization, and generic list
construction are compared across both compilers and the interpreter. The standard
list algorithms, including higher-order calls, are compared with the Rust compiler.
Interpreter tests cover multi-parameter traits, expected return dispatch, closures,
hygienic macros, declaration rollback, type errors, and argument effect ordering.
