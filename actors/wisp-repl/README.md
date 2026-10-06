# Wisp interpreter actor

A pure-Wisp Theater actor, structured like the other Wisp actors in
`../actors/lisp/*`: the whole actor is `actor.lisp`, compiled by the stock Wisp
compiler to `actor.wasm`, and spawned by stock Theater through `manifest.toml`.
There is no bundled Rust host, socket server, or ABI adapter. Each spawned
instance is one live evaluator session owning its own bindings, closures, types,
macros, and heap.

`actor.lisp` includes the shared evaluator from `interpreter/evaluator.lisp` and
declares the Theater ABI directly in Wisp — the language implementation stays in
one place for both the local and actor REPLs.

| File | Purpose |
| --- | --- |
| `actor.lisp` | Actor: includes the evaluator, declares `init` + `evaluate` exports |
| `actor.wasm` | Compiled actor, with interface-qualified exports and CGRF metadata |
| `actor.wat` | Generated readable Wasm, for inspection |
| `manifest.toml` | Actor manifest with `runtime` + `rpc` handlers |
| `source.pact` | The custom `wisp-source` host interface the embedder must provide |
| `sources.json` | Example immutable logical source bundle for that host |
| `legacy-host/` | The former Rust host/socket/CLI, retired — kept for reference |

## Build

From the Wisp repository root:

```sh
cargo run -- compile actors/wisp-repl/actor.lisp actors/wisp-repl/actor --emit-wat
```

That writes `actor.wasm` and `actor.wat` in place. The stock compiler emits the
interface-qualified exports (`theater:simple/actor.init`,
`theater:simple/wisp.evaluate`) and the embedded CGRF type metadata; nothing
further rewrites the module.

## The ABI

`actor.lisp` exports two functions:

- `theater:simple/actor.init(state: option<list<u8>>) -> result<tuple<option<list<u8>>>, string>`
  — the standard lifecycle entry point. Config is ignored: a fresh module is an
  empty session, so init is not a reset. Returns `ok` carrying empty state.
- `theater:simple/wisp.evaluate(source: string) -> string` — source in, printed
  value (or an `error:` diagnostic) out. Values and closures stay in the session
  heap; runtime/trap failures surface as transport errors.

## Host import: `wisp-source`

The evaluator imports a custom `wisp-source` interface (declared in Wisp at
`interpreter/loading.lisp`) for relative `(include ...)`:

```
resolve-path: func(base: string, path: string) -> result<string, string>
read-source:  func(path: string) -> result<string, string>
```

Stock `theater spawn` does not provide this. The embedding host must register it
before spawning and construct the bundle from `sources.json`. These imports must
use Theater's normal recording/replay path. Logical paths never touch the host
filesystem.

## Embed and run

Validated against Theater revision
`e2546700e00cb8c4a8050f27c62388bd57646483`, Packr 0.24.1.

1. Register the `wisp-source` host interface from `source.pact`, built from
   `sources.json`.
2. Spawn `actor.wasm` using `manifest.toml`. The actor accepts Theater's usual
   `actor.init(state)` and returns successful `result<unit, string>`. Init is not
   a session reset.
3. Keep the actor ID and call
   `theater:simple/wisp.evaluate(source: string) -> string` through `rpc.call`;
   `rpc.describe` discovers that signature from the embedded metadata.
4. Stop the actor when its session ends. Definitions persist in its Wasm heap
   until then; there is no GC — recreate a session to reclaim its heap.

Separate actors have separate sessions:

```text
(define add-two (lambda (x) (+ x 2)))   ->  "#<closure>"
(add-two 40)                            ->  "42"
(include "lib/math.lisp") (increment 41) ->  exercises the bundled source
```

Source is limited to 4096 bytes per interactive call (65536 for a bundled file);
the evaluator keeps its own file/step limits. Bound actor memory and execution
deadlines are supplied by the embedding host.

## Retired Rust host

The former `legacy-host/` crate compiled this `actor.lisp`, performed WAT surgery
to add the ABI, and bundled a local Theater host, a TCP/JSON socket REPL, a
`wisp-source` host implementation, and integration tests. The stock compiler now
emits the ABI and metadata directly, so the adapter is gone. The socket REPL and
the Rust `wisp-source` host implementation, if still wanted, belong in a separate
client/embedder crate (as with `../actors/control` and `../actors/chat/chat-cli`),
not in the actor package. The one behavior not reproduced by the stock compiler
is the adapter's memory-declaration shrink (`32000 32000` -> `16 4096`); that is a
compiler codegen default and should be addressed there, not by re-adding surgery.
