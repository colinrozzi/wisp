# Wisp REPL — a live developer hook into Theater

A pure-Wisp Theater actor that is a **live, accumulating REPL**: you send it Wisp
source, it evaluates in a persistent session (bindings, closures, types, macros,
and heap survive across evaluations), and it can **drive and observe the Theater
actor system** through a broad set of host interfaces. It's the interactive way to
prototype and develop Theater actors — poke at the system live, then crystallize
the working code into an actor.

`actor.lisp` is the whole actor: it `(include …)`s the shared evaluator
(`interpreter/evaluator.lisp`, which includes the codec `marshal.lisp` and the host
bridge `rpc.lisp`) and declares the Theater ABI in Wisp. The stock compiler emits
`actor.wasm`.

## Run it (the live socket REPL)

The REPL runs inside a small local Theater runtime that registers all the host
handlers and exposes the actor over a socket. From the repo root:

```sh
# 1. Build the actor (writes actor.wasm in place)
cargo run -- compile actors/wisp-repl/actor.lisp actors/wisp-repl/actor

# 2. Build + run the host (registers rpc/self/store/runtime/message-server/
#    timer/assembler/filesystem/terminal/http-client, then serves on 127.0.0.1:7777)
cd actors/wisp-repl/legacy-host && cargo build && ./target/debug/wisp-interpreter-actor serve
```

It prints `rpc target actor id: <uuid>` (the session's own actor id) and listens on
`127.0.0.1:7777`. The wire protocol is **one JSON string per line**: send a
JSON-encoded source string, read a JSON-encoded result string.

```sh
# Minimal client (bash): send "(…)", read the printed value
exec 3<>/dev/tcp/127.0.0.1/7777
printf '%s\n' '"(i32.add (i32.const 40) (i32.const 2))"' >&3; IFS= read -r line <&3; echo "$line"   # => "42"
```

Type `(help)` in the REPL for a live catalog of host verbs. (`cargo run -p
test-runtime -- --repl` gives a pure-language REPL, but it traps the Theater host
imports — use the `serve` host above for the full surface.)

## It's a live image — build up behavior interactively

Definitions accumulate in the session, so you grow an environment as you go:

```lisp
(define double (lambda (x) (i32.mul x (i32.const 2))))
(double (i32.const 21))        ; => 42
(define greet (lambda (who) (string-append "hi " who)))
(greet "theater")              ; => "hi theater"
```

Note: use `(define name (lambda (args) body))` — the `(define (f args) …)`
shorthand is not supported.

## Driving Theater — host verbs

Every verb hands results back as ordinary, inspectable Wisp values (records,
variants, lists, options…). Results are left **faithful**, e.g. a `result` prints
as `(ok <ok-ty> <err-ty> <value>)` / `(err …)`.

| Interface | Verbs |
| --- | --- |
| self / rpc | `(self)` · `(log msg)` · `(describe id)` · `(exports id)` · `(implements id iface)` · `(call id fn arg…)` |
| store (content-addressed) | `(store-new)` · `(store-put id text)` → ref · `(store-get id ref)` · `(store-label id lbl ref)` · `(store-get-by-label id lbl)` · `(store-list-labels id)` · `(store-exists id ref)` · `(store-size id)` · `(store-put-at id lbl text)` |
| runtime (manage actors) | `(list-actors)` · `(actor-status id)` · `(actor-state id)`¹ · `(actor-manifest id)` · `(stop-actor id)` · `(kill-actor id)` · `(subscribe-spawns)` · `(unsubscribe-spawns)` · `(shutdown-runtime)` |
| message-server | `(msg-register)` · `(msg-send id text)` · `(msg-request id text)` · `(msg-list-requests)` · `(msg-respond req text)` · `(msg-cancel req)` · `(msg-open id text)` · `(msg-send-channel cid text)` · `(msg-close-channel cid)` |
| filesystem² | `(fs-read p)` · `(fs-write p text)` · `(fs-exists p)` · `(fs-list p)` · `(fs-meta p)` · `(fs-append p text)` · `(fs-delete p)` · `(fs-mkdir p)` · `(fs-rmdir p)` |
| http-client³ | `(http-get url)` · `(http-req method url)` |
| assembler | `(wat-to-wasm wat-text)` → wasm bytes |
| timer | `(now)` → ms · `(set-interval name ms)` · `(clear-interval name)` |
| terminal | `(term-write s)` · `(term-write-err s)` · `(term-raw true\|false)` · `(term-size)` · `(term-input)` |
| meta | `(help)` · `(poll-events)` |

¹ `(actor-state (self))` deadlocks (an actor can't read its own state mid-call) —
query *other* actors. ² filesystem paths are relative to the sandbox root (the dir
`serve` runs in). ³ http hosts are an exact-match allowlist set when the host
registers the handler.

## Observing Theater — inbound triggers as live handlers

Theater delivers async events by **calling exports on the actor**. Because the
session is one live image (evaluate and the callbacks share the environment), an
event just dispatches to a handler you define by name — so "add a handler" is
literally a `(define …)`:

```lisp
(define on-tick    (lambda (name) (string-append "tick: " name)))
(define on-spawn   (lambda (info) info))                ; info = (seq id name parent)
(define on-message (lambda (msg)  msg))                 ; msg  = (seq message-bytes)

(set-interval "beat" 1000)     ; Theater now calls handle-tick → your on-tick
;; …later…
(poll-events)                  ; => (("on-tick" "beat" "tick: beat") …) — what fired + each result
```

Each firing is buffered as `(handler-name event result)`; `(poll-events)` drains
the buffer so you see what happened on the frontend. Redefine a handler any time to
change behavior mid-stream. Triggers wired today: `on-tick` (timer),
`on-spawn` (after `(subscribe-spawns)`), `on-message` (after `(msg-register)`).

## Extending — add a host verb

The bridge (`interpreter/rpc.lisp`) is uniform, so adding a verb is small:

1. `(import <iface> <fn> (params) ret)` — declare the import with its **real** typed
   signature (so the interface hash matches Theater). Named result types
   (records/variants) are declared as Wisp `(record …)`/`(variant …)` matching the
   peer's pact; the compiler registers them so hashes resolve structurally.
2. An `apply-*` verb: `(unmarshal (raw-invoke "<iface>" "<fn>" (arg-tuple args)))`.
   `arg-tuple` marshals a positional tuple; a single string arg marshals bare; a
   `list<u8>` arg is built from a string via `str->bytes` / `str-bytes-tuple`; a
   record arg via `marshal-record`.
3. One clause each in `host-builtin?` and `apply-host-builtin` (flat `cond`).
4. Register the interface's handler once in `legacy-host/src/lib.rs` (real Theater
   deployments enable handlers via the manifest instead), and stub it in
   `tests/interpreter_engine.rs` (the packr preflight has no wildcard trap).

For an inbound trigger: add an `export` in `actor.lisp` for the callback
(`handle-*`) that calls `(dispatch-event "on-…" <event>)` — the arg is a bare value
for single-arg callbacks, or the raw `any` unmarshalled to a `(sequence …)` for
Tuple-wrapped ones.

## The actor ABI and `wisp-source`

`actor.lisp` exports:

- `theater:simple/actor.init(state) -> result<tuple<option<list<u8>>>, string>` —
  lifecycle entry; a fresh module is an empty session, so init is not a reset.
- `theater:simple/wisp.evaluate(source: string) -> string` — source in, printed
  value (or an `error:` diagnostic) out.
- inbound callbacks: `theater:simple/timer.handle-tick`,
  `runtime-handlers.handle-actor-spawn`, `message-server-client.handle-send`.

The evaluator imports a custom `wisp-source` interface (see `source.pact`) for
relative `(include …)`:

```
resolve-path: func(base: string, path: string) -> result<string, string>
read-source:  func(path: string) -> result<string, string>
```

Stock `theater spawn` doesn't provide it; the embedding host (here `legacy-host/`)
registers it and builds the bundle from `sources.json`. Definitions persist in the
session's Wasm heap until the actor stops; there is no GC — recreate a session to
reclaim its heap. Validated against Theater rev
`e2546700e00cb8c4a8050f27c62388bd57646483`, Packr 0.24.1.

| File | Purpose |
| --- | --- |
| `actor.lisp` | The actor: includes the evaluator, declares ABI + inbound callbacks |
| `actor.wasm` | Compiled actor (interface-qualified exports + CGRF metadata) |
| `manifest.toml` | Manifest; grants the `runtime` control capability |
| `source.pact` / `sources.json` | The `wisp-source` host interface + example bundle |
| `legacy-host/` | The local Theater runtime + socket server that hosts the REPL |
