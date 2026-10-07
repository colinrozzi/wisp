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

## Install and run (zero to a live REPL)

You do **not** need to know Theater first. Install the self-contained binary — it
carries its own actor + Theater runtime:

```sh
curl -fsSL https://raw.githubusercontent.com/colinrozzi/wisp/main/install.sh | sh
```

`theater-repl serve` runs a **daemon**: a long-running Theater runtime that holds
**sessions**. Each session is a persistent live image — bindings, closures, types,
macros, and any actors you spawn survive between commands, because the daemon keeps
the session alive. You create sessions and talk to them by id:

```sh
theater-repl serve &                 # daemon on 127.0.0.1:7777 (holds sessions)
id=$(theater-repl new)               # create a session, capture its id
theater-repl eval $id '(define double (lambda (x) (i32.mul x (i32.const 2))))'
theater-repl eval $id '(double (i32.const 21))'        # => 42  (same live image)
theater-repl list                    # sessions on the daemon
theater-repl eval $id '(help)'       # catalog of Theater host verbs
```

Each command is one clean line — no socket syntax, no JSON escaping. A form with
awkward characters (or many lines) goes on **stdin**, which the shell leaves
untouched:

```sh
theater-repl eval $id - <<'LISP'
(define greet (lambda (who) (string-append "hi " who)))
(greet "theater")
LISP
```

### The command surface

| Command | Does |
| --- | --- |
| `serve` | run the daemon (default when no subcommand is given) |
| `new [name]` | create a session, print its id |
| `list` | list the daemon's sessions |
| `eval <id> [form]` | evaluate a form (omit `form` or pass `-` to read stdin); exits non-zero on an `error:` result |
| `read <id>` | drain the session's buffered events (inbound triggers: on-tick, on-message, …) |
| `follow <id>` | stream a session's events live until Ctrl-C |
| `status [id]` | show a session (with id) or the daemon (without) |
| `stop <id>` | end a session and reclaim its heap |
| `repl` | interactive stdin/stdout prompt against a private session (for a human) |
| `version` | print the installed version (also `--version`) |
| `upgrade` | download the latest release and replace this binary in place |

Every command takes **`-p PORT`** to pick a daemon (default `7777`, or
`$THEATER_REPL_PORT`). Run `theater-repl serve -p 3333` for a second, isolated
runtime and pass `-p 3333` to the others — nothing is remembered between commands,
so the port (or the default) is how a command finds its daemon.

### Build from source (for hacking on the REPL itself)

Needs a Rust toolchain and network access (crates.io for the compiler; GitHub for
the host's pinned Theater deps):

```sh
# 1. Build the actor -> actors/wisp-repl/actor.wasm (embedded into the binary).
#    Re-run after editing actor.lisp or any interpreter/*.lisp it includes.
actors/wisp-repl/build.sh

# 2. Build + run the host. First build pulls Theater from git (pinned rev) and is
#    slow (~minutes); later builds are fast. --actor-dir loads the just-built actor
#    from disk instead of the copy embedded at compile time.
cd actors/wisp-repl/legacy-host && cargo build
./target/debug/theater-repl --actor-dir .. serve
```

Releases are published by `.github/workflows/release.yml` on a `v*` tag (the
supported path); `install.sh` pulls the latest release asset. (`cargo run -p
test-runtime -- --repl` gives a pure-language REPL, but it traps the Theater host
imports — use `theater-repl` above for the full surface.)

## It's a live image — build up behavior interactively

Definitions accumulate in the session (each line below is a `theater-repl eval
$id '…'`), so you grow an environment as you go:

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
| tcp⁴ | `(tcp-connect addr)` · `(tcp-send conn data)` → bytes · `(tcp-receive conn max)`⁵ · `(tcp-close conn)` · `(tcp-peer conn)` · `(tcp-is-tls conn)` · `(tcp-listen addr)`⁶ · `(tcp-accept lst)`⁶ · `(tcp-activate conn)` · `(tcp-set-active conn mode)` · `(tcp-transfer conn actor)` · `(tcp-transfer-async conn actor)` · `(tcp-tls-client conn name)` · `(tcp-tls-server conn)` · `(tcp-close-listener lst)` |
| podman | `(podman-run image name)` · `(podman-stop name)` · `(podman-rm name force)` · `(podman-list)` |
| meta | `(help)` · `(poll-events)` |

¹ Self-targeted blocking calls — `(exports (self))`, `(implements (self) …)`,
`(call (self) …)`, `(actor-state (self))` — would deadlock (the actor can't
service its own request mid-eval), so they now return an immediate error instead
of hanging. Query *other* actors; for your own metadata use `(describe (self))`,
which Theater serves without calling back into the actor. ² filesystem paths are relative to the sandbox root (the dir
`serve` runs in). ³ http hosts are an exact-match allowlist set when the host
registers the handler.

⁴ TCP works both directions (verified live). **Client:** `(tcp-connect addr)` →
connection id; `(tcp-send conn text)` → byte count; `(tcp-receive conn max)` →
`list<u8>`. Data moves as `list<u8>` built from a string, so non-UTF-8 bytes need
care. ⁵ `(tcp-receive …)` takes a `u32` `max` — the codec carries it as a proper
CGRF `u32` (via `as-u32`), so a plain integer like `1024` works. ⁶ The **server**
path is Erlang/OTP-style and *event-driven* — you do **not** call `(tcp-accept)`.
`(tcp-listen addr)` starts a background accept loop; Theater then calls the actor's
`tcp-client.{handle-connection,on-data,on-close}` exports, which dispatch to user
handlers you define like any other trigger:

```lisp
(define on-connection (lambda (cid) cid))     ; a new connection id (PENDING)
(define on-data       (lambda (e) e))          ; e = (sequence conn-id bytes)
(tcp-listen "127.0.0.1:9000")
;; when a client connects, on-connection fires with the id; to receive data:
(tcp-activate cid)                             ; PENDING -> active
(tcp-set-active cid "active")                  ; push mode -> on-data fires per read
;; …or leave it activated and (tcp-receive cid max) to pull passively.
```

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

Each firing is buffered as `(handler-name event result)`. `(poll-events)` drains
the buffer — and that is exactly what **`theater-repl read <id>`** calls, with
**`theater-repl follow <id>`** polling it in a loop to stream firings live. So from
the shell:

```sh
theater-repl eval $id '(define on-tick (lambda (n) (string-append "fired: " n)))'
theater-repl eval $id '(set-interval "beat" 1000)'
theater-repl follow $id        # fired: beat … (Ctrl-C to stop)
```

Redefine a handler any time to change behavior mid-stream. Triggers wired today:
`on-tick` (timer), `on-spawn` (after `(subscribe-spawns)`), `on-message` (after
`(msg-register)`), and `on-connection` / `on-data` / `on-close` (after
`(tcp-listen)` — see the TCP footnote).

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
