# Wisp REPL — a live developer hook into Theater

A pure-Wisp Theater actor that is a **live, accumulating REPL**: you send it Wisp
source, it evaluates in a persistent session (bindings, closures, types, macros,
and heap survive across evaluations), and it can **drive and observe the Theater
actor system** through a broad set of host interfaces. It's the interactive way to
prototype and develop Theater actors — poke at the system live, then crystallize
the working code into an actor.

`actor.wisp` is the whole actor: it `(include …)`s the shared evaluator
(`interpreter/evaluator.wisp`, which includes the codec `marshal.wisp` and the host
bridge `rpc.wisp`) and declares the Theater ABI in Wisp. The stock compiler emits
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
theater-repl list                    # every session in the runtime
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
| `new [name] [--manifest M]` | spawn an interpreter actor named `name`, print its id; with `--manifest`, it runs the actor `M` names |
| `set-default <M>` / `set-default --reset` | change the actor later `new` sessions start from (checked first; running sessions keep theirs) |
| `list` | list every interpreter actor in the runtime, however it was spawned |
| `eval [-l] <s> [form]` | evaluate a form (omit `form` or pass `-` to read stdin); exits non-zero on an `error:` result. A string result prints raw (its text); `--literal`/`-l` prints it as a Wisp literal |
| `read [--full] <s>` | print the session's trigger firings (on-tick, on-message, …) since the `read` cursor, one `handler: result` per firing, then move the cursor; `--full` prints `(next lost (seq handler result) …)` (`(seq handler event result)` when unhandled) |
| `follow [--full] [--from-now] <s>` | stream firings until Ctrl-C (same format as `read`), resuming from the `follow` cursor |
| `wait [--timeout S] <s>` | block until there is a firing after the `wait` cursor, print all there are, move the cursor, exit (exit 2 on timeout). The wake-on-message monitor |
| `cursors <s>` | the session's event cursors: name, acknowledged seq, how far behind |
| `status [s]` | show a session (with one) or the daemon and its services (without) |
| `stop <s>` | stop the session's actor and reclaim its heap; a service stopped this way is not restarted |
| `services` | each boot-list service: state, restarts, session id, last death |
| `start <name>` | start a boot-list service that is stopped, exited or given up, with a fresh restart limit |
| `repl` | interactive stdin/stdout prompt against a private session (for a human) |
| `version` | print the installed version (also `--version`) |
| `upgrade` | download the latest release and replace this binary in place |

**Output for reading vs. parsing.** By default `eval` prints a top-level string
result raw — real newlines, no quotes or escapes, ending in one newline — so a
string like `(tickets)`'s table reads as a table; other values print as Wisp. `read`
and `follow` print one `handler: result` line per firing, the event dropped and a
string result raw. These defaults are for reading: a raw string can span lines or
look like any other value. Scripts should parse `eval --literal` and `read --full`
/ `follow --full`, which print exactly what the session's printer writes.

A **session** is any interpreter actor in the runtime: an actor that exports
`theater:simple/wisp.evaluate`, whether `new` made it or another actor spawned it
with `(spawn-actor …)`. The daemon asks the runtime each time and keeps no session
table, so `<s>` can be a full actor id, an id prefix (what `list` and `new` print),
or the actor's name (the name `new` gave it, or for `spawn-actor` the manifest's
`name`). A name or prefix that matches several sessions is an error that lists them,
and `new` refuses a name another session already has. `spawn-actor` cannot be
told a name: the runtime names the actor after its manifest, so every spawn of the
stock `manifest.toml` is called `wisp-interpreter`. To reach a spawned interpreter
by name, give its manifest its own `name`; otherwise use its id. `list` shows AGE
and SOURCE only for the sessions this daemon made; the runtime does not record them.

**Reading never removes firings.** `read`, `follow` and `wait` each read after
their own cursor in the session (`read`, `follow`, `wait`; `--cursor NAME` picks
another, `--after N` reads after seq N with no cursor, `--peek` doesn't move
the cursor), so a monitor, a person and a leftover `follow` never take firings
from one another. The client moves a cursor only after it has printed, so a
reader that dies or whose output is closed (`| head`) sees the same firings
again next time: duplicates are possible, loss is not. `wait` and `follow` cost
no evaluation while idle: the session tells the daemon each new seq through the
`wisp-events` interface (`events.pact`). The firings are buffered by the actor
itself, so this works for any session, including ones the daemon did not spawn.
A session whose actor predates cursors falls back to the old draining read. See
`docs/proposals/SESSION-INBOX.md`.

Every command takes **`-p PORT`** to pick a daemon (default `7777`, or
`$THEATER_REPL_PORT`). Run `theater-repl serve -p 3333` for a second, isolated
runtime and pass `-p 3333` to the others — nothing is remembered between commands,
so the port (or the default) is how a command finds its daemon.

### Running new Wisp without a restart

A session runs the actor it was created from, for its whole life. To try a
rebuilt interpreter in a running daemon, start a session from its manifest; to
move everyone over, make it the default:

```sh
bash wisp/actor/build.sh                                  # rebuild actor.wasm
theater-repl new trial --manifest wisp/actor/manifest.toml   # just this session
theater-repl set-default wisp/actor/manifest.toml            # all later sessions
```

`set-default` starts the actor once and checks that it evaluates `(+ 1 2)`; if it
does not, the default stays as it was. The daemon reads the files when you run the
command, so rebuilding afterwards does not change running sessions or the default.
Changes to the host itself (this crate) still need a daemon restart.

### Services: a boot list, restarted when they die

The config file (`--config PATH`, else `~/.config/theater-repl/config.toml`)
grants the sessions' filesystem root, and can list **services**: sessions that
`serve` creates at start and keeps running.

```toml
[filesystem]
root = "~/srv/fs"            # sessions' filesystem; start files live under it

[[service]]
name = "web"                 # the session's name
start = "serve.wisp"         # evaluated in each fresh session; relative to root
# Optional, with their defaults:
restart = "permanent"        # permanent | transient | temporary
max_restarts = 5             # more restarts than this within period_secs,
period_secs = 60             #   and the daemon gives up
```

For each service the daemon creates a session under its name and evaluates the
start file in it, as `theater-repl new web && theater-repl eval web - < serve.wisp`
would. A start file that returns an `error:` counts as a death: the daemon logs
it, stops that session, and applies the policy. When a service's session dies
(a trap, `kill-actor`, `stop-actor` from another session…), the daemon restarts
it per policy, with a fresh session (a new actor id) under the same name, running
the start file again (read again, so edits take effect):

- `permanent` always restarts; `transient` restarts unless the session
  completed or was stopped; `temporary` never restarts (state `exited`).
- More than `max_restarts` restarts within `period_secs` and the daemon gives
  up (state `given up`), and says so in its log and in `services`/`status`.
- `theater-repl stop web` marks the service `stopped` before stopping it, so it
  is not restarted. `theater-repl start web` starts it again (also after
  `given up`), with the restart count and limit reset.

```
$ theater-repl services
NAME             STATE      RESTARTS  ID        LAST DEATH
web              running           1  ab42f587  killed (120s ago)
mail             given up          5  -         start file failed: mail.wisp: error: unbound symbol: smtp (30s ago)
```

State does not survive a restart: a restarted session starts from the start
file, not from what the old one held. A service that wants state across restarts
keeps it in a file or `store` and loads it from its start file. Peers holding
the old actor id must look the service up again (by name: `list`, the registry).

How it works: this is the userland supervisor of theater#235's supervision
design; the runtime itself neither restarts nor owns actors. Before spawning
anything the daemon subscribes to every actor's chain
(`add_global_subscription`) and keeps only `terminated` events, so it hears
every death with its cause (`src/deaths.rs`); the policy is in `src/boot.rs`.
Two runtime behaviors the daemon works around (theater `cbb48982`, 2026-10-25):

- A force-killed (`kill-actor`) REPL session never finishes its teardown, so the
  runtime keeps listing it. The daemon leaves actors whose `terminated` event it
  has seen out of `list`, name lookups and `(sessions)`.
- The runtime exits once its last actor is gone, which used to leave a daemon
  with no runtime after `stop` of its last session (`error: channel closed`). The
  daemon now spawns a small keepalive actor (`theater-repl-keepalive`,
  `src/keepalive.wisp`) at start. It is not a session, but `(list-actors)`
  shows it.

### Build from source (for hacking on the REPL itself)

Needs a Rust toolchain and network access (crates.io for the compiler; GitHub for
the host's pinned Theater deps):

```sh
# 1. Build the actor -> actors/wisp-repl/actor.wasm (embedded into the binary).
#    Re-run after editing actor.wisp or any interpreter/*.wisp it includes.
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
| filesystem² | `(fs-read p)` (bytes) · `(fs-read-text p)` (a string) · `(fs-write p text)` · `(fs-exists p)` · `(fs-list p)` · `(fs-meta p)` · `(fs-append p text)` · `(fs-delete p)` · `(fs-mkdir p)` · `(fs-rmdir p)` · `(fs-write-durable p text)` (all-or-nothing, on disk when it returns) · `(fs-sync p)` · `(fs-sync-dir p)` · `(fs-rename from to)` |
| http-client³ | `(http-get url)` · `(http-req method url)` |
| assembler | `(wat-to-wasm wat-text)` → wasm bytes |
| timer | `(now)` → ms · `(set-interval name ms)` · `(clear-interval name)` |
| terminal | `(term-write s)` · `(term-write-err s)` · `(term-raw true\|false)` · `(term-size)` · `(term-input)` |
| tcp⁴ | `(tcp-connect addr)` · `(tcp-send conn data)` → bytes · `(tcp-receive conn max)`⁵ · `(tcp-close conn)` · `(tcp-peer conn)` · `(tcp-is-tls conn)` · `(tcp-listen addr)`⁶ · `(tcp-accept lst)`⁶ · `(tcp-activate conn)` · `(tcp-set-active conn mode)` · `(tcp-transfer conn actor)` · `(tcp-transfer-async conn actor)` · `(tcp-tls-client conn name)` · `(tcp-tls-server conn)` · `(tcp-close-listener lst)` |
| podman | `(podman-run image name)` · `(podman-stop name)` · `(podman-rm name force)` · `(podman-list)` |
| lifecycle⁷ | `(monitor id)` · `(unmonitor id)` · `(link id)` · `(unlink id)` |
| chain recorder⁸ | `(chain-watch id)` · `(chain-head id)` · `(chain-tail id n)` · `(chain-verify id)` · `(chain-unwatch id)` |
| meta | `(help)` · `(help "tcp")` · `(help "triggers")` · `(events-after N)` · `(event-read c)` · `(event-ack c seq)` · `(event-cursors)` · `(poll-events)` · `(event-buffer N)` · `(handles)` · `(memory-stats)` |

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

⁷ `(monitor id)` watches another actor's whole event chain (self-service — you can
only monitor on your own behalf); every chain event of the subject is delivered to
your `on-actor-event` handler as `(sequence subject event-type)`. `(link id)`
fate-shares instead (no callback — if the subject dies, you are stopped);
`(unmonitor)`/`(unlink)` detach. **Each call on the monitored actor yields two
`"wasm"` events (call + result)** plus one per host-call it makes, so it is chatty —
route on `event-type` (e.g. `"terminated"`) in your handler. (A host-side
`monitor-filtered` + the raw `data` payload are follow-ups.)

⁸ The **chain recorder** is in the daemon (`legacy-host/src/chain.rs`, interface
`chain.pact`). Theater keeps only an actor's head hash and streams each event to
subscribers; `(chain-watch id)` subscribes the daemon to actor `id`'s chain and
keeps its newest 256 events, as hash, parent hash, event type and payload size.
It never keeps payloads (they hold messages, file contents, request text), so no
verb returns them. Recording stops at the actor's `terminated` event; the ring of
a stopped actor stays readable (the newest 16 stopped actors are kept). At most
64 actors are watched at once.

```lisp
(chain-watch (self))       ; (ok ...); a session may watch itself
(chain-head (self))        ; (ok (chain-head hash count recording)): hex hash ("" before any event)
(chain-tail (self) 3)      ; (ok #<list chain-event ((chain-event hash parent event-type data-bytes) ...)>), oldest first
(chain-verify (self))      ; (ok (chain-check ok count kept dropped breaks from-root))
```

`breaks` counts events whose parent is not the previous event's hash, over every
event seen, not only those still kept; `dropped` is how many the ring has let go;
`from-root` says whether the first event seen was the actor's first. Sessions are
**not** recorded by default: every subscriber gets its own copy of each event,
payload included, and the actor waits for each subscriber to take it (theater
`docs/messaging-and-lifecycle-behavior.md` §3.1), so recording costs the watched
actor something, and most sessions are never inspected. Watch what you want to
read. Event types seen (2026-10-25, `tests/chain.rs` and a local `/system` run):
`wasm` (an export call, and its result: two per evaluation or trigger),
`<interface>/<function>` for each host call (e.g. `theater:simple/tcp/activate`,
`wisp-chain/head`), and `terminated`. Hashes are 20 bytes (40 hex digits). Serving
one `/system` page is about 23 events, so the ring holds about the last 11 requests.

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
(poll-events)                  ; => (("on-tick" "tick: beat") …) — what fired + each result
```

Each firing is buffered as `(handler-name result)`, or `(handler-name event result)`
when the event was not handled (no handler defined, or the handler failed): a
handled event was delivered already, so it is not kept. Each firing also has a
sequence number, 1, 2, 3, … for the life of the session, and reading removes
nothing:

```lisp
(events-after 41)              ; => (44 0 (42 "on-tick" "tick: beat") …): next, lost, firings
(events-after 41 10 "text")    ; at most 10, as "handler: result" lines
(event-read "mine")            ; the same, after cursor "mine" (a new cursor starts at 0)
(event-ack "mine" 44)          ; "mine" has handled up to 44 (never moves back)
(event-cursors) (event-cursor "mine") (event-cursor-drop "mine") (event-head)
```

`next` is the seq to acknowledge once the firings are handled; `lost` counts
firings after the seq that the limit already dropped. At most 16 cursors: a new
one past that replaces the least recently used. `(poll-events)` and
`(poll-events "text")` keep their old shapes (`(firing …)`, or `handler: result`
lines) but read through their own cursor, `poll`, so they no longer take firings
from the CLI's readers.

The buffer keeps the newest 256 firings. When it is full the oldest is dropped,
and every reader whose cursor is before it is told: `(poll-events)` puts the
count first, `(N firing …)`, and the text forms start with a `(N earlier firings
dropped)` line. `(event-buffer N)` sets the limit (0 = keep nothing; handlers
still run, and readers see each firing as dropped) and returns the old one.

`(handles)` lists the timers, listeners, connections, processes and channels the
session holds open, as `(kind id)`; the open and close verbs and the
`on-close`/`on-exit`/`on-channel-close` events keep it current. `(memory-stats)`
returns `(("heap-bytes" n) ("heap-pages" n) ("bindings" n) ("modules" n)
("types" n) ("macros" n) ("events" n) ("events-dropped" n) ("handles" n)
("event-seq" n) ("cursors" n))`; `heap-bytes` is the allocator's live bytes
(allocated and not yet freed; garbage nothing frees yet still counts),
`heap-pages` is the memory's size in 64 KiB pages (memory never shrinks, so
this is the high-water mark), and
`events-dropped` counts every firing the limit has dropped.

From the shell, **`theater-repl read <id>`** reads the firings (`--full` for the
lists), **`follow <id>`** streams them, and **`wait <id>`** blocks for the next:

```sh
theater-repl eval $id '(define on-tick (lambda (n) (string-append "fired: " n)))'
theater-repl eval $id '(set-interval "beat" 1000)'
theater-repl follow $id        # on-tick: fired: beat … (Ctrl-C to stop)
```

Redefine a handler any time to change behavior mid-stream. Triggers wired today:
`on-tick` (timer), `on-spawn` (after `(subscribe-spawns)`), `on-message` (after
`(msg-register)`), `on-actor-event` (after `(monitor id)`), and `on-connection` / `on-data` / `on-close` (after
`(tcp-listen)` — see the TCP footnote).

## Extending — add a host verb

The bridge (`interpreter/rpc.wisp`) is uniform, so adding a verb is small:

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

For an inbound trigger: add an `export` in `actor.wisp` for the callback
(`handle-*`) that calls `(dispatch-event "on-…" <event>)` — the arg is a bare value
for single-arg callbacks, or the raw `any` unmarshalled to a `(sequence …)` for
Tuple-wrapped ones.

## The actor ABI and `wisp-source`

`actor.wisp` exports:

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
| `actor.wisp` | The actor: includes the evaluator, declares ABI + inbound callbacks |
| `actor.wasm` | Compiled actor (interface-qualified exports + CGRF metadata) |
| `manifest.toml` | Manifest; grants the `runtime` control capability |
| `source.pact` / `sources.json` | The `wisp-source` host interface + example bundle |
| `legacy-host/` | The local Theater runtime + socket server that hosts the REPL |
