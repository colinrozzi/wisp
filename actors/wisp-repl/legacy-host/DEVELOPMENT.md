# Wisp interpreter actor

A Wisp-written evaluator running as a real Theater actor. One actor owns one
persistent session. The local host supplies immutable source bundles and routes
calls through Theater's mailbox; it does not evaluate Wisp itself.

The source and shareable package live together in this directory; see [README.md](README.md).
The `build` command compiles it explicitly; `repl` and `serve` load the built
Wasm without compiling anything.

This crate has its own Cargo workspace and lockfile. Theater is pinned to
`e2546700e00cb8c4a8050f27c62388bd57646483`, matching the root flake. Packr resolves
to 0.24.1 alongside Wisp; the upstream CLI's separate lockfile uses 0.24.0.

## Run

From the repository root:

```sh
nix develop .#theater
./actors/wisp-repl/build.sh
cargo run --locked --manifest-path actors/wisp-repl/Cargo.toml -- repl
```

```text
wisp> (define add-two (lambda (x) (+ x 2)))
#<closure>
wisp> (add-two 40)
42
wisp> :quit
```

Build just the actor artifacts:

```sh
cargo run --locked --manifest-path actors/wisp-repl/Cargo.toml -- build
```

This writes `actor.wasm` and `actor.wat` under `actors/wisp-repl/`, alongside the
tracked manifest, interfaces, and bundle. `--actor-dir DIR` before the subcommand
selects a different package directory. The build script additionally creates
`target/wisp-repl.tar.gz` for sharing. The Wasm requires the `wisp-source` handler
below: stock `theater
spawn` does not register this project-specific handler. Use this host or register
`SourceBundle` in the embedding runtime.

## Socket REPL

```sh
cargo run --locked --manifest-path actors/wisp-repl/Cargo.toml -- serve --listen 127.0.0.1:7777
```

Each connection gets a fresh actor; definitions persist until disconnect.
Requests and responses are JSON strings, one per line. This framing supports
multiline Wisp input and quoted strings without inventing an application envelope.
Malformed JSON gets an `error:` string and leaves the session usable. Oversized
frames close the connection. At most 16 client sessions run concurrently.
The listener binds only to loopback. A resident service actor keeps this pinned
Theater runtime alive between connections (Theater exits when its last actor stops).

A minimal client, run in another terminal:

```python
import json, socket

with socket.create_connection(("127.0.0.1", 7777)) as connection:
    with connection.makefile("rw", encoding="utf-8") as stream:
        for source in ["(define add-two (lambda (x) (+ x 2)))", "(add-two 40)"]:
            stream.write(json.dumps(source) + "\n")
            stream.flush()
            print(json.loads(stream.readline()))
```

## Actor contract and control-dev handoff

- `theater:simple/actor.init(config: value) -> result<unit, string>` accepts the
  runtime's real init argument. Config is currently ignored; a fresh Wasm
  instance is an empty session. Calling init again does not reset definitions.
- `theater:simple/wisp.evaluate(source: string) -> string` returns a printed Wisp
  value or `error: ...`. Transport/trap failures remain Theater errors.
- `rpc.describe` discovers the namespaced export and its string parameter/result.
  Call it via `rpc.call(actor-id, "theater:simple/wisp.evaluate", source, options)`.
  Retain the actor ID across calls; spawn a new actor for a new session.
- Register `source::SourceBundle` in the host's `HandlerRegistry`. The library
  also exposes `Runtime`, `Session`, and `adapter::build` for embedding.

The evaluator's state stays inside Wasm. The adapter only supplies the lifecycle
ABI, namespaced exports, metadata, and memory bound. It performs checked edits
to compiler-produced WAT because Wisp does not yet express Pack's dynamic
`value` init argument and its compiled unit construction is incomplete.

## Bundled source and limits

The host loads `sources.json` from the actor directory once at startup.
Optionally supply `--bundle PATH` before `repl` or `serve` to override it. The
file is a JSON object mapping logical paths to source strings:

```json
{"lib/math.lisp": "(fn increment ((x s32)) s32 (i32.add x 1))"}
```

Then evaluate `(include "lib/math.lisp") (increment 41)`. Relative includes
resolve within the bundle; `..` cannot escape its root. Guest paths never read
the host filesystem. The host registers `resolve-path` and `read-source` through
Theater's import registry, so their responses are recorded and replayable.

Limits: 4096 source bytes per evaluation, 32 KiB socket frames, 256 bundle files,
64 KiB per file, 1 MiB total bundle, and 256 MiB maximum Wasm memory per session
(initially 1 MiB). Existing evaluator depth/step limits still apply. Theater's
epoch deadlines are 60 seconds for init and 300 seconds for other guest calls;
the local Rust REPL's additional 20-million-fuel policy is not installed here.
There is no GC: close/recreate a session to reclaim its heap. Language errors
are recoverable; a Wasm trap or memory exhaustion may require a fresh actor.

## Validation

```sh
cargo test --locked --manifest-path actors/wisp-repl/Cargo.toml
cargo clippy --locked --manifest-path actors/wisp-repl/Cargo.toml --all-targets -- -D warnings
cargo fmt --manifest-path actors/wisp-repl/Cargo.toml -- --check
```

Integration tests exercise real actor spawn/init, persistent closures, isolation,
relative includes, failure recovery, limits, guest-to-guest RPC, typed discovery,
socket framing and disconnect cleanup. Replay drives the recorded call sequence
through a fresh actor with Theater's replay interceptor and an empty source
bundle, asserting every event hash matches. It does not yet provide a user-facing
chain archive or resume command.
