# Wisp REPL actor

This directory is the actor package. Build it once, then load `actor.wasm` into
Theater; starting a REPL or opening a socket does **not** recompile the actor.
Each new session instantiates the same Wasm with its own definitions and heap.
The actor source, adapter, local host, tests, and Cargo lockfile live here too.
`actor.lisp` includes the shared evaluator from `interpreter/evaluator.lisp`;
the language implementation is kept in one place for both local and actor REPLs.

## Build and share

From the Wisp repository root:

```sh
nix develop .#theater
./actors/wisp-repl/build.sh
```

The directory contains:

| File | Purpose |
| --- | --- |
| `actor.lisp` | Wisp entry point, including the shared evaluator |
| `src/adapter.rs` | Builds the Theater lifecycle/export adapter |
| `src/source.rs` | Bundled-source host handler |
| `src/lib.rs`, `src/main.rs`, `src/transport.rs` | Embedding API, local CLI, socket transport |
| `tests/`, `Cargo.toml`, `Cargo.lock` | Integration tests and pinned dependencies |
| `actor.wasm` | Built evaluator with Theater lifecycle and evaluation exports |
| `manifest.toml` | Actor manifest; package path is relative to this directory |
| `wisp.pact` | Public string-in/string-out evaluation interface |
| `source.pact` | Required bundled-source host interface |
| `sources.json` | Immutable logical source files loaded by the host |
| `actor.wat` | Generated readable Wasm, for inspection |

The build also writes **`target/wisp-repl.tar.gz`**, containing the Wasm, manifest,
interfaces, source bundle, actor/host source, and guides. Send that archive to control-dev or
copy the package directory. Generated Wasm/WAT stay out of Git; the tracked
source and build script reproduce the package within the Wisp repository.
The archive runs from its prebuilt Wasm; rebuilding uses the shared Wisp compiler
and evaluator from this repository. See [DEVELOPMENT.md](DEVELOPMENT.md).

## Run locally

From the repository root, after building:

```sh
cargo run --locked --manifest-path actors/wisp-repl/Cargo.toml -- repl
# Or start the local socket host:
cargo run --locked --manifest-path actors/wisp-repl/Cargo.toml -- serve
```

Load a copied/extracted package with `--actor-dir /path/to/wisp-repl` before the
subcommand. From this directory, `cargo run --locked -- repl` also works.
`repl` and `serve` only read the built package. Default socket address:
`127.0.0.1:7777`. Send one JSON string per line; receive one JSON string per line:

```text
"(define add-two (lambda (x) (+ x 2)))"
"(add-two 40)"
```

The responses are `"#<closure>"` and `"42"`. One connection owns one session;
disconnect stops that actor. `(include "lib/math.lisp") (increment 41)` exercises
the example source bundle. Edit `sources.json` before starting the host to change
the bundle; running actors do not read live files.

## Embed in control-dev

Validated against Theater revision
`e2546700e00cb8c4a8050f27c62388bd57646483`, with Packr 0.24.1 in the actor host.

1. Register the `wisp-source` interface from `source.pact`. Wisp's host provides
   this as `wisp_interpreter_actor::source::SourceBundle`; construct it from
   `sources.json`. These imports must use Theater's normal recording/replay path.
   Stock `theater spawn` does not include this project-specific handler.
2. Spawn `actor.wasm` using `manifest.toml`. The actor accepts Theater's usual
   `actor.init(config: value)` and returns successful `result<unit, string>`.
   Config is ignored; init is not a session reset operation.
3. Keep the actor ID and call
   `theater:simple/wisp.evaluate(source: string) -> string` through `rpc.call`.
   `rpc.describe` discovers that signature. The output is a printed value or an
   `error:` string; runtime/trap failures remain transport errors.
4. Stop the actor when its session ends. Definitions persist in its Wasm heap.

The socket endpoint is a convenience supplied by the local host; the actor
itself exposes RPC and has no TCP dependency. The host implementation, integration
tests, and detailed limits live in `actors/wisp-repl/` in the Wisp repo.
Source is limited to 4096 bytes per call, bundles to 256 files/1 MiB total,
individual files to 64 KiB, and each actor's memory to 256 MiB. There is no GC;
recreate a session to reclaim its heap. Theater supplies its existing execution
deadlines (60 seconds for init, 300 for other calls).
