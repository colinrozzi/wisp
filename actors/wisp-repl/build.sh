#!/usr/bin/env bash
# Build the wisp-repl actor: compile actor.wisp -> actor.wasm beside the manifest.
# The host (legacy-host) loads this actor.wasm at startup; rebuild after editing
# actor.wisp or any included evaluator source (interpreter/*.wisp).
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root"
cargo run -q -- compile actors/wisp-repl/actor.wisp actors/wisp-repl/actor
echo "built actors/wisp-repl/actor.wasm"
