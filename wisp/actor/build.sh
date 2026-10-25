#!/usr/bin/env bash
# Build the wisp-repl actor: compile actor.wisp -> actor.wasm beside the manifest.
# The host (legacy-host) loads this actor.wasm at startup; rebuild after editing
# actor.wisp or any included evaluator source (interpreter/*.wisp).
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root"
# The interpreter relies on reference counting's value-semantics list-push (its
# environments are shared, not copied), so the actor is always built with RC.
if [ "${GRANITE_RC:-1}" = "0" ]; then
  echo "build.sh: the Wisp interpreter needs reference counting; unset GRANITE_RC=0" >&2
  exit 1
fi
cargo run -q -- compile wisp/actor/actor.wisp wisp/actor/actor
echo "built wisp/actor/actor.wasm"
