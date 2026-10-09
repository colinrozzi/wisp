#!/usr/bin/env bash
set -euo pipefail
actor_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd -- "$actor_dir/../../.." && pwd)"
cd "$repo_dir"
cargo run --locked --manifest-path wisp/actor/legacy-host/Cargo.toml -- \
  --actor-dir "$actor_dir" build
mkdir -p target
tar -czf target/wisp-repl.tar.gz -C wisp \
  actor/actor.wasm actor/manifest.toml actor/wisp.pact \
  actor/source.pact actor/sources.json actor/README.md \
  actor/DEVELOPMENT.md actor/actor.wisp actor/src actor/tests \
  actor/Cargo.toml actor/Cargo.lock actor/build.sh
echo "Shareable bundle: $repo_dir/target/wisp-repl.tar.gz"
