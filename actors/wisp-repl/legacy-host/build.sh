#!/usr/bin/env bash
set -euo pipefail
actor_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd -- "$actor_dir/../.." && pwd)"
cd "$repo_dir"
cargo run --locked --manifest-path actors/wisp-repl/Cargo.toml -- \
  --actor-dir "$actor_dir" build
mkdir -p target
tar -czf target/wisp-repl.tar.gz -C actors \
  wisp-repl/actor.wasm wisp-repl/manifest.toml wisp-repl/wisp.pact \
  wisp-repl/source.pact wisp-repl/sources.json wisp-repl/README.md \
  wisp-repl/DEVELOPMENT.md wisp-repl/actor.wisp wisp-repl/src wisp-repl/tests \
  wisp-repl/Cargo.toml wisp-repl/Cargo.lock wisp-repl/build.sh
echo "Shareable bundle: $repo_dir/target/wisp-repl.tar.gz"
