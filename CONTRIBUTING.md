# Contributing

PRs merge fast and problems get fixed forward. CI is shaped around that.

## The flow

1. **A pull request runs the quick checks:** Format, Clippy and Build, for the
   compiler and the `theater-repl` daemon. They take a few minutes. Green quick
   checks are enough to merge.
2. **A push to main runs everything:** the quick checks, plus Test and Daemon tests.
3. **If main goes red, fix it forward** with a new PR. Revert only if the fix is
   not obvious. Nothing releases until main is green again.
4. **Releases only come from green.** When every CI job passes on a push to main,
   `.github/workflows/release.yml` publishes that commit. Nobody tags by hand.

To run the full suite on a branch before merging, because a change is risky:
`gh workflow run CI --ref <branch>`, or run the targeted tests locally.

## Releases

Tags are `v0.1.N`, and N rises by one with each release
(`.github/next-release-tag.sh`). `install.sh` and `theater-repl upgrade` fetch
GitHub's latest release, and `theater-repl version` prints the tag the binary was
built from (a local build prints `git describe`).

A commit doesn't release if:

- its CI wasn't all green;
- only `*.md` or `docs/` changed since the last release;
- a newer commit already released (CI runs can finish out of order).

If the release workflow itself fails, re-run it from the Actions tab. It leaves no
tag behind: the tag is created last, with the release.

## Generated files

`wisp/actor/actor.wasm` is generated and not in git, so PRs touch source only and
two interpreter PRs don't conflict over a binary.

```sh
# A fresh clone builds the daemon with nothing else to run first:
git clone https://github.com/colinrozzi/wisp && cd wisp
nix develop
cd wisp/actor/legacy-host && cargo build
./target/debug/theater-repl version
```

Its `build.rs` compiles the actor, and again when `wisp/actor/actor.wisp`,
`wisp/interpreter/` or the compiler changes. `bash wisp/actor/build.sh` writes the
same file without building the daemon. More in
`wisp/actor/legacy-host/DEVELOPMENT.md`.

## Before pushing

```sh
cargo fmt --all
cargo fmt --manifest-path wisp/actor/legacy-host/Cargo.toml
cargo clippy --locked --workspace --all-targets --all-features -- -D warnings
```

Run the tests for what you changed (`cargo test --test NAME`); main runs the rest.
