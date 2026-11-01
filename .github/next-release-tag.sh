#!/usr/bin/env bash
# Print the tag to release commit $1 under, or nothing if it should not release.
# Run from a clone with full history and tags. The reason goes to stderr.
#
# Tags are v0.1.N: N is one above the highest v0.1.* tag, so they only increase.
set -euo pipefail
sha="$(git rev-parse "$1^{commit}")"

last="$(git tag --list 'v0.1.*' --sort=-version:refname | head -n 1)"
if [ -z "$last" ]; then
  echo "v0.1.0"
  exit 0
fi

if [ "$(git rev-parse "$last^{commit}")" = "$sha" ]; then
  echo "skip: $sha is already $last" >&2
  exit 0
fi
# CI runs can finish out of order. /releases/latest must never move backward.
if ! git merge-base --is-ancestor "$last" "$sha"; then
  echo "skip: $sha does not contain $last, so a newer commit already released" >&2
  exit 0
fi
# Everything since the last release, so a docs push after a code push still ships it.
if [ -z "$(git diff --name-only "$last" "$sha" -- . ':(exclude)*.md' ':(exclude)docs/**')" ]; then
  echo "skip: only docs changed since $last" >&2
  exit 0
fi

echo "v0.1.$((${last##*.} + 1))"
