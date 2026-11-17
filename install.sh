#!/bin/sh
# theater-repl installer.
#
#   curl -fsSL https://raw.githubusercontent.com/colinrozzi/wisp/main/install.sh | sh
#
# Downloads a prebuilt, self-contained `theater-repl` into ~/.local/bin. The binary
# carries its own actor + Theater runtime, so nothing else is needed: run
# `theater-repl` and it serves a live REPL on 127.0.0.1:7777.
#
# It installs only an approved release (experiment field/SPEC-release.md 6): it
# fetches APPROVAL, the signed release.approve event, from colinrozzi.com (else
# from GitHub's latest release, if that carries one), checks its signature
# against the approver keys below, which are in this script and not taken from
# any server, and checks the download's size and sha256 against it. It needs
# openssl 3 (for Ed25519). THEATER_REPL_UNVERIFIED=1 skips all of that and takes
# GitHub's latest as it is (the old behaviour). "Approved" means one of these keys
# signed it, no more: the script does not repeat the server's other checks, so it
# is as strong as the weakest key listed (see release.rs).
set -eu

REPO="colinrozzi/wisp"
BIN="theater-repl"
DEST="${THEATER_REPL_BIN:-$HOME/.local/bin}"
# The approvers of wisp releases: [repo.wisp] approvers in server3's root copy
# (experiment site/server3/field-releases/repos.toml), and in release.rs.
APPROVERS="1908c021edbb19d46c8474cac5cd50731a097a822628c26345491b70ec514082 e24834e51845dc25438c5ba006e0db03ecf0273a624e5345f370e1bede907c3e"
SOURCES="https://colinrozzi.com/releases/wisp/latest https://github.com/$REPO/releases/latest/download"

os="$(uname -s)"
arch="$(uname -m)"
case "$os-$arch" in
  Linux-x86_64 | Linux-amd64) asset="theater-repl-linux-x86_64" ;;
  *)
    echo "no prebuilt theater-repl for $os-$arch." >&2
    echo "build from source: https://github.com/$REPO (wisp/actor)" >&2
    exit 1
    ;;
esac

die() { echo "install: $*" >&2; exit 1; }
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$DEST"

sha256() { if command -v sha256sum >/dev/null; then sha256sum "$1" | cut -c1-64; else shasum -a 256 "$1" | cut -c1-64; fi; }
# hex -> the bytes, in the shell alone (no xxd, no awk: an awk can't be trusted to
# print a byte, and the key always holds a 0x00; lux). Each pair becomes an octal
# escape and one printf writes them. The input is checked hex.
unhex() {
  h=$1 f=""
  while [ -n "$h" ]; do
    f="$f$(printf '\\%03o' "$((0x${h%"${h#??}"}))")"
    h=${h#??}
  done
  printf "$f"
}
ishex() { printf '%s' "$1" | grep -Eq "^[0-9a-f]{$2}\$"; }

# APPROVAL -> "tag sha256 size" of $asset on stdout, or exit
check_approval() {
  f=$1
  sig=$(head -n 1 "$f" | sed -n 's/^sig //p')
  ishex "$sig" 128 || die "the approval has no signature line"
  tail -c +134 "$f" > "$work/text"                       # "sig " + 128 hex + "\n" is 133 bytes
  id=$(sha256 "$work/text")
  n=$(grep -n -m 1 '^body [0-9][0-9]*$' "$work/text" | cut -d: -f1)
  [ -n "$n" ] || die "the approval has no body"
  head -n "$((n - 1))" "$work/text" > "$work/meta"
  len=$(sed -n "${n}s/^body //p" "$work/text")
  tail -n "+$((n + 1))" "$work/text" > "$work/body"
  [ "$(wc -c < "$work/body" | tr -d ' ')" = "$len" ] || die "the approval's body is not the $len bytes it says"
  author=$(sed -n 's/^author //p' "$work/meta")
  ishex "$author" 64 || die "the approval names no author key"
  case " $APPROVERS " in *" $author "*) ;; *) die "the approval is signed by $author, which is not an approver in this script" ;; esac
  # the author's key as PEM (an Ed25519 SubjectPublicKeyInfo is this prefix + the 32 bytes)
  unhex "302a300506032b6570032100$author" > "$work/pub.der"
  openssl pkey -pubin -inform DER -in "$work/pub.der" -out "$work/pub.pem" 2>/dev/null ||
    die "openssl 3 is needed to check the release (or THEATER_REPL_UNVERIFIED=1)"
  unhex "$sig" > "$work/sig"
  printf '%s' "$id" > "$work/id"
  openssl pkeyutl -verify -pubin -inkey "$work/pub.pem" -rawin -in "$work/id" -sigfile "$work/sig" >/dev/null 2>&1 ||
    die "the approval's signature does not check"
  [ "$(sed -n 's/^kind //p' "$work/meta")" = release.approve ] || die "the approval is not a release.approve"
  [ "$(sed -n 's/^scope //p' "$work/meta")" = release/approve/wisp ] || die "the approval is not for wisp"
  sed '/^$/q' "$work/body" > "$work/headers"
  [ "$(sed -n 's/^repo //p' "$work/headers")" = wisp ] || die "the approval is not for repo wisp"
  tag=$(sed -n 's/^tag //p' "$work/headers")
  printf '%s' "$tag" | grep -Eq '^v[0-9]{1,6}\.[0-9]{1,6}\.[0-9]{1,6}$' || die "a bad tag in the approval"
  line=$(sed '1,/^$/d' "$work/body" | grep "^$asset " || true)
  [ "$(printf '%s\n' "$line" | grep -c .)" = 1 ] || die "the approval lists $asset not exactly once"
  set -- $line
  ishex "$2" 64 && printf '%s' "$3" | grep -Eq '^[0-9]{1,12}$' || die "a bad artifact line for $asset"
  echo "$tag $2 $3"
}

if [ "${THEATER_REPL_UNVERIFIED:-}" = 1 ]; then
  url="https://github.com/$REPO/releases/latest/download/$asset"
  echo "downloading $asset (NOT verified) ..."
  curl -fSL --progress-bar "$url" -o "$work/bin" || die "download failed: $url"
else
  got=""
  for base in $SOURCES; do
    if ! curl -fsSL "$base/APPROVAL" -o "$work/APPROVAL" 2>/dev/null; then
      echo "no release approval at $base; trying the next source" >&2
      continue
    fi
    # an approval that is there but fails a check stops here: never fall back past it
    checked=$(check_approval "$work/APPROVAL") || exit 1
    set -- $checked
    tag=$1 want=$2 size=$3
    echo "downloading $asset $tag ($size bytes) from $base ..."
    curl -fSL --progress-bar "$base/$asset" -o "$work/bin" || die "download failed: $base/$asset"
    [ "$(wc -c < "$work/bin" | tr -d ' ')" = "$size" ] || die "the download is not the approved $size bytes"
    [ "$(sha256 "$work/bin")" = "$want" ] || die "the download's sha256 is not the approved $want"
    echo "checked: $tag, sha256 $want, approved"
    got=1
    break
  done
  [ -n "$got" ] || die "no source has a release approval (THEATER_REPL_UNVERIFIED=1 installs GitHub's latest unchecked)"
fi
chmod +x "$work/bin"
mv "$work/bin" "$DEST/$BIN"
echo "installed $DEST/$BIN"

case ":$PATH:" in
  *":$DEST:"*) ;;
  *) echo "note: $DEST is not on your PATH — add it:  export PATH=\"$DEST:\$PATH\"" ;;
esac

cat <<EOF

done. usage:

  $BIN serve &                     # start the daemon (holds sessions) on :7777
  id=\$($BIN new)                   # create a session, capture its id
  $BIN eval \$id '(i32.add (i32.const 40) (i32.const 2))'   # => 42
  $BIN eval \$id -  <<'LISP'        # or pipe a form on stdin (no quoting)
  (define greet (lambda (who) (string-append "hi " who)))
  LISP
  $BIN list                        # sessions on the daemon
  $BIN eval \$id '(help)'           # catalog of Theater host verbs
EOF
