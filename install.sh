#!/bin/sh
# theater-repl installer.
#
#   curl -fsSL https://raw.githubusercontent.com/colinrozzi/wisp/main/install.sh | sh
#
# Downloads a prebuilt, self-contained `theater-repl` into ~/.local/bin. The binary
# carries its own actor + Theater runtime, so nothing else is needed: run
# `theater-repl` and it serves a live REPL on 127.0.0.1:7777.
set -eu

REPO="colinrozzi/wisp"
BIN="theater-repl"
DEST="${THEATER_REPL_BIN:-$HOME/.local/bin}"

os="$(uname -s)"
arch="$(uname -m)"
case "$os-$arch" in
  Linux-x86_64 | Linux-amd64) asset="theater-repl-linux-x86_64" ;;
  *)
    echo "no prebuilt theater-repl for $os-$arch." >&2
    echo "build from source: https://github.com/$REPO (actors/wisp-repl)" >&2
    exit 1
    ;;
esac

url="https://github.com/$REPO/releases/latest/download/$asset"
echo "downloading $asset ..."
mkdir -p "$DEST"
tmp="$(mktemp)"
if ! curl -fSL --progress-bar "$url" -o "$tmp"; then
  echo "download failed: $url" >&2
  echo "(is there a published release yet? see https://github.com/$REPO/releases)" >&2
  rm -f "$tmp"
  exit 1
fi
chmod +x "$tmp"
mv "$tmp" "$DEST/$BIN"
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
