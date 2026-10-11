# Wisp Distribution (capability-bounded, explicit-source)

**Status**: Brick 1 done (filesystem). Network tier + registry later.
**Started**: 2026-10-11
**Related**: [WISP-MODULES.md](WISP-MODULES.md) (the module boundary this delivers to);
`wisp/interpreter/modules.wisp` (the implementation); `wisp/actor/` (the capabilities).

## Objective

Let people build and share **Wisp libraries** — author a module once, `require` it from
any session — without weakening the actor's sandbox. The module system gave us the
*boundary* (`module`/`provide`/`require`); distribution is the *delivery*: where a
library's source lives, how `require` finds it, and how it reaches the machine.

## The governing principle: loading inherits the actor's capabilities

Code loading is **not** a privileged side channel. `require` loads source through the
**same Theater capabilities** the actor already holds for any I/O — `theater:simple/filesystem`
and (later) `theater:simple/http-client`. Those handlers are the capability gates: a
real Theater deployment grants or withholds them per-actor in the manifest. So:

- An actor **with** the filesystem capability can `require` a library from disk.
- An actor **with** the network capability can `require` one from a URL (and, with a
  filesystem write grant, cache it locally).
- An actor **denied** both can only use what it already holds: modules compiled into
  its image, and modules already loaded this session. **It cannot pull new code.**

This falls out of Theater's existing machinery — no new enforcement, no outside
`install` command (that would route around the sandbox). `require` runs *inside* the
actor and is bounded by exactly what the actor is allowed to do. `require` *is* the
install.

## Surface: explicit source, no search path

```lisp
(require NAME SOURCE)   ; load NAME from exactly SOURCE (once), bind its provides
(require NAME)          ; reuse an already-loaded NAME; nothing resolves by name
```

- **`NAME`** is the module's identity — what load-once keys on and whose `provide`d
  names get bound into the requiring module, unqualified.
- **`SOURCE`** is an explicit locator string: *exactly* where to find it. There is **no
  resolver and no search-path tree-walking** — "this is where to find the thing I
  want." That keeps the policy (order, shadowing, "which `foo` did I get?") out of the
  core, and avoids freezing an implicit convention we would have to live with forever.
- **`(require NAME)` with no source** reuses `NAME` if it is already loaded (image or a
  prior require); otherwise it errors and tells you to give the source. Nothing loads
  implicitly by name.

```lisp
(require list "lib/list.wisp")   ; first time: read + instantiate from here
(require list)                    ; later: already loaded, just rebind its provides
```

The `SOURCE` form selects the capability:

- **Brick 1 — filesystem path** → `theater:simple/filesystem read-file`. Done.
- **Later — URL** (scheme-detected) → `theater:simple/http-client request`, with an
  optional write-back into a local store (both capability-gated).

## How it works (Brick 1)

In `wisp/interpreter/modules.wisp`:

- `read-source-from-path` reads `SOURCE` through the filesystem capability
  (`raw-invoke "theater:simple/filesystem" "read-file"`), decoding the returned
  `(result (list u8) filesystem-error)` — bytes→string on `ok`, a "cannot read source"
  failure on `err`. **Deny the capability and this read fails**, so loading inherits the
  sandbox.
- `load-module-source` evaluates the file's forms at top level; a well-formed file
  defines `(module NAME …)`, which registers `NAME` (via the module registry from
  [WISP-MODULES.md]).
- `ensure-module-loaded` is a no-op if `NAME` is already loaded; with no source path an
  unloaded module is an error (no implicit resolution); with a path it loads once,
  guarded against dependency cycles (`$loading`).
- `require-named` then binds `NAME`'s provides into the requiring module, unqualified
  (the Brick B import algebra: idempotent per source module, conflict on a *different*
  module providing the same name, privates never bound).

Because the filesystem handler in the daemon is sandboxed to the `serve` cwd, the
natural local store today is **project-local** — write `lib/foo.wisp`, then
`(require foo "lib/foo.wisp")` from a session in that directory.

The old `wisp-source` bundle (`resolve-path`/`read-source`) is **no longer** on the
`require` path; it remains only for `(include …)` and built-in/image modules. This
supersedes Brick B's implicit `NAME.wisp` lookup.

## Lineage

Racket's package model stratifies as *package* (distribution unit, fetched) → provides
*collections/modules* (`require` names) → resolved via a load path. Wisp keeps the
module as the unit and the `provide` set as the interface, but — for now — makes the
*source explicit* instead of search-path-resolved. The capability-bounded twist is
Wisp/Theater's own: Racket trusts the filesystem; Wisp subjects code loading to the
actor's sandbox.

## Scope

**Brick 1 (done):** `(require NAME SOURCE)` reading a filesystem path through the
filesystem capability; `(require NAME)` reuse-only; load-once; cycle guard; the import
algebra from the module campaign. Tested: engine suite loads a module from an explicit
path via a mocked `read-file` and reuses it (load-once); unit suite covers the no-source
and non-string-source errors.

**Later layers (hang off this core, no rebuild):**
- **Network tier** — `SOURCE` as a URL via `http-client`; fetch a library's source.
- **Local store + write-back** — caching a fetched library to disk (`write-file`), so a
  URL require installs it for next time; a conventional store dir / `$WISP_PATH`.
- **A registry/catalog** — name → source resolution as a *library*, not core (keeps the
  "no implicit resolver in the core" stance; any resolver is opt-in sugar over
  `(require NAME SOURCE)`).
- **Versioning** — a version alongside `NAME` in identity + store layout.
- **The compiled path** — a module compiled to a Pack package as the distribution
  artifact (the dual-back-end direction), sharing this same `require` boundary.
