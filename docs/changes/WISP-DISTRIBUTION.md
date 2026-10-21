# Wisp Distribution (capability-bounded, explicit-source)

**Status**: Bricks 1–2 done (filesystem + network). Registry/versioning later.
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
(reload NAME)           ; re-read NAME from its SOURCE and refresh every importer
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

What a user sees in practice:

- **Provided names come in unqualified.** After
  `(require sessions "experiment/lib/sessions.wisp")` you call `(session-actor "bob")`,
  not `sessions.session-actor`; there is no `NAME.` prefix. (Qualified access is the
  planned `prefix-in` layer from [WISP-MODULES.md](WISP-MODULES.md), not yet built.)
- **An already-loaded module is not re-read, even with a source.** Load-once is keyed
  on `NAME`, so a second `(require NAME "path")` in the same session reuses the
  in-memory instance and ignores the file. If you edit a library, the session that
  already loaded it keeps the old definitions; test the edit in a fresh session.
- The REPL documents both under `(help "modules")`.

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

The daemon's filesystem handler is sandboxed to a managed directory (`new_dir: true`),
and paths are sandbox-relative (absolute paths are rejected). So a library is any
`.wisp` file the actor's filesystem capability can reach — authored in-session via the
write capability, or placed in a directory the deployment roots the handler at. Dogfood
(v0.1.8): `(fs-write "greet.wisp" "(module greet (provide …) …)")` then
`(require greet "greet.wisp")` — authored and loaded entirely inside the actor's
sandbox, no privileged channel.

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

### No caching, no store (deliberate)

`require` is a **one-time cold read**, not a hot path: the module registry (`$modules`)
already gives in-memory load-once, so `(require greet …)` reads the source once and every
later `(require greet)` reuses the in-memory instance. A fresh session re-reads once —
cheap, and not worth a disk cache + invalidation surface. The **`SOURCE` is the source of
truth** (a path or a URL); there is nothing to "install" and no store to keep in sync.

The dogfood surfaced one nuance that is **not** caching: by default the daemon's
`FileSystemHandler` uses `new_dir: true`, so its sandbox is an ephemeral empty directory.
For *local-file* libraries to persist across sessions, the deployment points the actor's
filesystem capability at a real directory. That is the capability boundary ("where may
this actor read"), not a cache. URL libraries sidestep it entirely: the library *is* the
URL, fetched once per session.

### Reloading an edited module: `(reload NAME)`

Load-once means editing a library does not change a session that already loaded it.
Rather than modification-time checks, the session picks up an edit **explicitly**:

```lisp
(require counter "lib/counter.wisp")   ; first load; the module remembers its SOURCE
;; ... edit lib/counter.wisp ...
(reload counter)                        ; -> () or the list of dropped names, e.g. (bump)
```

- **Same source.** `reload` re-reads the path/URL the module was originally loaded
  from (the module record keeps it). Reloading a module that is not loaded is an
  error, and so is reloading one defined in-session with `(module …)` (it has no
  source), or the current module. `require` itself stays load-once.
- **Fresh environment.** The new source is evaluated into an empty module env that
  replaces the old one, so definitions removed from the file disappear.
- **Every importer is refreshed.** Each module in the session that imported `NAME`
  (the `imports` lists record each name's source module) has its copies overwritten
  with the new values, not just the module that called `reload`.
- **Dropped names.** If the new version no longer provides a name an importer had, it
  is unbound there, and `reload` returns the dropped names (`()` when none). Names the
  new version *adds* are not bound automatically; `(require NAME)` again picks them up.
- **Shallow.** `NAME`'s own dependencies are already loaded and are reused, not re-read;
  reload them first if they changed too.
- **All or nothing.** If the new source fails (unreadable, an eval error, no
  `(module NAME …)`, or a provided name it does not define), the old module and every
  binding stay as they were and `reload` returns the error.

Known limit: a value copied by hand, e.g. `(define f some-imported-fn)`, is not an
import, so `reload` does not touch it; `f` keeps the old code. (Top-level names inside
a copied closure still resolve in the module's current env, since a closure finds its
home module by name.)

### Granting a persistent filesystem root (config)

A `theater-repl` **config file** (TOML) grants the REPL actor's filesystem capability —
the human's slice of what an actor manifest declares in real Theater:

```toml
[filesystem]
root = "~/wisp/lib"    # the directory require may read libraries from
# read = true          # optional (default true)
# write = true         # optional (default true)
```

Resolution: `--config <path>`, else `~/.config/theater-repl/config.toml` if it exists,
else **no config → the historical ephemeral sandbox** (opt-in, so existing users see no
new filesystem exposure). With a `root`, the sandbox becomes that real directory
(`new_dir: false`), created if missing, and persists across sessions and restarts. Then
dropping `foo.wisp` in the root and `(require foo "foo.wisp")` just works — proven live
against a locally-built daemon (v0.1.8+config). This only affects `serve`/`repl`; client
commands talk to an already-running daemon.

**Brick 2 (done):** `SOURCE` as an `http`/`https` URL fetches via `http-client`
(`read-source-from-url`), decoding the `http-response` body (`option (list u8)`) to a
string and evaluating it — in memory, no write-back. The locator's form selects the
capability (`url-source?`: a leading `http`), so a library can be shared simply as a
URL. Tested in the engine suite: `(require netmod "https://…")` fetches over a mocked
`request` and binds its provides.

**Later layers (hang off this core, no rebuild):**
- **A registry/catalog** — name → source resolution as an *opt-in library*, not core
  (keeps the "no implicit resolver in the core" stance; any resolver is sugar over
  `(require NAME SOURCE)`).
- **Versioning** — a version alongside `NAME` in identity.
- **The compiled path** — a module compiled to a Pack package as the distribution
  artifact (the dual-back-end direction), sharing this same `require` boundary.
