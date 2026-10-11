# Wisp Modules (Racket-modeled)

**Status**: Agreed — ready to implement (decisions settled 2026-10-11; v1 scope below)
**Started**: 2026-10-11
**Related**: [../../wisp/interpreter/](../../wisp/interpreter/) (the interpreter this
extends), the Wisp dual-back-end direction (a module is the compilation unit the future
Wisp→wasm compiler turns into a Pack package).

## Objective

Give **Wisp** (the dynamic Lisp) a real module system: encapsulation, an explicit
public interface, and a Racket-style `provide`/`require` import algebra — scaled to
Wisp's tree-walking interpreter. Modules replace the current single flat global
namespace (`$bindings`), where everything a session `define`s or `(include)`s lands in
one bucket and any two libraries defining the same name collide.

This is **load-bearing core**, built as a real structure (not a naming-convention
hack over the flat env), for two reasons:

1. **Namespacing can't be retrofitted.** Once people publish libraries against a flat
   namespace, adding modules later breaks all of them. The substrate has to exist
   before the ecosystem does.
2. **A module is the compilation unit.** When Wisp gains a wasm back-end, a
   `(module m (provide …) …)` compiles to a Pack package whose interface *is* its
   provides. Designing the boundary now means the compiler inherits a real interface
   instead of inventing one.

The model is **Racket's**, because it is a proven, small-core-big-reach design:
private-by-default, explicit `provide`, and an import algebra (`only-in`, `prefix-in`,
`rename-in`, `except-in`) that is pure front-end sugar over one primitive — "resolve
name N against module M's exports."

## Surface

A library explicitly wraps itself in a module and declares its interface:

```lisp
(module list
  (require core)                 ; dependencies (other modules)
  (provide map filter fold)      ; the public interface

  (define (map f xs) ...)        ; exported (it's in provide)
  (define (fold f acc xs) ...)   ; exported
  (define (helper x) ...))       ; PRIVATE — not provided, invisible to requirers
```

- `(module NAME form ...)` — defines a module. Its forms evaluate in the module's own
  fresh environment. Definitions are **private by default**.
- `(provide name ...)` — adds names to the module's export set.
- `(require spec ...)` — import from other modules (see the algebra below).

The REPL session, and any plain top-level file, is itself a module (the implicit
`main`/user module); `require` brings libraries into it.

### `require` algebra

**v1 (core):**
- `(require m)` — bring `m`'s provided names into the requiring module, **unqualified**.
  Two requires that bring the same name is a **conflict error** (Racket semantics) —
  resolve it with the forms below. (This keeps simple programs clean and makes clashes
  loud rather than silently shadowing.)

**Layered later (designed-for; pure desugaring over the core resolver — no rebuild):**
- `(require (only-in m a b))` — bring only `a`, `b`.
- `(require (prefix-in p m))` — bring exports with a prefix (this is also how you get
  *qualified* access, e.g. `(require (prefix-in list/ list))` → `list/map`).
- `(require (rename-in m (old new)))` — import with renaming.
- `(require (except-in m x))` — everything except `x`.
- `(require (combine-in …))`, re-export via `(provide (all-from-out m))`, etc.

## Model (the structure)

### A module is first-class

```
Module = {
  name     : string
  env      : (list binding)     ; the module's own definitions (private)
  exports  : (list string)      ; names made public via provide
  imports  : (list binding)     ; names brought in from required modules
  requires : (list string)      ; dependency module names (for load-once + cycles)
}
```

A **module registry** maps name → Module. Modules are **instantiated once** (a second
`require` reuses the loaded instance), and the dependency graph is **cycle-checked**
(reusing the `include-seen` pattern already in `loading.wisp`).

### Resolution

Inside module `M`, a bare name resolves in order:
1. `M`'s own `env` (its private + its own defines),
2. `M`'s `imports` (names brought in by `require`),
3. the **prelude** (builtins + the base namespace everyone sees unqualified).

It can never reach another module's private `env` — only its `exports`, and only via
an `import`. `provide` is enforced: requiring a non-exported name is an error.

### Interpreter changes

- **`eval` gains a current-module context.** Today it threads `env`/`depth`/`top`; add
  the module so `define` writes into the right module `env` and unqualified lookup
  resolves module-locally. This is the real structural change vs. flat-global — and the
  part that is painful to add later, which is the argument for doing it now.
- **A module registry global**, structured (replacing the single flat `$bindings`
  root; the prelude becomes a base module).
- **`require` loads**: resolve source (via the existing host `resolve-path` /
  `read-source`), evaluate the module's forms in a fresh module env, register it,
  load-once, cycle-check.
- **`(include "…")` stays** as a raw textual splice into the *current* module — for
  multi-file single-module programs. `require` is the module boundary; `include` is
  not. (Clean split: `include` = "more of my own source"; `require` = "someone else's
  module.")

### The compiler seam (future)

A module is the natural **compilation unit**: `(module m (provide f g) …)` → a Pack
package exporting `f`, `g`. The `provide` set is the package interface; privates don't
escape. Nothing here needs the compiler to exist yet — but getting the boundary right
now means the back-end compiles a module straight to a package.

## Scope

**v1 (this campaign):** `module` / `provide` / `require m`; the module registry;
current-module in `eval`; private-by-default resolution with conflict detection;
load-once + cycle detection; REPL-and-top-level-file as a module; the prelude as a base
module.

**Layered later (hang off the v1 core without rebuilding it):** the `only-in` /
`prefix-in` / `rename-in` / `except-in` algebra; `provide` of macros (with whatever
phase story Wisp settles on); re-export; submodules; versioning + fetch/registry (how
a library's *source* reaches the bundle in the first place — the separate "distribution
layer").

## Decisions (agreed 2026-10-11)

1. **`require` default — Racket semantics: unqualified, with conflict = error.**
   `(require m)` brings `m`'s provided names in unqualified; two imports of the same
   name is an error, resolved with the layered `only-in`/`prefix-in`/`rename-in`/
   `except-in` forms. A local `define` may shadow an import (REPL ergonomics); two
   *imports* colliding is the error case. The core resolver supports qualified access
   too — it's just spelled `(require (prefix-in …))` once that layer lands.
2. **Qualifier/prefix character — deferred with `prefix-in` (not a v1 blocker).**
   When `prefix-in` lands it needs the reader to allow the separator in symbol
   constituents; leaning `:` (Racket-ish). Revisit at that layer.
3. **Locating a module's source — by name → bundle path** (like `include`
   resolution), not an explicit file path, so programs don't hardcode file layout.
4. **Macros across modules — deferred.** v1 provides functions/values only; `provide`
   of a macro waits for Wisp's macro/phase story to get its own design pass.
