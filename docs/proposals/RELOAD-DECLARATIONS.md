# Reload of modules that declare types, globals, traits or instances

Status: approved by tova (2026-10-25, a to f, b and c over always-replace) and
built in ticket 0037 (`modules.wisp`, "Declarations and reload"; tested by
`reload_declarations_session` in `tests/interpreter_engine.rs`).
Written 2026-10-25 against `ticket-0037` (base `efb58d1`).

## The problem

`(reload M)` fails for any module that declares a `record`, `variant`,
`global`, `trait` or `instance`: "invalid or already defined type name",
"global is already declared", and so on (found by ines in ticket 0031).

Why: `reload` gives M a fresh **env** (`modules.wisp`, `reload-named`), but
these declarations do not live in the env. They live in session-wide tables:

| Declaration | Table | Check that fails |
|---|---|---|
| `record`, `variant` | `$types` (`types.wisp:2`) | `known-type?` in `eval-record` / `eval-variant` (`types.wisp:120`, `:163`) |
| `global` | `$session-globals` (`globals.wisp:5`) | `global-index` in `declare-global` (`globals.wisp:42`) |
| `trait` | `$traits` (`traits.wisp:5`) | `trait-index` in `declare-trait` (`traits.wisp:35`), `method-owner` (`:52`) |
| `instance` | `$instances` (`traits.wisp:6`) | `instance-index` in `declare-instance` (`traits.wisp:74`) |

The tables do not record which module made a declaration.

A record or variant value is `(aggregate type-name id case-name fields)`. The
`id` is the index of its entry in `$types`. `match` reads the schema with
`(list-get $types id)` (`matching.wisp:42`), and `value-has-type?` and the
accessors compare ids. So an entry in `$types` must never be removed or
moved while a value can still refer to it.

## Proposal

a. **Ownership.** Record which module made each type, global, trait and
   instance declaration. During `(reload M)`, M can declare again only the
   names that M declared before. A name that another module declared still
   conflicts. A duplicate inside one file is still an error.

b. **Types.** If the new declaration is the same as the old one (same kind,
   same fields or cases), keep the old id. Old values then stay fully valid.
   This is the usual case: you changed a function, not the record.
   If the declaration changed, add a new entry with a new id, and the name
   resolves to the new entry. (`find-type-index` already searches from the
   newest entry.) Old values still print and still `match`, against their old
   schema. But they fail the checks of the new type: typed parameters, the
   new accessors, and constructors that expect the new type. Nothing converts
   them.

c. **Globals.** A `mut` global whose type is the same keeps its current
   value, as `defonce` does, so a reload does not reset the module's state.
   Otherwise the global takes the new initializer. A `const` always does: it
   holds no state, and keeping it would hide an edit to its initializer. To
   reset a `mut` global, `(global.set $g value)` after the reload (the
   `(help "modules")` text says so).

d. **Traits.** If the trait is the same, keep it. If it changed, replace it.
   If another module has an instance of a changed trait, the reload fails, and
   the error names that module. M's own instances are removed and then
   declared again from the new source.

e. **Removed declarations.** If M's new source does not declare a name, the
   declaration is retired: the name becomes free again. Old values keep their
   `$types` entry.

f. **Failure.** If the reload fails, restore all four tables with the old
   module. The tables are value lists, so a snapshot is enough.

### The alternative

For (b) and (c): always replace. Every changed or unchanged type gets a new
id, and every global is reset. This is simpler to build, but each reload then
breaks every live value of the module's types and clears its state. I
recommend (b) and (c) as written above.
