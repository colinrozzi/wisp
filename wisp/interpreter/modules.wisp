; Modules give Wisp encapsulation and an explicit public interface. The top-level
; namespace is one flat global `$bindings`, written through `publish` and read
; through `lookup`; neither changes. Instead `$bindings` always holds the CURRENT
; module's env, and a registry holds every suspended module. Entering a module
; saves the current env and points `$bindings` at the target's env; leaving
; restores. Module context is thus a dynamically-scoped global, save/restored at
; module boundaries, matching the interpreter's existing session-global style.
;
; `exports` names made public via provide. `imports` pairs each name brought in by
; require with the module it came from (stored as (binding name (symbol module))),
; so re-requiring the same module is idempotent and only a DIFFERENT module
; providing the same name is a conflict.
(record module (name string) (env (list binding)) (exports (list string)) (imports (list binding)))
(global $modules (list module) mut 0)
(global $current-module string mut 0)
(global $current-exports (list string) mut 0)
(global $current-imports (list binding) mut 0)
; Cycle guard listing modules whose source is being evaluated right now.
(global $loading (list string) mut 0)

; The registry holds only SUSPENDED modules; the current one lives in $bindings /
; $current-exports / $current-imports / $current-module. Dropping the current name
; before re-adding it keeps that invariant and makes a save an upsert, not a dup.
(fn drop-module ((modules (list module)) (index s32) (name string) (out (list module))) (list module)
  (if (i32.ge_s index (list-len modules)) out
    (drop-module modules (i32.add index 1) name
      (if (string=? name (module.name (list-get modules index))) out
        (list-push out (list-get modules index))))))

(fn find-module ((modules (list module)) (index s32) (name string)) s32
  (if (i32.lt_s index 0) -1
    (if (string=? name (module.name (list-get modules index))) index
      (find-module modules (i32.sub index 1) name))))

(fn module-loaded? ((name string)) s32
  (i32.ge_s (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name) 0))

(fn name-member? ((names (list string)) (index s32) (name string)) s32
  (if (i32.ge_s index (list-len names)) 0
    (if (string=? name (list-get names index)) 1
      (name-member? names (i32.add index 1) name))))

(fn drop-name ((names (list string)) (index s32) (name string) (out (list string))) (list string)
  (if (i32.ge_s index (list-len names)) out
    (drop-name names (i32.add index 1) name
      (if (string=? name (list-get names index)) out
        (list-push out (list-get names index))))))

; Suspend the current module into the registry and make `name` current, loading
; its saved state if it was entered before or a fresh empty module otherwise.
(fn switch-module ((name string)) value
  (begin
    (global.set $modules
      (list-push (drop-module (global.get $modules) 0 (global.get $current-module) (list-new module))
        (module (global.get $current-module) (global.get $bindings) (global.get $current-exports) (global.get $current-imports))))
    (let (index (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name))
      (if (i32.lt_s index 0)
        (begin
          (global.set $bindings (list-new binding))
          (global.set $current-exports (list-new string))
          (global.set $current-imports (list-new binding))
          (global.set $current-module name)
          (nil))
        (let (target (list-get (global.get $modules) index))
          (begin
            (global.set $bindings (module.env target))
            (global.set $current-exports (module.exports target))
            (global.set $current-imports (module.imports target))
            (global.set $current-module name)
            (global.set $modules (drop-module (global.get $modules) 0 name (list-new module)))
            (nil)))))))

; (module NAME body...) evaluates its body in NAME's own fresh env, so its
; defines never leak into the requiring module. The body runs at top level.
(fn eval-module ((items (list value)) (depth s32) (top s32)) value
  (if (i32.eq top 0) (failure "module is only supported at top level")
    (if (i32.lt_s (list-len items) 2) (failure "module expects a name")
      (if (symbol? (list-get items 1))
        (let (previous (global.get $current-module))
          (begin
            (switch-module (symbol-name (list-get items 1)))
            (let (result (eval-body (declaration-tail items) 0 (list-new binding) depth 1 (nil)))
              (begin
                (switch-module previous)
                (if (failed? result) result (nil))))))
        (failure "module expects a symbol name")))))

; (provide name...) records names on the current module's export set.
(fn add-exports ((items (list value)) (index s32)) value
  (if (i32.ge_s index (list-len items)) (nil)
    (if (symbol? (list-get items index))
      (begin
        (global.set $current-exports (list-push (global.get $current-exports) (symbol-name (list-get items index))))
        (add-exports items (i32.add index 1)))
      (failure "provide expects symbols"))))

(fn eval-provide ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "provide is only supported at top level")
    (add-exports items 1)))

; Read source text from an explicit path through the actor's FILESYSTEM capability
; (theater:simple/filesystem read-file). Loading thus inherits the actor's sandbox:
; deny the capability and this read fails, so the actor cannot load new code from
; disk — the same boundary as any other file access. Returns a (text …) value or a
; failure. read-file yields (result (list u8) filesystem-error); on ok we decode the
; bytes to a string, on err we report the path as unreadable.
(fn read-source-from-path ((path string)) value
  (let (res (unmarshal (raw-invoke "theater:simple/filesystem" "read-file" (marshal (text path)))))
    (value-case res
      ((compound ty case-name fields)
        (if (string=? case-name "ok")
          (if (i32.ge_s (list-len fields) 1)
            (apply-bytes-to-string (list-push (list-new value) (list-get fields 0)))
            (failure (string-append "empty source: " path)))
          (failure (string-append "cannot read source: " path))))
      (else (failure (string-append "cannot read source: " path))))))

; Load an explicit source file as a module and evaluate its forms at top level. A
; well-formed file defines (module NAME ...), which registers NAME in $modules on
; completion.
(fn load-module-source ((path string)) value
  (let (source (read-source-from-path path))
    (value-case source
      ((text text-source)
        (let (forms (expand-source text-source path 0))
          (value-case forms
            ((sequence items) (eval-body items 0 (list-new binding) 0 1 (nil)))
            (else forms))))
      (else source))))

; Ensure NAME is instantiated: a no-op if already loaded. Without an explicit source
; path (require with no source), an unloaded module is an error — nothing is resolved
; implicitly. With a path, load it (guarded against dependency cycles).
(fn ensure-module-loaded ((name string) (path string)) value
  (if (string=? name (global.get $current-module)) (failure (string-append "module cannot require itself: " name))
    (if (module-loaded? name) (nil)
      (if (i32.eq (string-len path) 0)
        (failure (string-append "module is not loaded; give its source like (require name \"path\"): " name))
        (if (name-member? (global.get $loading) 0 name) (failure (string-append "module dependency cycle: " name))
          (begin
            (global.set $loading (list-push (global.get $loading) name))
            (let (loaded (load-module-source path))
              (begin
                (global.set $loading (drop-name (global.get $loading) 0 name (list-new string)))
                (if (failed? loaded) loaded
                  (if (module-loaded? name) (nil)
                    (failure (string-append "source did not define module " name))))))))))))

; The module a name was imported from, or "" if it is not an import.
(fn import-source ((imports (list binding)) (index s32) (name string)) string
  (if (i32.lt_s index 0) ""
    (if (string=? name (binding.name (list-get imports index)))
      (symbol-name (binding.item (list-get imports index)))
      (import-source imports (i32.sub index 1) name))))

; Bind each of a required module's exports into the current module, unqualified.
; Idempotent per source module; a clash with a different module is an error; a
; provided-but-undefined name is an error. Privates are never bound.
(fn bind-exports ((source-name string) (env (list binding)) (exports (list string)) (index s32)) value
  (if (i32.ge_s index (list-len exports)) (nil)
    (let (n (list-get exports index))
      (let (prior (import-source (global.get $current-imports) (i32.sub (list-len (global.get $current-imports)) 1) n))
        (if (string=? prior source-name) (bind-exports source-name env exports (i32.add index 1))
          (if (string-len prior)
            (failure (string-append "conflicting import: " (string-append n (string-append " from " (string-append prior (string-append " and " source-name))))))
            (let (v (lookup-local n env (i32.sub (list-len env) 1)))
              (if (failed? v) (failure (string-append "module " (string-append source-name (string-append " provides undefined name " n))))
                (begin
                  (publish n v)
                  (global.set $current-imports (list-push (global.get $current-imports) (binding n (symbol source-name))))
                  (bind-exports source-name env exports (i32.add index 1)))))))))))

(fn import-exports ((name string)) value
  (let (index (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name))
    (if (i32.lt_s index 0) (failure (string-append "required source did not define module " name))
      (let (m (list-get (global.get $modules) index))
        (bind-exports name (module.env m) (module.exports m) 0)))))

; Resolve NAME (loading it from the explicit PATH if it is not already loaded),
; then bind its provided names into the current module, unqualified.
(fn require-named ((name-form value) (path string)) value
  (if (symbol? name-form)
    (let (name (symbol-name name-form))
      (let (loaded (ensure-module-loaded name path))
        (if (failed? loaded) loaded (import-exports name))))
    (failure "require expects a symbol module name")))

; (require NAME "source") loads NAME from the explicit source (once) and binds its
; provides. (require NAME) with no source reuses an already-loaded NAME; nothing is
; resolved implicitly by name. The source is read through the actor's capabilities.
(fn eval-require ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "require is only supported at top level")
    (if (i32.eq (list-len items) 2)
      (require-named (list-get items 1) "")
      (if (i32.eq (list-len items) 3)
        (value-case (list-get items 2)
          ((text path) (require-named (list-get items 1) path))
          (else (failure "require source must be a string path")))
        (failure "require expects (require name) or (require name \"source\")")))))
