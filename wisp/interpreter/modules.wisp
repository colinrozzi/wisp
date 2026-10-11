; Modules give Wisp encapsulation and an explicit public interface. The top-level
; namespace is one flat global `$bindings`, written through `publish` and read
; through `lookup`; neither changes. Instead `$bindings` always holds the CURRENT
; module's env, and a registry holds every suspended module. Entering a module
; saves the current env and points `$bindings` at the target's env; leaving
; restores. Module context is thus a dynamically-scoped global, save/restored at
; module boundaries, matching the interpreter's existing session-global style.
(record module (name string) (env (list binding)) (exports (list string)))
(global $modules (list module) mut 0)
(global $current-module string mut 0)
(global $current-exports (list string) mut 0)

; The registry holds only SUSPENDED modules; the current one lives in $bindings /
; $current-exports / $current-module. Dropping the current name before re-adding
; it keeps that invariant and makes a save an upsert rather than a duplicate.
(fn drop-module ((modules (list module)) (index s32) (name string) (out (list module))) (list module)
  (if (i32.ge_s index (list-len modules)) out
    (drop-module modules (i32.add index 1) name
      (if (string=? name (module.name (list-get modules index))) out
        (list-push out (list-get modules index))))))

(fn find-module ((modules (list module)) (index s32) (name string)) s32
  (if (i32.lt_s index 0) -1
    (if (string=? name (module.name (list-get modules index))) index
      (find-module modules (i32.sub index 1) name))))

; Suspend the current module into the registry and make `name` current, loading
; its saved env if it was entered before or a fresh empty env otherwise.
(fn switch-module ((name string)) value
  (begin
    (global.set $modules
      (list-push (drop-module (global.get $modules) 0 (global.get $current-module) (list-new module))
        (module (global.get $current-module) (global.get $bindings) (global.get $current-exports))))
    (let (index (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name))
      (if (i32.lt_s index 0)
        (begin
          (global.set $bindings (list-new binding))
          (global.set $current-exports (list-new string))
          (global.set $current-module name)
          (nil))
        (let (target (list-get (global.get $modules) index))
          (begin
            (global.set $bindings (module.env target))
            (global.set $current-exports (module.exports target))
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

; (provide name...) records names on the current module's export set. Brick A
; only records them; Brick B's require enforces them when it binds across modules.
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
