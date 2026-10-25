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
; providing the same name is a conflict, and so (reload NAME) can find every copy.
; `source` is the explicit path/URL the module was loaded from, or "" for a module
; defined in-session; reload re-reads it.
(record module (name string) (env (list binding)) (exports (list string)) (imports (list binding)) (source string))
(global $modules (list module) mut 0)
(global $current-module string mut 0)
(global $current-exports (list string) mut 0)
(global $current-imports (list binding) mut 0)
(global $current-source string mut 0)
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
        (module (global.get $current-module) (global.get $bindings) (global.get $current-exports) (global.get $current-imports) (global.get $current-source))))
    (let (index (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name))
      (if (i32.lt_s index 0)
        (begin
          (global.set $bindings (list-new binding))
          (global.set $current-exports (list-new string))
          (global.set $current-imports (list-new binding))
          (global.set $current-source "")
          (global.set $current-module name)
          (nil))
        (let (target (list-get (global.get $modules) index))
          (begin
            (global.set $bindings (module.env target))
            (global.set $current-exports (module.exports target))
            (global.set $current-imports (module.imports target))
            (global.set $current-source (module.source target))
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
            (let (result (eval-module-body (declaration-tail items) 0 depth))
              (begin
                (switch-module previous)
                (if (failed? result) result (nil))))))
        (failure "module expects a symbol name")))))

; Evaluate a module's forms in order. A failure names the form it came from.
(fn eval-module-body ((forms (list value)) (index s32) (depth s32)) value
  (if (i32.ge_s index (list-len forms)) (nil)
    (let (v (eval (list-get forms index) (list-new binding) (i32.add depth 1) 1))
      (if (failed? v)
        (failure (string-append "in " (string-append (form-summary (list-get forms index)) (string-append ": " (failure-message v)))))
        (eval-module-body forms (i32.add index 1) depth)))))

; A short picture of a source form for an error: the head and the atoms after it,
; up to three items, then "..." for the rest: (define web-connection ...).
(fn form-summary ((form value)) string
  (value-case form
    ((sequence items) (summary-items items 0 "("))
    (else (show-syntax form))))
(fn summary-items ((items (list value)) (index s32) (out string)) string
  (if (i32.ge_s index (list-len items)) (string-append out ")")
    (let (sep (if (i32.eq index 0) "" " "))
      (if (i32.or (i32.ge_s index 3) (sequence? (list-get items index)))
        (string-append out (string-append sep "...)"))
        (summary-items items (i32.add index 1) (string-append out (string-append sep (show-syntax (list-get items index)))))))))
(fn show-syntax ((v value)) string
  (value-case v
    ((integer-literal n) (let (q (quote-value v)) (if (failed? q) (show-wide n) (show q))))
    (else (show v))))

; (provide name...) records names on the current module's export set.
(fn add-exports ((items (list value)) (index s32)) value
  (if (i32.ge_s index (list-len items)) (nil)
    (if (symbol? (list-get items index))
      (begin
        (let (name (symbol-name (list-get items index)))
          (global.set $current-exports
            (if (name-member? (global.get $current-exports) 0 name) (global.get $current-exports)
              (list-push (global.get $current-exports) name))))
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

; Read source text from a URL through the actor's HTTP-CLIENT capability
; (theater:simple/http-client request). Same boundary story as the filesystem: deny
; the capability and the fetch fails, so the actor cannot pull remote code. GET the
; url; request yields (result http-response string); on ok, the http-response's body
; (option (list u8), field index 2) is decoded to a string. There is no cache — the
; module lives in memory once loaded (load-once), so this is a one-time read.
(fn http-response-body ((resp value) (url string)) value
  (value-case resp
    ((aggregate name id case-name fields)
      (if (i32.ge_s (list-len fields) 3)
        (value-case (list-get fields 2)
          ((compound oty ocase ofields)
            (if (i32.and (string=? ocase "some") (i32.ge_s (list-len ofields) 1))
              (apply-bytes-to-string (list-push (list-new value) (list-get ofields 0)))
              (failure (string-append "empty response body: " url))))
          (else (failure (string-append "unexpected response body: " url))))
        (failure (string-append "unexpected response: " url))))
    (else (failure (string-append "unexpected response: " url)))))
(fn read-source-from-url ((url string)) value
  (let (res (unmarshal (raw-invoke "theater:simple/http-client" "request" (http-request-blob "GET" url))))
    (value-case res
      ((compound ty case-name fields)
        (if (i32.and (string=? case-name "ok") (i32.ge_s (list-len fields) 1))
          (http-response-body (list-get fields 0) url)
          (failure (string-append "cannot fetch source: " url))))
      (else (failure (string-append "cannot fetch source: " url))))))

; The source locator's form selects the capability: an http(s) URL fetches over the
; network, anything else is a filesystem path. (No resolver — the locator is explicit.)
(fn char-at ((s string) (i s32)) s32
  (if (i32.lt_s i (string-len s)) (string-ref s i) -1))
(fn url-source? ((s string)) s32
  (i32.and (i32.eq (char-at s 0) 104)
    (i32.and (i32.eq (char-at s 1) 116)
      (i32.and (i32.eq (char-at s 2) 116) (i32.eq (char-at s 3) 112)))))
(fn read-source-at ((source string)) value
  (if (url-source? source) (read-source-from-url source) (read-source-from-path source)))

; Load an explicit source as a module and evaluate its forms at top level. A
; well-formed file defines (module NAME ...), which registers NAME in $modules on
; completion.
(fn load-module-source ((path string)) value
  (let (source (read-source-at path))
    (value-case source
      ((text text-source)
        (let (forms (expand-source text-source path 0))
          (value-case forms
            ((sequence items) (source-failure (eval-body items 0 (list-new binding) 0 1 (nil)) path))
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
          (let (mark (load-mark-now))
            (begin
              (global.set $loading (list-push (global.get $loading) name))
              (let (loaded (load-module-source path))
                (begin
                  (global.set $loading (drop-name (global.get $loading) 0 name (list-new string)))
                  (let (problem (if (failed? loaded) loaded (loaded-module-problem name)))
                    (if (failed? problem)
                      (roll-back-load mark (failure (string-append "cannot load module " (string-append name (string-append ": " (failure-message problem))))))
                      (set-module-source name path))))))))))))

; Why a load that evaluated cleanly still did not give a usable NAME, or nil.
(fn loaded-module-problem ((name string)) value
  (let (index (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name))
    (if (i32.lt_s index 0) (failure (string-append "source did not define module " name))
      (let (m (list-get (global.get $modules) index))
        (let (missing (undefined-export (module.env m) (module.exports m) 0))
          (if (string-len missing)
            (failure (string-append "module " (string-append name (string-append " provides undefined name " missing))))
            (nil)))))))

(fn failure-message ((v value)) string
  (value-case v ((failure message) message) (else (show v))))

; A failed load leaves nothing behind, so a later require reads the file again
; and can declare the same names. The mark is taken before the load: the modules
; that existed then, and the sizes of the session-wide tables a module's forms add
; to (types, globals, traits, instances, macros). Rolling back drops every module
; registered since (the half-built one and any dependency it loaded) and shrinks
; the tables back. A type keeps its slot, because values carry its index as their
; id, but loses its name.
(record load-mark (modules (list string)) (types s32) (globals s32) (traits s32) (instances s32) (macros s32))

(fn module-names ((modules (list module)) (index s32) (out (list string))) (list string)
  (if (i32.ge_s index (list-len modules)) out
    (module-names modules (i32.add index 1) (list-push out (module.name (list-get modules index))))))

(fn load-mark-now () load-mark
  (load-mark (module-names (global.get $modules) 0 (list-new string))
    (list-len (global.get $types)) (list-len (global.get $session-globals)) (list-len (global.get $traits))
    (list-len (global.get $instances)) (list-len (global.get $session-macros))))

(fn keep-modules ((modules (list module)) (index s32) (names (list string)) (out (list module))) (list module)
  (if (i32.ge_s index (list-len modules)) out
    (keep-modules modules (i32.add index 1) names
      (if (name-member? names 0 (module.name (list-get modules index))) (list-push out (list-get modules index)) out))))

(fn unname-types ((types (list named-type)) (index s32) (keep s32) (out (list named-type))) (list named-type)
  (if (i32.ge_s index (list-len types)) out
    (unname-types types (i32.add index 1) keep
      (list-push out
        (let (t (list-get types index))
          (if (i32.lt_s index keep) t (named-type "" (named-type.id t) (named-type.variant? t) (named-type.schema t))))))))

(fn list-prefix ((items (list T)) (n s32) (index s32) (out (list T))) (list T)
  (where T)
  (if (i32.ge_s index n) out
    (list-prefix items n (i32.add index 1) (list-push out (list-get items index)))))

(fn roll-back-load ((mark load-mark) (error value)) value
  (begin
    (global.set $modules (keep-modules (global.get $modules) 0 (load-mark.modules mark) (list-new module)))
    (if (i32.gt_s (list-len (global.get $types)) (load-mark.types mark))
      (begin (global.set $types (unname-types (global.get $types) 0 (load-mark.types mark) (list-new named-type))) 0) 0)
    (if (i32.gt_s (list-len (global.get $session-globals)) (load-mark.globals mark))
      (begin (global.set $session-globals (list-prefix (global.get $session-globals) (load-mark.globals mark) 0 (list-new global-binding))) 0) 0)
    (if (i32.gt_s (list-len (global.get $traits)) (load-mark.traits mark))
      (begin (global.set $traits (list-prefix (global.get $traits) (load-mark.traits mark) 0 (list-new trait-definition))) 0) 0)
    (if (i32.gt_s (list-len (global.get $instances)) (load-mark.instances mark))
      (begin (global.set $instances (list-prefix (global.get $instances) (load-mark.instances mark) 0 (list-new trait-implementation))) 0) 0)
    (if (i32.gt_s (list-len (global.get $session-macros)) (load-mark.macros mark))
      (begin (global.set $session-macros (list-prefix (global.get $session-macros) (load-mark.macros mark) 0 (list-new binding))) 0) 0)
    error))

; Remember where a just-loaded (suspended) module came from, for (reload NAME).
(fn set-module-source ((name string) (path string)) value
  (let (index (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name))
    (if (i32.lt_s index 0) (nil)
      (let (m (list-get (global.get $modules) index))
        (begin
          (global.set $modules
            (list-push (drop-module (global.get $modules) 0 name (list-new module))
              (module name (module.env m) (module.exports m) (module.imports m) path)))
          (nil))))))

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

; (reload NAME) re-reads an already-loaded module from the source it was loaded
; from, evaluates it into a fresh env that replaces the old one (so definitions
; removed from the file disappear), then rebinds the copies that EVERY importer in
; the session holds. Shallow: NAME's own requires are already loaded, so they are
; reused, not re-read. If the new source fails (read, eval, no module, a provided
; but undefined name) the old module stays registered and no importer is touched.
; Returns the names importers lost because the new version no longer provides them.
; A value copied by hand (`(define f imported-f)`) is not an import and keeps the
; old version (a copied closure keeps its old body, though top-level names in it
; still resolve in NAME's current env).
(fn eval-reload ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "reload is only supported at top level")
    (if (i32.ne (list-len items) 2) (failure "reload expects (reload name)")
      (if (symbol? (list-get items 1))
        (reload-named (symbol-name (list-get items 1)))
        (failure "reload expects a symbol module name")))))

(fn reload-named ((name string)) value
  (if (string=? name (global.get $current-module)) (failure (string-append "module cannot reload itself: " name))
    (if (name-member? (global.get $loading) 0 name) (failure (string-append "module dependency cycle: " name))
      (let (index (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name))
        (if (i32.lt_s index 0) (failure (string-append "module is not loaded: " name))
          (let (old (list-get (global.get $modules) index))
            (if (i32.eq (string-len (module.source old)) 0)
              (failure (string-append "module has no source to reload (it was defined in-session): " name))
              (begin
                ; Unregister NAME so its (module NAME ...) form starts from an empty env.
                (global.set $modules (drop-module (global.get $modules) 0 name (list-new module)))
                (let (saved (begin-redeclaring name))
                  (begin
                    (global.set $loading (list-push (global.get $loading) name))
                    (let (loaded (load-module-source (module.source old)))
                      (begin
                        (global.set $loading (drop-name (global.get $loading) 0 name (list-new string)))
                        (let (retired (global.get $redeclarable))
                          (let (problem (if (failed? loaded) loaded (retire-problem retired 0)))
                            (let (result (if (failed? problem) problem (install-reloaded name old)))
                              (if (failed? result)
                                (begin (restore-declarations saved) (restore-module name old result))
                                (begin (end-redeclaring) (retire-declarations retired 0) result)))))))))))))))))

; The new source evaluated cleanly; check it defined NAME with every provided name
; bound, then commit: keep the old source path and refresh the importers.
(fn install-reloaded ((name string) (old module)) value
  (let (found (find-module (global.get $modules) (i32.sub (list-len (global.get $modules)) 1) name))
    (if (i32.lt_s found 0) (restore-module name old (failure (string-append "source did not define module " name)))
      (let (fresh (list-get (global.get $modules) found))
        (let (missing (undefined-export (module.env fresh) (module.exports fresh) 0))
          (if (string-len missing)
            (restore-module name old
              (failure (string-append "module " (string-append name (string-append " provides undefined name " missing)))))
            (begin
              (set-module-source name (module.source old))
              (rebind-importers name (module.env fresh) (module.exports fresh)))))))))

; Put the old module back, discarding whatever the failed reload registered.
(fn restore-module ((name string) (old module) (error value)) value
  (begin
    (global.set $modules (list-push (drop-module (global.get $modules) 0 name (list-new module)) old))
    error))

; The first provided name that the env does not define, or "".
(fn undefined-export ((env (list binding)) (exports (list string)) (index s32)) string
  (if (i32.ge_s index (list-len exports)) ""
    (if (failed? (lookup-local (list-get exports index) env (i32.sub (list-len env) 1))) (list-get exports index)
      (undefined-export env exports (i32.add index 1)))))

(fn drop-binding ((env (list binding)) (index s32) (name string) (out (list binding))) (list binding)
  (if (i32.ge_s index (list-len env)) out
    (drop-binding env (i32.add index 1) name
      (if (string=? name (binding.name (list-get env index))) out
        (list-push out (list-get env index))))))

; One importer's env/imports after a reload, plus the names dropped so far.
(record rebound (env (list binding)) (imports (list binding)) (dropped (list string)))

; Walk one importer's imports. An import from SOURCE-NAME that the new version
; still provides is overwritten with the new value (the overwrite path require's
; idempotent skip lacks); one it no longer provides is unbound and reported.
; Imports from other modules pass through. drop-binding copies, so the importer's
; original env list is never mutated.
(fn rebind-imports ((source-name string) (env (list binding)) (exports (list string)) (imports (list binding)) (index s32) (acc rebound)) rebound
  (if (i32.ge_s index (list-len imports)) acc
    (let (entry (list-get imports index))
      (let (n (binding.name entry))
        (rebind-imports source-name env exports imports (i32.add index 1)
          (if (string=? source-name (symbol-name (binding.item entry)))
            (if (name-member? exports 0 n)
              (rebound
                (list-push (drop-binding (rebound.env acc) 0 n (list-new binding))
                  (binding n (lookup-local n env (i32.sub (list-len env) 1))))
                (list-push (rebound.imports acc) entry)
                (rebound.dropped acc))
              (rebound
                (drop-binding (rebound.env acc) 0 n (list-new binding))
                (rebound.imports acc)
                (if (name-member? (rebound.dropped acc) 0 n) (rebound.dropped acc)
                  (list-push (rebound.dropped acc) n))))
            (rebound (rebound.env acc) (list-push (rebound.imports acc) entry) (rebound.dropped acc))))))))

; Refresh every suspended module in the registry; returns the dropped names.
(fn rebind-modules ((mods (list module)) (index s32) (source-name string) (env (list binding)) (exports (list string)) (out (list module)) (dropped (list string))) (list string)
  (if (i32.ge_s index (list-len mods))
    (begin (global.set $modules out) dropped)
    (let (m (list-get mods index))
      (let (r (rebind-imports source-name env exports (module.imports m) 0 (rebound (module.env m) (list-new binding) dropped)))
        (rebind-modules mods (i32.add index 1) source-name env exports
          (list-push out (module (module.name m) (rebound.env r) (module.exports m) (rebound.imports r) (module.source m)))
          (rebound.dropped r))))))

(fn name-symbols ((names (list string)) (index s32) (out (list value))) value
  (if (i32.ge_s index (list-len names)) (sequence out)
    (name-symbols names (i32.add index 1) (list-push out (symbol (list-get names index))))))

; Refresh the suspended modules, then the current one (which lives in the globals).
(fn rebind-importers ((name string) (env (list binding)) (exports (list string))) value
  (let (dropped (rebind-modules (global.get $modules) 0 name env exports (list-new module) (list-new string)))
    (let (r (rebind-imports name env exports (global.get $current-imports) 0 (rebound (global.get $bindings) (list-new binding) dropped)))
      (begin
        (global.set $bindings (rebound.env r))
        (global.set $current-imports (rebound.imports r))
        (name-symbols (rebound.dropped r) 0 (list-new value))))))

; ---- Declarations and reload ----------------------------------------------
; Types, globals, traits and instances live in session-wide tables, not in a
; module's env, so a fresh env does not clear them. $declared-by records which
; module made each declaration, keyed "type NAME", "global $NAME", "trait NAME"
; or "instance TRAIT (TYPES)". During (reload M), $reloading is M and
; $redeclarable holds the keys M declared before that its new source has not yet
; declared again: only those may be declared a second time (once each), and only
; by M. What is left at the end was removed from the source and is retired.
; Design: docs/proposals/RELOAD-DECLARATIONS.md.
(global $declared-by (list binding) mut 0)
(global $reloading string mut 0)
(global $redeclarable (list string) mut 0)

(fn type-key ((name string)) string (string-append "type " name))
(fn global-key ((name string)) string (string-append "global " name))
(fn trait-key ((name string)) string (string-append "trait " name))
(fn instance-key ((trait string) (types (list value))) string
  (string-append "instance " (string-append trait (string-append " " (show (sequence types))))))

(fn declared! ((key string)) s32
  (begin
    (global.set $declared-by (upsert-binding (global.get $declared-by) key (symbol (global.get $current-module))))
    0))

; The module that declared KEY, or "".
(fn declarer ((key string)) string
  (let (owner (lookup-local key (global.get $declared-by) (i32.sub (list-len (global.get $declared-by)) 1)))
    (if (failed? owner) "" (symbol-name owner))))

; KEY may be declared again: M is being reloaded, M is evaluating, and M
; declared KEY before and has not declared it again in this reload.
(fn redeclarable? ((key string)) s32
  (if (string=? (global.get $reloading) (global.get $current-module))
    (name-member? (global.get $redeclarable) 0 key) 0))

(fn redeclared! ((key string)) s32
  (begin
    (global.set $redeclarable (drop-name (global.get $redeclarable) 0 key (list-new string)))
    (declared! key)))

(fn replace-at ((items (list T)) (target s32) (v T) (index s32) (out (list T))) (list T)
  (where T)
  (if (i32.ge_s index (list-len items)) out
    (replace-at items target v (i32.add index 1)
      (list-push out (if (i32.eq index target) v (list-get items index))))))

(fn drop-at ((items (list T)) (target s32) (index s32) (out (list T))) (list T)
  (where T)
  (if (i32.ge_s index (list-len items)) out
    (drop-at items target (i32.add index 1)
      (if (i32.eq index target) out (list-push out (list-get items index))))))

; Declare a record or variant: a new entry, unless this replaces M's own
; declaration and is identical to it (same kind, and the same field or case
; names and types in order). Then the old id is kept, so values made before the
; reload stay valid. A changed type gets a new id; the name resolves to it, and
; old values keep their old entry (they print and match, but fail the new
; type's checks). Returns the id.
(fn declare-type ((name string) (variant s32) (schema (list value))) s32
  (let (key (type-key name))
    (let (old (type-index name))
      (let (same (if (i32.and (redeclarable? key) (i32.ge_s old 0))
                   (let (t (list-get (global.get $types) old))
                     (i32.and (i32.eq (named-type.variant? t) variant)
                       (string=? (show (sequence (named-type.schema t))) (show (sequence schema)))))
                   0))
        (begin
          (if (redeclarable? key) (redeclared! key) (declared! key))
          (if same old
            (let (id (list-len (global.get $types)))
              (begin
                (global.set $types (list-push (global.get $types) (named-type name id variant schema)))
                id))))))))

; Whether NAME is taken for a new type declaration.
(fn type-name-taken? ((name string)) s32
  (if (redeclarable? (type-key name)) 0 (known-type? (symbol name) "")))

; Declare a global. Replacing M's own mutable global of the same type keeps its
; current value, so a reload does not reset the module's state; any other
; replacement (a const, or a changed type or mutability) takes the new initializer.
(fn declare-session-global ((entry global-binding)) value
  (let (name (global-binding.name entry))
    (let (index (global-index name))
      (if (i32.and (redeclarable? (global-key name)) (i32.ge_s index 0))
        (let (old (list-get (global.get $session-globals) index))
          (begin
            (redeclared! (global-key name))
            (if (i32.and (i32.and (global-binding.mutable old) (global-binding.mutable entry))
                  (string=? (show (global-binding.type old)) (show (global-binding.type entry))))
              (nil)
              (begin
                (global.set $session-globals (replace-at (global.get $session-globals) index entry 0 (list-new global-binding)))
                (nil)))))
        (begin
          (declared! (global-key name))
          (global.set $session-globals (list-push (global.get $session-globals) entry))
          (nil))))))

(fn global-name-taken? ((name string)) s32
  (if (redeclarable? (global-key name)) 0 (i32.ge_s (global-index name) 0)))

; The index of the trait that a declaration of NAME replaces, or -1.
(fn replaced-trait ((name string)) s32
  (if (redeclarable? (trait-key name)) (trait-index name) -1))

(fn trait-name-taken? ((name string)) s32
  (if (redeclarable? (trait-key name)) 0 (i32.ge_s (trait-index name) 0)))

; The first instance of trait NAME, or -1. During a reload M's own instances
; are already removed, so this finds only other modules' instances.
(fn instance-of ((name string) (index s32)) s32
  (if (i32.ge_s index (list-len (global.get $instances))) -1
    (if (string=? name (trait-implementation.name (list-get (global.get $instances) index))) index
      (instance-of name (i32.add index 1)))))

(fn instance-owner ((index s32)) string
  (let (entry (list-get (global.get $instances) index))
    (declarer (instance-key (trait-implementation.name entry) (trait-implementation.types entry)))))

; Declare a trait. Replacing M's own identical trait keeps it; a changed one
; replaces it in place, unless another module has an instance of it.
(fn declare-session-trait ((def trait-definition)) value
  (let (name (trait-definition.name def))
    (let (old (replaced-trait name))
      (if (i32.lt_s old 0)
        (begin
          (declared! (trait-key name))
          (global.set $traits (list-push (global.get $traits) def))
          (nil))
        (let (prev (list-get (global.get $traits) old))
          (if (i32.and (string=? (show (sequence (trait-definition.vars prev))) (show (sequence (trait-definition.vars def))))
                (string=? (show (sequence (trait-definition.methods prev))) (show (sequence (trait-definition.methods def)))))
            (begin (redeclared! (trait-key name)) (nil))
            (let (other (instance-of name 0))
              (if (i32.ge_s other 0)
                (failure (string-append "trait " (string-append name
                  (string-append " changed, but module " (string-append (instance-owner other) " has an instance of it")))))
                (begin
                  (redeclared! (trait-key name))
                  (global.set $traits (replace-at (global.get $traits) old def 0 (list-new trait-definition)))
                  (nil))))))))))

(fn declare-session-instance ((entry trait-implementation)) value
  (begin
    (declared! (instance-key (trait-implementation.name entry) (trait-implementation.types entry)))
    (global.set $instances (list-push (global.get $instances) entry))
    (nil)))

; The keys MODULE declared (instances aside).
(fn declared-keys ((owners (list binding)) (index s32) (module string) (out (list string))) (list string)
  (if (i32.ge_s index (list-len owners)) out
    (let (entry (list-get owners index))
      (declared-keys owners (i32.add index 1) module
        (if (i32.and (string=? module (symbol-name (binding.item entry)))
              (i32.eq (string-prefix? (binding.name entry) "instance ") 0))
          (list-push out (binding.name entry)) out)))))

(fn string-prefix? ((s string) (prefix string)) s32
  (if (i32.lt_s (string-len s) (string-len prefix)) 0
    (string=? (substring s 0 (string-len prefix)) prefix)))

(fn other-instances ((items (list trait-implementation)) (index s32) (module string) (out (list trait-implementation))) (list trait-implementation)
  (if (i32.ge_s index (list-len items)) out
    (let (entry (list-get items index))
      (other-instances items (i32.add index 1) module
        (if (string=? module (declarer (instance-key (trait-implementation.name entry) (trait-implementation.types entry)))) out
          (list-push out entry))))))

; What a reload must restore if it fails.
(record reload-snapshot (modules (list string)) (types (list named-type)) (globals (list global-binding))
  (traits (list trait-definition)) (instances (list trait-implementation)) (owners (list binding)))

; Start reloading NAME: snapshot the tables, open NAME's own declarations for
; replacement, and remove NAME's instances (its new source declares them again).
(fn begin-redeclaring ((name string)) reload-snapshot
  (let (saved (reload-snapshot (module-names (global.get $modules) 0 (list-new string))
                (global.get $types) (global.get $session-globals) (global.get $traits)
                (global.get $instances) (global.get $declared-by)))
    (begin
      (global.set $reloading name)
      (global.set $redeclarable (declared-keys (global.get $declared-by) 0 name (list-new string)))
      (global.set $instances (other-instances (global.get $instances) 0 name (list-new trait-implementation)))
      saved)))

(fn end-redeclaring () s32
  (begin (global.set $reloading "") (global.set $redeclarable (list-new string)) 0))

; Restore the snapshot. A type made since keeps its slot (values may carry its
; id) but loses its name, as a failed require does.
(fn restore-declarations ((saved reload-snapshot)) s32
  (begin
    (global.set $modules (keep-modules (global.get $modules) 0 (reload-snapshot.modules saved) (list-new module)))
    (global.set $types (restore-types (reload-snapshot.types saved) (global.get $types) (list-len (reload-snapshot.types saved))))
    (global.set $session-globals (reload-snapshot.globals saved))
    (global.set $traits (reload-snapshot.traits saved))
    (global.set $instances (reload-snapshot.instances saved))
    (global.set $declared-by (reload-snapshot.owners saved))
    (end-redeclaring)))

(fn restore-types ((saved (list named-type)) (now (list named-type)) (index s32)) (list named-type)
  (if (i32.ge_s index (list-len now)) saved
    (let (t (list-get now index))
      (restore-types (list-push saved (named-type "" (named-type.id t) (named-type.variant? t) (named-type.schema t))) now (i32.add index 1)))))

; Why the declarations left in $redeclarable cannot be retired, or nil: a trait
; that another module has an instance of.
(fn retire-problem ((keys (list string)) (index s32)) value
  (if (i32.ge_s index (list-len keys)) (nil)
    (let (key (list-get keys index))
      (if (string-prefix? key "trait ")
        (let (name (substring key 6 (string-len key)))
          (let (other (instance-of name 0))
            (if (i32.ge_s other 0)
              (failure (string-append "trait " (string-append name
                (string-append " was removed, but module " (string-append (instance-owner other) " has an instance of it")))))
              (retire-problem keys (i32.add index 1)))))
        (retire-problem keys (i32.add index 1))))))

; Retire what the new source no longer declares: the name is free again. A
; type keeps its slot for old values.
(fn retire-declarations ((keys (list string)) (index s32)) s32
  (if (i32.ge_s index (list-len keys)) 0
    (let (key (list-get keys index))
      (begin
        (global.set $declared-by (drop-binding (global.get $declared-by) 0 key (list-new binding)))
        (if (string-prefix? key "type ")
          (let (at (type-index (substring key 5 (string-len key))))
            (if (i32.ge_s at 0)
              (let (t (list-get (global.get $types) at))
                (begin
                  (global.set $types (replace-at (global.get $types) at
                    (named-type "" (named-type.id t) (named-type.variant? t) (named-type.schema t)) 0 (list-new named-type)))
                  0))
              0))
          (if (string-prefix? key "global ")
            (let (at (global-index (substring key 7 (string-len key))))
              (if (i32.ge_s at 0)
                (begin (global.set $session-globals (drop-at (global.get $session-globals) at 0 (list-new global-binding))) 0)
                0))
            (if (string-prefix? key "trait ")
              (let (at (trait-index (substring key 6 (string-len key))))
                (if (i32.ge_s at 0)
                  (begin (global.set $traits (drop-at (global.get $traits) at 0 (list-new trait-definition))) 0)
                  0))
              0)))
        (retire-declarations keys (i32.add index 1))))))
