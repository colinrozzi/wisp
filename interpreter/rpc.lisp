; rpc.lisp — the REPL's hook into Theater. REPL verbs that call Theater's rpc
; host interface and hand results back as ordinary, inspectable interpreter
; values via `unmarshal`. Requires marshal.lisp (for unmarshal) first.
;
; Each verb is the same uniform shape: evaluate string argument(s), call the
; matching static import, unmarshal the dynamic `value` result. Adding a verb is
; an import declaration + one `apply-*` + one dispatch line + one host-builtin?
; entry. (Wasm imports are static, so each needs its own named call site — but
; the glue is identical.) `call`, whose params are themselves dynamic values,
; needs import-side `any`-argument encoding and is a separate step.

(import theater:simple/rpc describe ((actor-id string)) any)
(import theater:simple/rpc exports ((actor-id string)) any)
(import theater:simple/rpc implements ((actor-id string) (interface string)) any)
(import theater:simple/rpc call ((actor-id string) (function string) (params any) (options any)) any)
; self: the actor's own identity + logging. These use only string/unit, so the
; typed import wrapper's result is directly an interp value (no codec needed) —
; the boilerplate shape for a simple-typed host binding.
(import theater:simple/self self () string)
(import theater:simple/self log ((msg string)) unit)

; store: content-addressed persistence. These results are compound
; (result<string>, result<list<u8>>, result<option<string>>, result<list<string>>),
; so each uses the raw-CGRF convention: the import is declared with its real typed
; signature (so the interface hash matches Theater) but the args cross as a
; marshalled CGRF blob via `raw-invoke` and the result is bridged by `unmarshal`.
; The typed wrapper the compiler emits for each is unused dead code; we call the
; raw entry point. Adding one is: import line + apply-* (same shape) + dispatch +
; host-builtin? entry — true boilerplate now the convention exists.
(import theater:simple/store new () (result string string))
(import theater:simple/store get ((store-id string) (content-ref string)) (result (list u8) string))
(import theater:simple/store get-by-label ((store-id string) (label string)) (result (option string) string))
(import theater:simple/store list-labels ((store-id string)) (result (list string) string))
; bool/u64 results — now first-class compiler types, so these hashes match Theater.
(import theater:simple/store exists ((store-id string) (content-ref string)) (result bool string))
(import theater:simple/store calculate-total-size ((store-id string)) (result u64 string))

; runtime: the actor-management control surface. Its results carry NAMED types
; (record actor-info, variant runtime-error/spawn-failure). We declare those types
; here, exactly matching theater:simple/runtime's pact — the compiler registers them
; as pack typedefs so the Ref-by-name in each signature resolves STRUCTURALLY, making
; our per-function interface hash identical to Theater's. (Pack hashes a record/
; variant by its sorted field/case names + child hashes; declaration order is free.)
; spawn-failure is declared before runtime-error because runtime-error references it.
(variant spawn-failure
  (bad-manifest string) (wasm-fetch string) (handler-registry string)
  (wasm-invalid string) (interface-mismatch string) (missing-interface string)
  (missing-metadata string) (init-failed string) (child-failed string)
  (child-stopped string) (timeout string) (internal string))
(variant runtime-error
  (permission-denied string) (runtime-unavailable) (actor-not-found string)
  (invalid-argument string) (spawn-failed spawn-failure) (internal string))
(record actor-info (id string) (name string) (parent-id (option string)))

(import theater:simple/runtime list-actors () (result (list actor-info) runtime-error))
(import theater:simple/runtime get-actor-status ((id string)) (result string runtime-error))
(import theater:simple/runtime stop-actor ((id string)) (result unit runtime-error))

; Names the evaluator routes to the Theater bridge rather than ordinary builtins.
(fn host-builtin? ((name string)) s32
  (i32.or (string=? name "self") (i32.or (string=? name "log")
    (i32.or (string=? name "call")
      (i32.or (string=? name "describe")
        (i32.or (string=? name "exports")
          (i32.or (string=? name "implements")
            (i32.or (string=? name "store-new")
              (i32.or (string=? name "store-get")
                (i32.or (string=? name "store-get-by-label")
                  (i32.or (string=? name "store-list-labels")
                    (i32.or (string=? name "store-exists")
                      (i32.or (string=? name "store-size")
                        (i32.or (string=? name "list-actors")
                          (i32.or (string=? name "actor-status")
                            (string=? name "stop-actor"))))))))))))))))

(fn string-arg? ((v value)) s32 (value-case v ((text s) 1) (else 0)))
(fn as-string ((v value)) string (value-case v ((text s) s) (else "")))

(fn apply-describe ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "describe expects (describe actor-id)")
    (if (string-arg? (list-get args 0)) (unmarshal (describe (as-string (list-get args 0))))
      (failure "describe expects a string actor id"))))

(fn apply-exports ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "exports expects (exports actor-id)")
    (if (string-arg? (list-get args 0)) (unmarshal (exports (as-string (list-get args 0))))
      (failure "exports expects a string actor id"))))

(fn apply-implements ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "implements expects (implements actor-id interface)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (implements (as-string (list-get args 0)) (as-string (list-get args 1))))
      (failure "implements expects two strings: actor id and interface"))))

; (call actor-id function arg...) -> calls function on actor-id with the args as
; Pack params (a Tuple), returns the result as an inspectable value. params are
; built in Wisp via marshal (which nests the tuple correctly), so no per-arg
; splicing is needed; call-raw hands the whole encoded args tuple to the import.
(fn args-from ((args (list value)) (i s32) (acc (list value))) (list value)
  (if (i32.ge_s i (list-len args)) acc
    (args-from args (i32.add i (i32.const 1)) (list-push acc (list-get args i)))))

(fn apply-call ((args (list value))) value
  (if (i32.lt_s (list-len args) (i32.const 2)) (failure "call expects (call actor-id function arg ...)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (let (params (sequence (args-from args (i32.const 2) (list-new value))))
        (let (full (sequence (list-push (list-push (list-push (list-push (list-new value)
                     (list-get args 0)) (list-get args 1)) params) (sequence (list-new value)))))
          (unmarshal (call-raw (marshal full)))))
      (failure "call expects actor-id and function as strings"))))

; Simple-typed host bindings: the typed wrapper's result is already an interp
; value (string -> text, unit -> nil), so no marshal/unmarshal needed.
(fn apply-self ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "self expects no arguments")
    (text (self))))
(fn apply-log ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "log expects (log message)")
    (if (string-arg? (list-get args 0))
      (begin (log (as-string (list-get args 0))) (nil))
      (failure "log expects a string message"))))

; store verbs (raw-CGRF convention). Each marshals its string args into a CGRF
; tuple, calls the matching raw import, and unmarshals the compound result. The
; import name passed to raw-invoke is a literal (Wasm imports are static), so each
; verb has its own call site — otherwise identical.
(fn all-strings? ((args (list value)) (i s32)) s32
  (if (i32.ge_s i (list-len args)) 1
    (if (string-arg? (list-get args i)) (all-strings? args (i32.add i (i32.const 1))) 0)))
(fn arg-tuple ((args (list value))) any (marshal (sequence args)))

(fn apply-store-new ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "store-new expects no arguments")
    (unmarshal (raw-invoke "new" (arg-tuple (list-new value))))))

(fn apply-store-get ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "store-get expects (store-get store-id content-ref)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "get" (arg-tuple args)))
      (failure "store-get expects two strings: store id and content ref"))))

(fn apply-store-get-by-label ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "store-get-by-label expects (store-get-by-label store-id label)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "get-by-label" (arg-tuple args)))
      (failure "store-get-by-label expects two strings: store id and label"))))

(fn apply-store-list-labels ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "store-list-labels expects (store-list-labels store-id)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "list-labels" (arg-tuple args)))
      (failure "store-list-labels expects one string: store id"))))

(fn apply-store-exists ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "store-exists expects (store-exists store-id content-ref)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "exists" (arg-tuple args)))
      (failure "store-exists expects two strings: store id and content ref"))))

(fn apply-store-size ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "store-size expects (store-size store-id)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "calculate-total-size" (arg-tuple args)))
      (failure "store-size expects one string: store id"))))

; runtime verbs. list-actors takes no args (empty-tuple blob; host ignores input).
; The single-id verbs marshal the BARE string (runtime's parse_target wants a
; Value::String, not a 1-tuple) — a reminder that arg shaping is per-host, not
; universal. Results unmarshal to inspectable values (actor-info records; a
; runtime-error open-variant on failure).
(fn apply-runtime-list-actors ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "list-actors expects no arguments")
    (unmarshal (raw-invoke "list-actors" (arg-tuple (list-new value))))))

(fn apply-runtime-status ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "actor-status expects (actor-status id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "get-actor-status" (marshal (list-get args 0))))
      (failure "actor-status expects a string actor id"))))

(fn apply-runtime-stop ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "stop-actor expects (stop-actor id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "stop-actor" (marshal (list-get args 0))))
      (failure "stop-actor expects a string actor id"))))

(fn apply-host-builtin ((name string) (args (list value))) value
  (if (string=? name "self") (apply-self args)
    (if (string=? name "log") (apply-log args)
      (if (string=? name "call") (apply-call args)
        (if (string=? name "describe") (apply-describe args)
          (if (string=? name "exports") (apply-exports args)
            (if (string=? name "implements") (apply-implements args)
              (if (string=? name "store-new") (apply-store-new args)
                (if (string=? name "store-get") (apply-store-get args)
                  (if (string=? name "store-get-by-label") (apply-store-get-by-label args)
                    (if (string=? name "store-list-labels") (apply-store-list-labels args)
                      (if (string=? name "store-exists") (apply-store-exists args)
                        (if (string=? name "store-size") (apply-store-size args)
                          (if (string=? name "list-actors") (apply-runtime-list-actors args)
                            (if (string=? name "actor-status") (apply-runtime-status args)
                              (if (string=? name "stop-actor") (apply-runtime-stop args)
                                (failure (string-append "unknown host builtin: " name))))))))))))))))))
