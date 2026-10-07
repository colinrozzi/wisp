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

; Flat multi-way dispatch, so adding a host verb is a single clause (no nested-if
; paren juggling). Standard recursive cond over the base compiler's syntax-rules.
(define-syntax cond
  (syntax-rules (else)
    ((_ (else result)) result)
    ((_ (test result) clause ...) (if test result (cond clause ...)))))

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
; writers: content crosses as list<u8>, built from a REPL string via str->bytes.
(import theater:simple/store store ((store-id string) (content (list u8))) (result string string))
(import theater:simple/store label ((store-id string) (label-name string) (content-ref string)) (result unit string))
(import theater:simple/store store-at-label ((store-id string) (label-name string) (content (list u8))) (result string string))

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
(import theater:simple/runtime get-actor-state ((id string)) (result (option (list u8)) runtime-error))
(import theater:simple/runtime get-actor-manifest ((id string)) (result string runtime-error))
(import theater:simple/runtime stop-actor ((id string)) (result unit runtime-error))
(import theater:simple/runtime kill-actor ((id string)) (result unit runtime-error))
(import theater:simple/runtime subscribe-to-spawns () (result unit runtime-error))
(import theater:simple/runtime unsubscribe-from-spawns () (result unit runtime-error))
(import theater:simple/runtime shutdown-runtime () (result unit runtime-error))

; message-server: inter-actor messaging. Clean string/list<u8>/result shapes; the
; message bodies cross as list<u8> via str->bytes. (send/request target ANOTHER
; actor, so they need a peer to exercise; register/list-requests are self-contained.)
(import theater:simple/message-server-host register () (result unit string))
(import theater:simple/message-server-host send ((actor-id string) (msg (list u8))) (result unit string))
(import theater:simple/message-server-host request ((actor-id string) (msg (list u8))) (result (list u8) string))
(import theater:simple/message-server-host list-outstanding-requests () (list string))
(import theater:simple/message-server-host respond-to-request ((request-id string) (response (list u8))) (result unit string))
(import theater:simple/message-server-host cancel-request ((request-id string)) (result unit string))
(import theater:simple/message-server-host open-channel ((actor-id string) (initial-msg (list u8))) (result string string))
(import theater:simple/message-server-host send-on-channel ((channel-id string) (msg (list u8))) (result unit string))
(import theater:simple/message-server-host close-channel ((channel-id string)) (result unit string))

; assembler (wisp:assembler/runtime): compile WAT text to a wasm byte vector —
; directly useful inside a Wisp REPL.
(import wisp:assembler/runtime wat-to-wasm ((wat string)) (result (list u8) string))

; timer: now() reads the clock (bare u64); set-interval/clear-interval drive the
; actor's tick callback.
(import theater:simple/timer now () u64)
(import theater:simple/timer set-interval ((name string) (interval-ms u64)) (result string string))
(import theater:simple/timer clear-interval ((name string)) (result unit string))

; Names the evaluator routes to the Theater bridge rather than ordinary builtins.
(fn host-builtin? ((name string)) s32
  (cond
    ((string=? name "self") 1)
    ((string=? name "log") 1)
    ((string=? name "call") 1)
    ((string=? name "describe") 1)
    ((string=? name "exports") 1)
    ((string=? name "implements") 1)
    ((string=? name "store-new") 1)
    ((string=? name "store-get") 1)
    ((string=? name "store-get-by-label") 1)
    ((string=? name "store-list-labels") 1)
    ((string=? name "store-exists") 1)
    ((string=? name "store-size") 1)
    ((string=? name "store-put") 1)
    ((string=? name "store-label") 1)
    ((string=? name "store-put-at") 1)
    ((string=? name "list-actors") 1)
    ((string=? name "actor-status") 1)
    ((string=? name "actor-state") 1)
    ((string=? name "actor-manifest") 1)
    ((string=? name "stop-actor") 1)
    ((string=? name "kill-actor") 1)
    ((string=? name "subscribe-spawns") 1)
    ((string=? name "unsubscribe-spawns") 1)
    ((string=? name "shutdown-runtime") 1)
    ((string=? name "msg-register") 1)
    ((string=? name "msg-send") 1)
    ((string=? name "msg-request") 1)
    ((string=? name "msg-list-requests") 1)
    ((string=? name "msg-respond") 1)
    ((string=? name "msg-cancel") 1)
    ((string=? name "msg-open") 1)
    ((string=? name "msg-send-channel") 1)
    ((string=? name "msg-close-channel") 1)
    ((string=? name "wat-to-wasm") 1)
    ((string=? name "now") 1)
    ((string=? name "set-interval") 1)
    ((string=? name "clear-interval") 1)
    (else 0)))

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

; Convert a REPL string value into a list<u8> value (typed-list of byte-value),
; so string payloads can cross as list<u8> (store content, message bodies). The
; element template is (symbol "u8") — the same descriptor unmarshal-array uses —
; so marshal routes it to the packed Array node (tag-of-type reads symbol-name,
; which is "" for a bare byte-value, so the template must be the symbol). The
; string is byte-indexed; length is the u32 at the head of its [len][bytes] buffer.
(fn str-bytes ((s string) (i s32) (n s32) (acc (list value))) (list value)
  (if (i32.ge_s i n) acc
    (str-bytes s (i32.add i (i32.const 1)) n (list-push acc (byte-value (string-ref s i))))))
(fn str-to-bytes ((v value)) value
  (value-case v
    ((text s) (typed-list (symbol "u8")
                (str-bytes s (i32.const 0) (i32.load (string-addr s)) (list-new value))))
    (else (typed-list (symbol "u8") (list-new value)))))

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

; writers: the content string becomes a list<u8> via str-to-bytes before marshal.
(fn apply-store-put ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "store-put expects (store-put store-id content)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "store"
        (marshal (sequence (list-push (list-push (list-new value)
          (list-get args 0)) (str-to-bytes (list-get args 1)))))))
      (failure "store-put expects two strings: store id and content"))))

(fn apply-store-label ((args (list value))) value
  (if (i32.ne (list-len args) 3) (failure "store-label expects (store-label store-id label content-ref)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "label" (arg-tuple args)))
      (failure "store-label expects three strings: store id, label, content ref"))))

(fn apply-store-put-at ((args (list value))) value
  (if (i32.ne (list-len args) 3) (failure "store-put-at expects (store-put-at store-id label content)")
    (if (all-strings? args 0)
      (unmarshal (raw-invoke "store-at-label"
        (marshal (sequence (list-push (list-push (list-push (list-new value)
          (list-get args 0)) (list-get args 1)) (str-to-bytes (list-get args 2)))))))
      (failure "store-put-at expects three strings: store id, label, content"))))

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

(fn apply-runtime-state ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "actor-state expects (actor-state id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "get-actor-state" (marshal (list-get args 0))))
      (failure "actor-state expects a string actor id"))))

(fn apply-runtime-manifest ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "actor-manifest expects (actor-manifest id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "get-actor-manifest" (marshal (list-get args 0))))
      (failure "actor-manifest expects a string actor id"))))

(fn apply-runtime-kill ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "kill-actor expects (kill-actor id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "kill-actor" (marshal (list-get args 0))))
      (failure "kill-actor expects a string actor id"))))

(fn apply-runtime-subscribe ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "subscribe-spawns expects no arguments")
    (unmarshal (raw-invoke "subscribe-to-spawns" (arg-tuple (list-new value))))))

(fn apply-runtime-unsubscribe ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "unsubscribe-spawns expects no arguments")
    (unmarshal (raw-invoke "unsubscribe-from-spawns" (arg-tuple (list-new value))))))

(fn apply-runtime-shutdown ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "shutdown-runtime expects no arguments")
    (unmarshal (raw-invoke "shutdown-runtime" (arg-tuple (list-new value))))))

; --- message-server --------------------------------------------------------
; Shared shapes: (string, message) -> Tuple(String, list<u8>); a single string
; arg marshals bare (accepted by every host's parser, required by some).
(fn str-bytes-tuple ((a value) (b value)) any
  (marshal (sequence (list-push (list-push (list-new value) a) (str-to-bytes b)))))

(fn apply-msg-register ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "msg-register expects no arguments")
    (unmarshal (raw-invoke "register" (arg-tuple (list-new value))))))
(fn apply-msg-list-requests ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "msg-list-requests expects no arguments")
    (unmarshal (raw-invoke "list-outstanding-requests" (arg-tuple (list-new value))))))
(fn apply-msg-send ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-send expects (msg-send actor-id message)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "send" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-send expects two strings: actor id and message"))))
(fn apply-msg-request ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-request expects (msg-request actor-id message)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "request" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-request expects two strings: actor id and message"))))
(fn apply-msg-respond ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-respond expects (msg-respond request-id response)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "respond-to-request" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-respond expects two strings: request id and response"))))
(fn apply-msg-cancel ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "msg-cancel expects (msg-cancel request-id)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "cancel-request" (marshal (list-get args 0))))
      (failure "msg-cancel expects a string request id"))))
(fn apply-msg-open ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-open expects (msg-open actor-id initial-message)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "open-channel" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-open expects two strings: actor id and initial message"))))
(fn apply-msg-send-channel ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-send-channel expects (msg-send-channel channel-id message)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "send-on-channel" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-send-channel expects two strings: channel id and message"))))
(fn apply-msg-close-channel ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "msg-close-channel expects (msg-close-channel channel-id)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "close-channel" (marshal (list-get args 0))))
      (failure "msg-close-channel expects a string channel id"))))

; --- assembler -------------------------------------------------------------
(fn apply-wat-to-wasm ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "wat-to-wasm expects (wat-to-wasm wat-text)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "wat-to-wasm" (marshal (list-get args 0))))
      (failure "wat-to-wasm expects a string of WAT"))))

; --- timer -----------------------------------------------------------------
(fn as-u64 ((v value)) value
  (value-case v
    ((integer n) (u64-value (i64.extend_i32_s n)))
    ((wide-integer n) (u64-value n))
    ((u64-value n) (u64-value n))
    (else (u64-value (i64.const 0)))))
(fn apply-timer-now ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "now expects no arguments")
    (unmarshal (raw-invoke "now" (arg-tuple (list-new value))))))
(fn apply-timer-set ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "set-interval expects (set-interval name interval-ms)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "set-interval"
        (marshal (sequence (list-push (list-push (list-new value)
          (list-get args 0)) (as-u64 (list-get args 1)))))))
      (failure "set-interval expects a string name and an integer interval"))))
(fn apply-timer-clear ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "clear-interval expects (clear-interval name)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "clear-interval" (marshal (list-get args 0))))
      (failure "clear-interval expects a string name"))))

(fn apply-host-builtin ((name string) (args (list value))) value
  (cond
    ((string=? name "self") (apply-self args))
    ((string=? name "log") (apply-log args))
    ((string=? name "call") (apply-call args))
    ((string=? name "describe") (apply-describe args))
    ((string=? name "exports") (apply-exports args))
    ((string=? name "implements") (apply-implements args))
    ((string=? name "store-new") (apply-store-new args))
    ((string=? name "store-get") (apply-store-get args))
    ((string=? name "store-get-by-label") (apply-store-get-by-label args))
    ((string=? name "store-list-labels") (apply-store-list-labels args))
    ((string=? name "store-exists") (apply-store-exists args))
    ((string=? name "store-size") (apply-store-size args))
    ((string=? name "store-put") (apply-store-put args))
    ((string=? name "store-label") (apply-store-label args))
    ((string=? name "store-put-at") (apply-store-put-at args))
    ((string=? name "list-actors") (apply-runtime-list-actors args))
    ((string=? name "actor-status") (apply-runtime-status args))
    ((string=? name "actor-state") (apply-runtime-state args))
    ((string=? name "actor-manifest") (apply-runtime-manifest args))
    ((string=? name "stop-actor") (apply-runtime-stop args))
    ((string=? name "kill-actor") (apply-runtime-kill args))
    ((string=? name "subscribe-spawns") (apply-runtime-subscribe args))
    ((string=? name "unsubscribe-spawns") (apply-runtime-unsubscribe args))
    ((string=? name "shutdown-runtime") (apply-runtime-shutdown args))
    ((string=? name "msg-register") (apply-msg-register args))
    ((string=? name "msg-send") (apply-msg-send args))
    ((string=? name "msg-request") (apply-msg-request args))
    ((string=? name "msg-list-requests") (apply-msg-list-requests args))
    ((string=? name "msg-respond") (apply-msg-respond args))
    ((string=? name "msg-cancel") (apply-msg-cancel args))
    ((string=? name "msg-open") (apply-msg-open args))
    ((string=? name "msg-send-channel") (apply-msg-send-channel args))
    ((string=? name "msg-close-channel") (apply-msg-close-channel args))
    ((string=? name "wat-to-wasm") (apply-wat-to-wasm args))
    ((string=? name "now") (apply-timer-now args))
    ((string=? name "set-interval") (apply-timer-set args))
    ((string=? name "clear-interval") (apply-timer-clear args))
    (else (failure (string-append "unknown host builtin: " name)))))
