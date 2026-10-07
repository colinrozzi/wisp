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

; Inbound triggers (timer ticks, spawns, messages) are delivered by Theater
; CALLING an export on this actor (handle-tick, handle-actor-spawn, ...). Because
; the whole session is one live image — evaluate and the callbacks share $bindings
; — a trigger just dispatches to a user-defined handler (on-tick, on-spawn, ...)
; looked up in that env. So "add a handler" is literally "(define (on-tick d) ...)".
; Each firing is buffered in $events (handler name, the event value, the handler's
; result) so the REPL can see what fired via (poll-events).
(global $events (list value) mut 0)

(fn record-event ((handler string) (event value) (result value)) s32
  (begin
    (global.set $events (list-push (global.get $events)
      (sequence (list-push (list-push (list-push (list-new value) (text handler)) event) result))))
    0))

; Dispatch an inbound event to a user handler named `handler` (a 1-arg function
; defined in the session). Resets the step budget (the handler runs fresh), applies
; the handler to the event, records (handler, event, result). If no such handler is
; defined, records the event with a "no handler" note so it is still visible.
(fn dispatch-event ((handler string) (event value)) value
  (begin
    (global.set $steps (i32.const 0))
    (let (h (lookup handler (list-new binding)))
      (if (failed? h)
        (begin (record-event handler event (text "no handler defined")) h)
        (let (result (apply-value h (list-push (list-new value) event) (i32.const 0) ""))
          (begin (record-event handler event result) result))))))

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

; filesystem: read/write files and dirs. Named types like runtime; filesystem.exists
; collides with store.exists by bare name — the raw symbol is now interface-qualified
; so both coexist (this interface is the proof of that compiler change).
(variant filesystem-error
  (not-found string) (permission-denied string) (already-exists string)
  (not-a-directory string) (is-a-directory string) (invalid-path string) (io-error string))
(record dir-entry (name string) (is-dir bool))
(record file-metadata (size u64) (is-dir bool) (read-only bool))
(import theater:simple/filesystem read-file ((path string)) (result (list u8) filesystem-error))
(import theater:simple/filesystem exists ((path string)) (result bool filesystem-error))
(import theater:simple/filesystem list-dir ((path string)) (result (list dir-entry) filesystem-error))
(import theater:simple/filesystem metadata ((path string)) (result file-metadata filesystem-error))
(import theater:simple/filesystem write-file ((path string) (content (list u8))) (result unit filesystem-error))
(import theater:simple/filesystem append-file ((path string) (content (list u8))) (result unit filesystem-error))
(import theater:simple/filesystem delete-file ((path string)) (result unit filesystem-error))
(import theater:simple/filesystem create-dir ((path string)) (result unit filesystem-error))
(import theater:simple/filesystem remove-dir ((path string)) (result unit filesystem-error))

; terminal: stdio + tty control. Exercises u16 (get-size tuple) and bool (set-raw).
(import theater:simple/terminal write-stdout ((data (list u8))) (result u64 string))
(import theater:simple/terminal write-stderr ((data (list u8))) (result u64 string))
(import theater:simple/terminal set-raw-mode ((enabled bool)) (result unit string))
(import theater:simple/terminal get-size () (result (tuple u16 u16) string))
(import theater:simple/terminal enable-input () (result unit string))

; http-client: outbound HTTP. The request is a RECORD argument (the first record
; ARG we marshal), built via marshal-record; the response record (u16 status)
; comes back through unmarshal/register-on-arrival.
(record http-header (name string) (value string))
(record http-request (method string) (url string) (headers (list http-header)) (body (option (list u8))))
(record http-response (status u16) (headers (list http-header)) (body (option (list u8))))
(import theater:simple/http-client request ((req http-request)) (result http-response string))

; tcp: raw sockets. Mechanical string/list<u8>/u64/bool shapes (receive's max-bytes
; is a u32 arg). tcp.send collides with message-server.send by bare name — the
; qualified raw symbols keep them distinct.
(import theater:simple/tcp connect ((address string)) (result string string))
(import theater:simple/tcp listen ((address string)) (result string string))
(import theater:simple/tcp accept ((listener-id string)) (result string string))
(import theater:simple/tcp activate ((connection-id string)) (result unit string))
(import theater:simple/tcp set-active ((connection-id string) (mode string)) (result unit string))
(import theater:simple/tcp transfer ((connection-id string) (target-actor string)) (result unit string))
(import theater:simple/tcp transfer-async ((connection-id string) (target-actor string)) (result unit string))
(import theater:simple/tcp peer-address ((connection-id string)) (result string string))
(import theater:simple/tcp is-tls ((connection-id string)) (result bool string))
(import theater:simple/tcp send ((connection-id string) (data (list u8))) (result u64 string))
(import theater:simple/tcp receive ((connection-id string) (max-bytes u32)) (result (list u8) string))
(import theater:simple/tcp close ((connection-id string)) (result unit string))
(import theater:simple/tcp close-listener ((listener-id string)) (result unit string))
(import theater:simple/tcp upgrade-to-tls-server ((connection-id string)) (result unit string))
(import theater:simple/tcp upgrade-to-tls-client ((connection-id string) (server-name string)) (result unit string))

; podman: container management. run takes a container-spec record (built via
; marshal-record); list returns container-info records.
(record mount-spec (source string) (target string) (read-only bool))
(record container-spec (image string) (name string) (env (list (tuple string string)))
  (mounts (list mount-spec)) (cmd (list string)) (tty bool) (interactive bool))
(record container-info (id string) (name string) (image string) (status string) (exit-code s32))
(import theater:simple/podman run ((spec container-spec)) (result string string))
(import theater:simple/podman stop ((name string)) (result unit string))
(import theater:simple/podman rm ((name string) (force bool)) (result unit string))
(import theater:simple/podman list () (result (list container-info) string))

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
    ((string=? name "fs-read") 1)
    ((string=? name "fs-exists") 1)
    ((string=? name "fs-list") 1)
    ((string=? name "fs-meta") 1)
    ((string=? name "fs-write") 1)
    ((string=? name "fs-append") 1)
    ((string=? name "fs-delete") 1)
    ((string=? name "fs-mkdir") 1)
    ((string=? name "fs-rmdir") 1)
    ((string=? name "term-write") 1)
    ((string=? name "term-write-err") 1)
    ((string=? name "term-raw") 1)
    ((string=? name "term-size") 1)
    ((string=? name "term-input") 1)
    ((string=? name "http-get") 1)
    ((string=? name "http-req") 1)
    ((string=? name "poll-events") 1)
    ((string=? name "help") 1)
    ((string=? name "tcp-connect") 1)
    ((string=? name "tcp-listen") 1)
    ((string=? name "tcp-accept") 1)
    ((string=? name "tcp-activate") 1)
    ((string=? name "tcp-set-active") 1)
    ((string=? name "tcp-transfer") 1)
    ((string=? name "tcp-transfer-async") 1)
    ((string=? name "tcp-peer") 1)
    ((string=? name "tcp-is-tls") 1)
    ((string=? name "tcp-send") 1)
    ((string=? name "tcp-receive") 1)
    ((string=? name "tcp-close") 1)
    ((string=? name "tcp-close-listener") 1)
    ((string=? name "tcp-tls-server") 1)
    ((string=? name "tcp-tls-client") 1)
    ((string=? name "podman-run") 1)
    ((string=? name "podman-stop") 1)
    ((string=? name "podman-rm") 1)
    ((string=? name "podman-list") 1)
    (else 0)))

(fn string-arg? ((v value)) s32 (value-case v ((text s) 1) (else 0)))
(fn as-string ((v value)) string (value-case v ((text s) s) (else "")))

; Self-targeted blocking RPC wedges the session: exports/implements/call and
; get-actor-state round-trip a request into the target actor and wait for its
; reply, but this actor is busy in the current eval and can never service its
; own inbound request — it hangs forever, and every later eval queues behind it.
; Guard those verbs on self with a clear error. describe and actor-status are
; served from runtime metadata (no call into the actor), so they are safe.
(fn self? ((aid string)) s32 (string=? aid (self)))

(fn apply-describe ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "describe expects (describe actor-id)")
    (if (string-arg? (list-get args 0)) (unmarshal (describe (as-string (list-get args 0))))
      (failure "describe expects a string actor id"))))

(fn apply-exports ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "exports expects (exports actor-id)")
    (if (string-arg? (list-get args 0))
      (if (self? (as-string (list-get args 0)))
        (failure "exports on self deadlocks (self-RPC); use (describe (self)) for your own exports")
        (unmarshal (exports (as-string (list-get args 0)))))
      (failure "exports expects a string actor id"))))

(fn apply-implements ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "implements expects (implements actor-id interface)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (if (self? (as-string (list-get args 0)))
        (failure "implements on self deadlocks (self-RPC); use (describe (self))")
        (unmarshal (implements (as-string (list-get args 0)) (as-string (list-get args 1)))))
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
      (if (self? (as-string (list-get args 0)))
        (failure "call on self deadlocks (an actor cannot call itself mid-eval); target another actor")
        (let (params (sequence (args-from args (i32.const 2) (list-new value))))
          (let (full (sequence (list-push (list-push (list-push (list-push (list-new value)
                       (list-get args 0)) (list-get args 1)) params) (sequence (list-new value)))))
            (unmarshal (call-raw (marshal full))))))
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
    (unmarshal (raw-invoke "theater:simple/store" "new" (arg-tuple (list-new value))))))

(fn apply-store-get ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "store-get expects (store-get store-id content-ref)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "theater:simple/store" "get" (arg-tuple args)))
      (failure "store-get expects two strings: store id and content ref"))))

(fn apply-store-get-by-label ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "store-get-by-label expects (store-get-by-label store-id label)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "theater:simple/store" "get-by-label" (arg-tuple args)))
      (failure "store-get-by-label expects two strings: store id and label"))))

(fn apply-store-list-labels ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "store-list-labels expects (store-list-labels store-id)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "theater:simple/store" "list-labels" (arg-tuple args)))
      (failure "store-list-labels expects one string: store id"))))

(fn apply-store-exists ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "store-exists expects (store-exists store-id content-ref)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "theater:simple/store" "exists" (arg-tuple args)))
      (failure "store-exists expects two strings: store id and content ref"))))

(fn apply-store-size ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "store-size expects (store-size store-id)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "theater:simple/store" "calculate-total-size" (arg-tuple args)))
      (failure "store-size expects one string: store id"))))

; writers: the content string becomes a list<u8> via str-to-bytes before marshal.
(fn apply-store-put ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "store-put expects (store-put store-id content)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/store" "store"
        (marshal (sequence (list-push (list-push (list-new value)
          (list-get args 0)) (str-to-bytes (list-get args 1)))))))
      (failure "store-put expects two strings: store id and content"))))

(fn apply-store-label ((args (list value))) value
  (if (i32.ne (list-len args) 3) (failure "store-label expects (store-label store-id label content-ref)")
    (if (all-strings? args 0) (unmarshal (raw-invoke "theater:simple/store" "label" (arg-tuple args)))
      (failure "store-label expects three strings: store id, label, content ref"))))

(fn apply-store-put-at ((args (list value))) value
  (if (i32.ne (list-len args) 3) (failure "store-put-at expects (store-put-at store-id label content)")
    (if (all-strings? args 0)
      (unmarshal (raw-invoke "theater:simple/store" "store-at-label"
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
    (unmarshal (raw-invoke "theater:simple/runtime" "list-actors" (arg-tuple (list-new value))))))

(fn apply-runtime-status ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "actor-status expects (actor-status id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "theater:simple/runtime" "get-actor-status" (marshal (list-get args 0))))
      (failure "actor-status expects a string actor id"))))

(fn apply-runtime-stop ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "stop-actor expects (stop-actor id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "theater:simple/runtime" "stop-actor" (marshal (list-get args 0))))
      (failure "stop-actor expects a string actor id"))))

(fn apply-runtime-state ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "actor-state expects (actor-state id)")
    (if (string-arg? (list-get args 0))
      (if (self? (as-string (list-get args 0)))
        (failure "actor-state on self deadlocks (an actor cannot read its own state mid-eval); query another actor")
        (unmarshal (raw-invoke "theater:simple/runtime" "get-actor-state" (marshal (list-get args 0)))))
      (failure "actor-state expects a string actor id"))))

(fn apply-runtime-manifest ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "actor-manifest expects (actor-manifest id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "theater:simple/runtime" "get-actor-manifest" (marshal (list-get args 0))))
      (failure "actor-manifest expects a string actor id"))))

(fn apply-runtime-kill ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "kill-actor expects (kill-actor id)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "theater:simple/runtime" "kill-actor" (marshal (list-get args 0))))
      (failure "kill-actor expects a string actor id"))))

(fn apply-runtime-subscribe ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "subscribe-spawns expects no arguments")
    (unmarshal (raw-invoke "theater:simple/runtime" "subscribe-to-spawns" (arg-tuple (list-new value))))))

(fn apply-runtime-unsubscribe ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "unsubscribe-spawns expects no arguments")
    (unmarshal (raw-invoke "theater:simple/runtime" "unsubscribe-from-spawns" (arg-tuple (list-new value))))))

(fn apply-runtime-shutdown ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "shutdown-runtime expects no arguments")
    (unmarshal (raw-invoke "theater:simple/runtime" "shutdown-runtime" (arg-tuple (list-new value))))))

; --- message-server --------------------------------------------------------
; Shared shapes: (string, message) -> Tuple(String, list<u8>); a single string
; arg marshals bare (accepted by every host's parser, required by some).
(fn str-bytes-tuple ((a value) (b value)) any
  (marshal (sequence (list-push (list-push (list-new value) a) (str-to-bytes b)))))

(fn apply-msg-register ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "msg-register expects no arguments")
    (unmarshal (raw-invoke "theater:simple/message-server-host" "register" (arg-tuple (list-new value))))))
(fn apply-msg-list-requests ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "msg-list-requests expects no arguments")
    (unmarshal (raw-invoke "theater:simple/message-server-host" "list-outstanding-requests" (arg-tuple (list-new value))))))
(fn apply-msg-send ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-send expects (msg-send actor-id message)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/message-server-host" "send" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-send expects two strings: actor id and message"))))
(fn apply-msg-request ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-request expects (msg-request actor-id message)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/message-server-host" "request" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-request expects two strings: actor id and message"))))
(fn apply-msg-respond ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-respond expects (msg-respond request-id response)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/message-server-host" "respond-to-request" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-respond expects two strings: request id and response"))))
(fn apply-msg-cancel ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "msg-cancel expects (msg-cancel request-id)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/message-server-host" "cancel-request" (marshal (list-get args 0))))
      (failure "msg-cancel expects a string request id"))))
(fn apply-msg-open ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-open expects (msg-open actor-id initial-message)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/message-server-host" "open-channel" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-open expects two strings: actor id and initial message"))))
(fn apply-msg-send-channel ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "msg-send-channel expects (msg-send-channel channel-id message)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/message-server-host" "send-on-channel" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "msg-send-channel expects two strings: channel id and message"))))
(fn apply-msg-close-channel ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "msg-close-channel expects (msg-close-channel channel-id)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/message-server-host" "close-channel" (marshal (list-get args 0))))
      (failure "msg-close-channel expects a string channel id"))))

; --- assembler -------------------------------------------------------------
(fn apply-wat-to-wasm ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "wat-to-wasm expects (wat-to-wasm wat-text)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "wisp:assembler/runtime" "wat-to-wasm" (marshal (list-get args 0))))
      (failure "wat-to-wasm expects a string of WAT"))))

; --- timer -----------------------------------------------------------------
(fn as-u64 ((v value)) value
  (value-case v
    ((integer n) (u64-value (i64.extend_i32_s n)))
    ((wide-integer n) (u64-value n))
    ((u64-value n) (u64-value n))
    (else (u64-value (i64.const 0)))))
; Coerce a REPL integer to a CGRF u32 argument (e.g. tcp-receive's max-bytes),
; which the host rejects if it crosses as a plain s32.
(fn as-u32 ((v value)) value
  (value-case v
    ((integer n) (u32-value n))
    ((wide-integer n) (u32-value (i32.wrap_i64 n)))
    ((u64-value n) (u32-value (i32.wrap_i64 n)))
    (else (u32-value (i32.const 0)))))
(fn apply-timer-now ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "now expects no arguments")
    (unmarshal (raw-invoke "theater:simple/timer" "now" (arg-tuple (list-new value))))))
(fn apply-timer-set ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "set-interval expects (set-interval name interval-ms)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "theater:simple/timer" "set-interval"
        (marshal (sequence (list-push (list-push (list-new value)
          (list-get args 0)) (as-u64 (list-get args 1)))))))
      (failure "set-interval expects a string name and an integer interval"))))
(fn apply-timer-clear ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "clear-interval expects (clear-interval name)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/timer" "clear-interval" (marshal (list-get args 0))))
      (failure "clear-interval expects a string name"))))

; --- filesystem ------------------------------------------------------------
; Single-path verbs marshal the path bare; write/append send (path, list<u8>).
(fn apply-fs-read ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "fs-read expects (fs-read path)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/filesystem" "read-file" (marshal (list-get args 0))))
      (failure "fs-read expects a string path"))))
(fn apply-fs-exists ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "fs-exists expects (fs-exists path)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/filesystem" "exists" (marshal (list-get args 0))))
      (failure "fs-exists expects a string path"))))
(fn apply-fs-list ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "fs-list expects (fs-list path)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/filesystem" "list-dir" (marshal (list-get args 0))))
      (failure "fs-list expects a string path"))))
(fn apply-fs-meta ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "fs-meta expects (fs-meta path)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/filesystem" "metadata" (marshal (list-get args 0))))
      (failure "fs-meta expects a string path"))))
(fn apply-fs-delete ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "fs-delete expects (fs-delete path)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/filesystem" "delete-file" (marshal (list-get args 0))))
      (failure "fs-delete expects a string path"))))
(fn apply-fs-mkdir ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "fs-mkdir expects (fs-mkdir path)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/filesystem" "create-dir" (marshal (list-get args 0))))
      (failure "fs-mkdir expects a string path"))))
(fn apply-fs-rmdir ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "fs-rmdir expects (fs-rmdir path)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/filesystem" "remove-dir" (marshal (list-get args 0))))
      (failure "fs-rmdir expects a string path"))))
(fn apply-fs-write ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "fs-write expects (fs-write path content)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/filesystem" "write-file" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "fs-write expects two strings: path and content"))))
(fn apply-fs-append ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "fs-append expects (fs-append path content)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/filesystem" "append-file" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "fs-append expects two strings: path and content"))))

; --- terminal --------------------------------------------------------------
(fn bool-arg? ((v value)) s32 (value-case v ((boolean b) 1) (else 0)))
(fn apply-term-write ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "term-write expects (term-write text)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/terminal" "write-stdout" (marshal (str-to-bytes (list-get args 0)))))
      (failure "term-write expects a string"))))
(fn apply-term-write-err ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "term-write-err expects (term-write-err text)")
    (if (string-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/terminal" "write-stderr" (marshal (str-to-bytes (list-get args 0)))))
      (failure "term-write-err expects a string"))))
(fn apply-term-raw ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "term-raw expects (term-raw true|false)")
    (if (bool-arg? (list-get args 0)) (unmarshal (raw-invoke "theater:simple/terminal" "set-raw-mode" (marshal (list-get args 0))))
      (failure "term-raw expects a boolean"))))
(fn apply-term-size ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "term-size expects no arguments")
    (unmarshal (raw-invoke "theater:simple/terminal" "get-size" (arg-tuple (list-new value))))))
(fn apply-term-input ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "term-input expects no arguments")
    (unmarshal (raw-invoke "theater:simple/terminal" "enable-input" (arg-tuple (list-new value))))))

; --- http-client -----------------------------------------------------------
; Build an http-request record value (empty headers, no body) and marshal it as
; the record ARG. field names/order match the pact; headers is an empty
; list<http-header>, body a none option<list<u8>> — their element/inner type tags
; are emitted by the codec from the descriptors below.
(fn http-request-blob ((method string) (url string)) any
  (marshal-record "http-request"
    (list-push (list-push (list-push (list-push (list-new value)
      (symbol "method")) (symbol "url")) (symbol "headers")) (symbol "body"))
    (list-push (list-push (list-push (list-push (list-new value)
      (text method)) (text url))
      (typed-list (symbol "http-header") (list-new value)))
      (compound (unary-type "option" (unary-type "list" (symbol "u8"))) "none" (list-new value)))))
(fn apply-http-req ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "http-req expects (http-req method url)")
    (if (all-strings? args 0)
      (unmarshal (raw-invoke "theater:simple/http-client" "request"
        (http-request-blob (as-string (list-get args 0)) (as-string (list-get args 1)))))
      (failure "http-req expects two strings: method and url"))))
(fn apply-http-get ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "http-get expects (http-get url)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "theater:simple/http-client" "request"
        (http-request-blob "GET" (as-string (list-get args 0)))))
      (failure "http-get expects a string url"))))

; --- tcp -------------------------------------------------------------------
; raw-invoke needs a literal import name, so one verb per import. one-str? and
; two-str? guard the common arg shapes; send/receive handle list<u8> / u32.
(fn one-str? ((args (list value))) s32 (i32.and (i32.eq (list-len args) 1) (string-arg? (list-get args 0))))
(fn two-str? ((args (list value))) s32 (i32.and (i32.eq (list-len args) 2) (all-strings? args 0)))
(fn apply-tcp-connect ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "connect" (marshal (list-get args 0)))) (failure "tcp-connect expects an address")))
(fn apply-tcp-listen ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "listen" (marshal (list-get args 0)))) (failure "tcp-listen expects an address")))
(fn apply-tcp-accept ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "accept" (marshal (list-get args 0)))) (failure "tcp-accept expects a listener id")))
(fn apply-tcp-activate ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "activate" (marshal (list-get args 0)))) (failure "tcp-activate expects a connection id")))
(fn apply-tcp-peer ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "peer-address" (marshal (list-get args 0)))) (failure "tcp-peer expects a connection id")))
(fn apply-tcp-is-tls ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "is-tls" (marshal (list-get args 0)))) (failure "tcp-is-tls expects a connection id")))
(fn apply-tcp-close ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "close" (marshal (list-get args 0)))) (failure "tcp-close expects a connection id")))
(fn apply-tcp-close-listener ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "close-listener" (marshal (list-get args 0)))) (failure "tcp-close-listener expects a listener id")))
(fn apply-tcp-tls-server ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "upgrade-to-tls-server" (marshal (list-get args 0)))) (failure "tcp-tls-server expects a connection id")))
(fn apply-tcp-set-active ((args (list value))) value
  (if (two-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "set-active" (arg-tuple args))) (failure "tcp-set-active expects connection id and mode")))
(fn apply-tcp-transfer ((args (list value))) value
  (if (two-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "transfer" (arg-tuple args))) (failure "tcp-transfer expects connection id and target actor")))
(fn apply-tcp-transfer-async ((args (list value))) value
  (if (two-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "transfer-async" (arg-tuple args))) (failure "tcp-transfer-async expects connection id and target actor")))
(fn apply-tcp-tls-client ((args (list value))) value
  (if (two-str? args) (unmarshal (raw-invoke "theater:simple/tcp" "upgrade-to-tls-client" (arg-tuple args))) (failure "tcp-tls-client expects connection id and server name")))
(fn apply-tcp-send ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "tcp-send expects (tcp-send conn-id data)")
    (if (i32.and (string-arg? (list-get args 0)) (string-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/tcp" "send" (str-bytes-tuple (list-get args 0) (list-get args 1))))
      (failure "tcp-send expects two strings: connection id and data"))))
(fn apply-tcp-receive ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "tcp-receive expects (tcp-receive conn-id max-bytes)")
    (if (string-arg? (list-get args 0))
      (unmarshal (raw-invoke "theater:simple/tcp" "receive"
        (marshal (sequence (list-push (list-push (list-new value) (list-get args 0)) (as-u32 (list-get args 1)))))))
      (failure "tcp-receive expects a connection id and an integer max-bytes"))))

; --- podman ----------------------------------------------------------------
; A minimal container-spec: image + name, empty env/mounts/cmd, tty/interactive
; off. The empty collections carry their element type descriptors.
(fn container-spec-blob ((image string) (nm string)) any
  (marshal-record "container-spec"
    (list-push (list-push (list-push (list-push (list-push (list-push (list-push (list-new value)
      (symbol "image")) (symbol "name")) (symbol "env")) (symbol "mounts")) (symbol "cmd")) (symbol "tty")) (symbol "interactive"))
    (list-push (list-push (list-push (list-push (list-push (list-push (list-push (list-new value)
      (text image)) (text nm))
      (typed-list (sequence (list-push (list-push (list-push (list-new value) (symbol "tuple")) (symbol "string")) (symbol "string"))) (list-new value)))
      (typed-list (symbol "mount-spec") (list-new value)))
      (typed-list (symbol "string") (list-new value)))
      (boolean (i32.const 0)))
      (boolean (i32.const 0)))))
(fn apply-podman-run ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "podman-run expects (podman-run image name)")
    (if (all-strings? args 0)
      (unmarshal (raw-invoke "theater:simple/podman" "run"
        (container-spec-blob (as-string (list-get args 0)) (as-string (list-get args 1)))))
      (failure "podman-run expects image and name strings"))))
(fn apply-podman-stop ((args (list value))) value
  (if (one-str? args) (unmarshal (raw-invoke "theater:simple/podman" "stop" (marshal (list-get args 0)))) (failure "podman-stop expects a name")))
(fn apply-podman-rm ((args (list value))) value
  (if (i32.ne (list-len args) 2) (failure "podman-rm expects (podman-rm name force)")
    (if (i32.and (string-arg? (list-get args 0)) (bool-arg? (list-get args 1)))
      (unmarshal (raw-invoke "theater:simple/podman" "rm"
        (marshal (sequence (list-push (list-push (list-new value) (list-get args 0)) (list-get args 1))))))
      (failure "podman-rm expects a name string and a boolean force"))))
(fn apply-podman-list ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "podman-list expects no arguments")
    (unmarshal (raw-invoke "theater:simple/podman" "list" (arg-tuple (list-new value))))))

; --- inbound events --------------------------------------------------------
; Drain the buffered triggers: each is (handler-name event result). Clears the
; buffer so you see only what fired since the last poll.
(fn apply-poll-events ((args (list value))) value
  (if (i32.ne (list-len args) 0) (failure "poll-events expects no arguments")
    (let (evs (global.get $events))
      (begin (global.set $events (list-new value)) (sequence evs)))))

; Live catalog of the Theater host verbs, so a session can discover them cold.
(fn apply-help ((args (list value))) value
  (text (string-append "Wisp REPL — a live session that drives and observes Theater.\nDefinitions accumulate: (define x ...) / (define f (lambda ...)) persist.\n\n"
    (string-append "self/rpc:  (self) (log msg) (describe id) (exports id) (implements id iface) (call id fn arg...)\n"
      (string-append "store:     (store-new) (store-put id text) (store-get id ref) (store-label id lbl ref)\n           (store-get-by-label id lbl) (store-list-labels id) (store-exists id ref) (store-size id) (store-put-at id lbl text)\n"
        (string-append "runtime:   (list-actors) (actor-status id) (actor-state id) (actor-manifest id)\n           (stop-actor id) (kill-actor id) (subscribe-spawns) (unsubscribe-spawns) (shutdown-runtime)\n"
          (string-append "messaging: (msg-register) (msg-send id text) (msg-request id text) (msg-list-requests)\n           (msg-respond req text) (msg-cancel req) (msg-open id text) (msg-send-channel cid text) (msg-close-channel cid)\n"
            (string-append "files:     (fs-read p) (fs-write p text) (fs-exists p) (fs-list p) (fs-meta p) (fs-append p text) (fs-delete p) (fs-mkdir p) (fs-rmdir p)\n"
              (string-append "http:      (http-get url) (http-req method url)        assembler: (wat-to-wasm wat-text)\n"
                (string-append "timer:     (now) (set-interval name ms) (clear-interval name)        terminal: (term-write s) (term-size) (term-raw bool) ...\n"
                  "triggers:  define on-tick / on-spawn / on-message (lambda (e) ...); then (poll-events) to see what fired + results"))))))))))

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
    ((string=? name "fs-read") (apply-fs-read args))
    ((string=? name "fs-exists") (apply-fs-exists args))
    ((string=? name "fs-list") (apply-fs-list args))
    ((string=? name "fs-meta") (apply-fs-meta args))
    ((string=? name "fs-write") (apply-fs-write args))
    ((string=? name "fs-append") (apply-fs-append args))
    ((string=? name "fs-delete") (apply-fs-delete args))
    ((string=? name "fs-mkdir") (apply-fs-mkdir args))
    ((string=? name "fs-rmdir") (apply-fs-rmdir args))
    ((string=? name "term-write") (apply-term-write args))
    ((string=? name "term-write-err") (apply-term-write-err args))
    ((string=? name "term-raw") (apply-term-raw args))
    ((string=? name "term-size") (apply-term-size args))
    ((string=? name "term-input") (apply-term-input args))
    ((string=? name "http-get") (apply-http-get args))
    ((string=? name "http-req") (apply-http-req args))
    ((string=? name "poll-events") (apply-poll-events args))
    ((string=? name "help") (apply-help args))
    ((string=? name "tcp-connect") (apply-tcp-connect args))
    ((string=? name "tcp-listen") (apply-tcp-listen args))
    ((string=? name "tcp-accept") (apply-tcp-accept args))
    ((string=? name "tcp-activate") (apply-tcp-activate args))
    ((string=? name "tcp-set-active") (apply-tcp-set-active args))
    ((string=? name "tcp-transfer") (apply-tcp-transfer args))
    ((string=? name "tcp-transfer-async") (apply-tcp-transfer-async args))
    ((string=? name "tcp-peer") (apply-tcp-peer args))
    ((string=? name "tcp-is-tls") (apply-tcp-is-tls args))
    ((string=? name "tcp-send") (apply-tcp-send args))
    ((string=? name "tcp-receive") (apply-tcp-receive args))
    ((string=? name "tcp-close") (apply-tcp-close args))
    ((string=? name "tcp-close-listener") (apply-tcp-close-listener args))
    ((string=? name "tcp-tls-server") (apply-tcp-tls-server args))
    ((string=? name "tcp-tls-client") (apply-tcp-tls-client args))
    ((string=? name "podman-run") (apply-podman-run args))
    ((string=? name "podman-stop") (apply-podman-stop args))
    ((string=? name "podman-rm") (apply-podman-rm args))
    ((string=? name "podman-list") (apply-podman-list args))
    (else (failure (string-append "unknown host builtin: " name)))))
