; Wisp REPL actor. A pure-Wisp Theater actor, mirroring ../actors/lisp/*:
; the Theater ABI is declared here in Wisp and the stock compiler emits the
; interface-qualified exports plus CGRF metadata. No Rust adapter is involved.
;
; The evaluator is shared with the local REPL; one compiled module is one live
; session that owns its own bindings, closures, types, macros, and heap.
(include "../../interpreter/evaluator.lisp")

; Theater lifecycle. Config is ignored: a fresh module is an empty session, so
; init is not a reset. State is the standard option<list<u8>> the runtime
; threads through handlers; we carry nothing in it.
(export "theater:simple/actor.init"
  (fn actor-init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
    (ok (tuple (option (list u8))) string (tuple (none (list u8))))))

; The public evaluation operation, reachable through rpc.call and discoverable
; through rpc.describe. Source in, printed value (or "error:" diagnostic) out.
(export "theater:simple/wisp.evaluate"
  (fn actor-evaluate ((source string)) string
    (evaluate source)))

; Inbound trigger: Theater calls this when a timer interval fires. We dispatch to
; a user-defined `on-tick` in the live session (define it from the REPL); the
; firing + the handler's result are buffered for (poll-events). The return is
; ignored by the runtime, so a trap-free "ok" suffices.
(export "theater:simple/timer.handle-tick"
  (fn handle-tick ((name string)) string
    (begin (dispatch-event "on-tick" (text name)) "ok")))
