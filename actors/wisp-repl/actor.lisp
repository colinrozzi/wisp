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

; Inbound triggers: Theater calls these exports when events occur; each dispatches
; to a user-defined handler in the live session (define it from the REPL), buffering
; the firing + result for (poll-events). handle-tick's arg is a bare timer-name
; string; the others arrive as a Tuple of args, so we take the raw CGRF as `any` and
; unmarshal it (the handler receives a (sequence ...) of the args). Returns are
; ignored by the runtime (handle-request's reply is a future refinement).
(export "theater:simple/timer.handle-tick"
  (fn handle-tick ((name string)) string
    (begin (dispatch-event "on-tick" (text name)) "ok")))
; After (subscribe-spawns): on-spawn receives (sequence id name parent-option).
(export "theater:simple/runtime-handlers.handle-actor-spawn"
  (fn handle-actor-spawn ((raw any)) string
    (begin (dispatch-event "on-spawn" (unmarshal raw)) "ok")))
; After (msg-register): on-message receives (sequence message-bytes).
(export "theater:simple/message-server-client.handle-send"
  (fn handle-send ((raw any)) string
    (begin (dispatch-event "on-message" (unmarshal raw)) "ok")))

; TCP server callbacks (theater:simple/tcp-client). After (tcp-listen addr),
; Theater runs a background accept loop and delivers events by CALLING these
; exports — so a TCP server in the REPL is just defining on-connection / on-data /
; on-close handlers (like on-tick). handle-connection gets a bare connection id;
; on-data/on-close arrive as a Tuple (taken raw as `any`, unmarshalled to a
; sequence). on-data fires only for connections in active/once mode (set-active).
; The pact return is result<_, string>, but the runtime only checks for a
; transport error and otherwise ignores the value (and the dispatch already ran),
; so — like handle-tick/handle-send — we return a plain string.
(export "theater:simple/tcp-client.handle-connection"
  (fn tcp-handle-connection ((connection-id string)) string
    (begin (dispatch-event "on-connection" (text connection-id)) "ok")))
(export "theater:simple/tcp-client.handle-connection-transfer"
  (fn tcp-handle-transfer ((connection-id string)) string
    (begin (dispatch-event "on-connection" (text connection-id)) "ok")))
(export "theater:simple/tcp-client.on-data"
  (fn tcp-on-data ((raw any)) string
    (begin (dispatch-event "on-data" (unmarshal raw)) "ok")))
(export "theater:simple/tcp-client.on-close"
  (fn tcp-on-close ((raw any)) string
    (begin (dispatch-event "on-close" (unmarshal raw)) "ok")))
