; Wisp REPL actor. A pure-Wisp Theater actor, mirroring ../actors/lisp/*:
; the Theater ABI is declared here in Wisp and the stock compiler emits the
; interface-qualified exports plus CGRF metadata. No Rust adapter is involved.
;
; The evaluator is shared with the local REPL; one compiled module is one live
; session that owns its own bindings, closures, types, macros, and heap.
(include "../interpreter/evaluator.wisp")

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
; After (msg-register): on-message receives a 1-element list (message-bytes);
; a handler reads the text with (bytes->string (car e)).
(export "theater:simple/message-server-client.handle-send"
  (fn handle-send ((raw any)) string
    (begin (dispatch-event "on-message" (unmarshal raw)) "ok")))
; Channel callbacks (same interface). After a peer (msg-open target initial-msg),
; Theater delivers the open/messages/close here — dispatched to on-channel-open /
; on-channel-message / on-channel-close for (poll-events). open/message arrive as a
; Tuple (channel-id message-bytes); close as (channel-id). handle-channel-open must
; return a channel-accept {accepted message} — the host reads field 0 (accepted) to
; decide whether to open; we always accept (true, none). The others' returns are
; ignored, so a plain string is fine (by-name delivery, not has_export-gated).
(export "theater:simple/message-server-client.handle-channel-open"
  (fn handle-channel-open ((raw any))
    (tuple bool (option (list u8)))
    (begin
      (dispatch-event "on-channel-open" (unmarshal raw))
      (tuple (bool 1) (none (list u8))))))
(export "theater:simple/message-server-client.handle-channel-message"
  (fn handle-channel-message ((raw any)) string
    (begin (dispatch-event "on-channel-message" (unmarshal raw)) "ok")))
(export "theater:simple/message-server-client.handle-channel-close"
  (fn handle-channel-close ((raw any)) string
    (begin (dispatch-event "on-channel-close" (unmarshal raw)) "ok")))

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

; Process callbacks (theater:simple/process-client). After (process-set-active id
; "active"|"once"), Theater pushes each output chunk here as on-output and the final
; on-exit — delivered by CALLING these exports (by name, not has_export-gated, like
; tcp), so observing a process is just defining on-output / on-exit / on-process-
; transfer handlers and draining (poll-events). on-output arrives as a Tuple
; (process-id stream data) and on-exit as (process-id status-record), taken raw as
; `any` and unmarshalled to a sequence; handle-process-transfer gets a bare id.
(export "theater:simple/process-client.on-output"
  (fn process-on-output ((raw any)) string
    (begin (dispatch-event "on-output" (unmarshal raw)) "ok")))
(export "theater:simple/process-client.on-exit"
  (fn process-on-exit ((raw any)) string
    (begin (dispatch-event "on-exit" (unmarshal raw)) "ok")))
(export "theater:simple/process-client.handle-process-transfer"
  (fn process-on-transfer ((process-id string)) string
    (begin (dispatch-event "on-process-transfer" (text process-id)) "ok")))

; Lifecycle monitoring (theater:simple/lifecycle-handlers). After (monitor id),
; Theater delivers every chain event of the monitored actor here, dispatched to a
; user on-actor-event handler as (sequence subject event-type data) — route on
; event-type (a string); data is the pack-encoded ChainEventPayload as bytes.
; This export MUST match the pact signature exactly: the lifecycle handler gates
; delivery on has_export(lifecycle-handlers, handle-actor-event), so a (raw any)
; shortcut (fine for tcp/message callbacks, which call by name) makes has_export
; false here and every event is silently dropped.
; Return type is (result bool string): pack maps result<_, E> to Result{ok:Bool,E},
; so this hashes identically to the pact's result<_, string> while giving us a
; constructible ok value. The 3-param signature must match the pact exactly (the
; lifecycle handler gates delivery on has_export). on-actor-event receives
; (sequence subject event-type) — route on event-type; the raw ChainEventPayload
; bytes in `data` are not yet surfaced (needs a list<u8> -> value helper; follow-up).
(export "theater:simple/lifecycle-handlers.handle-actor-event"
  (fn handle-actor-event ((subject string) (event-type string) (data (list u8)))
    (result bool string)
    (begin
      (dispatch-event "on-actor-event"
        (sequence (list-push (list-push (list-new value)
          (text subject)) (text event-type))))
      (ok bool string (bool 1)))))
