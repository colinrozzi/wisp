; Wisp REPL actor. A pure-Wisp Theater actor, mirroring ../actors/lisp/*:
; the Theater ABI is declared here in Wisp and the stock compiler emits the
; interface-qualified exports plus CGRF metadata. No Rust adapter is involved.
;
; The evaluator is shared with the local REPL; one compiled module is one live
; session that owns its own bindings, closures, types, macros, and heap.
(include "../interpreter/evaluator.wisp")

; Theater lifecycle. A fresh module is an empty session, so init is not a reset,
; and we carry nothing in the state we hand back.
;
; A start file (0156, the root supervisor). With no initial state, or none, the
; session starts empty, as before. A manifest's initial_state (a string) of
;   start PATH
;   report ACTOR-ID
; makes it run PATH (relative to this actor's fs root) as its start file, the
; way the boot list runs one, and send the actor named by report a message:
; "start ready SELF" when the file evaluates, "start failed SELF REASON" when it
; doesn't. The file runs on a 1 ms timer after init returns, never inside it:
; Theater's spawn waits for the child's init, so a start file that hangs must
; not hang its spawner (lux, tova 2026-11-19). Any other string is refused, so
; spawn-actor fails, rather than ignored.
;
; The report line may also carry a token, `report ACTOR-ID TOKEN` (16 to 64 of
; [0-9a-f]); the messages then carry it after SELF: "start ready SELF TOKEN",
; "start failed SELF TOKEN REASON". A message doesn't say who sent it, so a
; spawner that gives each child its own secret token can tell that child's
; report from one a sibling forged (lux, 2026-11-19).
(global $start-path string mut 0)
(global $start-report string mut 0)
(global $start-token string mut 0)

(export "theater:simple/actor.init"
  (fn actor-init ((state any))
    (result (tuple (option (list u8))) string)
    (value-case (start-state (unmarshal state))
      ((text s)
        (let (why (start-parse s))
          (if (string-len why)
            (err (tuple (option (list u8))) string (string-append "start: " why))
            (value-case (unmarshal (raw-invoke "theater:simple/timer" "set-interval"
                          (marshal (sequence (list-push (list-push (list-new value)
                            (text (start-timer))) (u64-value (i64.const 1)))))))
              ((compound ty case-name fields)
                (if (string=? case-name "ok")
                  (ok (tuple (option (list u8))) string (tuple (none (list u8))))
                  (err (tuple (option (list u8))) string "start: no timer (the start file needs the timer handler)")))
              (else (err (tuple (option (list u8))) string "start: no timer (the start file needs the timer handler)"))))))
      (else (ok (tuple (option (list u8))) string (tuple (none (list u8))))))))

; init's `any` is the argument tuple, a one-element sequence: its element is
; the state (none, or a manifest's initial_state as text)
(fn start-state ((args value)) value
  (value-case args
    ((sequence items) (if (i32.eq (list-len items) 1) (list-get items 0) (nil)))
    (else args)))

(fn start-timer () string "wisp.start")

; "" when s is exactly "start PATH\nreport ID\n" (the last newline optional),
; with PATH and ID valid, setting the globals; else why it isn't.
(fn start-parse ((s string)) string
  (let (nl (start-line-end s 0))
    (let (first (substring s 0 nl))
      (if (i32.eq (string-prefix? first "start ") 0) "the first line must be 'start PATH'"
        (let (path (substring first 6 (string-len first)))
          (let (why (start-path-why path))
            (if (string-len why) why
              (let (rest (if (i32.lt_s nl (string-len s)) (substring s (i32.add nl 1) (string-len s)) ""))
                (let (nl2 (start-line-end rest 0))
                  (let (second (substring rest 0 nl2))
                    (if (i32.eq (string-prefix? second "report ") 0) "the second line must be 'report ACTOR-ID'"
                      (let (words (substring second 7 (string-len second)))
                        (let (sp (start-space words 0))
                          (let (id (substring words 0 sp))
                            (let (token (if (i32.lt_s sp (string-len words)) (substring words (i32.add sp 1) (string-len words)) ""))
                              (if (i32.eq (start-id-ok id 0) 0) "the report id must be 1 to 64 of [0-9a-f-]"
                                (if (i32.and (i32.lt_s sp (string-len words)) (i32.eq (start-token-ok token 0) 0))
                                  "the report token must be 16 to 64 of [0-9a-f]"
                                  (if (i32.lt_s (i32.add nl2 1) (string-len rest)) "nothing may follow the report line"
                                    (begin
                                      (global.set $start-path path)
                                      (global.set $start-report id)
                                      (global.set $start-token token)
                                      "")))))))))))))))))))

(fn start-space ((s string) (i s32)) s32
  (if (i32.ge_s i (string-len s)) i
    (if (i32.eq (string-ref s i) 32) i (start-space s (i32.add i 1)))))

; 16 to 64 lowercase hex digits
(fn start-token-ok ((t string) (i s32)) s32
  (if (i32.lt_s (string-len t) 16) 0
    (if (i32.gt_s (string-len t) 64) 0
      (if (i32.ge_s i (string-len t)) 1
        (let (c (string-ref t i))
          (if (i32.or (i32.and (i32.ge_s c 48) (i32.le_s c 57)) (i32.and (i32.ge_s c 97) (i32.le_s c 102)))
            (start-token-ok t (i32.add i 1)) 0))))))

(fn start-line-end ((s string) (i s32)) s32
  (if (i32.ge_s i (string-len s)) i
    (if (i32.eq (string-ref s i) 10) i (start-line-end s (i32.add i 1)))))

; a path of 1 to 256 bytes, relative, each /-separated part non-empty and
; neither "." nor ".." (lux)
(fn start-path-why ((p string)) string
  (if (i32.eq (string-len p) 0) "the path is empty"
    (if (i32.gt_s (string-len p) 256) "the path is over 256 bytes"
      (if (i32.eq (string-ref p 0) 47) "the path must be relative to the fs root"
        (start-parts-why p 0 0)))))

(fn start-parts-why ((p string) (from s32) (i s32)) string
  (if (i32.lt_s i (string-len p))
    (if (i32.eq (string-ref p i) 47)
      (let (why (start-part-why (substring p from i))) (if (string-len why) why (start-parts-why p (i32.add i 1) (i32.add i 1))))
      (start-parts-why p from (i32.add i 1)))
    (start-part-why (substring p from i))))

(fn start-part-why ((part string)) string
  (if (i32.eq (string-len part) 0) "the path has an empty part"
    (if (string=? part ".") "the path has a '.' part"
      (if (string=? part "..") "the path has a '..' part" ""))))

(fn start-id-ok ((id string) (i s32)) s32
  (if (i32.eq (string-len id) 0) 0
    (if (i32.gt_s (string-len id) 64) 0
      (if (i32.ge_s i (string-len id)) 1
        (let (c (string-ref id i))
          (if (i32.or (i32.and (i32.ge_s c 48) (i32.le_s c 57))
                (i32.or (i32.and (i32.ge_s c 97) (i32.le_s c 102)) (i32.eq c 45)))
            (start-id-ok id (i32.add i 1)) 0))))))

; The start timer's tick: clear it, run the file once, and report. A report
; is "start ready SELF" or "start failed SELF REASON", REASON cut to 200 bytes.
(fn start-run () string
  (begin
    (unmarshal (raw-invoke "theater:simple/timer" "clear-interval" (marshal (text (start-timer)))))
    (let (path (global.get $start-path))
      (begin
        (global.set $start-path "")
        (let (src (read-source-from-path path))
          (let (out (value-case src
                      ((text s) (evaluate-from s path))
                      (else (string-append "error: cannot read " path))))
            (let (who (if (string-len (global.get $start-token))
                        (string-append (self) (string-append " " (global.get $start-token)))
                        (self)))
              (let (msg (if (string-prefix? out "error:")
                        (string-append "start failed " (string-append who (string-append " " (substring out 0 (if (i32.gt_s (string-len out) 200) 200 (string-len out))))))
                        (string-append "start ready " who)))
              (begin
                (unmarshal (raw-invoke "theater:simple/message-server-host" "send"
                  (str-bytes-tuple (text (global.get $start-report)) (text msg))))
                "ok")))))))))

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
; ignored by the runtime (handle-request's reply is a future refinement); each
; answers "ok" or "error: ..." (handler-answer) so the chain shows failed handlers.
(export "theater:simple/timer.handle-tick"
  (fn handle-tick ((name string)) string
    (if (i32.and (string=? name (start-timer)) (i32.ne (string-len (global.get $start-path)) 0))
      (start-run)
      (handler-answer (dispatch-event "on-tick" (text name))))))
; After (subscribe-spawns): on-spawn receives (sequence id name parent-option).
(export "theater:simple/runtime-handlers.handle-actor-spawn"
  (fn handle-actor-spawn ((raw any)) string
    (handler-answer (dispatch-event "on-spawn" (unmarshal raw)))))
; After (msg-register): on-message receives a 1-element list (message-bytes);
; a handler reads the text with (bytes->string (car e)).
(export "theater:simple/message-server-client.handle-send"
  (fn handle-send ((raw any)) string
    (handler-answer (dispatch-event "on-message" (unmarshal raw)))))
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
    (handler-answer (dispatch-event "on-channel-message" (unmarshal raw)))))
(export "theater:simple/message-server-client.handle-channel-close"
  (fn handle-channel-close ((raw any)) string
    (handler-answer (dispatch-event "on-channel-close" (unmarshal raw)))))

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
    (handler-answer (dispatch-event "on-connection" (text connection-id)))))
(export "theater:simple/tcp-client.handle-connection-transfer"
  (fn tcp-handle-transfer ((connection-id string)) string
    (handler-answer (dispatch-event "on-connection" (text connection-id)))))
(export "theater:simple/tcp-client.on-data"
  (fn tcp-on-data ((raw any)) string
    (handler-answer (dispatch-event "on-data" (unmarshal raw)))))
(export "theater:simple/tcp-client.on-close"
  (fn tcp-on-close ((raw any)) string
    (handler-answer (dispatch-event "on-close" (unmarshal raw)))))

; Process callbacks (theater:simple/process-client). After (process-set-active id
; "active"|"once"), Theater pushes each output chunk here as on-output and the final
; on-exit — delivered by CALLING these exports (by name, not has_export-gated, like
; tcp), so observing a process is just defining on-output / on-exit / on-process-
; transfer handlers and draining (poll-events). on-output arrives as a Tuple
; (process-id stream data) and on-exit as (process-id status-record), taken raw as
; `any` and unmarshalled to a sequence; handle-process-transfer gets a bare id.
(export "theater:simple/process-client.on-output"
  (fn process-on-output ((raw any)) string
    (handler-answer (dispatch-event "on-output" (unmarshal raw)))))
(export "theater:simple/process-client.on-exit"
  (fn process-on-exit ((raw any)) string
    (handler-answer (dispatch-event "on-exit" (unmarshal raw)))))
(export "theater:simple/process-client.handle-process-transfer"
  (fn process-on-transfer ((process-id string)) string
    (handler-answer (dispatch-event "on-process-transfer" (text process-id)))))

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
