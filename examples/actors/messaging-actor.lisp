; messaging-actor.lisp — Theater actor with message-server handlers
;
; Exports:
;   theater:simple/actor.init — actor initialization
;   theater:simple/message-server-client.handle-send — fire-and-forget messages
;   theater:simple/message-server-client.handle-request — request-response messages
;
; All handlers pass state through unchanged. handle-request returns None as
; the response body.

(import theater:simple/runtime log ((msg string)) s32)

(export "theater:simple/actor.init"
  (fn init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Messaging actor initialized!")
      (ok (tuple (option (list u8))) string
          (tuple state)))))

(export "theater:simple/message-server-client.handle-send"
  (fn handle-send ((state (option (list u8))) (params (tuple (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Received message!")
      (ok (tuple (option (list u8))) string
          (tuple state)))))

(export "theater:simple/message-server-client.handle-request"
  (fn handle-request ((state (option (list u8))) (params (tuple string (list u8))))
    (result (tuple (option (list u8)) (tuple (option (list u8)))) string)
    (begin
      (log "Received request!")
      (ok (tuple (option (list u8)) (tuple (option (list u8)))) string
          (tuple state (tuple (none (list u8))))))))
