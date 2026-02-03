; repl-actor.lisp — Theater actor that evaluates Wisp expressions
;
; Exports:
;   theater:simple/actor.init — actor initialization
;   theater:simple/message-server-client.handle-send — fire-and-forget messages (ignored)
;   theater:simple/message-server-client.handle-request — request-response (evaluates Wisp)
;
; Imports:
;   theater:simple/runtime.log — logging
;   wisp:evaluator.eval-request — evaluates Wisp source, returns result
;
; The actor receives Wisp source code as the request body (UTF-8 bytes),
; evaluates it via the eval-request host function, and returns the result.

(import theater:simple/runtime log ((msg string)) s32)
(import wisp:evaluator eval-request
  ((params (tuple string (list u8))))
  (tuple (option (list u8))))

(export "theater:simple/actor.init"
  (fn init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "REPL actor initialized!")
      (ok (tuple (option (list u8))) string
          (tuple state)))))

(export "theater:simple/message-server-client.handle-send"
  (fn handle-send ((state (option (list u8))) (params (tuple (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "REPL actor received send (ignoring)")
      (ok (tuple (option (list u8))) string
          (tuple state)))))

(export "theater:simple/message-server-client.handle-request"
  (fn handle-request ((state (option (list u8))) (params (tuple string (list u8))))
    (result (tuple (option (list u8)) (tuple (option (list u8)))) string)
    (begin
      (log "Evaluating request...")
      (let (response (eval-request params))
        (ok (tuple (option (list u8)) (tuple (option (list u8)))) string
            (tuple state response))))))
