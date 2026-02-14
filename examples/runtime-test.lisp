; Test actor that imports theater:simple/runtime
; This can be used to verify interface hash matching with Theater's RuntimeHandler

; Import the runtime interface with matching signatures
; log takes a string and returns unit (empty tuple)
(import theater:simple/runtime log ((msg string)) unit)

; Actor init function required by Theater
; Takes state (option<list<u8>>) and returns result<tuple<option<list<u8>>>, string>
; For now, we just pass through the state unchanged
(export "theater:simple/actor.init" (fn init ((state (option (list u8)))) (result (tuple (option (list u8))) string)
  (begin
    (log "Wisp actor initializing...")
    (ok (tuple (option (list u8))) string (tuple state)))))

; Simple function that calls log and returns unit
(export (fn test-log () unit
  (log "Hello from Wisp test actor!")))
