; Test actor that imports theater:simple/runtime
; This can be used to verify interface hash matching with Theater's RuntimeHandler

; Import the runtime interface with matching signatures
; log takes a string and returns unit (empty tuple)
(import theater:simple/runtime log ((msg string)) unit)

; Simple function that calls log and returns unit
(export (fn test-log () unit
  (log "Hello from Wisp test actor!")))
