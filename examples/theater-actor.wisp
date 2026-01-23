; Simple Theater Actor
; Demonstrates external WIT package import

; Declare external WIT package usage
(world hello-actor
  (wit-deps wit/deps/theater-simple)
  (import theater:simple/runtime))

; For now, just a simple function that returns a value
; (Calling log() requires void return type support which wisp doesn't have yet)
(fn hello () s32
  42)

(export hello)
