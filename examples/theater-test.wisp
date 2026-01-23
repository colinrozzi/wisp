; Theater Actor Test
; Tests external WIT package loading and referencing

; Declare we're using external WIT packages
(world theater-test
  (wit-deps wit/deps/theater-simple)
  (import theater:simple/runtime))

; For now, just export a simple test function
; (We can't implement the full actor interface yet due to complex types)
(fn test-value () s32
  42)

(export test-value)
