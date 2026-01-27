; Theater Eval Wrapper
; Generic wrapper that bridges compiled Wisp expressions to Theater's actor model
;
; This component:
; - Imports `eval` from the composed Wisp expression module
; - Exports `init` for Theater actor interface
; - Calls eval() and returns the result

; Import the eval function from the compiled expression
; (The actual expression module will be composed with this wrapper)
(import wisp-expr eval () s32)

; Theater actor init function
; Calls eval and returns the result
(export (fn init () s32
  (eval)))
