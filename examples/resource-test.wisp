; Test for WIT resource types
; Resources are opaque handles managed externally

; Declare a resource type
(resource sexpr)

; A function that takes a borrowed resource handle
; In the future, the host would implement operations on sexpr
(fn identity ((s (borrow sexpr))) s32
  (i32.const 42))

; Export the function
(export identity)
