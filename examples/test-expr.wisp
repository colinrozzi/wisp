; Test expression module - exports eval
; This is what the self-hosted compiler would generate for "(i32.add 40 2)"

(export (fn eval () s32
  (i32.add (i32.const 40) (i32.const 2))))
