(include "../shared.lisp")
(fn bump () s32
  (begin (global.set $count (i32.add (global.get $count) (global.get $step))) (global.get $count)))
