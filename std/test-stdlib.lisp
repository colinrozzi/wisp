; std/test-stdlib.lisp — End-to-end test for stdlib packages

(import math abs ((x s32)) s32)
(import math min ((a s32) (b s32)) s32)
(import math max ((a s32) (b s32)) s32)
(import math pow ((base s32) (exp s32)) s32)
(import logic not ((x s32)) s32)
(import logic and ((a s32) (b s32)) s32)

(export
  (fn test-math () s32
    ; abs(-5) + min(3,7) + max(3,7) + pow(2,3) = 5 + 3 + 7 + 8 = 23
    (i32.add
      (i32.add (abs (i32.const -5)) (min (i32.const 3) (i32.const 7)))
      (i32.add (max (i32.const 3) (i32.const 7)) (pow (i32.const 2) (i32.const 3))))))

(export
  (fn test-logic () s32
    ; not(0) + and(1,1) + not(and(1,0)) = 1 + 1 + 1 = 3
    (i32.add
      (i32.add (not (i32.const 0)) (and (i32.const 1) (i32.const 1)))
      (not (and (i32.const 1) (i32.const 0))))))
