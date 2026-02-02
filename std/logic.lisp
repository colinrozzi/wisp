; std/logic.lisp — Boolean and bitwise utilities

(export
  (fn not ((x s32)) s32
    (if (i32.eq x (i32.const 0)) (i32.const 1) (i32.const 0))))

(export
  (fn and ((a s32) (b s32)) s32
    (if (i32.eq a (i32.const 0)) (i32.const 0)
      (if (i32.eq b (i32.const 0)) (i32.const 0) (i32.const 1)))))

(export
  (fn or ((a s32) (b s32)) s32
    (if (i32.eq a (i32.const 0))
      (if (i32.eq b (i32.const 0)) (i32.const 0) (i32.const 1))
      (i32.const 1))))

(export
  (fn xor ((a s32) (b s32)) s32
    (if (i32.eq a (i32.const 0))
      (if (i32.eq b (i32.const 0)) (i32.const 0) (i32.const 1))
      (if (i32.eq b (i32.const 0)) (i32.const 1) (i32.const 0)))))

(export
  (fn bool ((x s32)) s32
    (if (i32.eq x (i32.const 0)) (i32.const 0) (i32.const 1))))
