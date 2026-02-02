; std/math.lisp — Arithmetic utilities

(export
  (fn abs ((x s32)) s32
    (if (i32.lt_s x (i32.const 0))
      (i32.sub (i32.const 0) x)
      x)))

(export
  (fn min ((a s32) (b s32)) s32
    (if (i32.lt_s a b) a b)))

(export
  (fn max ((a s32) (b s32)) s32
    (if (i32.gt_s a b) a b)))

(export
  (fn clamp ((x s32) (lo s32) (hi s32)) s32
    (if (i32.lt_s x lo) lo
      (if (i32.gt_s x hi) hi x))))

(export
  (fn pow ((base s32) (exp s32)) s32
    (if (i32.le_s exp (i32.const 0))
      (i32.const 1)
      (i32.mul base (pow base (i32.sub exp (i32.const 1)))))))
