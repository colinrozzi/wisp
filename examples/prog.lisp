(export
  (fn double ((x s32)) s32
    (i32.mul x (i32.const 2))))

(fn bigger ((a s32) (b s32)) s32
  (if (i32.gt_s a b)
      a
      b))

(fn smaller ((a s32) (b s32)) s32
  (if (i32.lt_s a b)
      a
      b))

(export smaller)
(export factorial)

(fn factorial ((n s32)) s32
  (if (i32.eq n (i32.const 0))
      (i32.const 1)
      (i32.mul n (factorial (i32.sub n (i32.const 1))))))

(fn main ((x s32)) s32
  (factorial x))

(export (fn run ((x s32)) s32
            (main x)))
