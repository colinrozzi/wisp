(export add-and-scale)
(export dot-f32)
(export widen-then-trunc)

(fn add-and-scale ((x f64) (y f64)) f64
  (f64.mul (f64.add x y) (f64.const 1.5)))

(fn dot-f32 ((ax f32) (ay f32) (bx f32) (by f32)) f32
  (f32.add (f32.mul ax bx) (f32.mul ay by)))

(fn widen-then-trunc ((x s32)) s32
  (i32.wrap_i64 (i64.mul (i64.const 2) (i64.extend_i32_s x))))
