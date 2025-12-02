(export add-and-scale)
(export dot-f32)

(fn add-and-scale ((x f64) (y f64)) f64
  (* (+ x y) 1.5f64))

(fn dot-f32 ((ax f32) (ay f32) (bx f32) (by f32)) f32
  (+ (* ax bx) (* ay by)))
