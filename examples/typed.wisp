(export add64)
(export mul-f64)

(fn add64 ((x s64) (y s64)) s64
  (i64.add x y))

(fn mul-f64 ((x f64)) f64
  (f64.mul x (f64.const 2.5)))
