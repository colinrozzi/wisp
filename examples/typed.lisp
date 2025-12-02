(export add64)
(export mul-f64)

(fn add64 ((x s64) (y s64)) s64
  (+ x y))

(fn mul-f64 ((x f64)) f64
  (* x 2.5f64))
