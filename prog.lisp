(export
  (fn double (x)
    (* x 2)))

(fn bigger (a b)
  (if (> a b)
      a
      b))

(fn smaller (a b)
  (if (< a b)
      a
      b))

(export smaller)
(export factorial)

(fn factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(fn main (x)
  (factorial x))
