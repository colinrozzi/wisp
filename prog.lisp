(fn main (x y)
  (let (double (* x 2))
    (if (> double y)
        double
        y)))
