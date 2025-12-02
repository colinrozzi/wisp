(export factorial64)

(fn factorial64 ((n s64)) s64
  (if (= n 0s64)
      1s64
      (* n (factorial64 (- n 1s64)))))
