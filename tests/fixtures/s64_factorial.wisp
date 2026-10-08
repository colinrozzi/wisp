(export factorial64)

(fn factorial64 ((n s64)) s64
  (if (i64.eq n (i64.const 0))
      (i64.const 1)
      (i64.mul n (factorial64 (i64.sub n (i64.const 1))))))
