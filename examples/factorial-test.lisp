; Simple factorial for self-hosted compiler test
(export (fn factorial ((n s32)) s32
  (if (i32.le_s n (i32.const 1))
    (i32.const 1)
    (i32.mul n (factorial (i32.sub n (i32.const 1)))))))
