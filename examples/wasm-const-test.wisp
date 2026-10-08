(export test-const)
(export test-convert)

(fn test-const ((x s32)) s32
  (i32.add x (i32.const 42)))

(fn test-convert ((x s32)) s64
  (i64.extend_i32_s x))
