; Test parameter decoding for exported functions

; s32 tests
(fn add-ten ((x s32)) s32
  (i32.add x 10))

(fn double ((n s32)) s32
  (i32.mul n 2))

; s64 test
(fn add-hundred64 ((x s64)) s64
  (i64.add x 100s64))

; f32 test
(fn half-f32 ((x f32)) f32
  (f32.div x 2.0f32))

; f64 test
(fn square-f64 ((x f64)) f64
  (f64.mul x x))

; string test - return the length
(fn get-string-len ((s string)) s32
  (string-len s))

(export add-ten)
(export double)
(export add-hundred64)
(export half-f32)
(export square-f64)
(export get-string-len)
