; Test result parameter decoding

; Function that takes a result<s32, s32> and returns ok value or error negated
(fn handle-result ((r (result s32 s32))) s32
  (match r
    ((ok val) val)
    ((err e) (i32.sub 0 e))))

; Function that doubles the ok value, or returns 0 for err
(fn double-ok ((r (result s32 s32))) s32
  (match r
    ((ok val) (i32.mul val 2))
    ((err e) 0)))

; Function that checks if result is ok (returns 1) or err (returns 0)
(fn is-ok ((r (result s32 s32))) s32
  (match r
    ((ok val) 1)
    ((err e) 0)))

(export handle-result)
(export double-ok)
(export is-ok)
