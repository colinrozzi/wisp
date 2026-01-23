; Test multi-parameter functions

; Two s32 parameters
(fn add ((a s32) (b s32)) s32
  (i32.add a b))

; Three s32 parameters
(fn sum3 ((a s32) (b s32) (c s32)) s32
  (i32.add (i32.add a b) c))

; Mixed types: s32 and s64
(fn mixed-add ((x s32) (y s64)) s64
  (i64.add (i64.extend_i32_s x) y))

(export add)
(export sum3)
(export mixed-add)
