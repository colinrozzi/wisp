; Simple composite test
(fn add ((a s32) (b s32)) s32
  (i32.add a b))

(fn hello () s32
  42)

(export hello)
