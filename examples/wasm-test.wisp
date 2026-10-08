(export test-add)
(export test-div)
(export test-compare)

(fn test-add ((a s32) (b s32)) s32
  (i32.add a b))

(fn test-div ((a s32) (b s32)) s32
  (i32.div_s a b))

(fn test-compare ((a s32) (b s32)) s32
  (i32.eq a b))
