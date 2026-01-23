; Test returning variant types from exported functions

(variant shape
  (circle s32)
  (square s32))

(fn make-circle () shape
  (circle 10))

(fn make-square () shape
  (square 20))

(export make-circle)
(export make-square)
