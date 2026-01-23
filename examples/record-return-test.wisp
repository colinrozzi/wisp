; Test returning a record from an exported function

(record point
  (x s32)
  (y s32))

(fn make-point () point
  (point 10 20))

(export make-point)
