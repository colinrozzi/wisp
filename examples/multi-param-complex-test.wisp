; Test multi-param functions with complex types

; Record definition
(record point
  (x s32)
  (y s32))

; A function that takes a scalar and a record
(fn add-to-point ((n s32) (p point)) s32
  (i32.add n (i32.add (point.x p) (point.y p))))

; A function that takes a scalar and an option
(fn add-or-default ((n s32) (opt (option s32))) s32
  (match opt
    ((some v) (i32.add n v))
    ((none) n)))

; A function that takes two options
(fn both-or-zero ((a (option s32)) (b (option s32))) s32
  (match a
    ((some va)
      (match b
        ((some vb) (i32.add va vb))
        ((none) 0)))
    ((none) 0)))

(export add-to-point)
(export add-or-default)
(export both-or-zero)
