; Test compound parameter types

; Record parameter
(record point
  (x s32)
  (y s32))

(fn point-sum ((p point)) s32
  (i32.add (point.x p) (point.y p)))

; Option parameter
(fn unwrap-or-zero ((opt (option s32))) s32
  (match opt
    ((some v) v)
    ((none) 0)))

; String in multi-param context
(fn greet-with-count ((name string) (count s32)) s32
  (i32.add (string-len name) count))

(export point-sum)
(export unwrap-or-zero)
(export greet-with-count)
