; Test returning complex types

; A simple point record
(record point
  (x s32)
  (y s32))

; Return a record
(fn make-point ((x s32) (y s32)) point
  (point x y))

; Return an option - some case
(fn make-some ((x s32)) (option s32)
  (some s32 x))

; Return an option - none case
(fn make-none () (option s32)
  (none s32))

; Return a result - ok case
(fn make-ok ((x s32)) (result s32 s32)
  (ok s32 s32 x))

; Return a result - err case
(fn make-err ((x s32)) (result s32 s32)
  (err s32 s32 x))

; Return a string
(fn get-greeting () string
  "hello")

; Variant definition
(variant color
  (red)
  (green)
  (blue))

; Return a variant - no payload
(fn get-red () color
  (red))

(fn get-green () color
  (green))

(export make-point)
(export make-some)
(export make-none)
(export make-ok)
(export make-err)
(export get-greeting)
(export get-red)
(export get-green)
