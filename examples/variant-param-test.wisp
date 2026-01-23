; Test variant parameter decoding

; Define a simple variant with scalar payloads
(variant color
  (red)
  (green)
  (blue))

; Variant with payload
(variant shape
  (circle s32)
  (square s32))

; Check which color it is (returns 1 for red, 2 for green, 3 for blue)
(fn color-to-num ((c color)) s32
  (match c
    ((red) 1)
    ((green) 2)
    ((blue) 3)))

; Get the dimension from a shape (radius for circle, side for square)
(fn get-dimension ((s shape)) s32
  (match s
    ((circle r) r)
    ((square side) side)))

; Double the dimension
(fn double-dimension ((s shape)) s32
  (match s
    ((circle r) (i32.mul r 2))
    ((square side) (i32.mul side 2))))

(export color-to-num)
(export get-dimension)
(export double-dimension)
