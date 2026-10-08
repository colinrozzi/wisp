; Test variants (sum types)

; Define a shape variant with three cases
(variant shape
  (circle s32)           ; circle with radius
  (rectangle s32 s32)    ; rectangle with width and height
  (point))               ; point with no payload

; Calculate area of a shape (using integer approximation for circle)
(fn area ((s shape)) s32
  (match s
    ((circle r) (i32.mul (i32.mul r r) (i32.const 3)))
    ((rectangle w h) (i32.mul w h))
    ((point) (i32.const 0))))

; Test 1: area of circle with radius 5 -> 3 * 5 * 5 = 75
(export (fn test-circle () s32
  (let (c (circle (i32.const 5)))
    (area c))))

; Test 2: area of rectangle 4x7 -> 28
(export (fn test-rect () s32
  (let (r (rectangle (i32.const 4) (i32.const 7)))
    (area r))))

; Test 3: area of point -> 0
(export (fn test-point () s32
  (let (p (point))
    (area p))))
