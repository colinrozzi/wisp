; Test component ABI for variants

(variant shape
  (circle s32)
  (rectangle s32 s32))

; Calculate area of a shape
(export (fn shape-area ((s shape)) s32
  (match s
    ((circle r) (i32.mul (i32.mul r r) (i32.const 3)))
    ((rectangle w h) (i32.mul w h)))))

; Test with circle internally
(export (fn test-circle () s32
  (let (s (circle (i32.const 5)))
    (match s
      ((circle r) (i32.mul (i32.mul r r) (i32.const 3)))
      ((rectangle w h) (i32.mul w h))))))

; Test with rectangle internally
(export (fn test-rectangle () s32
  (let (s (rectangle (i32.const 4) (i32.const 6)))
    (match s
      ((circle r) (i32.mul (i32.mul r r) (i32.const 3)))
      ((rectangle w h) (i32.mul w h))))))
