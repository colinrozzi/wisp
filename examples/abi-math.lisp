; Component that exports functions with rich types for cross-component testing

; Define a point record
(record point
  (x s32)
  (y s32))

; Create and return a point
(export (fn make-point ((x s32) (y s32)) point
  (point x y)))

; Get sum of coordinates
(export (fn point-sum ((p point)) s32
  (i32.add (point.x p) (point.y p))))

; Test function - uses record internally and returns scalar
(export (fn test-internal () s32
  (let (p (point (i32.const 15) (i32.const 25)))
    (i32.add (point.x p) (point.y p)))))
