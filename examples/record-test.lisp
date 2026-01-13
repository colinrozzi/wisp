; Test file for record types

; Define a simple point record
(record point
  (x s32)
  (y s32))

; Function that creates a point
(fn make-point ((x s32) (y s32)) point
  (point x y))

; Function that gets the x coordinate
(fn get-x ((p point)) s32
  (point.x p))

; Function that gets the y coordinate
(fn get-y ((p point)) s32
  (point.y p))

; Function that adds two points' x coordinates
(fn add-x ((a point) (b point)) s32
  (i32.add (point.x a) (point.x b)))

; Test function - creates a point and returns its x + y
(export (fn test () s32
  (let (p (point (i32.const 10) (i32.const 20)))
    (i32.add (point.x p) (point.y p)))))

; Another test - using make-point
(export (fn test2 () s32
  (let (p (make-point (i32.const 5) (i32.const 7)))
    (get-y p))))
