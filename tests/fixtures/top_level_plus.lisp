(trait (Add T)
  (fn + ((a : T) (b : T)) : T))
(instance (Add s32)
  (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))
(instance (Add f64)
  (fn + ((a : f64) (b : f64)) : f64 (f64.add a b)))

; `+` used directly in plain functions — no generic wrapper
(export (fn add-i ((a : s32) (b : s32)) : s32 (+ a b)))
(export (fn add-f ((a : f64) (b : f64)) : f64 (+ a b)))
(export (fn lit () : s32 (+ (i32.const 40) (i32.const 2))))
