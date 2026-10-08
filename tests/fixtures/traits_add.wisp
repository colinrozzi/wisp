; First slice: generics + traits, lowered by monomorphization.
; Uses the new `(name : type)` parameter syntax (Form A).

(trait (Add T)
  (fn + ((a : T) (b : T)) T))

(instance (Add s32)
  (fn + ((a : s32) (b : s32)) s32 (i32.add a b)))

(instance (Add f64)
  (fn + ((a : f64) (b : f64)) f64 (f64.add a b)))

; one generic function, constrained by Add
(fn double ((x : T)) T
  (where (Add T))
  (+ x x))

; two concrete entry points that force two specializations
(export (fn double-int ((n : s32)) s32 (double n)))
(export (fn double-flt ((x : f64)) f64 (double x)))
