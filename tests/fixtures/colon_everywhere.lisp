; The colon notation `(name : type)` in every type position.

; global: (global $name : type mut|const init)
(global $counter : s32 mut 0)

; import: colon before the return type
; (uses host debug interface shape; kept minimal)

; trait + instance with colon params and colon return type
(trait (Add T)
  (fn + ((a : T) (b : T)) : T))

(instance (Add s32)
  (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))

(instance (Add f64)
  (fn + ((a : f64) (b : f64)) : f64 (f64.add a b)))

; generic fn: colon params, colon return, where clause
(fn double ((x : T)) : T
  (where (Add T))
  (+ x x))

; ascription in colon form: (expr : type)
(export (fn widen ((n : s32)) : s64 (n : s64)))

(export (fn double-int ((n : s32)) : s32 (double n)))
(export (fn double-flt ((x : f64)) : f64 (double x)))
