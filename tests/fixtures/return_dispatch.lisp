; Return-type dispatch: the type comes from context, not from arguments.

; Zero has its type parameter ONLY in the return, so no argument can pick the
; instance. The expected type must flow down from the call site.
(trait (Zero T)
  (fn zero () : T))

(instance (Zero s32)
  (fn zero () : s32 (i32.const 0)))

(instance (Zero f64)
  (fn zero () : f64 (f64.const 0.0)))

; One, same shape, for the sibling-argument test.
(trait (One T)
  (fn one () : T))

(instance (One s32)
  (fn one () : s32 (i32.const 1)))

(instance (One f64)
  (fn one () : f64 (f64.const 1.0)))

; Add, to combine return-typed constants.
(trait (Add T)
  (fn + ((a : T) (b : T)) : T))

(instance (Add s32)
  (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))

(instance (Add f64)
  (fn + ((a : f64) (b : f64)) : f64 (f64.add a b)))

; 1. Return position: the fn return type resolves (zero).
(export (fn z32 () : s32 (zero)))
(export (fn z64 () : f64 (zero)))

; 2. Ascription: (zero) resolved by an inline annotation.
(export (fn z-asc () : s32 ((zero) : s32)))

; 3. Sibling argument: (one) takes the type of x.
(export (fn inc ((x : s32)) : s32 (+ x (one))))

; 4. Both constants return-dispatched, driven by the return type.
(export (fn two () : f64 (+ (one) (one))))

; 5. Through an if: both branches inherit the expected type.
(export (fn pick ((c : s32)) : s32 (if c (zero) (one))))
