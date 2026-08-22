; std/num.lisp — a minimal numeric standard library for Wisp.
;
; Traits over the scalar types (s32, s64, f32, f64), with one instance each.
; These ARE the arithmetic operators: `+` is just the `Add` method, resolved at
; the concrete type after monomorphization. Bring them in with:
;
;   (include "std/num.lisp")   ; path is relative to your source file
;
; Constants (`zero`, `one`) dispatch on the return type, so they need the
; expected type from context — a `: type` annotation or the surrounding call.

; --- arithmetic ---------------------------------------------------------------

(trait (Add T) (fn + ((a : T) (b : T)) : T))
(instance (Add s32) (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))
(instance (Add s64) (fn + ((a : s64) (b : s64)) : s64 (i64.add a b)))
(instance (Add f32) (fn + ((a : f32) (b : f32)) : f32 (f32.add a b)))
(instance (Add f64) (fn + ((a : f64) (b : f64)) : f64 (f64.add a b)))

(trait (Sub T) (fn - ((a : T) (b : T)) : T))
(instance (Sub s32) (fn - ((a : s32) (b : s32)) : s32 (i32.sub a b)))
(instance (Sub s64) (fn - ((a : s64) (b : s64)) : s64 (i64.sub a b)))
(instance (Sub f32) (fn - ((a : f32) (b : f32)) : f32 (f32.sub a b)))
(instance (Sub f64) (fn - ((a : f64) (b : f64)) : f64 (f64.sub a b)))

(trait (Mul T) (fn * ((a : T) (b : T)) : T))
(instance (Mul s32) (fn * ((a : s32) (b : s32)) : s32 (i32.mul a b)))
(instance (Mul s64) (fn * ((a : s64) (b : s64)) : s64 (i64.mul a b)))
(instance (Mul f32) (fn * ((a : f32) (b : f32)) : f32 (f32.mul a b)))
(instance (Mul f64) (fn * ((a : f64) (b : f64)) : f64 (f64.mul a b)))

(trait (Div T) (fn / ((a : T) (b : T)) : T))
(instance (Div s32) (fn / ((a : s32) (b : s32)) : s32 (i32.div_s a b)))
(instance (Div s64) (fn / ((a : s64) (b : s64)) : s64 (i64.div_s a b)))
(instance (Div f32) (fn / ((a : f32) (b : f32)) : f32 (f32.div a b)))
(instance (Div f64) (fn / ((a : f64) (b : f64)) : f64 (f64.div a b)))

; --- comparison (return s32: 1 = true, 0 = false) -----------------------------

(trait (Ord T)
  (fn < ((a : T) (b : T)) : s32)
  (fn > ((a : T) (b : T)) : s32))
(instance (Ord s32)
  (fn < ((a : s32) (b : s32)) : s32 (i32.lt_s a b))
  (fn > ((a : s32) (b : s32)) : s32 (i32.gt_s a b)))
(instance (Ord s64)
  (fn < ((a : s64) (b : s64)) : s32 (i64.lt_s a b))
  (fn > ((a : s64) (b : s64)) : s32 (i64.gt_s a b)))
(instance (Ord f32)
  (fn < ((a : f32) (b : f32)) : s32 (f32.lt a b))
  (fn > ((a : f32) (b : f32)) : s32 (f32.gt a b)))
(instance (Ord f64)
  (fn < ((a : f64) (b : f64)) : s32 (f64.lt a b))
  (fn > ((a : f64) (b : f64)) : s32 (f64.gt a b)))

(trait (Eq T) (fn = ((a : T) (b : T)) : s32))
(instance (Eq s32) (fn = ((a : s32) (b : s32)) : s32 (i32.eq a b)))
(instance (Eq s64) (fn = ((a : s64) (b : s64)) : s32 (i64.eq a b)))
(instance (Eq f32) (fn = ((a : f32) (b : f32)) : s32 (f32.eq a b)))
(instance (Eq f64) (fn = ((a : f64) (b : f64)) : s32 (f64.eq a b)))

; --- typed constants (dispatch on the return type) ----------------------------

(trait (Zero T) (fn zero () : T))
(instance (Zero s32) (fn zero () : s32 (i32.const 0)))
(instance (Zero s64) (fn zero () : s64 (i64.const 0)))
(instance (Zero f32) (fn zero () : f32 (f32.const 0.0)))
(instance (Zero f64) (fn zero () : f64 (f64.const 0.0)))

(trait (One T) (fn one () : T))
(instance (One s32) (fn one () : s32 (i32.const 1)))
(instance (One s64) (fn one () : s64 (i64.const 1)))
(instance (One f32) (fn one () : f32 (f32.const 1.0)))
(instance (One f64) (fn one () : f64 (f64.const 1.0)))
