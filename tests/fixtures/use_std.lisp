; Use the minimal numeric standard library through `(include ...)`.
; The path is relative to this file: tests/fixtures -> repo root -> std.
(include "../../std/num.lisp")

; Operators on s32, resolved to i32.* by the stdlib instances.
(export (fn add3 ((a : s32) (b : s32) (c : s32)) : s32
  (+ (+ a b) c)))

; A polynomial on f64: +, -, * all resolve to f64.*, and (one) return-dispatches.
(export (fn poly ((x : f64)) : f64
  (- (* x x) (one))))

; A generic constrained by the stdlib's Add trait, reused at two types.
(fn double ((x : T)) : T
  (where (Add T))
  (+ x x))
(export (fn d32 ((n : s32)) : s32 (double n)))
(export (fn d64 ((x : f64)) : f64 (double x)))

; Comparison plus typed constants: (one)/(zero) take the s32 return type.
(export (fn clamp-sign ((n : s32)) : s32
  (if (< n (zero)) (- (zero) (one)) (one))))
