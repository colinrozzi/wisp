; std/list.lisp — generic algorithms over (list T).
;
; These are ordinary generic functions, monomorphized per element type when used.
; An unused function costs nothing. Bring them in with:
;
;   (include "std/list.lisp")   ; path is relative to your source file
;
; The list operations recurse by index (list-len + list-get); Wisp lists have no
; nil/cons pattern matching. Numeric constraints (Add, Zero, Eq) come from num.lisp.

(include "num.lisp")

; --- length: fully parametric, no constraint ---------------------------------

(fn length ((xs : (list T))) : s32
  (where T)
  (list-len xs))

; --- sum: Add + Zero ---------------------------------------------------------

(fn sum-go ((xs : (list T)) (i : s32) (acc : T)) : T
  (where (Add T) (Zero T))
  (if (i32.lt_s i (list-len xs))
      (sum-go xs (i32.add i (i32.const 1)) (+ acc (list-get xs i)))
      acc))

(fn sum ((xs : (list T))) : T
  (where (Add T) (Zero T))
  (sum-go xs (i32.const 0) (zero)))

; --- contains: membership by Eq ----------------------------------------------

(fn contains-go ((xs : (list T)) (i : s32) (target : T)) : s32
  (where (Eq T))
  (if (i32.lt_s i (list-len xs))
      (if (= (list-get xs i) target)
          (i32.const 1)
          (contains-go xs (i32.add i (i32.const 1)) target))
      (i32.const 0)))

(fn contains ((xs : (list T)) (target : T)) : s32
  (where (Eq T))
  (contains-go xs (i32.const 0) target))

; --- reverse: parametric; builds a new list ----------------------------------

(fn reverse-go ((xs : (list T)) (i : s32) (acc : (list T))) : (list T)
  (where T)
  (if (i32.lt_s i (i32.const 0))
      acc
      (reverse-go xs (i32.sub i (i32.const 1)) (list-push acc (list-get xs i)))))

(fn reverse ((xs : (list T))) : (list T)
  (where T)
  (reverse-go xs (i32.sub (list-len xs) (i32.const 1)) (list-new T)))
