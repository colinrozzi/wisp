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

; --- higher-order: fold, map, filter ----------------------------------------
; Function arguments are compile-time: (fold f init xs) specializes fold for that
; specific f, inlining it. No runtime function values, no closures.

; fold: reduce with a binary function and a seed.
(fn fold-go ((f : (-> T T T)) (xs : (list T)) (i : s32) (acc : T)) : T
  (where T)
  (if (i32.lt_s i (list-len xs))
      (fold-go f xs (i32.add i (i32.const 1)) (f acc (list-get xs i)))
      acc))

(fn fold ((f : (-> T T T)) (init : T) (xs : (list T))) : T
  (where T)
  (fold-go f xs (i32.const 0) init))

; map: apply a function to each element (same element type).
(fn map-go ((f : (-> T T)) (xs : (list T)) (i : s32) (acc : (list T))) : (list T)
  (where T)
  (if (i32.lt_s i (list-len xs))
      (map-go f xs (i32.add i (i32.const 1)) (list-push acc (f (list-get xs i))))
      acc))

(fn map ((f : (-> T T)) (xs : (list T))) : (list T)
  (where T)
  (map-go f xs (i32.const 0) (list-new T)))

; filter: keep the elements a predicate accepts (predicate returns s32: 1 = keep).
(fn filter-go ((p : (-> T s32)) (xs : (list T)) (i : s32) (acc : (list T))) : (list T)
  (where T)
  (if (i32.lt_s i (list-len xs))
      (filter-go p xs (i32.add i (i32.const 1))
        (if (p (list-get xs i)) (list-push acc (list-get xs i)) acc))
      acc))

(fn filter ((p : (-> T s32)) (xs : (list T))) : (list T)
  (where T)
  (filter-go p xs (i32.const 0) (list-new T)))

; --- sum: Add + Zero, expressed with fold ------------------------------------

(fn sum ((xs : (list T))) : T
  (where (Add T) (Zero T))
  (fold + (zero) xs))

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
