; Test pattern matching on S-expression-like variants
; This is a proof-of-concept for the self-hosted compiler

; Define a full S-expression variant type with recursive lists
(variant sexpr
  (sym string)           ; symbol with name
  (num s32)              ; number
  (lst (list sexpr)))    ; list of s-expressions

; Helper to check if sexpr is a symbol
(fn is-symbol ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 1))
    ((num n) (i32.const 0))
    ((lst l) (i32.const 0))))

; Helper to check if sexpr is a number
(fn is-number ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 1))
    ((lst l) (i32.const 0))))

; Helper to check if sexpr is a list
(fn is-list ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((lst l) (i32.const 1))))

; Get number value (returns 0 if not a number)
(fn get-num ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) n)
    ((lst l) (i32.const 0))))

; Get symbol length (returns 0 if not a symbol)
(fn get-sym-len ((e sexpr)) s32
  (match e
    ((sym s) (string-len s))
    ((num n) (i32.const 0))
    ((lst l) (i32.const 0))))

; Get list length (returns 0 if not a list)
(fn get-list-len ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((lst l) (list-len l))))

; Check if symbol equals a specific string
(fn sym-eq ((e sexpr) (target string)) s32
  (match e
    ((sym s) (string=? s target))
    ((num n) (i32.const 0))
    ((lst l) (i32.const 0))))

; Test 1: Create a symbol and check it
(export (fn test-symbol () s32
  (let (e (sym "hello"))
    (is-symbol e))))  ; Expected: 1

; Test 2: Create a number and check it
(export (fn test-number () s32
  (let (e (num (i32.const 42)))
    (is-number e))))  ; Expected: 1

; Test 3: Get number value
(export (fn test-get-num () s32
  (let (e (num (i32.const 42)))
    (get-num e))))  ; Expected: 42

; Test 4: Get symbol length
(export (fn test-sym-len () s32
  (let (e (sym "hello"))
    (get-sym-len e))))  ; Expected: 5

; Test 5: Symbol is not a number
(export (fn test-sym-not-num () s32
  (let (e (sym "x"))
    (is-number e))))  ; Expected: 0

; Test 6: Check symbol equality
(export (fn test-sym-eq () s32
  (let (e (sym "fn"))
    (sym-eq e "fn"))))  ; Expected: 1

; Test 7: Check symbol inequality
(export (fn test-sym-neq () s32
  (let (e (sym "let"))
    (sym-eq e "fn"))))  ; Expected: 0

; Test 8: Create an empty list
(export (fn test-empty-list () s32
  (let (e (lst (list-new sexpr)))
    (is-list e))))  ; Expected: 1

; Test 9: Get length of empty list
(export (fn test-empty-list-len () s32
  (let (e (lst (list-new sexpr)))
    (get-list-len e))))  ; Expected: 0

; Test 10: Create a list with items
(export (fn test-list-with-items () s32
  (let (items (list-new sexpr))
    (let (items2 (list-push items (num (i32.const 1))))
      (let (items3 (list-push items2 (sym "x")))
        (let (e (lst items3))
          (get-list-len e)))))))  ; Expected: 2
