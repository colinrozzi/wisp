; Test syntax-rules macros

; Test 1: Simple pattern matching
(define-syntax my-add
  (syntax-rules ()
    ((my-add a b)
     (i32.add a b))))

(export (fn test-simple ((x s32) (y s32)) s32
  (my-add x y)))

; Test 2: Multiple patterns (different arities)
(define-syntax my-inc
  (syntax-rules ()
    ((my-inc x)
     (i32.add x (i32.const 1)))
    ((my-inc x n)
     (i32.add x n))))

(export (fn test-inc ((x s32)) s32
  (my-inc x)))

(export (fn test-inc-n ((x s32) (n s32)) s32
  (my-inc x n)))

; Test 3: Ellipsis pattern (begin-like)
(define-syntax my-begin
  (syntax-rules ()
    ((my-begin e)
     e)
    ((my-begin e1 e2 ...)
     (let (_ e1) (my-begin e2 ...)))))

(export (fn test-begin ((x s32)) s32
  (my-begin
    (i32.const 1)
    (i32.const 2)
    (i32.add x (i32.const 10)))))

; Test 4: Literal keywords
(define-syntax my-cond
  (syntax-rules (else)
    ((my-cond (else result))
     result)
    ((my-cond (test result))
     (if test result (i32.const 0)))))

(export (fn test-else () s32
  (my-cond (else (i32.const 42)))))

(export (fn test-cond ((x s32)) s32
  (my-cond ((i32.gt_s x (i32.const 0)) (i32.const 100)))))
