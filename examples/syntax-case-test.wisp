; Test file for syntax-case macros (Phase 5)

; Simple syntax-case macro - equivalent to my-add from syntax-rules
(define-syntax my-add
  (syntax-case-lambda (stx)
    ((my-add a b)
     #'(i32.add a b))))

; Test: Simple macro expansion
(export (fn test-simple ((x s32) (y s32)) s32
  (my-add x y)))

; Macro with guard - only match if the argument is an identifier
(define-syntax double-if-id
  (syntax-case-lambda (stx)
    ((double-if-id x)
     (identifier? #'x)
     #'(i32.add x x))
    ((double-if-id x)
     #'x)))

; Test: Macro with guard (identifier)
(export (fn test-guard-id ((n s32)) s32
  (double-if-id n)))

; Test: Macro with guard (non-identifier falls through)
(export (fn test-guard-nonid () s32
  (double-if-id (i32.const 21))))

; Macro using quasisyntax with unsyntax
(define-syntax add-one
  (syntax-case-lambda (stx)
    ((add-one x)
     #`(i32.add #,x (i32.const 1)))))

; Test: Quasisyntax
(export (fn test-quasisyntax ((n s32)) s32
  (add-one n)))

; Macro with compile-time computation using let
(define-syntax triple
  (syntax-case-lambda (stx)
    ((triple x)
     (let (doubled #'(i32.add x x))
       #`(i32.add #,doubled x)))))

; Test: Compile-time let
(export (fn test-ct-let ((n s32)) s32
  (triple n)))

; Simple when macro using syntax-case
(define-syntax my-when
  (syntax-case-lambda (stx)
    ((my-when test body)
     #'(if test body (i32.const 0)))))

; Test: Simple when
(export (fn test-when ((x s32)) s32
  (my-when (i32.gt_s x (i32.const 0))
    (i32.mul x x))))
