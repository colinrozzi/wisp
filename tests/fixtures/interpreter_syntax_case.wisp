(define-syntax fold-add
  (syntax-case-lambda (stx)
    ((_ x y)
     (and (integer? #'x) (integer? #'y))
     (let (sum (+ (syntax->datum #'x) (syntax->datum #'y)))
       #`(i32.const #,sum)))
    ((_ x y) #'(i32.add x y))))
(define-syntax subtract
  (syntax-case-lambda (stx)
    ((_ x y)
     (let (n (- (syntax->datum #'x) (syntax->datum #'y))) #`(i32.const #,n)))))
(define-syntax splice-add
  (syntax-case-lambda (stx) ((_ x ...) #`(i32.add #,@x))))
(define-syntax choose
  (syntax-case-lambda (stx)
    (syntax-case stx (else)
      ((_ else x) #'x)
      ((_ condition x) #'(if condition x 0)))))
(define-syntax with-temp
  (syntax-case-lambda (stx) ((_ body) #'(let (tmp 0) body))))
(define-syntax predicates
  (syntax-case-lambda (stx)
    ((_ x) (if (or (identifier? #'x) (not (number? #'x))) #'1 #'2))))
(export (fn folded () s32 (fold-add 20 22)))
(export (fn dynamic ((n s32)) s32 (fold-add n 2)))
(export (fn difference () s32 (subtract 50 8)))
(export (fn spliced () s32 (splice-add 20 22)))
(export (fn literal () s32 (choose else 42)))
(export (fn conditional ((n s32)) s32 (choose n 42)))
(export (fn hygienic () s32 (let (tmp 42) (with-temp tmp))))
(export (fn identifier-predicate ((n s32)) s32 (predicates n)))
(export (fn numeric-predicate () s32 (predicates 1.5)))
