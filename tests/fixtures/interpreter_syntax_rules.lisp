(define-syntax with-temp
  (syntax-rules () ((_ body) (let (tmp 0) body))))
(define-syntax make-temp
  (syntax-rules () ((_) (let (tmp 100) tmp))))
(define-syntax outer
  (syntax-rules () ((_ body) (let (tmp 1) (inner body)))))
(define-syntax inner
  (syntax-rules () ((_ body) (let (tmp 2) (i32.add tmp body)))))
(define-syntax outer-reference
  (syntax-rules () ((_) (let (tmp 40) (inner tmp)))))
(export (fn no-capture () s32 (let (tmp 42) (with-temp tmp))))
(export (fn self-reference () s32 (make-temp)))
(export (fn nested () s32 (let (tmp 100) (outer tmp))))
(export (fn passed-reference () s32 (outer-reference)))
(define-syntax last-value
  (syntax-rules (done)
    ((_ e ... (done result)) (begin e ... result))))
(export (fn empty () s32 (last-value (done 42))))
(export (fn many () s32 (last-value 1 2 3 (done 42))))
(define-syntax wide
  (syntax-rules () ((_ x) (let (n : s64 x) (i64.eq n 4294967296s64)))))
(export (fn typed () s32 (wide 4294967296s64)))
