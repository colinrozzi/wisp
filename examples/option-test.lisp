; Test file for option, result, and list types

; Test option<s32> construction
(fn make-some-value () (option s32)
  (some s32 (i32.const 42)))

(fn make-none-value () (option s32)
  (none s32))

; Test result<s32, s32> construction
(fn make-ok-value () (result s32 s32)
  (ok s32 s32 (i32.const 100)))

(fn make-err-value () (result s32 s32)
  (err s32 s32 (i32.const 999)))

; Test list<s32> construction
(fn make-list () (list s32)
  (let (lst (list-new s32))
    (let (lst2 (list-push lst (i32.const 10)))
      (let (lst3 (list-push lst2 (i32.const 20)))
        lst3))))

; Test option pattern matching - unwrap_or
(fn unwrap-or ((opt (option s32)) (default s32)) s32
  (match opt
    ((some x) x)
    ((none) default)))

; Test result pattern matching - unwrap_or_else
(fn result-to-s32 ((res (result s32 s32))) s32
  (match res
    ((ok x) x)
    ((err e) (i32.mul e (i32.const -1)))))

; Export test functions
(export (fn test-some () s32
  (let (opt (some s32 (i32.const 100)))
    (i32.const 1))))

(export (fn test-none () s32
  (let (opt (none s32))
    (i32.const 0))))

(export (fn test-ok () s32
  (let (res (ok s32 s32 (i32.const 42)))
    (i32.const 1))))

(export (fn test-err () s32
  (let (res (err s32 s32 (i32.const 500)))
    (i32.const 2))))

(export (fn test-list-len () s32
  (let (lst (list-new s32))
    (let (lst2 (list-push lst (i32.const 10)))
      (let (lst3 (list-push lst2 (i32.const 20)))
        (list-len lst3))))))

(export (fn test-list-get () s32
  (let (lst (list-new s32))
    (let (lst2 (list-push lst (i32.const 10)))
      (let (lst3 (list-push lst2 (i32.const 20)))
        (list-get lst3 (i32.const 1)))))))

; Test pattern matching on option
(export (fn test-unwrap-some () s32
  (let (opt (some s32 (i32.const 42)))
    (unwrap-or opt (i32.const 0)))))

(export (fn test-unwrap-none () s32
  (let (opt (none s32))
    (unwrap-or opt (i32.const 99)))))

; Test pattern matching on result
(export (fn test-result-ok () s32
  (let (res (ok s32 s32 (i32.const 100)))
    (result-to-s32 res))))

(export (fn test-result-err () s32
  (let (res (err s32 s32 (i32.const 50)))
    (result-to-s32 res))))
