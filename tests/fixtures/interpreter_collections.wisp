(fn sum ((xs (list s32))) s32 (i32.add (list-get xs 0) (list-get xs 1)))
(fn option-value ((v (option s32))) s32 (match v ((some n) n) ((none) 7)))
(fn result-value ((v (result s32 string))) s32 (match v ((ok n) n) ((err message) (string-len message))))
(fn accept-tuple ((v (tuple s32 string))) s32 42)
(record bucket (items (list s32)))
(variant batch (values (list s32)) (empty))
(export (fn list-sum () s32 (sum (list-push (list-push (list-new s32) 20) 22))))
(export (fn list-alias () s32
  (let (xs (list-new s32)) (let (alias xs) (begin (list-push xs 42) (list-get alias 0))))))
(export (fn nested-push () s32
  (let (xs (list-new s32))
    (begin (list-push xs (begin (list-push xs 1) 2))
      (if (i32.eq (list-len xs) 2) (i32.add (i32.mul (list-get xs 0) 10) (list-get xs 1)) -1)))))
(export (fn option-present () s32 (option-value (some s32 42))))
(export (fn option-absent () s32 (option-value (none s32))))
(export (fn result-ok () s32 (result-value (ok s32 string 42))))
(export (fn result-err () s32 (result-value (err s32 string "oops"))))
(export (fn tuple-argument () s32 (accept-tuple (tuple 7 "x"))))
(export (fn nested-list () s32
  (list-get (list-get (list-push (list-new (list s32)) (list-push (list-new s32) 42)) 0) 0)))
(export (fn record-list () s32 (list-get (bucket.items (bucket (list-push (list-new s32) 42))) 0)))
(export (fn variant-list () s32
  (match (values (list-push (list-new s32) 42)) ((values xs) (list-get xs 0)) ((empty) 0))))
(export (fn nested-option () s32
  (match (some (option s32) (none s32))
    ((some inner) (match inner ((some n) n) ((none) 42))) ((none) 0))))
