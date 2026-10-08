(import values echo ((value string)) string)
(export (fn imported-echo ((value string)) string (echo value)))
(import values list-id ((value (list s32))) (list s32))
(import values make-list () (list s32))
(export (fn imported-list () s32
  (list-get (list-id (make-list)) (i32.const 0))))
(import values tuple-id ((value (tuple string (list u8)))) (tuple string (list u8)))
(export (fn imported-tuple () (tuple string (list u8))
  (tuple-id (tuple "λ" (list-new u8)))))
