; Arithmetic and byte-oriented strings shared by both compilers and interpreter.
(export (fn wrap () s32 (i32.add 2147483647 1)))
(export (fn unsigned-div () s32 (i32.div_u -1 2)))
(export (fn signed-rem () s32 (i32.rem_s (i32.add 2147483647 1) -1)))
(export (fn shift () s32 (i32.shl 1 33)))
(export (fn rotate () s32 (i32.rotl (i32.add 2147483647 1) 1)))
(export (fn compare-unsigned () s32 (i32.gt_u -1 0)))
(export (fn utf8-length () s32 (string-len "λ")))
(export (fn byte-at () s32 (string-ref "abc" 1)))
(export (fn append-strings () s32
  (string=? (string-append "hello" " world") "hello world")))
(export (fn slice-string () s32
  (string=? (substring "hello" 1 4) "ell")))
