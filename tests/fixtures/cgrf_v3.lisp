; Values exchanged with the CGRF v3 host encoder/decoder.
(export (fn answer () s32 (i32.const 42)))
(export (fn echo ((value string)) string value))
(export (fn identity64 ((value s64)) s64 value))
(export (fn add ((a s32) (b s32)) s32 (i32.add a b)))
(export (fn list-id ((value (list s32))) (list s32) value))
(export (fn bytes-id ((value (list u8))) (list u8) value))
(export (fn list64-id ((value (list s64))) (list s64) value))
(export (fn floats-id ((value (list f32))) (list f32) value))
(export (fn doubles-id ((value (list f64))) (list f64) value))
(export (fn strings-id ((value (list string))) (list string) value))
(export (fn nested-id ((value (list (list s32)))) (list (list s32)) value))
(export (fn make-list () (list s32) (list-push (list-new s32) (i32.const 42))))
(export (fn tuple-id ((value (tuple string (list u8)))) (tuple string (list u8)) value))
(export (fn some-value () (option s32) (some s32 (i32.const 42))))
(export (fn none-value () (option s32) (none s32)))
(export (fn ok-value () (result s32 s32) (ok s32 s32 (i32.const 42))))
(export (fn err-value () (result s32 s32) (err s32 s32 (i32.const -1))))
; Pack dynamic `value`: a top-level `any` passes straight through the guest.
(export (fn roundtrip ((value any)) any value))
; Inspect the s32 inside an `any`, add one, and construct a fresh `any`.
(export (fn any-inc ((value any)) any (any-s32 (i32.add (any-as-s32 value) (i32.const 1)))))
; Read the string out of an `any`, append to it, and construct a fresh `any`.
(export (fn any-shout ((value any)) any (any-string (string-append (any-as-string value) "!"))))
; Recursive byte copy (the codec's building block).
(fn blit ((dst s32) (src s32) (n s32)) s32
  (if (i32.eq n (i32.const 0)) (i32.const 0)
    (begin
      (i32.store8 dst (i32.load8_u src))
      (blit (i32.add dst (i32.const 1)) (i32.add src (i32.const 1)) (i32.sub n (i32.const 1))))))
; Copy an `any` blob entirely in Wisp via the byte-view primitives: read the
; blob address, allocate a fresh buffer, copy [len:u32][cgrf] bytes, re-wrap.
; Proves the marshal/unmarshal codec needs no per-type compiler support.
(export (fn any-echo ((value any)) any
  (let (p (any-addr value))
    (let (total (i32.add (i32.load p) (i32.const 4)))
      (let (q (heap-alloc total))
        (begin (blit q p total) (any-from-addr q)))))))
