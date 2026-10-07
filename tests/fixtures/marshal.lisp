; Exercise the marshal/unmarshal codec. It now lives inside the interpreter
; (evaluator.lisp includes marshal.lisp), so record/variant register-on-arrival
; can reach the $types registry. A trivial `evaluate ""` first initialises the
; session globals ($types, etc.) before any record round-trips.
(include "../../interpreter/evaluator.lisp")
(export (fn m-int ((n s32)) any (marshal (integer n))))
(export (fn m-wide ((n s64)) any (marshal (wide-integer n))))
(export (fn m-double ((d f64)) any (marshal (double d))))
(export (fn m-text ((s string)) any (marshal (text s))))
; Compound: a heterogeneous sequence marshals to a Tuple.
(export (fn m-pair ((a s32) (b s32)) any
  (marshal (sequence (list-push (list-push (list-new value) (integer a)) (integer b))))))
(export (fn m-mixed ((a s32) (s string)) any
  (marshal (sequence (list-push (list-push (list-new value) (integer a)) (text s))))))
; Nested: a tuple containing a tuple, to exercise recursive index assignment.
(export (fn m-nested ((a s32) (b s32) (c s32)) any
  (marshal (sequence (list-push
    (list-push (list-new value) (integer a))
    (sequence (list-push (list-push (list-new value) (integer b)) (integer c))))))))
(export (fn m-bool ((b s32)) any (marshal (boolean b))))
(export (fn m-u64 ((n s64)) any (marshal (u64-value n))))
(export (fn u-remarshal ((x any)) any (marshal (unmarshal x))))
; A tuple whose second element is a list<u8> built from a string via str->bytes —
; the shape a store/message-server writer sends. Lets the host decode and verify
; the real args-blob encoding (the element template must be the "u8" symbol so it
; routes to the packed Array node, not a List of u8 nodes).
(export (fn m-strbytes ((s string)) any
  (marshal (sequence (list-push (list-push (list-new value) (text "id")) (str-to-bytes (text s)))))))
; An http-request record arg (empty headers, no body) — the record-valued host
; argument path. The host decodes it to a Record{method,url,headers,body}.
(export (fn m-http-req ((method string) (url string)) any (http-request-blob method url)))
