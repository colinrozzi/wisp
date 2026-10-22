; Retention measurement probe (ticket 0012): the Wisp evaluator plus two test-only
; exports. heap-mark is the bump pointer (a zero-byte allocation returns it);
; fire-message delivers TEXT to the session's on-message exactly the way the
; actor's handle-send does, i.e. as (bytes) through dispatch-event.
(include "../../../wisp/interpreter/evaluator.wisp")
(export (fn heap-mark () s32 (heap-alloc 0)))
(export (fn fire-message ((s string)) string
  (begin
    (dispatch-event "on-message"
      (sequence (list-push (list-new value) (apply-string-to-bytes (list-push (list-new value) (text s))))))
    "ok")))
