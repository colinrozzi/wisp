; Retention measurement probe (ticket 0012): the Wisp evaluator plus two test-only
; exports. heap-mark is the allocator's live bytes (+0 of the heap-stats record);
; fire-message delivers TEXT to the session's on-message exactly the way the
; actor's handle-send does, i.e. as (bytes) through dispatch-event.
(include "../../../wisp/interpreter/evaluator.wisp")
(export (fn heap-mark () s32 (i32.load (heap-stats))))
(export (fn fire-message ((s string)) string
  (begin
    (dispatch-event "on-message"
      (sequence (list-push (list-new value) (apply-string-to-bytes (list-push (list-new value) (text s))))))
    "ok")))
