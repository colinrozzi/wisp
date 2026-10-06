; Single-payload variant behavior shared by all three execution paths.
(variant maybe-number (absent) (present s32))

(fn unwrap ((v maybe-number)) s32
  (match v
    ((absent) (i32.const 0))
    ((present n) n)))

(export (fn test-present () s32 (unwrap (present (i32.const 42)))))
(export (fn test-absent () s32 (unwrap (absent))))
