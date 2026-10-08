; Explicit conversions keep this subset readable by both compiled frontends.
(export (fn double-rounding () s32
  (f64.eq (f64.add 0.1 0.2) 0.30000000000000004)))
(export (fn single-rounding () s32
  (f32.eq (f32.add (f32.demote_f64 16777216.0) (f32.demote_f64 1.0)) (f32.demote_f64 16777216.0))))
(export (fn infinity () s32
  (f64.gt (f64.div 1.0 0.0) 1000000.0)))
(export (fn not-a-number () s32
  (let (n (f64.div 0.0 0.0)) (f64.ne n n))))
(export (fn truncate () s32 (i32.trunc_f64_s -2.75)))
