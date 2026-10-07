; Signature-driven dispatch: the trait's type parameter is the 2nd arg.
; A competing (Scale s32) instance must NOT fool the dispatch.
(trait (Scale T) (fn scale ((factor : s32) (v : T)) : T))
(instance (Scale s32) (fn scale ((factor : s32) (v : s32)) : s32 (i32.mul factor v)))
(instance (Scale f64) (fn scale ((factor : s32) (v : f64)) : f64 (f64.mul (f64.convert_i32_s factor) v)))
(export (fn s () : f64 (scale (i32.const 3) (f64.const 2.0))))
