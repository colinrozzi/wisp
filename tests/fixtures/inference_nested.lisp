; Inference reaches through a generic call: (double 5) is known to be s32,
; so the outer + resolves to i32.add.
(trait (Add T) (fn + ((a : T) (b : T)) : T))
(instance (Add s32) (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))
(fn double ((x : T)) : T (where (Add T)) (+ x x))
(export (fn nested () : s32 (+ (double (i32.const 5)) (i32.const 1))))
