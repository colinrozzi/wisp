; A default integer literal adopts the expected type flowing down from context.
; (There is no `s32` suffix, so a literal typed s32 is always a default and safe
;  to retype. An explicit `s64`/`f32`/`f64` suffix is respected.)

; 1. Return position: the body literal takes the return type.
(export (fn ret-s64 () : s64 5))
(export (fn ret-f64 () : f64 3))

; 2. Wasm operand: both literals take the instruction's operand type.
(export (fn add64 () : s64 (i64.add 1 2)))
(export (fn addf32 () : f32 (f32.add 1 2)))

; 3. Ascription: the literal takes the annotated type.
(export (fn asc () : s64 (5 : s64)))

; 4. Typed let: the value literal takes the binding type.
(export (fn letw () : s64 (let (big : s64 100) (i64.add big 1))))

; 5. Trait interaction: dispatch on x fixes the type, then the sibling
;    literal adopts it. `(+ x 1)` with x : f64 makes `1` an f64.
(trait (Add T)
  (fn + ((a : T) (b : T)) : T))
(instance (Add s32)
  (fn + ((a : s32) (b : s32)) : s32 (i32.add a b)))
(instance (Add f64)
  (fn + ((a : f64) (b : f64)) : f64 (f64.add a b)))

(export (fn bump ((x : f64)) : f64 (+ x 1)))
