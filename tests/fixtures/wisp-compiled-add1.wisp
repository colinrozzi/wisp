; B0 spike for the Wisp compiler: the SHAPE the emitter will produce for a
; first-order Wisp function. `(define add1 (lambda (x) (+ x 1)))` compiled by
; specializing the interpreter — a Granite fn over the real `value` type, reusing
; the interpreter's value representation. Proves: value ABI at the export boundary,
; the runtime is includable standalone, and the whole thing → a Pack package that runs.
(include "../../wisp/interpreter/values.wisp")

; compiled body of (lambda (x) (+ x 1)): the + is specialized to the integer case.
(export (fn compiled-add1 ((x value)) value
  (value-case x
    ((integer n) (integer (i32.add n 1)))
    (else (failure "add1 expects an integer")))))

; nullary entry that builds the argument value and applies the compiled function,
; so `run` can drive it without passing a tagged value on the command line.
(export (fn demo () value (compiled-add1 (integer 41))))

; same, but unwrap to s32 at the boundary — isolates the compiled logic from the
; question of encoding a recursive `value` variant across the Pack/CGRF boundary.
(export (fn demo-int () s32
  (value-case (compiled-add1 (integer 41))
    ((integer n) n)
    (else -1))))
