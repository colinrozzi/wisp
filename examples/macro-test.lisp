; Test macros - convenience operators as macros

; Define arithmetic macros
(defmacro + (a b) `(i32.add ,a ,b))
(defmacro - (a b) `(i32.sub ,a ,b))
(defmacro * (a b) `(i32.mul ,a ,b))
(defmacro = (a b) `(i32.eq ,a ,b))

; Define a control flow macro
(defmacro when (cond body)
  `(if ,cond ,body (i32.const 0)))

; Use macros in actual code
(export (fn double ((x s32)) s32
  (* x 2)))

(export (fn add-five ((x s32)) s32
  (+ x 5)))

(export (fn factorial ((n s32)) s32
  (if (= n 0)
      1
      (* n (factorial (- n 1))))))

(export (fn test-when ((x s32)) s32
  (when (i32.gt_s x 0)
    (* x x))))
