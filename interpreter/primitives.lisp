; Direct Wisp implementations of the compiler's s32 Wasm primitives.
; The explicit table excludes raw memory access to the evaluator's own heap.
(fn i32-primitive? ((name string)) s32
  (i32.or (string=? name "i32.const")
    (i32.or (i32-arithmetic? name) (i32.or (i32-bitwise? name) (i32-comparison? name)))))

(fn i32-arithmetic? ((name string)) s32
  (if (string=? name "i32.add") 1
    (if (string=? name "i32.sub") 1
      (if (string=? name "i32.mul") 1
        (if (string=? name "i32.div_s") 1
          (if (string=? name "i32.div_u") 1
            (if (string=? name "i32.rem_s") 1
              (if (string=? name "i32.rem_u") 1
                0))))))))

(fn i32-bitwise? ((name string)) s32
  (if (string=? name "i32.and") 1
    (if (string=? name "i32.or") 1
      (if (string=? name "i32.xor") 1
        (if (string=? name "i32.shl") 1
          (if (string=? name "i32.shr_s") 1
            (if (string=? name "i32.shr_u") 1
              (if (string=? name "i32.rotl") 1
                (if (string=? name "i32.rotr") 1
                  0)))))))))

(fn i32-comparison? ((name string)) s32
  (if (string=? name "i32.eq") 1
    (if (string=? name "i32.ne") 1
      (if (string=? name "i32.lt_s") 1
        (if (string=? name "i32.lt_u") 1
          (if (string=? name "i32.gt_s") 1
            (if (string=? name "i32.gt_u") 1
              (if (string=? name "i32.le_s") 1
                (if (string=? name "i32.le_u") 1
                  (if (string=? name "i32.ge_s") 1
                    (if (string=? name "i32.ge_u") 1
                      0)))))))))))

(fn apply-i32-pair ((name string) (a s32) (b s32)) value
  (if (string=? name "i32.add")
    (integer (i32.add a b))
    (if (string=? name "i32.sub")
    (integer (i32.sub a b))
    (if (string=? name "i32.mul")
    (integer (i32.mul a b))
    (if (string=? name "i32.div_s")
    (if (i32.eq b 0) (failure "division by zero") (if (i32.and (i32.eq a -2147483648) (i32.eq b -1)) (failure "division overflow") (integer (i32.div_s a b))))
    (if (string=? name "i32.div_u")
    (if (i32.eq b 0) (failure "division by zero") (integer (i32.div_u a b)))
    (if (string=? name "i32.rem_s")
    (if (i32.eq b 0) (failure "division by zero") (integer (i32.rem_s a b)))
    (if (string=? name "i32.rem_u")
    (if (i32.eq b 0) (failure "division by zero") (integer (i32.rem_u a b)))
    (apply-i32-group-1 name a b)))))))))

(fn apply-i32-group-1 ((name string) (a s32) (b s32)) value
  (if (string=? name "i32.and")
    (integer (i32.and a b))
    (if (string=? name "i32.or")
    (integer (i32.or a b))
    (if (string=? name "i32.xor")
    (integer (i32.xor a b))
    (if (string=? name "i32.shl")
    (integer (i32.shl a b))
    (if (string=? name "i32.shr_s")
    (integer (i32.shr_s a b))
    (if (string=? name "i32.shr_u")
    (integer (i32.shr_u a b))
    (if (string=? name "i32.rotl")
    (integer (i32.rotl a b))
    (if (string=? name "i32.rotr")
    (integer (i32.rotr a b))
    (apply-i32-group-2 name a b))))))))))

(fn apply-i32-group-2 ((name string) (a s32) (b s32)) value
  (if (string=? name "i32.eq")
    (integer (i32.eq a b))
    (if (string=? name "i32.ne")
    (integer (i32.ne a b))
    (if (string=? name "i32.lt_s")
    (integer (i32.lt_s a b))
    (if (string=? name "i32.lt_u")
    (integer (i32.lt_u a b))
    (if (string=? name "i32.gt_s")
    (integer (i32.gt_s a b))
    (if (string=? name "i32.gt_u")
    (integer (i32.gt_u a b))
    (if (string=? name "i32.le_s")
    (integer (i32.le_s a b))
    (if (string=? name "i32.le_u")
    (integer (i32.le_u a b))
    (if (string=? name "i32.ge_s")
    (integer (i32.ge_s a b))
    (if (string=? name "i32.ge_u")
    (integer (i32.ge_u a b))
    (failure "unknown i32 primitive"))))))))))))

(fn apply-i32 ((name string) (args (list value))) value
  (if (i32.ne (list-len args) (if (string=? name "i32.const") 1 2))
    (failure "wrong number of arguments")
    (value-case (list-get args 0)
      ((integer a)
        (if (string=? name "i32.const") (integer a)
          (value-case (list-get args 1)
            ((integer b) (apply-i32-pair name a b))
            (else (failure "expected s32 arguments")))))
      (else (failure "expected s32 arguments")))))

(fn string-primitive? ((name string)) s32
  (i32.or (i32.or (string=? name "string-len") (string=? name "string-ref"))
    (i32.or (string=? name "string-append")
      (i32.or (string=? name "string=?") (string=? name "substring")))))

(fn apply-string ((name string) (args (list value))) value
  (let (arity (if (string=? name "string-len") 1 (if (string=? name "substring") 3 2)))
    (if (i32.ne (list-len args) arity) (failure "wrong number of arguments")
      (value-case (list-get args 0)
        ((text s)
          (if (string=? name "string-len") (integer (string-len s))
            (if (i32.or (string=? name "string-append") (string=? name "string=?"))
              (value-case (list-get args 1)
                ((text t) (if (string=? name "string-append") (text (string-append s t)) (integer (string=? s t))))
                (else (failure "expected string arguments")))
              (value-case (list-get args 1)
                ((integer start)
                  (if (string=? name "string-ref")
                    (if (i32.and (i32.ge_s start 0) (i32.lt_s start (string-len s)))
                      (integer (string-ref s start)) (failure "string index out of bounds"))
                    (value-case (list-get args 2)
                      ((integer end)
                        (if (i32.and (i32.ge_s start 0)
                              (i32.and (i32.ge_s end start) (i32.le_s end (string-len s))))
                          (text (substring s start end)) (failure "substring bounds out of range")))
                      (else (failure "expected s32 bounds")))))
                (else (failure "expected s32 index"))))))
        (else (failure "expected string argument"))))))
