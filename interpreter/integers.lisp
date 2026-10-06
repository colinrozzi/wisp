; Exact integer values and safe Wasm arithmetic. Memory instructions are excluded.
(fn integer-type? ((name string)) s32
  (i32.or (string=? name "s32") (string=? name "s64")))

(fn eval-integer-ascription ((items (list value)) (env (list binding)) (depth s32)) value
  (let (name (symbol-name (list-get items 2)))
    (if (integer-type? name)
      (let (v (eval-expected (list-get items 0) env depth 0 name))
        (if (failed? v) v (apply-integer-conversion name v)))
      (failure "unsupported ascription type"))))

(fn resolve-integer ((n s64) (expected string)) value
  (if (string=? expected "s64") (wide-integer n)
    (if (i32.or (i64.lt_s n -2147483648) (i64.gt_s n 2147483647))
      (failure "integer out of s32 range") (integer (i32.wrap_i64 n)))))

; Quoted data is materialized with default types, never adopted at a later call.
(fn quote-value ((v value)) value
  (value-case v
    ((integer-literal n) (resolve-integer n ""))
    ((sequence items) (quote-items items 0 (list-new value)))
    (else v)))

(fn quote-items ((items (list value)) (index s32) (out (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence out)
    (let (v (quote-value (list-get items index)))
      (if (failed? v) v
        (quote-items items (i32.add index 1) (list-push out v))))))

(fn integer-conversion? ((name string)) s32
  (i32.or (integer-type? name)
    (i32.or (string=? name "i32.wrap_i64")
      (i32.or (string=? name "i64.extend_i32_s") (string=? name "i64.extend_i32_u")))))

(fn integer-primitive? ((name string)) s32
  (if (integer-conversion? name) 1
    (if (i32.ge_s (string-len name) 4)
      (let (prefix (substring name 0 4))
        (if (string=? prefix "i32.") (i32-primitive? name)
          (if (string=? prefix "i64.") (i64-primitive? name) 0))) 0)))

(fn integer-operand-type ((name string)) string
  (if (i32.or (string=? name "s64") (string=? name "i32.wrap_i64")) "s64"
    (if (integer-conversion? name) "s32"
      (if (i32.ge_s (string-len name) 4)
        (let (prefix (substring name 0 4))
          (if (string=? prefix "i64.") "s64"
            (if (string=? prefix "i32.") "s32" ""))) ""))))

(fn argument-type ((callee value) (index s32)) string
  (value-case callee
    ((typed-function params result body)
      (if (i32.lt_s index (list-len params)) (symbol-name (field-type (list-get params index))) ""))
    ((constructor name id case-name types)
      (if (i32.lt_s index (list-len types)) (symbol-name (list-get types index)) ""))
    ((builtin name) (integer-operand-type name))
    (else "")))

(fn apply-integer-conversion ((name string) (v value)) value
  (value-case v
    ((integer n)
      (if (string=? name "i32.wrap_i64") (failure "expected s64 argument")
        (if (string=? name "s32") v
          (wide-integer (if (string=? name "i64.extend_i32_u") (i64.extend_i32_u n) (i64.extend_i32_s n))))))
    ((wide-integer n)
      (if (string=? name "s64") v
        (if (i32.or (string=? name "s32") (string=? name "i32.wrap_i64"))
          (integer (i32.wrap_i64 n)) (failure "expected s32 argument"))))
    (else (failure "expected integer argument"))))

(fn apply-integer-primitive ((name string) (args (list value))) value
  (if (integer-conversion? name)
    (if (i32.eq (list-len args) 1) (apply-integer-conversion name (list-get args 0))
      (failure "wrong number of arguments"))
    (if (i64-primitive? name) (apply-i64 name args) (apply-i32 name args))))

(fn wide-builtin-name ((name string)) string
  (if (string=? name "+") "i64.add"
    (if (string=? name "-") "i64.sub"
      (if (string=? name "*") "i64.mul"
        (if (string=? name "=") "i64.eq"
          (if (string=? name "<") "i64.lt_s" "i64.div_s"))))))

(fn apply-integer-builtin ((name string) (left value) (right value)) value
  (value-case left
    ((integer a)
      (value-case right
        ((integer b) (numeric-op name a b))
        (else (failure "expected matching integer types"))))
    ((wide-integer a)
      (value-case right
        ((wide-integer b) (apply-i64-pair (wide-builtin-name name) a b))
        (else (failure "expected matching integer types"))))
    (else (failure "expected integer arguments"))))

(fn i64-primitive? ((name string)) s32
  (i32.or (string=? name "i64.const")
    (i32.or (i64-arithmetic? name) (i32.or (i64-bitwise? name) (i64-comparison? name)))))

(fn i64-arithmetic? ((name string)) s32
  (if (string=? name "i64.add") 1
    (if (string=? name "i64.sub") 1
      (if (string=? name "i64.mul") 1
        (if (string=? name "i64.div_s") 1
          (if (string=? name "i64.div_u") 1
            (if (string=? name "i64.rem_s") 1
              (if (string=? name "i64.rem_u") 1
                0))))))))

(fn i64-bitwise? ((name string)) s32
  (if (string=? name "i64.and") 1
    (if (string=? name "i64.or") 1
      (if (string=? name "i64.xor") 1
        (if (string=? name "i64.shl") 1
          (if (string=? name "i64.shr_s") 1
            (if (string=? name "i64.shr_u") 1
              (if (string=? name "i64.rotl") 1
                (if (string=? name "i64.rotr") 1
                  0)))))))))

(fn i64-comparison? ((name string)) s32
  (if (string=? name "i64.eq") 1
    (if (string=? name "i64.ne") 1
      (if (string=? name "i64.lt_s") 1
        (if (string=? name "i64.lt_u") 1
          (if (string=? name "i64.gt_s") 1
            (if (string=? name "i64.gt_u") 1
              (if (string=? name "i64.le_s") 1
                (if (string=? name "i64.le_u") 1
                  (if (string=? name "i64.ge_s") 1
                    (if (string=? name "i64.ge_u") 1
                      0)))))))))))

(fn apply-i64-pair ((name string) (a s64) (b s64)) value
  (if (string=? name "i64.add")
    (wide-integer (i64.add a b))
    (if (string=? name "i64.sub")
    (wide-integer (i64.sub a b))
    (if (string=? name "i64.mul")
    (wide-integer (i64.mul a b))
    (if (string=? name "i64.div_s")
    (if (i64.eq b 0) (failure "division by zero") (if (i32.and (i64.eq a -9223372036854775808) (i64.eq b -1)) (failure "division overflow") (wide-integer (i64.div_s a b))))
    (if (string=? name "i64.div_u")
    (if (i64.eq b 0) (failure "division by zero") (wide-integer (i64.div_u a b)))
    (if (string=? name "i64.rem_s")
    (if (i64.eq b 0) (failure "division by zero") (wide-integer (i64.rem_s a b)))
    (if (string=? name "i64.rem_u")
    (if (i64.eq b 0) (failure "division by zero") (wide-integer (i64.rem_u a b)))
    (apply-i64-group-1 name a b)))))))))

(fn apply-i64-group-1 ((name string) (a s64) (b s64)) value
  (if (string=? name "i64.and")
    (wide-integer (i64.and a b))
    (if (string=? name "i64.or")
    (wide-integer (i64.or a b))
    (if (string=? name "i64.xor")
    (wide-integer (i64.xor a b))
    (if (string=? name "i64.shl")
    (wide-integer (i64.shl a b))
    (if (string=? name "i64.shr_s")
    (wide-integer (i64.shr_s a b))
    (if (string=? name "i64.shr_u")
    (wide-integer (i64.shr_u a b))
    (if (string=? name "i64.rotl")
    (wide-integer (i64.rotl a b))
    (if (string=? name "i64.rotr")
    (wide-integer (i64.rotr a b))
    (apply-i64-group-2 name a b))))))))))

(fn apply-i64-group-2 ((name string) (a s64) (b s64)) value
  (if (string=? name "i64.eq")
    (integer (i64.eq a b))
    (if (string=? name "i64.ne")
    (integer (i64.ne a b))
    (if (string=? name "i64.lt_s")
    (integer (i64.lt_s a b))
    (if (string=? name "i64.lt_u")
    (integer (i64.lt_u a b))
    (if (string=? name "i64.gt_s")
    (integer (i64.gt_s a b))
    (if (string=? name "i64.gt_u")
    (integer (i64.gt_u a b))
    (if (string=? name "i64.le_s")
    (integer (i64.le_s a b))
    (if (string=? name "i64.le_u")
    (integer (i64.le_u a b))
    (if (string=? name "i64.ge_s")
    (integer (i64.ge_s a b))
    (if (string=? name "i64.ge_u")
    (integer (i64.ge_u a b))
    (failure "unknown i64 primitive"))))))))))))

(fn apply-i64 ((name string) (args (list value))) value
  (if (i32.ne (list-len args) (if (string=? name "i64.const") 1 2))
    (failure "wrong number of arguments")
    (value-case (list-get args 0)
      ((wide-integer a)
        (if (string=? name "i64.const") (wide-integer a)
          (value-case (list-get args 1)
            ((wide-integer b) (apply-i64-pair name a b))
            (else (failure "expected s64 arguments")))))
      (else (failure "expected s64 arguments")))))
