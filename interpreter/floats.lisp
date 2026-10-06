; Scalar float values use native Wasm arithmetic. Decimal I/O lives in Wisp too.
(fn float-type? ((name string)) s32
  (i32.or (string=? name "f32") (string=? name "f64")))
(fn numeric-type? ((name string)) s32
  (i32.or (integer-type? name) (float-type? name)))
(fn float-suffix ((s string)) string
  (let (n (string-len s))
    (if (i32.gt_s n 3)
      (let (suffix (substring s (i32.sub n 3) n))
        (if (float-type? suffix) suffix "")) "")))
(fn contains-dot? ((s string) (pos s32)) s32
  (if (i32.ge_s pos (string-len s)) 0
    (if (i32.eq (string-ref s pos) 46) 1 (contains-dot? s (i32.add pos 1)))))
(fn float-word ((s string) (pos s32) (out string)) string
  (if (i32.ge_s pos (string-len s)) out
    (let (c (string-ref s pos))
      (let (part (if (i32.and (i32.ge_s c 65) (i32.le_s c 90))
                    (substring "abcdefghijklmnopqrstuvwxyz" (i32.sub c 65) (i32.sub c 64))
                    (substring s pos (i32.add pos 1))))
        (float-word s (i32.add pos 1) (string-append out part))))))
(fn numeric-token? ((s string)) s32
  (let (c (string-ref s 0))
    (let (pos (if (i32.or (i32.eq c 43) (i32.eq c 45)) 1 0))
      (if (i32.ge_s pos (string-len s)) 0
        (let (first (string-ref s pos))
          (if (i32.or (digit? first) (i32.eq first 46)) 1
            (let (suffix (float-suffix s))
              (if (string=? suffix "") 0
                (let (base (float-word (substring s pos (i32.sub (string-len s) 3)) 0 ""))
                  (i32.or (string=? base "inf") (i32.or (string=? base "infinity") (string=? base "nan"))))))))))))
(fn read-number ((s string)) value
  (let (suffix (float-suffix s))
    (if (i32.and (string=? suffix "") (i32.eq (contains-dot? s 0) 0)) (read-integer-token s)
      (let (base (if (string=? suffix "") s (substring s 0 (i32.sub (string-len s) 3))))
        (let (c (string-ref base 0))
          (let (negative (i32.eq c 45))
            (let (start (if (i32.or negative (i32.eq c 43)) 1 0))
              (let (unsigned (if (i32.le_s (string-len base) 9) (float-word (substring base start (string-len base)) 0 "") ""))
                (if (i32.or (string=? unsigned "inf") (string=? unsigned "infinity"))
                  (finish-float (f64.div 1.0 0.0) negative suffix)
                  (if (string=? unsigned "nan") (finish-float (f64.div 0.0 0.0) negative suffix)
                    (let (parts (decimal-scan base start "" 0 0))
                      (if (decimal-parts.valid parts)
                        (finish-float (decimal-convert (decimal-parts.digits parts) (decimal-parts.exponent parts)) negative suffix)
                        (failure "unsupported number literal")))))))))))))
(fn finish-float ((n f64) (negative s32) (suffix string)) value
  (let (signed (if negative (f64.mul n -1.0) n))
    (if (string=? suffix "f32") (single (f32.demote_f64 signed)) (double signed))))

(fn numeric-primitive? ((name string)) s32
  (if (numeric-type? name) 1
    (if (i32.lt_s (string-len name) 4) 0
      (if (string=? (float-conversion-source name) "")
        (if (float-type? (substring name 0 3)) (float-primitive? name) (integer-primitive? name)) 1))))
(fn numeric-operand-type ((name string)) string
  (if (numeric-type? name) name
    (if (i32.lt_s (string-len name) 4) ""
      (let (source (float-conversion-source name))
        (if (string=? source "")
          (let (prefix (substring name 0 3))
            (if (float-type? prefix) prefix (integer-operand-type name))) source)))))
(fn apply-numeric-primitive ((name string) (args (list value))) value
  (if (numeric-type? name)
    (if (i32.eq (list-len args) 1) (numeric-cast name (list-get args 0)) (failure "wrong number of arguments"))
    (let (source (float-conversion-source name))
      (if (string=? source "")
        (if (float-type? (substring name 0 3)) (apply-float name args) (apply-integer-primitive name args))
        (if (i32.ne (list-len args) 1) (failure "wrong number of arguments")
          (let (v (require-type (list-get args 0) (symbol source)))
            (if (failed? v) v (apply-float-conversion name v))))))))

(fn truncate-float ((name string) (n f64)) value
  (let (wide (string=? (substring name 0 3) "i64"))
    (let (unsigned (string=? (substring name (i32.sub (string-len name) 1) (string-len name)) "u"))
      (let (valid (if unsigned
                    (i32.and (f64.gt n -1.0) (f64.lt n (if wide 18446744073709551616.0 4294967296.0)))
                    (if wide (i32.and (f64.ge n -9223372036854775808.0) (f64.lt n 9223372036854775808.0))
                      (i32.and (f64.gt n -2147483649.0) (f64.lt n 2147483648.0)))))
        (if (i32.eq valid 0) (failure "float-to-integer conversion out of range")
          (if wide (wide-integer (if unsigned (i64.trunc_f64_u n) (i64.trunc_f64_s n)))
            (integer (if unsigned (i32.trunc_f64_u n) (i32.trunc_f64_s n)))))))))
(fn cast-double ((name string) (n f64)) value
  (if (string=? name "f64") (double n)
    (if (string=? name "f32") (single (f32.demote_f64 n))
      (truncate-float (if (string=? name "s64") "i64.trunc_f64_s" "i32.trunc_f64_s") n))))
(fn numeric-cast ((name string) (v value)) value
  (value-case v
    ((integer n)
      (if (string=? name "f32") (single (f32.convert_i32_s n))
        (if (string=? name "f64") (double (f64.convert_i32_s n)) (apply-integer-conversion name v))))
    ((wide-integer n)
      (if (string=? name "f32") (single (f32.convert_i64_s n))
        (if (string=? name "f64") (double (f64.convert_i64_s n)) (apply-integer-conversion name v))))
    ((single n) (cast-double name (f64.promote_f32 n)))
    ((double n) (cast-double name n))
    (else (failure "expected numeric argument"))))
(fn apply-float-conversion ((name string) (v value)) value
  (let (target (substring name 0 3))
    (let (unsigned (string=? (substring name (i32.sub (string-len name) 1) (string-len name)) "u"))
      (value-case v
        ((integer n)
          (if (string=? target "f32")
            (single (if unsigned (f32.convert_i32_u n) (f32.convert_i32_s n)))
            (double (if unsigned (f64.convert_i32_u n) (f64.convert_i32_s n)))))
        ((wide-integer n)
          (if (string=? target "f32")
            (single (if unsigned (f32.convert_i64_u n) (f32.convert_i64_s n)))
            (double (if unsigned (f64.convert_i64_u n) (f64.convert_i64_s n)))))
        ((single n)
          (if (float-type? target) (cast-double target (f64.promote_f32 n)) (truncate-float name (f64.promote_f32 n))))
        ((double n) (if (float-type? target) (cast-double target n) (truncate-float name n)))
        (else (failure "expected numeric argument"))))))

(fn float-builtin-name ((name string) (prefix string)) string
  (string-append prefix
    (if (string=? name "+") ".add"
      (if (string=? name "-") ".sub"
        (if (string=? name "*") ".mul"
          (if (string=? name "=") ".eq"
            (if (string=? name "<") ".lt" ".div")))))))
(fn apply-numeric-builtin ((name string) (left value) (right value)) value
  (value-case left
    ((single a)
      (value-case right
        ((single b) (apply-f32-pair (float-builtin-name name "f32") a b))
        (else (failure "expected matching numeric types"))))
    ((double a)
      (value-case right
        ((double b) (apply-f64-pair (float-builtin-name name "f64") a b))
        (else (failure "expected matching numeric types"))))
    (else (apply-integer-builtin name left right))))

(fn float-conversion-source ((name string)) string
  (if (string=? name "f32.convert_i32_s") "s32"
    (if (string=? name "f32.convert_i32_u") "s32"
    (if (string=? name "f64.convert_i32_s") "s32"
    (if (string=? name "f64.convert_i32_u") "s32"
    (float-conversion-source-1 name))))))

(fn float-conversion-source-1 ((name string)) string
  (if (string=? name "f32.convert_i64_s") "s64"
    (if (string=? name "f32.convert_i64_u") "s64"
    (if (string=? name "f64.convert_i64_s") "s64"
    (if (string=? name "f64.convert_i64_u") "s64"
    (float-conversion-source-2 name))))))

(fn float-conversion-source-2 ((name string)) string
  (if (string=? name "i32.trunc_f32_s") "f32"
    (if (string=? name "i32.trunc_f32_u") "f32"
    (if (string=? name "i64.trunc_f32_s") "f32"
    (if (string=? name "i64.trunc_f32_u") "f32"
    (float-conversion-source-3 name))))))

(fn float-conversion-source-3 ((name string)) string
  (if (string=? name "i32.trunc_f64_s") "f64"
    (if (string=? name "i32.trunc_f64_u") "f64"
    (if (string=? name "i64.trunc_f64_s") "f64"
    (if (string=? name "i64.trunc_f64_u") "f64"
    (float-conversion-source-4 name))))))

(fn float-conversion-source-4 ((name string)) string
  (if (string=? name "f32.demote_f64") "f64"
    (if (string=? name "f64.promote_f32") "f32"
    "")))

(fn float-primitive? ((name string)) s32
  (if (i32.eq (string-ref name 3) 46)
    (let (op (substring name 4 (string-len name)))
      (if (string=? op "const") 1 (float-binary? op))) 0))

(fn float-binary? ((op string)) s32
  (if (string=? op "add") 1
    (if (string=? op "sub") 1
    (if (string=? op "mul") 1
    (if (string=? op "div") 1
    (if (string=? op "eq") 1
    (if (string=? op "ne") 1
    (if (string=? op "lt") 1
    (if (string=? op "gt") 1
    (if (string=? op "le") 1
    (if (string=? op "ge") 1
    0)))))))))))

(fn apply-f32-pair ((name string) (a f32) (b f32)) value
  (if (string=? name "f32.add") (single (f32.add a b))
    (if (string=? name "f32.sub") (single (f32.sub a b))
    (if (string=? name "f32.mul") (single (f32.mul a b))
    (if (string=? name "f32.div") (single (f32.div a b))
    (if (string=? name "f32.eq") (integer (f32.eq a b))
    (if (string=? name "f32.ne") (integer (f32.ne a b))
    (if (string=? name "f32.lt") (integer (f32.lt a b))
    (if (string=? name "f32.gt") (integer (f32.gt a b))
    (if (string=? name "f32.le") (integer (f32.le a b))
    (if (string=? name "f32.ge") (integer (f32.ge a b))
    (failure "unknown float primitive"))))))))))))

(fn apply-f64-pair ((name string) (a f64) (b f64)) value
  (if (string=? name "f64.add") (double (f64.add a b))
    (if (string=? name "f64.sub") (double (f64.sub a b))
    (if (string=? name "f64.mul") (double (f64.mul a b))
    (if (string=? name "f64.div") (double (f64.div a b))
    (if (string=? name "f64.eq") (integer (f64.eq a b))
    (if (string=? name "f64.ne") (integer (f64.ne a b))
    (if (string=? name "f64.lt") (integer (f64.lt a b))
    (if (string=? name "f64.gt") (integer (f64.gt a b))
    (if (string=? name "f64.le") (integer (f64.le a b))
    (if (string=? name "f64.ge") (integer (f64.ge a b))
    (failure "unknown float primitive"))))))))))))

(fn apply-float ((name string) (args (list value))) value
  (let (constant (string=? (substring name 4 (string-len name)) "const"))
    (if (i32.ne (list-len args) (if constant 1 2)) (failure "wrong number of arguments")
      (let (ty (substring name 0 3))
        (let (a (require-type (list-get args 0) (symbol ty)))
          (if (failed? a) a
            (if constant a
              (let (b (require-type (list-get args 1) (symbol ty)))
                (if (failed? b) b
                  (value-case a
                    ((single x) (value-case b ((single y) (apply-f32-pair name x y)) (else (failure "expected f32"))))
                    ((double x) (value-case b ((double y) (apply-f64-pair name x y)) (else (failure "expected f64"))))
                    (else (failure "expected float"))))))))))))
