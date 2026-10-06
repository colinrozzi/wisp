; Print values as readable Lisp data. Closures remain session-local.
(fn digits ((n s32) (acc string)) string
  (if (i32.eq n 0) acc
    (let (d (i32.rem_u n 10))
      (digits (i32.div_u n 10)
        (string-append (substring "0123456789" d (i32.add d 1)) acc)))))

(fn show-integer ((n s32)) string
  (if (i32.eq n 0) "0"
    (if (i32.lt_s n 0) (string-append "-" (digits (i32.sub 0 n) ""))
      (digits n ""))))

(fn wide-digits ((n s64) (acc string)) string
  (if (i64.eq n 0) acc
    (let (d (i32.wrap_i64 (i64.rem_u n 10)))
      (wide-digits (i64.div_u n 10)
        (string-append (substring "0123456789" d (i32.add d 1)) acc)))))

(fn show-wide ((n s64)) string
  (string-append
    (if (i64.eq n 0) "0"
      (if (i64.lt_s n 0) (string-append "-" (wide-digits (i64.sub 0 n) ""))
        (wide-digits n ""))) "s64"))

(fn escape-text ((s string) (pos s32) (out string)) string
  (if (i32.ge_s pos (string-len s)) out
    (let (c (string-ref s pos))
      (let (part
        (if (i32.eq c 34) "\\\""
          (if (i32.eq c 92) "\\\\"
            (if (i32.eq c 10) "\\n"
              (if (i32.eq c 9) "\\t"
                (if (i32.eq c 13) "\\r" (substring s pos (i32.add pos 1))))))))
        (escape-text s (i32.add pos 1) (string-append out part))))))

(fn show-list-at ((items (list value)) (index s32) (out string) (depth s32)) string
  (if (i32.ge_s index (list-len items)) (string-append out ")")
    (show-list-at items (i32.add index 1)
      (string-append (if (i32.eq index 0) out (string-append out " "))
        (show-at (list-get items index) (i32.add depth 1))) depth)))

(fn show ((v value)) string
  (show-at v 0))

; Typed lists can be cyclic through records. Bound recursive rendering even
; though the reader can only create finite syntax trees.
(fn show-at ((v value) (depth s32)) string
  (if (i32.ge_s depth 64) "#<depth-limit>"
  (match v
    ((integer n) (show-integer n))
    ((wide-integer n) (show-wide n))
    ((integer-literal n) "#<integer-literal>")
    ((single n) (show-float (f64.promote_f32 n) 9 "f32"))
    ((double n) (show-float n 17 "f64"))
    ((typed-list element items)
      (string-append "#<list " (string-append (show element)
        (string-append " " (string-append (show-list-at items 0 "(" depth) ">")))))
    ((compound ty case-name fields) (show-compound ty case-name fields depth))
    ((text s) (string-append "\"" (string-append (escape-text s 0 "") "\"")))
    ((symbol s) s)
    ((identifier name key) name)
    ((sequence items) (show-list-at items 0 "(" depth))
    ((closure params body env) "#<closure>")
    ((typed-function params result body) "#<function>")
    ((generic-function params result body vars constraints) "#<generic-function>")
    ((trait-method trait method) (string-append "#<method " (string-append method ">")))
    ((checked-function target params result) "#<function>")
    ((aggregate name id case-name fields)
      (show-list-at fields 0 (string-append "(" (string-append case-name (if (list-len fields) " " ""))) depth))
    ((constructor name id case-name types) (string-append "#<constructor " (string-append case-name ">")))
    ((field-reader name id index) (string-append "#<field " (string-append name ">")))
    ((builtin name) (string-append "#<builtin " (string-append name ">")))
    ((failure message) (string-append "error: " message)))))
