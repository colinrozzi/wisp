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

(fn show-list ((items (list value)) (index s32) (out string)) string
  (if (i32.ge_s index (list-len items)) (string-append out ")")
    (show-list items (i32.add index 1)
      (string-append (if (i32.eq index 0) out (string-append out " "))
        (show (list-get items index))))))

(fn show ((v value)) string
  (match v
    ((integer n) (show-integer n))
    ((text s) (string-append "\"" (string-append (escape-text s 0 "") "\"")))
    ((symbol s) s)
    ((sequence items) (show-list items 0 "("))
    ((closure params body env) "#<closure>")
    ((typed-function params result body) "#<function>")
    ((aggregate name id case-name fields)
      (show-list fields 0 (string-append "(" (string-append case-name (if (list-len fields) " " "")))))
    ((constructor name id case-name types) (string-append "#<constructor " (string-append case-name ">")))
    ((field-reader name id index) (string-append "#<field " (string-append name ">")))
    ((builtin name) (string-append "#<builtin " (string-append name ">")))
    ((failure message) (string-append "error: " message))))
