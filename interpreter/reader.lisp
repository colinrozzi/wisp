; Reader for the interpreted REPL. Adapted from examples/wisp-compiler.lisp.
; Syntax and runtime data share the same values; malformed input is a failure.

(include "values.lisp")

(fn whitespace? ((c s32)) s32
  (i32.or (i32.or (i32.eq c 32) (i32.eq c 9))
          (i32.or (i32.eq c 10) (i32.eq c 13))))

(fn digit? ((c s32)) s32
  (i32.and (i32.ge_s c 48) (i32.le_s c 57)))

(fn delimiter? ((c s32)) s32
  (i32.or (whitespace? c)
    (i32.or (i32.or (i32.eq c 40) (i32.eq c 41))
      (i32.or (i32.eq c 59) (i32.or (i32.eq c 34) (i32.eq c 39))))))

(fn skip-comment ((src string) (pos s32)) s32
  (if (i32.ge_s pos (string-len src)) pos
    (if (i32.eq (string-ref src pos) 10) pos
      (skip-comment src (i32.add pos 1)))))

(fn skip-space ((src string) (pos s32)) s32
  (if (i32.ge_s pos (string-len src)) pos
    (let (c (string-ref src pos))
      (if (whitespace? c) (skip-space src (i32.add pos 1))
        (if (i32.eq c 59) (skip-space src (skip-comment src pos)) pos)))))

(fn atom-end ((src string) (pos s32)) s32
  (if (i32.ge_s pos (string-len src)) pos
    (if (delimiter? (string-ref src pos)) pos
      (atom-end src (i32.add pos 1)))))

; Accumulate negatively to represent -2147483648 without overflow.
(fn read-integer ((s string) (pos s32) (acc s32) (negative s32)) value
  (if (i32.ge_s pos (string-len s))
    (if negative (integer acc) (integer (i32.sub 0 acc)))
    (let (c (string-ref s pos))
      (if (digit? c)
        (let (d (i32.sub c 48))
          (if (i32.or (i32.lt_s acc -214748364)
                (i32.and (i32.eq acc -214748364)
                  (i32.gt_s d (if negative 8 7))))
            (failure "integer out of s32 range")
            (read-integer s (i32.add pos 1) (i32.sub (i32.mul acc 10) d) negative)))
        (failure "unsupported number literal")))))

(fn read-atom ((s string)) value
  (if (digit? (string-ref s 0)) (read-integer s 0 0 0)
    (if (i32.and (i32.eq (string-ref s 0) 45) (i32.gt_s (string-len s) 1))
      (if (digit? (string-ref s 1)) (read-integer s 1 0 1) (symbol s))
      (symbol s))))

(fn read-string ((src string) (pos s32) (acc string)) read-result
  (if (i32.ge_s pos (string-len src))
    (read-result (failure "unterminated string") pos)
    (let (c (string-ref src pos))
      (if (i32.eq c 34) (read-result (text acc) (i32.add pos 1))
        (if (i32.eq c 92)
          (if (i32.ge_s (i32.add pos 1) (string-len src))
            (read-result (failure "unterminated string escape") pos)
            (let (e (string-ref src (i32.add pos 1)))
              (let (decoded
                (if (i32.eq e 110) "\n"
                  (if (i32.eq e 116) "\t"
                    (if (i32.eq e 114) "\r"
                      (substring src (i32.add pos 1) (i32.add pos 2))))))
                (if (i32.or (i32.or (i32.eq e 110) (i32.eq e 116))
                      (i32.or (i32.eq e 114) (i32.or (i32.eq e 34) (i32.eq e 92))))
                  (read-string src (i32.add pos 2) (string-append acc decoded))
                  (read-result (failure "unknown string escape") pos)))))
          (read-string src (i32.add pos 1)
            (string-append acc (substring src pos (i32.add pos 1)))))))))

(fn read-list ((src string) (pos s32) (items (list value)) (depth s32)) read-result
  (let (start (skip-space src pos))
    (if (i32.ge_s start (string-len src))
      (read-result (failure "unclosed parenthesis") start)
      (if (i32.eq (string-ref src start) 41)
        (read-result (sequence items) (i32.add start 1))
        (let (one (read-one src start depth))
          (if (failed? (read-result.item one)) one
            (read-list src (read-result.next one)
              (list-push items (read-result.item one)) depth)))))))

(fn read-one ((src string) (pos s32) (depth s32)) read-result
  (let (start (skip-space src pos))
    (if (i32.gt_s depth 64) (read-result (failure "reader nesting limit") start)
      (if (i32.ge_s start (string-len src))
        (read-result (failure "expected expression") start)
        (let (c (string-ref src start))
          (if (i32.eq c 40) (read-list src (i32.add start 1) (list-new value) (i32.add depth 1))
            (if (i32.eq c 41) (read-result (failure "unexpected closing parenthesis") start)
              (if (i32.eq c 34) (read-string src (i32.add start 1) "")
                (if (i32.eq c 39)
                  (let (one (read-one src (i32.add start 1) (i32.add depth 1)))
                    (if (failed? (read-result.item one)) one
                      (read-result
                        (sequence (list-push (list-push (list-new value) (symbol "quote")) (read-result.item one)))
                        (read-result.next one))))
                  (let (end (atom-end src start))
                    (read-result (read-atom (substring src start end)) end)))))))))))

(fn read-forms ((src string) (pos s32) (items (list value))) value
  (let (start (skip-space src pos))
    (if (i32.ge_s start (string-len src)) (sequence items)
      (let (one (read-one src start 0))
        (if (failed? (read-result.item one)) (read-result.item one)
          (read-forms src (read-result.next one) (list-push items (read-result.item one))))))))
