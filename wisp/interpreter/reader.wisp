; Reader for the interpreted REPL. Adapted from examples/wisp-compiler.wisp.
; Syntax and runtime data share the same values; malformed input is a failure.

(include "values.wisp")

(fn whitespace? ((c s32)) s32
  (i32.or (i32.or (i32.eq c 32) (i32.eq c 9))
          (i32.or (i32.eq c 10) (i32.eq c 13))))

(fn digit? ((c s32)) s32
  (i32.and (i32.ge_s c 48) (i32.le_s c 57)))

(fn delimiter? ((c s32)) s32
  (i32.or (whitespace? c)
    (i32.or (i32.or (i32.eq c 40) (i32.eq c 41))
      (i32.or (i32.eq c 59) (i32.or (i32.eq c 34)
        (i32.or (i32.eq c 39) (i32.or (i32.eq c 96) (i32.eq c 44))))))))

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
    (if (i32.or (delimiter? (string-ref src pos)) (syntax-reader-prefix? src pos)) pos
      (atom-end src (i32.add pos 1)))))

; Retain unsuffixed literals until evaluation knows their expected type.
; Accumulate negatively so the minimum signed 64-bit value never overflows.
(fn read-integer ((s string) (pos s32) (acc s64) (negative s32)) value
  (if (i32.ge_s pos (string-len s))
    (integer-literal (if negative acc (i64.sub 0 acc)))
    (let (c (string-ref s pos))
      (if (digit? c)
        (let (d (i32.sub c 48))
          (if (i32.or (i64.lt_s acc -922337203685477580)
                (i32.and (i64.eq acc -922337203685477580)
                  (i32.gt_s d (if negative 8 7))))
            (failure "integer out of s64 range")
            (read-integer s (i32.add pos 1) (i64.sub (i64.mul acc 10) (i64.extend_i32_s d)) negative)))
        (failure "unsupported number literal")))))

(fn read-integer-token ((s string)) value
  (let (size (string-len s))
    (let (suffix (if (i32.gt_s size 3) (substring s (i32.sub size 3) size) ""))
      (let (wide (string=? suffix "s64"))
        (let (uwide (string=? suffix "u64"))
          (let (base (if (i32.or wide uwide) (substring s 0 (i32.sub size 3)) s))
            (let (negative (i32.eq (string-ref base 0) 45))
              (let (v (read-integer base (if (i32.or negative (i32.eq (string-ref base 0) 43)) 1 0) 0s64 negative))
                (value-case v
                  ; Note: u64 literals above s64 range are not yet readable (the
                  ; accumulator is s64-range-checked); covers the common cases.
                  ((integer-literal n) (if wide (wide-integer n) (if uwide (u64-value n) v)))
                  (else v))))))))))

(fn read-atom ((s string)) value
  (if (string=? s "...") (symbol s)
    (if (string=? s "true") (boolean 1)
      (if (string=? s "false") (boolean 0)
        (if (numeric-token? s) (read-number s) (symbol s))))))

; A one-byte string holding `code` (0-255). A string is a [len:u32][bytes]
; buffer, so build that buffer and reinterpret it via string-from-addr.
(fn byte->string ((code s32)) string
  (let (buf (heap-alloc 5))
    (begin
      (i32.store buf 1)
      (i32.store8 (i32.add buf 4) code)
      (string-from-addr buf))))

; The value of hex digit `c` (0-9, a-f, A-F), or -1.
(fn hex-digit ((c s32)) s32
  (if (digit? c) (i32.sub c 48)
    (if (i32.and (i32.ge_s c 97) (i32.le_s c 102)) (i32.sub c 87)
      (if (i32.and (i32.ge_s c 65) (i32.le_s c 70)) (i32.sub c 55) -1))))

; The byte that the one-letter escape `\e` stands for, or -1 if it is unknown.
; \x is not here: it reads two more characters (see read-hex-escape).
(fn simple-escape ((e s32)) s32
  (if (i32.eq e 110) 10                 ; \n newline
    (if (i32.eq e 116) 9                ; \t tab
      (if (i32.eq e 114) 13             ; \r carriage return
        (if (i32.eq e 101) 27           ; \e escape (ESC)
          (if (i32.eq e 48) 0           ; \0 NUL
            (if (i32.or (i32.eq e 34) (i32.eq e 92)) e -1)))))))  ; \" and \\

; `\xHH` at `pos` (the backslash): exactly two hex digits, any byte value.
(fn read-hex-escape ((src string) (pos s32) (acc string)) read-result
  (if (i32.ge_s (i32.add pos 3) (string-len src))
    (read-result (failure "\\x escape needs two hex digits") pos)
    (let (hi (hex-digit (string-ref src (i32.add pos 2))))
      (let (lo (hex-digit (string-ref src (i32.add pos 3))))
        (if (i32.or (i32.lt_s hi 0) (i32.lt_s lo 0))
          (read-result (failure "\\x escape needs two hex digits") pos)
          (read-string src (i32.add pos 4)
            (string-append acc (byte->string (i32.add (i32.mul hi 16) lo)))))))))

(fn read-string ((src string) (pos s32) (acc string)) read-result
  (if (i32.ge_s pos (string-len src))
    (read-result (failure "unterminated string") pos)
    (let (c (string-ref src pos))
      (if (i32.eq c 34) (read-result (text acc) (i32.add pos 1))
        (if (i32.eq c 92)
          (if (i32.ge_s (i32.add pos 1) (string-len src))
            (read-result (failure "unterminated string escape") pos)
            (let (e (string-ref src (i32.add pos 1)))
              (if (i32.eq e 120) (read-hex-escape src pos acc)
                (let (code (simple-escape e))
                  (if (i32.lt_s code 0)
                    (read-result (failure "unknown string escape") pos)
                    (read-string src (i32.add pos 2)
                      (string-append acc (byte->string code))))))))
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

(fn read-prefix ((src string) (pos s32) (depth s32) (name string)) read-result
  (let (one (read-one src pos (i32.add depth 1)))
    (if (failed? (read-result.item one)) one
      (read-result
        (sequence (list-push (list-push (list-new value) (symbol name)) (read-result.item one)))
        (read-result.next one)))))

(fn read-one ((src string) (pos s32) (depth s32)) read-result
  (let (start (skip-space src pos))
    (if (i32.gt_s depth 64) (read-result (failure "reader nesting limit") start)
      (if (i32.ge_s start (string-len src))
        (read-result (failure "expected expression") start)
        (if (syntax-reader-prefix? src start) (read-syntax-prefix src start depth)
          (let (c (string-ref src start))
            (if (i32.eq c 40) (read-list src (i32.add start 1) (list-new value) (i32.add depth 1))
              (if (i32.eq c 41) (read-result (failure "unexpected closing parenthesis") start)
                (if (i32.eq c 34) (read-string src (i32.add start 1) "")
                  (if (i32.eq c 39) (read-prefix src (i32.add start 1) depth "quote")
                    (if (i32.eq c 96) (read-prefix src (i32.add start 1) depth "quasiquote")
                      (if (i32.eq c 44)
                        (if (if (i32.lt_s (i32.add start 1) (string-len src)) (i32.eq (string-ref src (i32.add start 1)) 64) 0)
                          (read-prefix src (i32.add start 2) depth "unquote-splice")
                          (read-prefix src (i32.add start 1) depth "unquote"))
                        (let (end (atom-end src start))
                          (read-result (read-atom (substring src start end)) end))))))))))))))

(fn read-forms ((src string) (pos s32) (items (list value))) value
  (let (start (skip-space src pos))
    (if (i32.ge_s start (string-len src)) (sequence items)
      (let (one (read-one src start 0))
        (if (failed? (read-result.item one)) (read-result.item one)
          (read-forms src (read-result.next one) (list-push items (read-result.item one))))))))

(fn syntax-reader-prefix? ((src string) (pos s32)) s32
  (if (i32.ge_s (i32.add pos 1) (string-len src)) 0
    (if (i32.eq (string-ref src pos) 35)
      (let (c (string-ref src (i32.add pos 1)))
        (i32.or (i32.eq c 39) (i32.or (i32.eq c 96) (i32.eq c 44)))) 0)))
(fn read-syntax-prefix ((src string) (pos s32) (depth s32)) read-result
  (let (c (string-ref src (i32.add pos 1)))
    (if (i32.eq c 39) (read-prefix src (i32.add pos 2) depth "syntax")
      (if (i32.eq c 96) (read-prefix src (i32.add pos 2) depth "quasisyntax")
        (if (if (i32.lt_s (i32.add pos 2) (string-len src)) (i32.eq (string-ref src (i32.add pos 2)) 64) 0)
          (read-prefix src (i32.add pos 3) depth "unsyntax-splice")
          (read-prefix src (i32.add pos 2) depth "unsyntax"))))))
