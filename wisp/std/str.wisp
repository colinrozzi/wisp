; std/str — string and number-text helpers built on the primitive string ops
; (string-len, string-ref -> byte, string-index, substring, string=?, string-append).
; Load with (require str "std/str.wisp").
(module str
  (provide index-of index-of-from byte-space byte-cr byte-nl field-between
           index-of-sub starts-with drop after split join
           number->string u64->string string->number mod)

  ; Common byte codes, read from one-char string literals so we don't hardcode ASCII.
  (define byte-space (string-ref " " 0))
  (define byte-cr    (string-ref "\r" 0))
  (define byte-nl    (string-ref "\n" 0))

  ; index of byte `b` in `s` at or after `i`; -1 if not found.
  (define index-of-from (lambda (s b i) (string-index s b i)))

  ; index of the first `b` in `s`; -1 if absent.
  (define index-of (lambda (s b) (index-of-from s b 0)))

  ; substring of `s` from `start` up to (exclusive) the next `b`, plus that
  ; delimiter's index — returned as a 2-element list (field next-index).
  (define field-between (lambda (s start b)
    (let (stop (index-of-from s b start))
      (if (= stop -1)
        (list (substring s start (string-len s)) (string-len s))
        (list (substring s start stop) stop)))))

  ; index of the first `sub` in `s`; -1 if absent; 0 if `sub` is empty.
  (define index-of-sub (lambda (s sub) (string-index s sub)))

  ; does `s` begin with `prefix`?
  (define starts-with (lambda (s prefix)
    (if (< (string-len s) (string-len prefix)) 0
      (string=? (substring s 0 (string-len prefix)) prefix))))

  ; substring of `s` from index `n` to the end.
  (define drop (lambda (s n) (substring s n (string-len s))))

  ; the part of `s` after the first occurrence of `sep`; "" if `sep` is absent.
  (define after (lambda (s sep)
    (let (i (index-of-sub s sep))
      (if (= i -1) "" (drop s (+ i (string-len sep)))))))

  (define split-from (lambda (s sep start out)
    (let (stop (string-index s sep start))
      (if (= stop -1)
        (list-push out (drop s start))
        (split-from s sep (+ stop (string-len sep))
          (list-push out (substring s start stop)))))))
  ; (split s sep): the fields of `s` between each `sep`, so "a,,b" gives 3 fields
  ; and "" gives one empty one. `sep` may be several bytes but not empty.
  (define split (lambda (s sep)
    (if (= (string-len sep) 0) (error "split: empty separator")
      (split-from s sep 0 (list)))))

  ; Joins halves, not left to right: string-append copies, so repeatedly appending
  ; to one growing string is quadratic in its length. Nests only log2(n) deep.
  (define join-range (lambda (xs sep lo hi)
    (if (>= lo hi) ""
      (if (= (+ lo 1) hi) (list-get xs lo)
        (let (mid (+ lo (/ (- hi lo) 2)))
          (string-append (join-range xs sep lo mid) sep (join-range xs sep mid hi)))))))
  ; (join xs sep): the strings of `xs` with `sep` between them.
  (define join (lambda (xs sep) (join-range xs sep 0 (length xs))))

  (define digits "0123456789")
  (define digit (lambda (d) (substring digits d (+ d 1))))   ; 0..9 -> "0".."9"
  (define mod (lambda (n m) (i32.rem_s n m)))

  ; Decimal digits of n <= 0. Working on the negative side covers -2147483648,
  ; whose positive value does not fit an s32.
  (define neg-digits (lambda (n acc)
    (if (= n 0) acc
      (neg-digits (/ n 10) (string-append (digit (- 0 (i32.rem_s n 10))) acc)))))
  ; s32 -> decimal string.
  (define number->string (lambda (n)
    (if (= n 0) "0"
      (if (< n 0)
        (string-append "-" (neg-digits n ""))
        (neg-digits (- 0 n) "")))))

  (define u64-digits (lambda (n acc)
    (if (= n 0u64) acc
      (let (q (/ n 10u64))
        (u64-digits q (string-append (digit (s32 (- n (* q 10u64)))) acc))))))
  ; u64 -> decimal string, without the u64 suffix the printer adds. Separate from
  ; number->string because Wisp code cannot ask a number its type.
  (define u64->string (lambda (n)
    (if (= n 0u64) "0" (u64-digits n ""))))

  ; parse a non-negative decimal string -> s32 (stops making sense on non-digits).
  (define parse-from (lambda (s i acc)
    (if (< i (string-len s))
      (parse-from s (+ i 1) (+ (* acc 10) (- (string-ref s i) 48)))  ; '0' = 48
      acc)))
  (define string->number (lambda (s) (parse-from s 0 0))))
