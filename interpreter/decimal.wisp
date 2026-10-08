; Unsigned base-10^9 integers for exact decimal/binary conversion. These helpers
; are private to the reader/printer; interpreted programs cannot mutate them.
(fn big-size-at ((n (list s32)) (i s32)) s32
  (if (i32.lt_s i 0) 0
    (if (list-get n i) (i32.add i 1) (big-size-at n (i32.sub i 1)))))
(fn big-size ((n (list s32))) s32 (big-size-at n (i32.sub (list-len n) 1)))
(fn big-digit ((n (list s32)) (i s32)) s32
  (if (i32.lt_s i (list-len n)) (list-get n i) 0))
(fn big-compare-at ((a (list s32)) (b (list s32)) (i s32)) s32
  (if (i32.lt_s i 0) 0
    (let (x (list-get a i)) (let (y (list-get b i))
      (if (i32.eq x y) (big-compare-at a b (i32.sub i 1))
        (if (i32.lt_s x y) -1 1))))))
(fn big-compare ((a (list s32)) (b (list s32))) s32
  (let (x (big-size a)) (let (y (big-size b))
    (if (i32.eq x y) (big-compare-at a b (i32.sub x 1))
      (if (i32.lt_s x y) -1 1)))))
(fn big-scale-at ((n (list s32)) (factor s32) (carry s32) (i s32) (size s32) (out (list s32))) (list s32)
  (if (i32.ge_s i size) (if carry (list-push out carry) out)
    (let (v (i64.add (i64.mul (i64.extend_i32_s (list-get n i)) (i64.extend_i32_s factor)) (i64.extend_i32_s carry)))
      (big-scale-at n factor (i32.wrap_i64 (i64.div_u v 1000000000)) (i32.add i 1) size
        (list-push out (i32.wrap_i64 (i64.rem_u v 1000000000)))))))
(fn big-scale ((n (list s32)) (factor s32) (carry s32)) (list s32)
  (big-scale-at n factor carry 0 (big-size n) (list-new s32)))
(fn big-power ((n (list s32)) (factor s32) (count s32)) (list s32)
  (if (i32.le_s count 0) n (big-power (big-scale n factor 0) factor (i32.sub count 1))))
(fn big-sub-at ((a (list s32)) (b (list s32)) (i s32) (size s32) (borrow s32) (out (list s32))) (list s32)
  (if (i32.ge_s i size) out
    (let (v (i32.sub (i32.sub (list-get a i) (big-digit b i)) borrow))
      (big-sub-at a b (i32.add i 1) size (i32.lt_s v 0)
        (list-push out (if (i32.lt_s v 0) (i32.add v 1000000000) v))))))
(fn big-sub ((a (list s32)) (b (list s32))) (list s32)
  (big-sub-at a b 0 (big-size a) 0 (list-new s32)))
(fn big-read ((s string) (pos s32) (n (list s32))) (list s32)
  (if (i32.ge_s pos (string-len s)) n
    (big-read s (i32.add pos 1) (big-scale n 10 (i32.sub (string-ref s pos) 48)))))
(fn big-from-wide ((n s64)) (list s32)
  (if (i64.eq n 0) (list-new s32)
    (if (i64.lt_u n 1000000000) (list-push (list-new s32) (i32.wrap_i64 n))
      (list-push (list-push (list-new s32) (i32.wrap_i64 (i64.rem_u n 1000000000)))
        (i32.wrap_i64 (i64.div_u n 1000000000))))))

; Read syntax completely before converting. Exponents saturate safely outside
; the range relevant to a <=4096-byte source, while malformed tails still fail.
(record decimal-parts (digits string) (exponent s32) (valid s32))
(fn decimal-exponent ((s string) (pos s32) (n s32) (negative s32)) s32
  (if (i32.ge_s pos (string-len s)) (if negative (i32.sub 0 n) n)
    (let (c (string-ref s pos))
      (if (digit? c)
        (decimal-exponent s (i32.add pos 1)
          (if (i32.ge_s n 10000) 10000 (i32.add (i32.mul n 10) (i32.sub c 48))) negative)
        200000))))
(fn decimal-exp-start ((s string) (pos s32)) s32
  (if (i32.ge_s pos (string-len s)) 200000
    (let (c (string-ref s pos))
      (let (start (if (i32.or (i32.eq c 43) (i32.eq c 45)) (i32.add pos 1) pos))
        (if (i32.ge_s start (string-len s)) 200000
          (decimal-exponent s start 0 (i32.eq c 45)))))))
(fn decimal-scan ((s string) (pos s32) (digits string) (fraction s32) (dot s32)) decimal-parts
  (if (i32.ge_s pos (string-len s))
    (decimal-parts digits (i32.sub 0 fraction) (i32.gt_s (string-len digits) 0))
    (let (c (string-ref s pos))
      (if (digit? c)
        (decimal-scan s (i32.add pos 1) (string-append digits (substring s pos (i32.add pos 1))) (i32.add fraction dot) dot)
        (if (i32.and (i32.eq c 46) (i32.eq dot 0))
          (decimal-scan s (i32.add pos 1) digits fraction 1)
          (if (i32.or (i32.eq c 101) (i32.eq c 69))
            (let (exp (decimal-exp-start s (i32.add pos 1)))
              (decimal-parts digits (i32.sub exp fraction)
                (i32.and (i32.ne exp 200000) (i32.gt_s (string-len digits) 0))))
            (decimal-parts "" 0 0)))))))
(fn decimal-first ((s string) (pos s32)) s32
  (if (i32.ge_s pos (string-len s)) pos
    (if (i32.eq (string-ref s pos) 48) (decimal-first s (i32.add pos 1)) pos)))

; Normalize the exact rational to [1,2), collect at most 53 bits, then round
; once to nearest, ties to even. Subnormals use the fixed 2^-1074 quantum.
(fn float-scale ((n f64) (exponent s32)) f64
  (if (i32.eq exponent 0) n
    (if (i32.gt_s exponent 0) (float-scale (f64.mul n 2.0) (i32.sub exponent 1))
      (float-scale (f64.mul n 0.5) (i32.add exponent 1)))))
(fn rational-bits ((n (list s32)) (d (list s32)) (bits s32) (acc s64) (exponent s32)) f64
  (if (i32.eq bits 0)
    (let (cmp (big-compare (big-scale n 2 0) d))
      (let (round-up (i32.or (i32.gt_s cmp 0)
                      (i32.and (i32.eq cmp 0) (i64.ne (i64.and acc 1) 0))))
        (float-scale (f64.convert_i64_s (i64.add acc (i64.extend_i32_s round-up))) exponent)))
    (let (bit (i32.ge_s (big-compare n d) 0))
      (let (rest (if bit (big-sub n d) n))
        (rational-bits (if (i32.eq bits 1) rest (big-scale rest 2 0)) d (i32.sub bits 1)
          (i64.add (i64.mul acc 2) (i64.extend_i32_s bit)) exponent)))))
(fn rational-ready ((n (list s32)) (d (list s32)) (exponent s32)) f64
  (if (i32.gt_s exponent 1023) (f64.div 1.0 0.0)
    (if (i32.lt_s exponent -1075) 0.0
      (if (i32.eq exponent -1075)
        (if (i32.eq (big-compare n d) 0) 0.0 (float-scale 1.0 -1074))
        (let (quantum (if (i32.lt_s exponent -1022) -1074 (i32.sub exponent 52)))
          (rational-bits n d (i32.add (i32.sub exponent quantum) 1) 0s64 quantum))))))
(fn rational-normalize ((n (list s32)) (d (list s32)) (exponent s32)) f64
  (if (i32.lt_s (big-compare n d) 0)
    (rational-normalize (big-scale n 2 0) d (i32.sub exponent 1))
    (let (twice (big-scale d 2 0))
      (if (i32.ge_s (big-compare n twice) 0)
        (rational-normalize n twice (i32.add exponent 1))
        (rational-ready n d exponent)))))
(fn decimal-convert ((s string) (exponent s32)) f64
  (let (first (decimal-first s 0))
    (let (order (i32.sub (i32.add (i32.sub (string-len s) first) exponent) 1))
      (if (i32.eq first (string-len s)) 0.0
        (if (i32.gt_s order 308) (f64.div 1.0 0.0)
          (if (i32.lt_s order -324) 0.0
            (let (n (big-read s first (list-new s32)))
              (let (one (list-push (list-new s32) 1))
                (if (i32.ge_s exponent 0)
                  (rational-normalize (big-power n 10 exponent) one 0)
                  (rational-normalize n (big-power one 10 (i32.sub 0 exponent)) 0))))))))))

; Printing uses the exact decimal expansion, rounded to 17 (f64) or 9 (f32)
; significant digits. This is round-trippable, though not always shortest.
(fn decimal-zeroes ((count s32)) string
  (if (i32.le_s count 0) "" (string-append "0" (decimal-zeroes (i32.sub count 1)))))
(fn big-show-at ((n (list s32)) (i s32) (out string)) string
  (if (i32.lt_s i 0) out
    (let (part (show-integer (list-get n i)))
      (big-show-at n (i32.sub i 1)
        (string-append out (string-append (decimal-zeroes (i32.sub 9 (string-len part))) part))))))
(fn big-show ((n (list s32))) string
  (let (last (i32.sub (big-size n) 1))
    (if (i32.lt_s last 0) "0" (big-show-at n (i32.sub last 1) (show-integer (list-get n last))))))
(fn decimal-increment ((s string) (pos s32)) string
  (if (i32.lt_s pos 0) (string-append "1" (decimal-zeroes (string-len s)))
    (let (digit (i32.sub (string-ref s pos) 48))
      (if (i32.eq digit 9)
        (string-append (decimal-increment (substring s 0 pos) (i32.sub pos 1)) "0")
        (string-append (substring s 0 pos) (substring "0123456789" (i32.add digit 1) (i32.add digit 2)))))))
(fn decimal-trim ((s string) (end s32)) string
  (if (i32.le_s end 1) (substring s 0 end)
    (if (i32.eq (string-ref s (i32.sub end 1)) 48) (decimal-trim s (i32.sub end 1)) (substring s 0 end))))
(fn decimal-format ((digits string) (order s32)) string
  (let (n (string-len digits))
    (if (i32.or (i32.lt_s order -6) (i32.ge_s order 17))
      (string-append (substring digits 0 1)
        (string-append (if (i32.gt_s n 1) (string-append "." (substring digits 1 n)) "")
          (string-append "e" (show-integer order))))
      (if (i32.lt_s order 0)
        (string-append "0." (string-append (decimal-zeroes (i32.sub -1 order)) digits))
        (let (point (i32.add order 1))
          (if (i32.ge_s point n) (string-append digits (decimal-zeroes (i32.sub point n)))
            (string-append (substring digits 0 point) (string-append "." (substring digits point n)))))))))
(fn decimal-rounded ((digits string) (exponent s32) (precision s32)) string
  (let (size (string-len digits))
    (let (order (i32.sub (i32.add size exponent) 1))
      (let (rounded (if (i32.le_s size precision) digits
        (let (prefix (substring digits 0 precision))
          (if (i32.ge_s (string-ref digits precision) 53) (decimal-increment prefix (i32.sub precision 1)) prefix))))
        (decimal-format (decimal-trim rounded (string-len rounded))
          (if (i32.and (i32.gt_s size precision) (i32.gt_s (string-len rounded) precision)) (i32.add order 1) order))))))
(fn float-decimal ((n f64) (exponent s32) (precision s32)) string
  (if (f64.lt n 1.0) (float-decimal (f64.mul n 2.0) (i32.sub exponent 1) precision)
    (if (f64.ge n 2.0) (float-decimal (f64.mul n 0.5) (i32.add exponent 1) precision)
      (let (mantissa (big-from-wide (i64.trunc_f64_s (f64.mul n 4503599627370496.0))))
        (let (power (i32.sub exponent 52))
          (if (i32.ge_s power 0) (decimal-rounded (big-show (big-power mantissa 2 power)) 0 precision)
            (decimal-rounded (big-show (big-power mantissa 5 (i32.sub 0 power))) power precision)))))))
(fn show-float ((n f64) (precision s32) (suffix string)) string
  (string-append
    (if (f64.ne n n) "nan"
      (if (f64.eq n 0.0) (if (f64.lt (f64.div 1.0 n) 0.0) "-0" "0")
        (let (negative (f64.lt n 0.0))
          (let (magnitude (if negative (f64.sub 0.0 n) n))
            (string-append (if negative "-" "")
              (if (f64.eq magnitude (f64.div 1.0 0.0)) "inf" (float-decimal magnitude 0 precision))))))) suffix))
