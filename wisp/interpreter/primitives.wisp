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
  (i32.or (i32.or (string=? name "string-len") (i32.or (string=? name "string-ref") (string=? name "string-index")))
    (i32.or (string=? name "string-append")
      (i32.or (string=? name "string=?")
        (i32.or (string=? name "substring")
          (i32.or (string=? name "bytes->string") (string=? name "string->bytes")))))))

; A byte element carries its code as either a dedicated byte-value or a plain
; integer (host arrays unmarshal to byte-value; REPL-built lists may hold integer).
(fn byte-code ((v value)) s32
  (value-case v
    ((byte-value n) n)
    ((integer n) n)
    (else 0)))

; A string is a [len:u32][bytes] buffer, so decoding bytes is: build that buffer
; in a raw block, writing each code as one byte (the (u8 ...) cast masks to a
; byte), copy it into a string with string-from-addr, and free the raw block.
(fn pack-bytes ((items (list value)) (i s32) (n s32) (buf s32)) s32
  (if (i32.ge_s i n) buf
    (begin
      (i32.store8 (i32.add (i32.add buf (i32.const 4)) i)
                  (u8 (byte-code (list-get items i))))
      (pack-bytes items (i32.add i (i32.const 1)) n buf))))

(fn byte-values->string ((items (list value))) value
  (let (n (list-len items))
    (let (buf (heap-alloc (i32.add n (i32.const 4))))
      (begin
        (i32.store buf n)
        (pack-bytes items (i32.const 0) n buf)
        (let (s (string-from-addr buf))
          (begin (heap-free buf) (text s)))))))

; bytes->string: decode a byte list (typed-list or sequence of byte-values) to a
; string. The inverse of string->bytes; together they bridge the host byte boundary.
(fn apply-bytes-to-string ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "bytes->string expects one argument")
    (value-case (list-get args 0)
      ((typed-list element items) (byte-values->string items))
      ((sequence items) (byte-values->string items))
      (else (failure "bytes->string expects a byte list")))))

; string->bytes: encode a string to a (list u8) of byte-values (str-to-bytes is
; the shared encoder the host-call marshalling already uses).
(fn apply-string-to-bytes ((args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "string->bytes expects one argument")
    (value-case (list-get args 0)
      ((text s) (str-to-bytes (list-get args 0)))
      (else (failure "string->bytes expects a string")))))

; string-append joins any number of strings; with none it gives "".
(fn append-strings ((args (list value)) (index s32) (out string)) value
  (if (i32.ge_s index (list-len args)) (text out)
    (value-case (list-get args index)
      ((text s) (append-strings args (i32.add index 1) (string-append out s)))
      (else (failure "string-append expects string arguments")))))

; string-index is one evaluation step at any length, so its time must stay linear
; like the other string primitives: a string needle uses Knuth-Morris-Pratt, because
; a plain scan is quadratic on input like "aaaa...". The table holds, for each needle
; prefix, the length of its longest proper prefix that is also a suffix.
(fn scan-byte ((s string) (b s32) (i s32)) s32
  (if (i32.ge_s i (string-len s)) -1
    (if (i32.eq (string-ref s i) b) i
      (scan-byte s b (i32.add i 1)))))

(fn kmp-entry ((table s32) (i s32)) s32
  (i32.load (i32.add table (i32.mul i 4))))

; The matched length after byte C, given K bytes of the needle matched so far.
(fn kmp-step ((needle string) (table s32) (k s32) (c s32)) s32
  (if (i32.eq (string-ref needle k) c) (i32.add k 1)
    (if (i32.eq k 0) 0
      (kmp-step needle table (kmp-entry table (i32.sub k 1)) c))))

(fn kmp-fill ((needle string) (table s32) (i s32)) s32
  (if (i32.ge_s i (string-len needle)) table
    (begin
      (i32.store (i32.add table (i32.mul i 4))
        (kmp-step needle table (kmp-entry table (i32.sub i 1)) (string-ref needle i)))
      (kmp-fill needle table (i32.add i 1)))))

(fn kmp-scan ((s string) (needle string) (table s32) (k s32) (i s32)) s32
  (if (i32.eq k (string-len needle)) (i32.sub i k)
    (if (i32.ge_s i (string-len s)) -1
      (kmp-scan s needle table (kmp-step needle table k (string-ref s i)) (i32.add i 1)))))

(fn scan-string ((s string) (needle string) (from s32)) s32
  (if (i32.eq (string-len needle) 0) (if (i32.le_s from (string-len s)) from -1)
    (if (i32.eq (string-len needle) 1) (scan-byte s (string-ref needle 0) from)
      (if (i32.gt_s from (i32.sub (string-len s) (string-len needle))) -1
        (let (table (heap-alloc (i32.mul (string-len needle) 4)))
          (begin
            (i32.store table 0)
            (kmp-fill needle table 1)
            (let (found (kmp-scan s needle table 0 from))
              (begin (heap-free table) found))))))))

(fn string-index-from ((s string) (needle value) (from s32)) value
  (value-case needle
    ((text t) (integer (scan-string s t from)))
    ((integer b) (integer (scan-byte s b from)))
    ((byte-value b) (integer (scan-byte s b from)))
    (else (failure "string-index expects a byte or a string to find"))))

; (string-index s byte-or-string [from]): the position of the first match at or
; after FROM, or -1. A FROM past the end finds nothing.
(fn apply-string-index ((args (list value))) value
  (if (i32.or (i32.lt_s (list-len args) 2) (i32.gt_s (list-len args) 3))
    (failure "string-index expects (string-index s byte-or-string [from])")
    (value-case (list-get args 0)
      ((text s)
        (if (i32.eq (list-len args) 2) (string-index-from s (list-get args 1) 0)
          (value-case (list-get args 2)
            ((integer from)
              (if (i32.lt_s from 0) (failure "string-index expects a start of 0 or more")
                (string-index-from s (list-get args 1) from)))
            (else (failure "string-index expects an integer start")))))
      (else (failure "string-index expects a string to search")))))

(fn apply-string ((name string) (args (list value))) value
  (if (string=? name "string-index") (apply-string-index args)
  (if (string=? name "bytes->string") (apply-bytes-to-string args)
    (if (string=? name "string->bytes") (apply-string-to-bytes args)
    (if (string=? name "string-append") (append-strings args 0 "")
  (let (arity (if (string=? name "string-len") 1 (if (string=? name "substring") 3 2)))
    (if (i32.ne (list-len args) arity) (failure "wrong number of arguments")
      (value-case (list-get args 0)
        ((text s)
          (if (string=? name "string-len") (integer (string-len s))
            (if (string=? name "string=?")
              (value-case (list-get args 1)
                ((text t) (integer (string=? s t)))
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
        (else (failure "expected string argument"))))))))))
