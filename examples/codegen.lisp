; Wisp Code Generator
; Compiles parsed S-expressions to WASM bytecode
;
; This is the final piece of the self-hosting compiler:
; Source -> Tokenizer -> Parser -> AST -> CodeGen -> WASM

;; ============================================================
;; Memory Layout
;; ============================================================
;; 0-255:      Source input string
;; 256-1023:   Token array (from tokenizer)
;; 1024-2047:  AST (from parser)
;; 2048-4095:  Generated WASM code
;; 4096+:      String table for symbol comparison

;; ============================================================
;; Primitives
;; ============================================================

(fn write-byte ((offset s32) (byte s32)) s32
  (let (_ (i32.store8 offset byte))
    (i32.add offset 1)))

(fn write-uleb128 ((offset s32) (value s32)) s32
  (if (i32.lt_u value 128)
    (write-byte offset value)
    (let (byte (i32.or (i32.and value 127) 128))
      (let (offset (write-byte offset byte))
        (write-uleb128 offset (i32.shr_u value 7))))))

(fn write-sleb128 ((offset s32) (value s32)) s32
  (let (byte (i32.and value 127))
    (let (value (i32.shr_s value 7))
      (if (i32.or
            (i32.and (i32.eq value 0) (i32.eq (i32.and byte 64) 0))
            (i32.and (i32.eq value -1) (i32.ne (i32.and byte 64) 0)))
        (write-byte offset byte)
        (let (offset (write-byte offset (i32.or byte 128)))
          (write-sleb128 offset value))))))

;; ============================================================
;; WASM Opcodes
;; ============================================================

(fn op-end () s32 11)
(fn op-call () s32 16)
(fn op-local-get () s32 32)
(fn op-local-set () s32 33)
(fn op-i32-const () s32 65)
(fn op-i32-eqz () s32 69)
(fn op-i32-eq () s32 70)
(fn op-i32-ne () s32 71)
(fn op-i32-lt-s () s32 72)
(fn op-i32-lt-u () s32 73)
(fn op-i32-gt-s () s32 74)
(fn op-i32-gt-u () s32 75)
(fn op-i32-le-s () s32 76)
(fn op-i32-le-u () s32 77)
(fn op-i32-ge-s () s32 78)
(fn op-i32-ge-u () s32 79)
(fn op-i32-add () s32 106)
(fn op-i32-sub () s32 107)
(fn op-i32-mul () s32 108)
(fn op-i32-div-s () s32 109)
(fn op-i32-div-u () s32 110)
(fn op-i32-rem-s () s32 111)
(fn op-i32-rem-u () s32 112)
(fn op-i32-and () s32 113)
(fn op-i32-or () s32 114)
(fn op-i32-xor () s32 115)
(fn op-i32-shl () s32 116)
(fn op-i32-shr-s () s32 117)
(fn op-i32-shr-u () s32 118)

;; ============================================================
;; String Comparison
;; ============================================================

; Compare two strings in memory
; Returns 1 if equal, 0 if not
(fn str-eq ((ptr1 s32) (len1 s32) (ptr2 s32) (len2 s32)) s32
  (if (i32.ne len1 len2)
    0  ; Different lengths
    (str-eq-chars ptr1 ptr2 len1)))

(fn str-eq-chars ((ptr1 s32) (ptr2 s32) (len s32)) s32
  (if (i32.eq len 0)
    1  ; All chars matched
    (if (i32.ne (i32.load8_u ptr1) (i32.load8_u ptr2))
      0  ; Mismatch
      (str-eq-chars (i32.add ptr1 1) (i32.add ptr2 1) (i32.sub len 1)))))

; Parse integer from string
; Assumes valid decimal integer
(fn parse-int ((ptr s32) (len s32)) s32
  (parse-int-acc ptr len 0))

(fn parse-int-acc ((ptr s32) (len s32) (acc s32)) s32
  (if (i32.eq len 0)
    acc
    (let (digit (i32.sub (i32.load8_u ptr) 48))  ; '0' = 48
      (parse-int-acc
        (i32.add ptr 1)
        (i32.sub len 1)
        (i32.add (i32.mul acc 10) digit)))))

;; ============================================================
;; Known instruction strings at fixed locations
;; ============================================================
;; We store known instruction names at offset 4096

; Helper to write string to memory
(fn write-str-7 ((ptr s32) (c0 s32) (c1 s32) (c2 s32) (c3 s32) (c4 s32) (c5 s32) (c6 s32)) s32
  (let (_ (i32.store8 ptr c0))
    (let (_ (i32.store8 (i32.add ptr 1) c1))
      (let (_ (i32.store8 (i32.add ptr 2) c2))
        (let (_ (i32.store8 (i32.add ptr 3) c3))
          (let (_ (i32.store8 (i32.add ptr 4) c4))
            (let (_ (i32.store8 (i32.add ptr 5) c5))
              (let (_ (i32.store8 (i32.add ptr 6) c6))
                7))))))))

(fn setup-strings () s32
  ; "i32.add" at 4096
  (let (_ (write-str-7 4096 105 51 50 46 97 100 100))
    ; "i32.sub" at 4103
    (let (_ (write-str-7 4103 105 51 50 46 115 117 98))
      ; "i32.mul" at 4110
      (let (_ (write-str-7 4110 105 51 50 46 109 117 108))
        0))))

; String locations
(fn str-i32-add-ptr () s32 4096)
(fn str-i32-add-len () s32 7)
(fn str-i32-sub-ptr () s32 4103)
(fn str-i32-sub-len () s32 7)
(fn str-i32-mul-ptr () s32 4110)
(fn str-i32-mul-len () s32 7)

;; ============================================================
;; Tokenizer (simplified from tokenizer.lisp)
;; ============================================================

(global $tok-input-ptr s32 mut 0)
(global $tok-input-len s32 mut 0)
(global $tok-pos s32 mut 0)
(global $tok-output-ptr s32 mut 0)
(global $tok-output-pos s32 mut 0)

(fn is-whitespace ((c s32)) s32
  (i32.or (i32.or (i32.eq c 32) (i32.eq c 9))
          (i32.or (i32.eq c 10) (i32.eq c 13))))

(fn is-digit ((c s32)) s32
  (i32.and (i32.ge_s c 48) (i32.le_s c 57)))

(fn is-letter ((c s32)) s32
  (i32.or (i32.and (i32.ge_s c 65) (i32.le_s c 90))
          (i32.and (i32.ge_s c 97) (i32.le_s c 122))))

(fn is-symbol-char ((c s32)) s32
  (i32.or (i32.or (is-letter c) (is-digit c))
          (i32.or (i32.or (i32.eq c 45) (i32.eq c 95))
                  (i32.or (i32.eq c 46) (i32.eq c 36)))))

(fn tok-init ((ptr s32) (len s32)) s32
  (let (_ (global.set $tok-input-ptr ptr))
    (let (_ (global.set $tok-input-len len))
      (let (_ (global.set $tok-pos 0))
        0))))

(fn tok-output-init ((ptr s32)) s32
  (let (_ (global.set $tok-output-ptr ptr))
    (let (_ (global.set $tok-output-pos 0))
      0)))

(fn tok-at-end () s32
  (i32.ge_s (global.get $tok-pos) (global.get $tok-input-len)))

(fn tok-peek () s32
  (if (tok-at-end) -1
    (i32.load8_u (i32.add (global.get $tok-input-ptr) (global.get $tok-pos)))))

(fn tok-advance ((n s32)) s32
  (let (_ (global.set $tok-pos (i32.add (global.get $tok-pos) n)))
    (global.get $tok-pos)))

(fn tok-emit ((type s32) (start s32) (len s32)) s32
  (let (out (i32.add (global.get $tok-output-ptr) (global.get $tok-output-pos)))
    (let (_ (i32.store8 out type))
      (let (_ (i32.store (i32.add out 1) start))
        (let (_ (i32.store (i32.add out 5) len))
          (let (_ (global.set $tok-output-pos (i32.add (global.get $tok-output-pos) 9)))
            (global.get $tok-output-pos)))))))

(fn tok-count () s32
  (i32.div_s (global.get $tok-output-pos) 9))

(fn tok-skip-ws () s32
  (if (tok-at-end) 0
    (if (is-whitespace (tok-peek))
      (let (_ (tok-advance 1)) (tok-skip-ws))
      0)))

(fn tok-scan-num () s32
  (let (start (global.get $tok-pos))
    (let (_ (tok-scan-num-loop))
      (i32.sub (global.get $tok-pos) start))))

(fn tok-scan-num-loop () s32
  (if (is-digit (tok-peek))
    (let (_ (tok-advance 1)) (tok-scan-num-loop))
    0))

(fn tok-scan-sym () s32
  (let (start (global.get $tok-pos))
    (let (_ (tok-scan-sym-loop))
      (i32.sub (global.get $tok-pos) start))))

(fn tok-scan-sym-loop () s32
  (if (is-symbol-char (tok-peek))
    (let (_ (tok-advance 1)) (tok-scan-sym-loop))
    0))

(fn tok-next () s32
  (let (_ (tok-skip-ws))
    (if (tok-at-end) 6
      (let (start (global.get $tok-pos))
        (let (c (tok-peek))
          (if (i32.eq c 40)
            (let (_ (tok-advance 1)) (let (_ (tok-emit 0 start 1)) 0))
            (if (i32.eq c 41)
              (let (_ (tok-advance 1)) (let (_ (tok-emit 1 start 1)) 1))
              (if (is-digit c)
                (let (len (tok-scan-num)) (let (_ (tok-emit 3 start len)) 3))
                (if (is-symbol-char c)
                  (let (len (tok-scan-sym)) (let (_ (tok-emit 2 start len)) 2))
                  7)))))))))

(fn tok-all () s32
  (let (type (tok-next))
    (if (i32.eq type 6) (tok-count)
      (if (i32.eq type 7) -1
        (tok-all)))))

;; ============================================================
;; Parser (simplified from parser.lisp)
;; ============================================================

(global $parse-tok-ptr s32 mut 0)
(global $parse-tok-idx s32 mut 0)
(global $parse-tok-count s32 mut 0)
(global $parse-ast-ptr s32 mut 0)
(global $parse-ast-pos s32 mut 0)

(fn parse-get-tok-type ((idx s32)) s32
  (i32.load8_u (i32.add (global.get $parse-tok-ptr) (i32.mul idx 9))))

(fn parse-get-tok-start ((idx s32)) s32
  (i32.load (i32.add (i32.add (global.get $parse-tok-ptr) 1) (i32.mul idx 9))))

(fn parse-get-tok-len ((idx s32)) s32
  (i32.load (i32.add (i32.add (global.get $parse-tok-ptr) 5) (i32.mul idx 9))))

(fn parse-cur-type () s32
  (if (i32.ge_s (global.get $parse-tok-idx) (global.get $parse-tok-count))
    6
    (parse-get-tok-type (global.get $parse-tok-idx))))

(fn parse-advance () s32
  (let (_ (global.set $parse-tok-idx (i32.add (global.get $parse-tok-idx) 1)))
    (global.get $parse-tok-idx)))

(fn parse-write-byte ((b s32)) s32
  (let (_ (i32.store8 (i32.add (global.get $parse-ast-ptr) (global.get $parse-ast-pos)) b))
    (let (_ (global.set $parse-ast-pos (i32.add (global.get $parse-ast-pos) 1)))
      (global.get $parse-ast-pos))))

(fn parse-write-i32 ((v s32)) s32
  (let (_ (i32.store (i32.add (global.get $parse-ast-ptr) (global.get $parse-ast-pos)) v))
    (let (_ (global.set $parse-ast-pos (i32.add (global.get $parse-ast-pos) 4)))
      (global.get $parse-ast-pos))))

(fn parse-init ((tok-ptr s32) (tok-count s32) (ast-ptr s32)) s32
  (let (_ (global.set $parse-tok-ptr tok-ptr))
    (let (_ (global.set $parse-tok-count tok-count))
      (let (_ (global.set $parse-tok-idx 0))
        (let (_ (global.set $parse-ast-ptr ast-ptr))
          (let (_ (global.set $parse-ast-pos 0))
            0))))))

(fn parse-expr () s32
  (let (tt (parse-cur-type))
    (if (i32.eq tt 0)  ; LPAREN
      (parse-list)
      (if (i32.or (i32.eq tt 2) (i32.eq tt 3))  ; SYMBOL or NUMBER
        (parse-atom)
        0))))

(fn parse-atom () s32
  (let (idx (global.get $parse-tok-idx))
    (let (_ (parse-advance))
      (let (_ (parse-write-byte 0))  ; ATOM type
        (let (_ (parse-write-i32 idx))
          1)))))

(fn parse-list () s32
  (let (_ (parse-advance))  ; skip (
    (let (_ (parse-write-byte 1))  ; LIST type
      (let (count-pos (global.get $parse-ast-pos))
        (let (_ (parse-write-i32 0))  ; placeholder count
          (let (count (parse-list-items 0))
            (let (_ (i32.store (i32.add (global.get $parse-ast-ptr) count-pos) count))
              (let (_ (parse-write-byte 2))  ; END
                1))))))))

(fn parse-list-items ((count s32)) s32
  (let (tt (parse-cur-type))
    (if (i32.eq tt 1)  ; RPAREN
      (let (_ (parse-advance)) count)
      (if (i32.eq tt 6)  ; EOF
        count
        (let (_ (parse-expr))
          (parse-list-items (i32.add count 1)))))))

(fn parse-all () s32
  (let (_ (parse-expr))
    (global.get $parse-ast-pos)))

;; ============================================================
;; Code Generator
;; ============================================================

(global $cg-ast-ptr s32 mut 0)
(global $cg-ast-pos s32 mut 0)
(global $cg-out-ptr s32 mut 0)
(global $cg-out-pos s32 mut 0)
(global $cg-src-ptr s32 mut 0)  ; Source string for symbol lookup

; Initialize code generator
(fn cg-init ((ast-ptr s32) (out-ptr s32) (src-ptr s32)) s32
  (let (_ (global.set $cg-ast-ptr ast-ptr))
    (let (_ (global.set $cg-ast-pos 0))
      (let (_ (global.set $cg-out-ptr out-ptr))
        (let (_ (global.set $cg-out-pos 0))
          (let (_ (global.set $cg-src-ptr src-ptr))
            0))))))

; Read byte from AST
(fn cg-read-byte () s32
  (let (b (i32.load8_u (i32.add (global.get $cg-ast-ptr) (global.get $cg-ast-pos))))
    (let (_ (global.set $cg-ast-pos (i32.add (global.get $cg-ast-pos) 1)))
      b)))

; Read i32 from AST
(fn cg-read-i32 () s32
  (let (v (i32.load (i32.add (global.get $cg-ast-ptr) (global.get $cg-ast-pos))))
    (let (_ (global.set $cg-ast-pos (i32.add (global.get $cg-ast-pos) 4)))
      v)))

; Write byte to output
(fn cg-write-byte ((b s32)) s32
  (let (_ (i32.store8 (i32.add (global.get $cg-out-ptr) (global.get $cg-out-pos)) b))
    (let (_ (global.set $cg-out-pos (i32.add (global.get $cg-out-pos) 1)))
      (global.get $cg-out-pos))))

; Write sleb128 to output
(fn cg-write-sleb128 ((value s32)) s32
  (let (byte (i32.and value 127))
    (let (value (i32.shr_s value 7))
      (if (i32.or
            (i32.and (i32.eq value 0) (i32.eq (i32.and byte 64) 0))
            (i32.and (i32.eq value -1) (i32.ne (i32.and byte 64) 0)))
        (cg-write-byte byte)
        (let (_ (cg-write-byte (i32.or byte 128)))
          (cg-write-sleb128 value))))))

; Get output size
(fn cg-size () s32
  (global.get $cg-out-pos))

;; ============================================================
;; Expression Compilation
;; ============================================================

; Compile an AST node
(fn cg-compile-node () s32
  (let (node-type (cg-read-byte))
    (if (i32.eq node-type 0)  ; ATOM
      (cg-compile-atom)
      (if (i32.eq node-type 1)  ; LIST
        (cg-compile-list)
        0))))  ; END or error

; Compile an atom (number literal)
(fn cg-compile-atom () s32
  (let (tok-idx (cg-read-i32))
    (let (tok-start (parse-get-tok-start tok-idx))
      (let (tok-len (parse-get-tok-len tok-idx))
        (let (tok-type (parse-get-tok-type tok-idx))
          (if (i32.eq tok-type 3)  ; NUMBER
            ; Emit i32.const <value>
            (let (value (parse-int (i32.add (global.get $cg-src-ptr) tok-start) tok-len))
              (let (_ (cg-write-byte (op-i32-const)))
                (cg-write-sleb128 value)))
            0))))))  ; Symbol as atom - not handled yet

; Compile a list (function call or instruction)
(fn cg-compile-list () s32
  (let (count (cg-read-i32))
    (if (i32.eq count 0)
      ; Empty list - skip END marker
      (let (_ (cg-read-byte)) 0)
      ; Non-empty list: first item should be operator/function name
      (let (first-type (cg-read-byte))
        (if (i32.ne first-type 0)
          ; First item is not an atom - error
          0
          ; Get the symbol
          (let (tok-idx (cg-read-i32))
            (let (tok-start (parse-get-tok-start tok-idx))
              (let (tok-len (parse-get-tok-len tok-idx))
                (let (sym-ptr (i32.add (global.get $cg-src-ptr) tok-start))
                  ; Compile arguments first
                  (let (_ (cg-compile-args (i32.sub count 1)))
                    ; Then emit the instruction
                    (let (_ (cg-emit-instr sym-ptr tok-len))
                      ; Skip END marker
                      (let (_ (cg-read-byte))
                        1))))))))))))

; Compile N arguments
(fn cg-compile-args ((n s32)) s32
  (if (i32.eq n 0)
    0
    (let (_ (cg-compile-node))
      (cg-compile-args (i32.sub n 1)))))

; Emit instruction based on symbol name
(fn cg-emit-instr ((sym-ptr s32) (sym-len s32)) s32
  ; Check for i32.add
  (if (str-eq sym-ptr sym-len (str-i32-add-ptr) (str-i32-add-len))
    (cg-write-byte (op-i32-add))
    ; Check for i32.sub
    (if (str-eq sym-ptr sym-len (str-i32-sub-ptr) (str-i32-sub-len))
      (cg-write-byte (op-i32-sub))
      ; Check for i32.mul
      (if (str-eq sym-ptr sym-len (str-i32-mul-ptr) (str-i32-mul-len))
        (cg-write-byte (op-i32-mul))
        ; Unknown instruction
        0))))

;; ============================================================
;; Test: Compile "(i32.add 1 2)"
;; ============================================================

(fn setup-test-input () s32
  ; Write "(i32.add 1 2)" at offset 0
  ; ( i 3 2 . a d d   1   2  )
  (let (_ (i32.store8 0 40))    ; (
    (let (_ (i32.store8 1 105))   ; i
      (let (_ (i32.store8 2 51))    ; 3
        (let (_ (i32.store8 3 50))    ; 2
          (let (_ (i32.store8 4 46))    ; .
            (let (_ (i32.store8 5 97))    ; a
              (let (_ (i32.store8 6 100))   ; d
                (let (_ (i32.store8 7 100))   ; d
                  (let (_ (i32.store8 8 32))    ; space
                    (let (_ (i32.store8 9 49))    ; 1
                      (let (_ (i32.store8 10 32))   ; space
                        (let (_ (i32.store8 11 50))   ; 2
                          (let (_ (i32.store8 12 41))   ; )
                            13))))))))))))))  ; return length

; Full compilation pipeline test
(fn test-compile () s32
  (let (_ (setup-strings))
    (let (input-len (setup-test-input))
      (let (_ (tok-init 0 input-len))
        (let (_ (tok-output-init 256))
          (let (tok-count (tok-all))
            (let (_ (parse-init 256 tok-count 1024))
              (let (_ (parse-all))
                (let (_ (cg-init 1024 2048 0))
                  (let (_ (cg-compile-node))
                    (cg-size)))))))))))

; Get generated code byte
(fn get-code-byte ((idx s32)) s32
  (let (_ (setup-strings))
    (let (input-len (setup-test-input))
      (let (_ (tok-init 0 input-len))
        (let (_ (tok-output-init 256))
          (let (tok-count (tok-all))
            (let (_ (parse-init 256 tok-count 1024))
              (let (_ (parse-all))
                (let (_ (cg-init 1024 2048 0))
                  (let (_ (cg-compile-node))
                    (i32.load8_u (i32.add 2048 idx))))))))))))

(export test-compile)
(export get-code-byte)
