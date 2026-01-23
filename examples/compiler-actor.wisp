; Wisp Compiler Actor
; A Theater actor that demonstrates the self-hosting compiler
;
; On init, it compiles "(i32.add 1 2)" and logs the resulting bytecode.

;; ============================================================
;; Compiler Pipeline (embedded from codegen.wisp)
;; ============================================================

; --- Primitives ---

(fn write-byte ((offset s32) (byte s32)) s32
  (let (_ (i32.store8 offset byte))
    (i32.add offset 1)))

(fn write-sleb128 ((offset s32) (value s32)) s32
  (let (byte (i32.and value 127))
    (let (value (i32.shr_s value 7))
      (if (i32.or
            (i32.and (i32.eq value 0) (i32.eq (i32.and byte 64) 0))
            (i32.and (i32.eq value -1) (i32.ne (i32.and byte 64) 0)))
        (write-byte offset byte)
        (let (offset (write-byte offset (i32.or byte 128)))
          (write-sleb128 offset value))))))

; --- String helpers ---

(fn str-eq ((ptr1 s32) (len1 s32) (ptr2 s32) (len2 s32)) s32
  (if (i32.ne len1 len2) 0
    (str-eq-chars ptr1 ptr2 len1)))

(fn str-eq-chars ((ptr1 s32) (ptr2 s32) (len s32)) s32
  (if (i32.eq len 0) 1
    (if (i32.ne (i32.load8_u ptr1) (i32.load8_u ptr2)) 0
      (str-eq-chars (i32.add ptr1 1) (i32.add ptr2 1) (i32.sub len 1)))))

(fn parse-int ((ptr s32) (len s32)) s32
  (parse-int-acc ptr len 0))

(fn parse-int-acc ((ptr s32) (len s32) (acc s32)) s32
  (if (i32.eq len 0) acc
    (let (digit (i32.sub (i32.load8_u ptr) 48))
      (parse-int-acc (i32.add ptr 1) (i32.sub len 1)
        (i32.add (i32.mul acc 10) digit)))))

; --- Known strings setup ---

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
  (let (_ (write-str-7 4096 105 51 50 46 97 100 100))   ; i32.add
    (let (_ (write-str-7 4103 105 51 50 46 115 117 98)) ; i32.sub
      (let (_ (write-str-7 4110 105 51 50 46 109 117 108)) ; i32.mul
        0))))

(fn str-i32-add-ptr () s32 4096)
(fn str-i32-add-len () s32 7)
(fn str-i32-sub-ptr () s32 4103)
(fn str-i32-sub-len () s32 7)
(fn str-i32-mul-ptr () s32 4110)
(fn str-i32-mul-len () s32 7)

; --- WASM Opcodes ---

(fn op-i32-const () s32 65)
(fn op-i32-add () s32 106)
(fn op-i32-sub () s32 107)
(fn op-i32-mul () s32 108)

; --- Tokenizer ---

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
      (let (_ (global.set $tok-pos 0)) 0))))

(fn tok-output-init ((ptr s32)) s32
  (let (_ (global.set $tok-output-ptr ptr))
    (let (_ (global.set $tok-output-pos 0)) 0)))

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

; --- Parser ---

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
  (if (i32.ge_s (global.get $parse-tok-idx) (global.get $parse-tok-count)) 6
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
          (let (_ (global.set $parse-ast-pos 0)) 0))))))

(fn parse-expr () s32
  (let (tt (parse-cur-type))
    (if (i32.eq tt 0) (parse-list)
      (if (i32.or (i32.eq tt 2) (i32.eq tt 3)) (parse-atom)
        0))))

(fn parse-atom () s32
  (let (idx (global.get $parse-tok-idx))
    (let (_ (parse-advance))
      (let (_ (parse-write-byte 0))
        (let (_ (parse-write-i32 idx)) 1)))))

(fn parse-list () s32
  (let (_ (parse-advance))
    (let (_ (parse-write-byte 1))
      (let (count-pos (global.get $parse-ast-pos))
        (let (_ (parse-write-i32 0))
          (let (count (parse-list-items 0))
            (let (_ (i32.store (i32.add (global.get $parse-ast-ptr) count-pos) count))
              (let (_ (parse-write-byte 2)) 1))))))))

(fn parse-list-items ((count s32)) s32
  (let (tt (parse-cur-type))
    (if (i32.eq tt 1) (let (_ (parse-advance)) count)
      (if (i32.eq tt 6) count
        (let (_ (parse-expr))
          (parse-list-items (i32.add count 1)))))))

(fn parse-all () s32
  (let (_ (parse-expr))
    (global.get $parse-ast-pos)))

; --- Code Generator ---

(global $cg-ast-ptr s32 mut 0)
(global $cg-ast-pos s32 mut 0)
(global $cg-out-ptr s32 mut 0)
(global $cg-out-pos s32 mut 0)
(global $cg-src-ptr s32 mut 0)

(fn cg-init ((ast-ptr s32) (out-ptr s32) (src-ptr s32)) s32
  (let (_ (global.set $cg-ast-ptr ast-ptr))
    (let (_ (global.set $cg-ast-pos 0))
      (let (_ (global.set $cg-out-ptr out-ptr))
        (let (_ (global.set $cg-out-pos 0))
          (let (_ (global.set $cg-src-ptr src-ptr)) 0))))))

(fn cg-read-byte () s32
  (let (b (i32.load8_u (i32.add (global.get $cg-ast-ptr) (global.get $cg-ast-pos))))
    (let (_ (global.set $cg-ast-pos (i32.add (global.get $cg-ast-pos) 1))) b)))

(fn cg-read-i32 () s32
  (let (v (i32.load (i32.add (global.get $cg-ast-ptr) (global.get $cg-ast-pos))))
    (let (_ (global.set $cg-ast-pos (i32.add (global.get $cg-ast-pos) 4))) v)))

(fn cg-write-byte ((b s32)) s32
  (let (_ (i32.store8 (i32.add (global.get $cg-out-ptr) (global.get $cg-out-pos)) b))
    (let (_ (global.set $cg-out-pos (i32.add (global.get $cg-out-pos) 1)))
      (global.get $cg-out-pos))))

(fn cg-write-sleb128 ((value s32)) s32
  (let (byte (i32.and value 127))
    (let (value (i32.shr_s value 7))
      (if (i32.or
            (i32.and (i32.eq value 0) (i32.eq (i32.and byte 64) 0))
            (i32.and (i32.eq value -1) (i32.ne (i32.and byte 64) 0)))
        (cg-write-byte byte)
        (let (_ (cg-write-byte (i32.or byte 128)))
          (cg-write-sleb128 value))))))

(fn cg-size () s32 (global.get $cg-out-pos))

(fn cg-compile-node () s32
  (let (node-type (cg-read-byte))
    (if (i32.eq node-type 0) (cg-compile-atom)
      (if (i32.eq node-type 1) (cg-compile-list)
        0))))

(fn cg-compile-atom () s32
  (let (tok-idx (cg-read-i32))
    (let (tok-start (parse-get-tok-start tok-idx))
      (let (tok-len (parse-get-tok-len tok-idx))
        (let (tok-type (parse-get-tok-type tok-idx))
          (if (i32.eq tok-type 3)
            (let (value (parse-int (i32.add (global.get $cg-src-ptr) tok-start) tok-len))
              (let (_ (cg-write-byte (op-i32-const)))
                (cg-write-sleb128 value)))
            0))))))

(fn cg-compile-list () s32
  (let (count (cg-read-i32))
    (if (i32.eq count 0)
      (let (_ (cg-read-byte)) 0)
      (let (first-type (cg-read-byte))
        (if (i32.ne first-type 0) 0
          (let (tok-idx (cg-read-i32))
            (let (tok-start (parse-get-tok-start tok-idx))
              (let (tok-len (parse-get-tok-len tok-idx))
                (let (sym-ptr (i32.add (global.get $cg-src-ptr) tok-start))
                  (let (_ (cg-compile-args (i32.sub count 1)))
                    (let (_ (cg-emit-instr sym-ptr tok-len))
                      (let (_ (cg-read-byte)) 1))))))))))))

(fn cg-compile-args ((n s32)) s32
  (if (i32.eq n 0) 0
    (let (_ (cg-compile-node))
      (cg-compile-args (i32.sub n 1)))))

(fn cg-emit-instr ((sym-ptr s32) (sym-len s32)) s32
  (if (str-eq sym-ptr sym-len (str-i32-add-ptr) (str-i32-add-len))
    (cg-write-byte (op-i32-add))
    (if (str-eq sym-ptr sym-len (str-i32-sub-ptr) (str-i32-sub-len))
      (cg-write-byte (op-i32-sub))
      (if (str-eq sym-ptr sym-len (str-i32-mul-ptr) (str-i32-mul-len))
        (cg-write-byte (op-i32-mul))
        0))))

;; ============================================================
;; Full Compilation Pipeline
;; ============================================================

; Compile a source expression and return the generated code size
; Source at offset 0, tokens at 256, AST at 1024, output at 2048
(fn compile-expr ((src-ptr s32) (src-len s32)) s32
  (let (_ (setup-strings))
    (let (_ (tok-init src-ptr src-len))
      (let (_ (tok-output-init 256))
        (let (tok-count (tok-all))
          (let (_ (parse-init 256 tok-count 1024))
            (let (_ (parse-all))
              (let (_ (cg-init 1024 2048 src-ptr))
                (let (_ (cg-compile-node))
                  (cg-size))))))))))

; Get compiled byte at index (after calling compile-expr)
(fn get-compiled-byte ((idx s32)) s32
  (i32.load8_u (i32.add 2048 idx)))

;; ============================================================
;; Theater Actor Init
;; ============================================================

; Write test source "(i32.add 1 2)" at offset 8192
(fn setup-test-source () s32
  (let (_ (i32.store8 8192 40))    ; (
    (let (_ (i32.store8 8193 105))   ; i
      (let (_ (i32.store8 8194 51))    ; 3
        (let (_ (i32.store8 8195 50))    ; 2
          (let (_ (i32.store8 8196 46))    ; .
            (let (_ (i32.store8 8197 97))    ; a
              (let (_ (i32.store8 8198 100))   ; d
                (let (_ (i32.store8 8199 100))   ; d
                  (let (_ (i32.store8 8200 32))    ; space
                    (let (_ (i32.store8 8201 49))    ; 1
                      (let (_ (i32.store8 8202 32))   ; space
                        (let (_ (i32.store8 8203 50))   ; 2
                          (let (_ (i32.store8 8204 41))   ; )
                            13))))))))))))))

; Format output bytes as hex string for logging
; For simplicity, we'll just log the size for now
(fn format-size-msg ((size s32)) s32
  ; Write "Compiled X bytes: " at offset 9000
  ; C o m p i l e d
  (let (_ (i32.store8 9000 67))   ; C
    (let (_ (i32.store8 9001 111))  ; o
      (let (_ (i32.store8 9002 109))  ; m
        (let (_ (i32.store8 9003 112))  ; p
          (let (_ (i32.store8 9004 105))  ; i
            (let (_ (i32.store8 9005 108))  ; l
              (let (_ (i32.store8 9006 101))  ; e
                (let (_ (i32.store8 9007 100))  ; d
                  (let (_ (i32.store8 9008 32))   ; space
                    ; Add the size digit (assuming single digit for now)
                    (let (_ (i32.store8 9009 (i32.add 48 size)))
                      (let (_ (i32.store8 9010 32))   ; space
                        (let (_ (i32.store8 9011 98))   ; b
                          (let (_ (i32.store8 9012 121))  ; y
                            (let (_ (i32.store8 9013 116))  ; t
                              (let (_ (i32.store8 9014 101))  ; e
                                (let (_ (i32.store8 9015 115))  ; s
                                  16)))))))))))))))))

; Simple test function that doesn't use Theater imports
(fn test-actor-compile () s32
  (let (src-len (setup-test-source))
    (compile-expr 8192 src-len)))

; Compile and return byte at index (all in one call)
(fn compile-and-get-byte ((idx s32)) s32
  (let (src-len (setup-test-source))
    (let (_ (compile-expr 8192 src-len))
      (i32.load8_u (i32.add 2048 idx)))))

(export test-actor-compile)
(export get-compiled-byte)
(export compile-and-get-byte)
