; WASM Factorial Emitter
; Emits a complete WASM module with a recursive factorial function

;; === Primitives (copied for now, will use imports later) ===

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

;; === Opcode Constants ===

(fn op-end () s32 11)
(fn op-else () s32 5)
(fn op-if () s32 4)
(fn op-call () s32 16)
(fn op-local-get () s32 32)
(fn op-i32-const () s32 65)
(fn op-i32-le-s () s32 76)
(fn op-i32-sub () s32 107)
(fn op-i32-mul () s32 108)

;; === WASM Header ===

(fn emit-header ((out s32)) s32
  (let (out (write-byte out 0))
    (let (out (write-byte out 97))
      (let (out (write-byte out 115))
        (let (out (write-byte out 109))
          (let (out (write-byte out 1))
            (let (out (write-byte out 0))
              (let (out (write-byte out 0))
                (write-byte out 0)))))))))

;; === Type Section ===
; One type: (i32) -> i32

(fn emit-type-section ((out s32)) s32
  (let (out (write-byte out 1))    ; section id
    (let (out (write-byte out 6))    ; section size
      (let (out (write-byte out 1))    ; num types
        (let (out (write-byte out 96))   ; func type
          (let (out (write-byte out 1))    ; 1 param
            (let (out (write-byte out 127))  ; i32
              (let (out (write-byte out 1))    ; 1 result
                (write-byte out 127)))))))))   ; i32

;; === Function Section ===

(fn emit-func-section ((out s32)) s32
  (let (out (write-byte out 3))    ; section id
    (let (out (write-byte out 2))    ; section size
      (let (out (write-byte out 1))    ; num functions
        (write-byte out 0)))))         ; function 0 uses type 0

;; === Export Section ===
; Export "factorial" as function 0

(fn emit-export-section ((out s32)) s32
  (let (out (write-byte out 7))    ; section id
    (let (out (write-byte out 13))   ; section size
      (let (out (write-byte out 1))    ; num exports
        ; name length: 9 ("factorial")
        (let (out (write-byte out 9))
          ; "factorial"
          (let (out (write-byte out 102)) ; f
            (let (out (write-byte out 97))  ; a
              (let (out (write-byte out 99))  ; c
                (let (out (write-byte out 116)) ; t
                  (let (out (write-byte out 111)) ; o
                    (let (out (write-byte out 114)) ; r
                      (let (out (write-byte out 105)) ; i
                        (let (out (write-byte out 97))  ; a
                          (let (out (write-byte out 108)) ; l
                            ; export kind: function
                            (let (out (write-byte out 0))
                              ; function index: 0
                              (write-byte out 0))))))))))))))))

;; === Code Section ===
; Factorial function body:
;   if (n <= 1) { 1 } else { n * factorial(n - 1) }

(fn emit-factorial-body ((out s32)) s32
  ; local.get 0  (param n)
  (let (out (write-byte out 32))
    (let (out (write-byte out 0))
      ; i32.const 1
      (let (out (write-byte out 65))
        (let (out (write-byte out 1))
          ; i32.le_s
          (let (out (write-byte out 76))
            ; if (result i32)
            (let (out (write-byte out 4))
              (let (out (write-byte out 127))  ; result type i32
                ; then: i32.const 1
                (let (out (write-byte out 65))
                  (let (out (write-byte out 1))
                    ; else
                    (let (out (write-byte out 5))
                      ; local.get 0
                      (let (out (write-byte out 32))
                        (let (out (write-byte out 0))
                          ; local.get 0
                          (let (out (write-byte out 32))
                            (let (out (write-byte out 0))
                              ; i32.const 1
                              (let (out (write-byte out 65))
                                (let (out (write-byte out 1))
                                  ; i32.sub
                                  (let (out (write-byte out 107))
                                    ; call 0 (factorial)
                                    (let (out (write-byte out 16))
                                      (let (out (write-byte out 0))
                                        ; i32.mul
                                        (let (out (write-byte out 108))
                                          ; end (if)
                                          (let (out (write-byte out 11))
                                            ; end (function)
                                            (write-byte out 11)))))))))))))))))))))))

(fn emit-code-section ((out s32)) s32
  ; Section id 10 (code)
  (let (out (write-byte out 10))
    ; Section size: 25 bytes (1 func count + 1 body size + 1 locals + 22 code)
    (let (out (write-byte out 25))
      ; Num functions: 1
      (let (out (write-byte out 1))
        ; Function body size: 23 bytes (1 local decl count + 22 instructions)
        (let (out (write-byte out 23))
          ; Local declaration count: 0
          (let (out (write-byte out 0))
            ; Function body
            (emit-factorial-body out)))))))

;; === Complete Module ===

(fn emit-factorial-module ((out s32)) s32
  (let (start out)
    (let (out (emit-header out))
      (let (out (emit-type-section out))
        (let (out (emit-func-section out))
          (let (out (emit-export-section out))
            (let (out (emit-code-section out))
              (i32.sub out start))))))))

;; === Entry Points ===

(fn emit-factorial () s32
  (emit-factorial-module 1024))

(fn get-byte ((index s32)) s32
  (let (_ (emit-factorial-module 1024))
    (i32.load8_u (i32.add 1024 index))))

(fn get-magic () s32
  (let (_ (emit-factorial-module 1024))
    (i32.load 1024)))

(export emit-factorial)
(export get-byte)
(export get-magic)
