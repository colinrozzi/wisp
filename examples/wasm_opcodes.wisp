; WASM Opcodes - Constants and Instruction Emitters
; This file provides opcode constants and emit helpers for codegen

;; === Memory Write Primitives ===

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

;; === WASM Section IDs ===
; Returned as constants for use in section emitters

(fn section-custom () s32 0)
(fn section-type () s32 1)
(fn section-import () s32 2)
(fn section-function () s32 3)
(fn section-table () s32 4)
(fn section-memory () s32 5)
(fn section-global () s32 6)
(fn section-export () s32 7)
(fn section-start () s32 8)
(fn section-element () s32 9)
(fn section-code () s32 10)
(fn section-data () s32 11)

;; === WASM Value Types ===

(fn type-i32 () s32 127)  ; 0x7F
(fn type-i64 () s32 126)  ; 0x7E
(fn type-f32 () s32 125)  ; 0x7D
(fn type-f64 () s32 124)  ; 0x7C
(fn type-funcref () s32 112)  ; 0x70
(fn type-externref () s32 111)  ; 0x6F

;; === WASM Opcodes - Control Flow ===

(fn op-unreachable () s32 0)    ; 0x00
(fn op-nop () s32 1)            ; 0x01
(fn op-block () s32 2)          ; 0x02 - blocktype
(fn op-loop () s32 3)           ; 0x03 - blocktype
(fn op-if () s32 4)             ; 0x04 - blocktype
(fn op-else () s32 5)           ; 0x05
(fn op-end () s32 11)           ; 0x0B
(fn op-br () s32 12)            ; 0x0C - labelidx
(fn op-br-if () s32 13)         ; 0x0D - labelidx
(fn op-br-table () s32 14)      ; 0x0E - vec(labelidx) labelidx
(fn op-return () s32 15)        ; 0x0F
(fn op-call () s32 16)          ; 0x10 - funcidx
(fn op-call-indirect () s32 17) ; 0x11 - typeidx tableidx

;; === WASM Opcodes - Parametric ===

(fn op-drop () s32 26)          ; 0x1A
(fn op-select () s32 27)        ; 0x1B

;; === WASM Opcodes - Variable ===

(fn op-local-get () s32 32)     ; 0x20 - localidx
(fn op-local-set () s32 33)     ; 0x21 - localidx
(fn op-local-tee () s32 34)     ; 0x22 - localidx
(fn op-global-get () s32 35)    ; 0x23 - globalidx
(fn op-global-set () s32 36)    ; 0x24 - globalidx

;; === WASM Opcodes - Memory ===

(fn op-i32-load () s32 40)      ; 0x28 - memarg
(fn op-i64-load () s32 41)      ; 0x29 - memarg
(fn op-f32-load () s32 42)      ; 0x2A - memarg
(fn op-f64-load () s32 43)      ; 0x2B - memarg
(fn op-i32-load8-s () s32 44)   ; 0x2C - memarg
(fn op-i32-load8-u () s32 45)   ; 0x2D - memarg
(fn op-i32-load16-s () s32 46)  ; 0x2E - memarg
(fn op-i32-load16-u () s32 47)  ; 0x2F - memarg

(fn op-i32-store () s32 54)     ; 0x36 - memarg
(fn op-i64-store () s32 55)     ; 0x37 - memarg
(fn op-f32-store () s32 56)     ; 0x38 - memarg
(fn op-f64-store () s32 57)     ; 0x39 - memarg
(fn op-i32-store8 () s32 58)    ; 0x3A - memarg
(fn op-i32-store16 () s32 59)   ; 0x3B - memarg

(fn op-memory-size () s32 63)   ; 0x3F - 0x00
(fn op-memory-grow () s32 64)   ; 0x40 - 0x00

;; === WASM Opcodes - Numeric Constants ===

(fn op-i32-const () s32 65)     ; 0x41 - i32 (sleb128)
(fn op-i64-const () s32 66)     ; 0x42 - i64 (sleb128)
(fn op-f32-const () s32 67)     ; 0x43 - f32 (4 bytes)
(fn op-f64-const () s32 68)     ; 0x44 - f64 (8 bytes)

;; === WASM Opcodes - i32 Comparison ===

(fn op-i32-eqz () s32 69)       ; 0x45
(fn op-i32-eq () s32 70)        ; 0x46
(fn op-i32-ne () s32 71)        ; 0x47
(fn op-i32-lt-s () s32 72)      ; 0x48
(fn op-i32-lt-u () s32 73)      ; 0x49
(fn op-i32-gt-s () s32 74)      ; 0x4A
(fn op-i32-gt-u () s32 75)      ; 0x4B
(fn op-i32-le-s () s32 76)      ; 0x4C
(fn op-i32-le-u () s32 77)      ; 0x4D
(fn op-i32-ge-s () s32 78)      ; 0x4E
(fn op-i32-ge-u () s32 79)      ; 0x4F

;; === WASM Opcodes - i64 Comparison ===

(fn op-i64-eqz () s32 80)       ; 0x50
(fn op-i64-eq () s32 81)        ; 0x51
(fn op-i64-ne () s32 82)        ; 0x52
(fn op-i64-lt-s () s32 83)      ; 0x53
(fn op-i64-lt-u () s32 84)      ; 0x54
(fn op-i64-gt-s () s32 85)      ; 0x55
(fn op-i64-gt-u () s32 86)      ; 0x56
(fn op-i64-le-s () s32 87)      ; 0x57
(fn op-i64-le-u () s32 88)      ; 0x58
(fn op-i64-ge-s () s32 89)      ; 0x59
(fn op-i64-ge-u () s32 90)      ; 0x5A

;; === WASM Opcodes - i32 Arithmetic ===

(fn op-i32-clz () s32 103)      ; 0x67
(fn op-i32-ctz () s32 104)      ; 0x68
(fn op-i32-popcnt () s32 105)   ; 0x69
(fn op-i32-add () s32 106)      ; 0x6A
(fn op-i32-sub () s32 107)      ; 0x6B
(fn op-i32-mul () s32 108)      ; 0x6C
(fn op-i32-div-s () s32 109)    ; 0x6D
(fn op-i32-div-u () s32 110)    ; 0x6E
(fn op-i32-rem-s () s32 111)    ; 0x6F
(fn op-i32-rem-u () s32 112)    ; 0x70
(fn op-i32-and () s32 113)      ; 0x71
(fn op-i32-or () s32 114)       ; 0x72
(fn op-i32-xor () s32 115)      ; 0x73
(fn op-i32-shl () s32 116)      ; 0x74
(fn op-i32-shr-s () s32 117)    ; 0x75
(fn op-i32-shr-u () s32 118)    ; 0x76
(fn op-i32-rotl () s32 119)     ; 0x77
(fn op-i32-rotr () s32 120)     ; 0x78

;; === Instruction Emitters ===

; Emit local.get with index
(fn emit-local-get ((out s32) (idx s32)) s32
  (let (out (write-byte out (op-local-get)))
    (write-uleb128 out idx)))

; Emit local.set with index
(fn emit-local-set ((out s32) (idx s32)) s32
  (let (out (write-byte out (op-local-set)))
    (write-uleb128 out idx)))

; Emit local.tee with index
(fn emit-local-tee ((out s32) (idx s32)) s32
  (let (out (write-byte out (op-local-tee)))
    (write-uleb128 out idx)))

; Emit i32.const with value
(fn emit-i32-const ((out s32) (value s32)) s32
  (let (out (write-byte out (op-i32-const)))
    (write-sleb128 out value)))

; Emit call with function index
(fn emit-call ((out s32) (funcidx s32)) s32
  (let (out (write-byte out (op-call)))
    (write-uleb128 out funcidx)))

; Emit simple opcode (no arguments)
(fn emit-op ((out s32) (opcode s32)) s32
  (write-byte out opcode))

; Emit br with label index
(fn emit-br ((out s32) (labelidx s32)) s32
  (let (out (write-byte out (op-br)))
    (write-uleb128 out labelidx)))

; Emit br_if with label index
(fn emit-br-if ((out s32) (labelidx s32)) s32
  (let (out (write-byte out (op-br-if)))
    (write-uleb128 out labelidx)))

; Emit block with void result type
(fn emit-block-void ((out s32)) s32
  (let (out (write-byte out (op-block)))
    (write-byte out 64)))  ; 0x40 = empty blocktype

; Emit loop with void result type
(fn emit-loop-void ((out s32)) s32
  (let (out (write-byte out (op-loop)))
    (write-byte out 64)))  ; 0x40 = empty blocktype

; Emit if with void result type
(fn emit-if-void ((out s32)) s32
  (let (out (write-byte out (op-if)))
    (write-byte out 64)))  ; 0x40 = empty blocktype

; Emit block with i32 result type
(fn emit-block-i32 ((out s32)) s32
  (let (out (write-byte out (op-block)))
    (write-byte out 127)))  ; 0x7F = i32

; Emit if with i32 result type
(fn emit-if-i32 ((out s32)) s32
  (let (out (write-byte out (op-if)))
    (write-byte out 127)))  ; 0x7F = i32

;; === Memory Instruction Emitters ===

; Emit i32.load with default alignment and offset 0
(fn emit-i32-load ((out s32)) s32
  (let (out (write-byte out (op-i32-load)))
    (let (out (write-uleb128 out 2))  ; align = 2 (4-byte aligned)
      (write-uleb128 out 0))))         ; offset = 0

; Emit i32.store with default alignment and offset 0
(fn emit-i32-store ((out s32)) s32
  (let (out (write-byte out (op-i32-store)))
    (let (out (write-uleb128 out 2))  ; align = 2
      (write-uleb128 out 0))))         ; offset = 0

; Emit i32.store8 with default alignment and offset 0
(fn emit-i32-store8 ((out s32)) s32
  (let (out (write-byte out (op-i32-store8)))
    (let (out (write-uleb128 out 0))  ; align = 0
      (write-uleb128 out 0))))         ; offset = 0

; Emit i32.load8_u with default alignment and offset 0
(fn emit-i32-load8-u ((out s32)) s32
  (let (out (write-byte out (op-i32-load8-u)))
    (let (out (write-uleb128 out 0))  ; align = 0
      (write-uleb128 out 0))))         ; offset = 0

;; === Test: Emit a factorial function ===
; fn factorial(n: i32) -> i32 {
;   if n <= 1 { 1 } else { n * factorial(n - 1) }
; }

; This test verifies we can emit control flow correctly
(fn test-factorial-body ((out s32)) s32
  ; local.get 0  (n)
  (let (out (emit-local-get out 0))
    ; i32.const 1
    (let (out (emit-i32-const out 1))
      ; i32.le_s
      (let (out (emit-op out (op-i32-le-s)))
        ; if (result i32)
        (let (out (emit-if-i32 out))
          ; then: i32.const 1
          (let (out (emit-i32-const out 1))
            ; else
            (let (out (emit-op out (op-else)))
              ; local.get 0
              (let (out (emit-local-get out 0))
                ; local.get 0
                (let (out (emit-local-get out 0))
                  ; i32.const 1
                  (let (out (emit-i32-const out 1))
                    ; i32.sub
                    (let (out (emit-op out (op-i32-sub)))
                      ; call 0 (factorial)
                      (let (out (emit-call out 0))
                        ; i32.mul
                        (let (out (emit-op out (op-i32-mul)))
                          ; end (if)
                          (emit-op out (op-end)))))))))))))))

; Test: get the size of the factorial body
(fn test-factorial-size () s32
  (let (start 1024)
    (i32.sub (test-factorial-body start) start)))

; Export for testing
(export test-factorial-size)
(export emit-i32-const)
(export emit-local-get)
(export emit-op)
(export emit-call)
