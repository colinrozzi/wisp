; WASM Code Generator - Foundation for self-hosting compiler
; Combines LEB128 encoding with WASM binary format emission

;; === Memory Write Primitives ===

; Write a byte to memory at offset, return next offset
(fn write-byte ((offset s32) (byte s32)) s32
  (let (_ (i32.store8 offset byte))
    (i32.add offset 1)))

; Write unsigned LEB128 encoded integer
(fn write-uleb128 ((offset s32) (value s32)) s32
  (if (i32.lt_u value 128)
    (write-byte offset value)
    (let (byte (i32.or (i32.and value 127) 128))
      (let (offset (write-byte offset byte))
        (write-uleb128 offset (i32.shr_u value 7))))))

; Write signed LEB128 encoded integer
(fn write-sleb128 ((offset s32) (value s32)) s32
  (let (byte (i32.and value 127))
    (let (value (i32.shr_s value 7))
      (if (i32.or
            (i32.and (i32.eq value 0) (i32.eq (i32.and byte 64) 0))
            (i32.and (i32.eq value -1) (i32.ne (i32.and byte 64) 0)))
        (write-byte offset byte)
        (let (offset (write-byte offset (i32.or byte 128)))
          (write-sleb128 offset value))))))

;; === WASM Constants ===
; Section IDs
; 0 = custom, 1 = type, 2 = import, 3 = function, 4 = table,
; 5 = memory, 6 = global, 7 = export, 8 = start, 9 = element, 10 = code, 11 = data

; WASM value types
; 0x7F = i32 (127), 0x7E = i64 (126), 0x7D = f32 (125), 0x7C = f64 (124)

; WASM opcodes we need for basic compilation
; 0x00 = unreachable, 0x01 = nop
; 0x02 = block, 0x03 = loop, 0x04 = if, 0x05 = else
; 0x0B = end, 0x0C = br, 0x0D = br_if, 0x0F = return
; 0x10 = call
; 0x20 = local.get, 0x21 = local.set, 0x22 = local.tee
; 0x28 = i32.load, 0x3A = i32.store8
; 0x41 = i32.const, 0x42 = i64.const, 0x43 = f32.const, 0x44 = f64.const
; 0x6A = i32.add, 0x6B = i32.sub, 0x6C = i32.mul, 0x6D = i32.div_s
; 0x71 = i32.and, 0x72 = i32.or, 0x73 = i32.xor
; 0x74 = i32.shl, 0x75 = i32.shr_s, 0x76 = i32.shr_u
; 0x46 = i32.eq, 0x47 = i32.ne
; 0x48 = i32.lt_s, 0x49 = i32.lt_u
; 0x4A = i32.gt_s, 0x4B = i32.gt_u
; 0x4C = i32.le_s, 0x4D = i32.le_u
; 0x4E = i32.ge_s, 0x4F = i32.ge_u

;; === WASM Header ===

; Write WASM magic "\0asm" and version 1
(fn emit-header ((out s32)) s32
  (let (out (write-byte out 0))    ; \0
    (let (out (write-byte out 97))   ; a
      (let (out (write-byte out 115))  ; s
        (let (out (write-byte out 109))  ; m
          (let (out (write-byte out 1))    ; version 1
            (let (out (write-byte out 0))
              (let (out (write-byte out 0))
                (write-byte out 0)))))))))

;; === Section Helpers ===
; WASM sections have format: section_id (1 byte) + size (uleb128) + content

; Reserve space for a section header, return struct {start, content_start}
; We'll write: section_id, then reserve 5 bytes for size (max uleb128 for 32-bit)
; After writing content, we need to patch the size
(fn begin-section ((out s32) (section-id s32)) s32
  (let (out (write-byte out section-id))
    out))  ; caller will patch size later

;; === Type Section ===
; For a function with signature (i32, i32) -> i32

; Emit type section for a single function type: (i32, i32) -> i32
(fn emit-type-section-add ((out s32)) s32
  ; Section 1 (type)
  (let (out (write-byte out 1))
    ; Section size: 7 bytes
    (let (out (write-byte out 7))
      ; Number of types: 1
      (let (out (write-byte out 1))
        ; Type 0: function type (0x60)
        (let (out (write-byte out 96))
          ; Param count: 2
          (let (out (write-byte out 2))
            ; Param 0: i32 (0x7F = 127)
            (let (out (write-byte out 127))
              ; Param 1: i32
              (let (out (write-byte out 127))
                ; Result count: 1
                (let (out (write-byte out 1))
                  ; Result 0: i32
                  (write-byte out 127))))))))))

;; === Function Section ===
; Maps functions to their type indices

; Emit function section for a single function using type 0
(fn emit-func-section-one ((out s32)) s32
  ; Section 3 (function)
  (let (out (write-byte out 3))
    ; Section size: 2 bytes
    (let (out (write-byte out 2))
      ; Number of functions: 1
      (let (out (write-byte out 1))
        ; Function 0 uses type 0
        (write-byte out 0)))))

;; === Memory Section ===
; Declare linear memory

(fn emit-memory-section ((out s32)) s32
  ; Section 5 (memory)
  (let (out (write-byte out 5))
    ; Section size: 3 bytes
    (let (out (write-byte out 3))
      ; Number of memories: 1
      (let (out (write-byte out 1))
        ; Flags: 0 (no max)
        (let (out (write-byte out 0))
          ; Initial: 1 page (64KB)
          (write-byte out 1))))))

;; === Export Section ===

; Emit export section for "add" function
(fn emit-export-section-add ((out s32)) s32
  ; Section 7 (export)
  (let (out (write-byte out 7))
    ; Section size: 7 bytes
    (let (out (write-byte out 7))
      ; Number of exports: 1
      (let (out (write-byte out 1))
        ; Export name length: 3
        (let (out (write-byte out 3))
          ; "add"
          (let (out (write-byte out 97))  ; a
            (let (out (write-byte out 100)) ; d
              (let (out (write-byte out 100)) ; d
                ; Export kind: function (0)
                (let (out (write-byte out 0))
                  ; Function index: 0
                  (write-byte out 0))))))))))

;; === Code Section ===

; Emit code section for: fn add(a, b) -> a + b
(fn emit-code-section-add ((out s32)) s32
  ; Section 10 (code)
  (let (out (write-byte out 10))
    ; Section size: 9 bytes
    (let (out (write-byte out 9))
      ; Number of functions: 1
      (let (out (write-byte out 1))
        ; Function body size: 7 bytes
        (let (out (write-byte out 7))
          ; Local decl count: 0
          (let (out (write-byte out 0))
            ; local.get 0 (param a)
            (let (out (write-byte out 32))
              (let (out (write-byte out 0))
                ; local.get 1 (param b)
                (let (out (write-byte out 32))
                  (let (out (write-byte out 1))
                    ; i32.add
                    (let (out (write-byte out 106))
                      ; end
                      (write-byte out 11))))))))))))

;; === Complete Module Emitter ===

; Emit a complete WASM module with an "add" function
; Returns: number of bytes written
(fn emit-add-module ((out s32)) s32
  (let (start out)
    (let (out (emit-header out))
      (let (out (emit-type-section-add out))
        (let (out (emit-func-section-one out))
          (let (out (emit-export-section-add out))
            (let (out (emit-code-section-add out))
              (i32.sub out start))))))))

;; === Test Entry Points ===

; Emit the "add" module at offset 1024
(fn emit-add () s32
  (emit-add-module 1024))

; Get a byte from the emitted module
(fn get-emitted-byte ((index s32)) s32
  (let (_ (emit-add-module 1024))
    (i32.load8_u (i32.add 1024 index))))

; Get the magic number
(fn get-magic () s32
  (let (_ (emit-add-module 1024))
    (i32.load 1024)))

(export emit-add)
(export get-emitted-byte)
(export get-magic)
