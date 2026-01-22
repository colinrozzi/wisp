; WASM Emitter - writes to memory
; Uses i32.store8 to write bytes directly

; Write a byte to memory at offset, return next offset
(fn write-byte ((offset s32) (byte s32)) s32
  (let (_ (i32.store8 offset byte))
    (i32.add offset 1)))

; Emit WASM header (magic + version) - 8 bytes
(fn emit-header ((out s32)) s32
  (let (out (write-byte out 0))
    (let (out (write-byte out 97))
      (let (out (write-byte out 115))
        (let (out (write-byte out 109))
          (let (out (write-byte out 1))
            (let (out (write-byte out 0))
              (let (out (write-byte out 0))
                (write-byte out 0)))))))))

; Emit type section: () -> i32 - 7 bytes
(fn emit-type-section ((out s32)) s32
  (let (out (write-byte out 1))
    (let (out (write-byte out 5))
      (let (out (write-byte out 1))
        (let (out (write-byte out 96))
          (let (out (write-byte out 0))
            (let (out (write-byte out 1))
              (write-byte out 127))))))))

; Emit function section - 4 bytes
(fn emit-func-section ((out s32)) s32
  (let (out (write-byte out 3))
    (let (out (write-byte out 2))
      (let (out (write-byte out 1))
        (write-byte out 0)))))

; Emit export section: "eval" - 10 bytes
(fn emit-export-section ((out s32)) s32
  (let (out (write-byte out 7))
    (let (out (write-byte out 8))
      (let (out (write-byte out 1))
        (let (out (write-byte out 4))
          (let (out (write-byte out 101))
            (let (out (write-byte out 118))
              (let (out (write-byte out 97))
                (let (out (write-byte out 108))
                  (let (out (write-byte out 0))
                    (write-byte out 0)))))))))))

; Emit code section with i32.const 42 - 8 bytes
(fn emit-code-section ((out s32)) s32
  (let (out (write-byte out 10))
    (let (out (write-byte out 6))
      (let (out (write-byte out 1))
        (let (out (write-byte out 4))
          (let (out (write-byte out 0))
            (let (out (write-byte out 65))
              (let (out (write-byte out 42))
                (write-byte out 11)))))))))

; Emit complete minimal WASM module
; Returns: number of bytes written
(fn emit-module ((out s32)) s32
  (let (start out)
    (let (out (emit-header out))
      (let (out (emit-type-section out))
        (let (out (emit-func-section out))
          (let (out (emit-export-section out))
            (let (out (emit-code-section out))
              (i32.sub out start))))))))

; Entry point: write to offset 1024, return byte count
; (avoid offset 0 to not conflict with heap)
(fn emit-wasm () s32
  (emit-module 1024))

; Emit the module and return a specific byte (for verification)
(fn emit-and-get-byte ((index s32)) s32
  (let (_ (emit-module 1024))
    (i32.load8_u (i32.add 1024 index))))

; Get the WASM magic as a single i32 (for quick verification)
(fn emit-and-get-magic () s32
  (let (_ (emit-module 1024))
    (i32.load 1024)))

(export emit-wasm)
(export emit-and-get-byte)
(export emit-and-get-magic)
