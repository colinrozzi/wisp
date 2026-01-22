; WASM Emitter in Wisp
;
; This demonstrates wisp compiling to WASM that itself produces WASM bytes.
; A step toward a self-hosting compiler!

; Helper: push multiple bytes to a list
(fn push-bytes ((bytes (list s32)) (b1 s32)) (list s32)
  (list-push bytes b1))

(fn push-bytes2 ((bytes (list s32)) (b1 s32) (b2 s32)) (list s32)
  (list-push (list-push bytes b1) b2))

(fn push-bytes4 ((bytes (list s32)) (b1 s32) (b2 s32) (b3 s32) (b4 s32)) (list s32)
  (list-push (list-push (list-push (list-push bytes b1) b2) b3) b4))

; Emit a minimal WASM module that returns a constant i32
; Module structure:
;   - Header (magic + version)
;   - Type section (one func type: () -> i32)
;   - Function section (one func using type 0)
;   - Export section (export "eval" as func 0)
;   - Code section (i32.const <value>, end)
(fn emit-const-i32 ((value s32)) (list s32)
  (let (bytes (list-new s32))
    ; WASM magic: "\0asm" = 0x00 0x61 0x73 0x6D
    (let (bytes (push-bytes4 bytes 0 97 115 109))
      ; WASM version: 1
      (let (bytes (push-bytes4 bytes 1 0 0 0))

        ; Type section (ID=1): one type () -> i32
        ; 01 05 01 60 00 01 7f
        (let (bytes (push-bytes bytes 1))      ; section ID
          (let (bytes (push-bytes bytes 5))    ; section length
            (let (bytes (push-bytes bytes 1))  ; type count
              (let (bytes (push-bytes bytes 96)) ; func type marker (0x60)
                (let (bytes (push-bytes bytes 0))  ; param count
                  (let (bytes (push-bytes2 bytes 1 127)) ; result count=1, type=i32 (0x7f)

                    ; Function section (ID=3): func 0 uses type 0
                    ; 03 02 01 00
                    (let (bytes (push-bytes4 bytes 3 2 1 0))

                      ; Export section (ID=7): "eval" -> func 0
                      ; 07 08 01 04 "eval" 00 00
                      (let (bytes (push-bytes2 bytes 7 8)) ; section ID, length
                        (let (bytes (push-bytes bytes 1))   ; export count
                          (let (bytes (push-bytes bytes 4)) ; name length
                            ; "eval" = 101 118 97 108
                            (let (bytes (push-bytes4 bytes 101 118 97 108))
                              (let (bytes (push-bytes2 bytes 0 0)) ; kind=func, index=0

                                ; Code section (ID=10): function body
                                ; 0a 06 01 04 00 41 <value> 0b
                                (let (bytes (push-bytes2 bytes 10 6)) ; section ID, length
                                  (let (bytes (push-bytes bytes 1))   ; func count
                                    (let (bytes (push-bytes bytes 4)) ; body length
                                      (let (bytes (push-bytes bytes 0)) ; local count
                                        ; i32.const <value>
                                        (let (bytes (push-bytes2 bytes 65 value))
                                          ; end
                                          (push-bytes bytes 11))))))))))))))))))))))

; Main entry point: emit a module that returns 42
(fn emit-wasm () (list s32)
  (emit-const-i32 42))

(export emit-wasm)
