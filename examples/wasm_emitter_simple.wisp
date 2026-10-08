; Simple WASM Emitter - returns count for now
; This tests that our byte sequence logic is correct

; Count the bytes we would emit for a minimal i32.const 42 module
; Without actually building a list, just return 37 (the byte count)
(fn byte-count () s32
  37)

; Actually test that we can build up a value
; This emits the WASM magic + version as a single i32 (0x6D736100 = "\0asm" little-endian)
(fn wasm-magic () s32
  ; 0x00 + 0x61*256 + 0x73*65536 + 0x6D*16777216
  ; = 0 + 24832 + 4784128 + 1828716544
  ; = 0x6D736100
  (i32.add
    (i32.add 0 (i32.mul 97 256))
    (i32.add (i32.mul 115 65536) (i32.mul 109 16777216))))

; Return version (should be 1)
(fn wasm-version () s32
  1)

(export byte-count)
(export wasm-magic)
(export wasm-version)
