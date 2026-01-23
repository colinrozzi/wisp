; LEB128 Encoding for WASM
;
; WASM uses LEB128 (Little Endian Base 128) for encoding integers.
; This is essential for the self-hosting compiler.

; Write a byte to memory, return next offset
(fn write-byte ((offset s32) (byte s32)) s32
  (let (_ (i32.store8 offset byte))
    (i32.add offset 1)))

; Write unsigned LEB128 encoded integer to memory
; Returns: new offset after writing
(fn write-uleb128 ((offset s32) (value s32)) s32
  (if (i32.lt_u value 128)
    ; Single byte case: value < 128, no continuation bit
    (write-byte offset value)
    ; Multi-byte case: write low 7 bits with continuation bit, recurse
    (let (byte (i32.or (i32.and value 127) 128))
      (let (offset (write-byte offset byte))
        (write-uleb128 offset (i32.shr_u value 7))))))

; Write signed LEB128 encoded integer to memory
; Returns: new offset after writing
(fn write-sleb128 ((offset s32) (value s32)) s32
  (let (byte (i32.and value 127))
    (let (value (i32.shr_s value 7))
      ; Check if we're done: value is 0 and sign bit clear, or -1 and sign bit set
      (if (i32.or
            (i32.and (i32.eq value 0) (i32.eq (i32.and byte 64) 0))
            (i32.and (i32.eq value -1) (i32.ne (i32.and byte 64) 0)))
        ; Done - write final byte without continuation
        (write-byte offset byte)
        ; More bytes needed - set continuation bit and recurse
        (let (offset (write-byte offset (i32.or byte 128)))
          (write-sleb128 offset value))))))

; Test: write various values and return the byte count
(fn test-uleb128 ((value s32)) s32
  (let (start 1024)
    (i32.sub (write-uleb128 start value) start)))

; Test: get a byte from the output
(fn get-byte ((index s32)) s32
  (i32.load8_u (i32.add 1024 index)))

; Test specific values
(fn test-uleb128-zero () s32 (test-uleb128 0))        ; should be 1 byte
(fn test-uleb128-max-single () s32 (test-uleb128 127))    ; should be 1 byte
(fn test-uleb128-two-bytes () s32 (test-uleb128 128))    ; should be 2 bytes
(fn test-uleb128-three-bytes () s32 (test-uleb128 624485)) ; should be 3 bytes

; Combined test: encode value and return byte at index
(fn test-encode-byte ((value s32) (index s32)) s32
  (let (start 1024)
    (let (_ (write-uleb128 start value))
      (i32.load8_u (i32.add start index)))))

(export test-uleb128-zero)
(export test-uleb128-max-single)
(export test-uleb128-two-bytes)
(export test-uleb128-three-bytes)
(export test-uleb128)
(export get-byte)
(export test-encode-byte)
