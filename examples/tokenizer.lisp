; Wisp Tokenizer
; Tokenizes S-expression source code from memory
;
; Token types:
; 0 = LPAREN    (
; 1 = RPAREN    )
; 2 = SYMBOL    identifier
; 3 = NUMBER    integer literal
; 4 = STRING    "..."
; 5 = COMMENT   ; to end of line
; 6 = EOF       end of input
; 7 = ERROR     invalid token

;; === Character Classification ===

; Is character whitespace? (space, tab, newline, carriage return)
(fn is-whitespace ((c s32)) s32
  (i32.or
    (i32.or
      (i32.eq c 32)   ; space
      (i32.eq c 9))   ; tab
    (i32.or
      (i32.eq c 10)   ; newline
      (i32.eq c 13)))) ; carriage return

; Is character a digit? (0-9)
(fn is-digit ((c s32)) s32
  (i32.and
    (i32.ge_s c 48)   ; >= '0'
    (i32.le_s c 57))) ; <= '9'

; Is character a letter? (a-z, A-Z)
(fn is-letter ((c s32)) s32
  (i32.or
    (i32.and
      (i32.ge_s c 65)   ; >= 'A'
      (i32.le_s c 90))  ; <= 'Z'
    (i32.and
      (i32.ge_s c 97)   ; >= 'a'
      (i32.le_s c 122)))) ; <= 'z'

; Is character valid in a symbol? (letter, digit, or -_.$)
(fn is-symbol-char ((c s32)) s32
  (i32.or
    (i32.or
      (is-letter c)
      (is-digit c))
    (i32.or
      (i32.or
        (i32.eq c 45)    ; -
        (i32.eq c 95))   ; _
      (i32.or
        (i32.eq c 46)    ; .
        (i32.eq c 36))))) ; $

; Is character a sign? (+ or -)
(fn is-sign ((c s32)) s32
  (i32.or
    (i32.eq c 43)   ; +
    (i32.eq c 45))) ; -

;; === Tokenizer State ===
; We use a simple approach: store state in globals
; This could be refactored to pass state explicitly

; Globals for tokenizer state
(global $tok-input-ptr s32 mut 0)  ; Pointer to input string
(global $tok-input-len s32 mut 0)  ; Length of input
(global $tok-pos s32 mut 0)        ; Current position in input

; Initialize tokenizer with input string location
(fn tok-init ((ptr s32) (len s32)) s32
  (let (_ (global.set $tok-input-ptr ptr))
    (let (_ (global.set $tok-input-len len))
      (let (_ (global.set $tok-pos 0))
        0))))  ; Return 0 for success

; Get current position
(fn tok-pos () s32
  (global.get $tok-pos))

; Check if at end of input
(fn tok-at-end () s32
  (i32.ge_s (global.get $tok-pos) (global.get $tok-input-len)))

; Peek at current character (returns -1 if at end)
(fn tok-peek () s32
  (if (tok-at-end)
    -1
    (i32.load8_u (i32.add (global.get $tok-input-ptr) (global.get $tok-pos)))))

; Peek at character at offset from current position
(fn tok-peek-at ((offset s32)) s32
  (let (pos (i32.add (global.get $tok-pos) offset))
    (if (i32.ge_s pos (global.get $tok-input-len))
      -1
      (i32.load8_u (i32.add (global.get $tok-input-ptr) pos)))))

; Advance position by n characters
(fn tok-advance ((n s32)) s32
  (let (new-pos (i32.add (global.get $tok-pos) n))
    (let (_ (global.set $tok-pos new-pos))
      new-pos)))

;; === Token Output ===
; We write tokens to an output buffer with format:
; [type:1][start:4][len:4] = 9 bytes per token

(global $tok-output-ptr s32 mut 0)  ; Pointer to output buffer
(global $tok-output-pos s32 mut 0)  ; Current write position in output

; Initialize output buffer
(fn tok-output-init ((ptr s32)) s32
  (let (_ (global.set $tok-output-ptr ptr))
    (let (_ (global.set $tok-output-pos 0))
      0)))

; Write a token to output
(fn tok-emit ((type s32) (start s32) (len s32)) s32
  (let (out (i32.add (global.get $tok-output-ptr) (global.get $tok-output-pos)))
    (let (_ (i32.store8 out type))
      (let (_ (i32.store (i32.add out 1) start))
        (let (_ (i32.store (i32.add out 5) len))
          (let (_ (global.set $tok-output-pos (i32.add (global.get $tok-output-pos) 9)))
            (global.get $tok-output-pos)))))))

; Get number of tokens emitted
(fn tok-count () s32
  (i32.div_s (global.get $tok-output-pos) 9))

;; === Tokenizer Functions ===

; Skip whitespace and comments
(fn tok-skip-whitespace () s32
  (if (tok-at-end)
    0
    (let (c (tok-peek))
      (if (is-whitespace c)
        (let (_ (tok-advance 1))
          (tok-skip-whitespace))
        (if (i32.eq c 59)  ; semicolon - start of comment
          (let (_ (tok-skip-comment))
            (tok-skip-whitespace))
          0)))))

; Skip to end of line (for comments)
(fn tok-skip-comment () s32
  (if (tok-at-end)
    0
    (let (c (tok-peek))
      (if (i32.eq c 10)  ; newline
        (let (_ (tok-advance 1))
          0)
        (let (_ (tok-advance 1))
          (tok-skip-comment))))))

; Scan a number (integer only for now)
; Returns length of number
(fn tok-scan-number () s32
  (let (start (global.get $tok-pos))
    (let (_ (tok-scan-number-chars))
      (i32.sub (global.get $tok-pos) start))))

(fn tok-scan-number-chars () s32
  (let (c (tok-peek))
    (if (is-digit c)
      (let (_ (tok-advance 1))
        (tok-scan-number-chars))
      0)))

; Scan a symbol
; Returns length of symbol
(fn tok-scan-symbol () s32
  (let (start (global.get $tok-pos))
    (let (_ (tok-scan-symbol-chars))
      (i32.sub (global.get $tok-pos) start))))

(fn tok-scan-symbol-chars () s32
  (let (c (tok-peek))
    (if (is-symbol-char c)
      (let (_ (tok-advance 1))
        (tok-scan-symbol-chars))
      0)))

; Scan a string (returns length including quotes)
(fn tok-scan-string () s32
  (let (start (global.get $tok-pos))
    ; Skip opening quote
    (let (_ (tok-advance 1))
      (let (_ (tok-scan-string-chars))
        ; Skip closing quote
        (let (_ (tok-advance 1))
          (i32.sub (global.get $tok-pos) start))))))

(fn tok-scan-string-chars () s32
  (let (c (tok-peek))
    (if (i32.eq c 34)  ; closing quote
      0
      (if (i32.eq c -1) ; EOF
        0  ; error - unterminated string
        (let (_ (tok-advance 1))
          (tok-scan-string-chars))))))

;; === Main Tokenizer ===

; Tokenize a single token, returns token type
(fn tok-next () s32
  (let (_ (tok-skip-whitespace))
    (if (tok-at-end)
      6  ; EOF
      (let (start (global.get $tok-pos))
        (let (c (tok-peek))
          (if (i32.eq c 40)  ; (
            (let (_ (tok-advance 1))
              (let (_ (tok-emit 0 start 1))
                0))
            (if (i32.eq c 41)  ; )
              (let (_ (tok-advance 1))
                (let (_ (tok-emit 1 start 1))
                  1))
              (if (is-digit c)
                (let (len (tok-scan-number))
                  (let (_ (tok-emit 3 start len))
                    3))
                (if (i32.eq c 34)  ; "
                  (let (len (tok-scan-string))
                    (let (_ (tok-emit 4 start len))
                      4))
                  (if (is-symbol-char c)
                    (let (len (tok-scan-symbol))
                      (let (_ (tok-emit 2 start len))
                        2))
                    ; Handle negative numbers: if we see - followed by digit
                    (if (i32.and (is-sign c) (is-digit (tok-peek-at 1)))
                      (let (_ (tok-advance 1))  ; skip the sign
                        (let (len (i32.add 1 (tok-scan-number)))
                          (let (_ (tok-emit 3 start len))
                            3)))
                      7)))))))))))  ; ERROR

; Tokenize entire input, returns token count
(fn tok-all () s32
  (let (type (tok-next))
    (if (i32.eq type 6)  ; EOF
      (tok-count)
      (if (i32.eq type 7)  ; ERROR
        -1  ; Return -1 on error
        (tok-all)))))

;; === Test Entry Points ===

; Helper to set up the test input
(fn setup-test-input () s32
  ; Write "(add 1 2)" to memory at offset 0
  (let (_ (i32.store8 0 40))   ; (
    (let (_ (i32.store8 1 97))   ; a
      (let (_ (i32.store8 2 100))  ; d
        (let (_ (i32.store8 3 100))  ; d
          (let (_ (i32.store8 4 32))   ; space
            (let (_ (i32.store8 5 49))   ; 1
              (let (_ (i32.store8 6 32))   ; space
                (let (_ (i32.store8 7 50))   ; 2
                  (let (_ (i32.store8 8 41))   ; )
                    9))))))))))  ; return length

; Test tokenizing a simple expression: "(add 1 2)"
; Returns number of tokens
(fn test-simple () s32
  (let (_ (setup-test-input))
    (let (_ (tok-init 0 9))
      (let (_ (tok-output-init 256))
        (tok-all)))))

; Combined test: tokenize and return specific token field
; These all do the full tokenization in one call

(fn get-token-type ((index s32)) s32
  (let (_ (setup-test-input))
    (let (_ (tok-init 0 9))
      (let (_ (tok-output-init 256))
        (let (_ (tok-all))
          (i32.load8_u (i32.add 256 (i32.mul index 9))))))))

(fn get-token-start ((index s32)) s32
  (let (_ (setup-test-input))
    (let (_ (tok-init 0 9))
      (let (_ (tok-output-init 256))
        (let (_ (tok-all))
          (i32.load (i32.add 257 (i32.mul index 9))))))))

(fn get-token-len ((index s32)) s32
  (let (_ (setup-test-input))
    (let (_ (tok-init 0 9))
      (let (_ (tok-output-init 256))
        (let (_ (tok-all))
          (i32.load (i32.add 261 (i32.mul index 9))))))))

; Debug: directly write and read back a token
(fn test-direct-write () s32
  ; Write token directly at 256: type=2, start=100, len=5
  (let (_ (i32.store8 256 2))    ; type
    (let (_ (i32.store 257 100))   ; start
      (let (_ (i32.store 261 5))     ; len
        ; Read back the type
        (i32.load8_u 256)))))

(fn test-direct-start () s32
  (let (_ (i32.store8 256 2))
    (let (_ (i32.store 257 100))
      (let (_ (i32.store 261 5))
        (i32.load 257)))))

(fn test-direct-len () s32
  (let (_ (i32.store8 256 2))
    (let (_ (i32.store 257 100))
      (let (_ (i32.store 261 5))
        (i32.load 261)))))

; Test globals are working
(fn test-globals () s32
  (let (_ (global.set $tok-output-ptr 256))
    (global.get $tok-output-ptr)))

; Test tok-emit directly
(fn test-emit () s32
  (let (_ (tok-output-init 256))
    (let (_ (tok-emit 2 100 5))
      ; Read back type at 256
      (i32.load8_u 256))))

(fn test-emit-start () s32
  (let (_ (tok-output-init 256))
    (let (_ (tok-emit 2 100 5))
      (i32.load 257))))

(fn test-emit-len () s32
  (let (_ (tok-output-init 256))
    (let (_ (tok-emit 2 100 5))
      (i32.load 261))))

(export test-simple)
(export get-token-type)
(export get-token-start)
(export get-token-len)
(export test-direct-write)
(export test-direct-start)
(export test-direct-len)
(export test-globals)
(export test-emit)
(export test-emit-start)
(export test-emit-len)
