; Wisp S-Expression Parser
; Parses tokenized input into a simple tree structure
;
; AST Node format in memory:
; [type:1][data...] where:
;   type 0 = ATOM:  [type:1][token_idx:4] = 5 bytes
;   type 1 = LIST:  [type:1][num_children:4] followed by children, then END
;   type 2 = END:   [type:1] = 1 byte (marks end of list)

;; === Token type constants ===

(fn tok-lparen () s32 0)
(fn tok-rparen () s32 1)
(fn tok-symbol () s32 2)
(fn tok-number () s32 3)
(fn tok-string () s32 4)
(fn tok-eof () s32 6)
(fn tok-error () s32 7)

;; === AST node type constants ===

(fn ast-atom () s32 0)
(fn ast-list () s32 1)
(fn ast-end () s32 2)

;; === Memory layout ===
; Input tokens: 256..2048 (from tokenizer)
; AST output: 2048..4096

;; === Parser state globals ===

(global $parse-tok-ptr s32 mut 0)   ; Pointer to token array
(global $parse-tok-idx s32 mut 0)   ; Current token index
(global $parse-tok-count s32 mut 0) ; Total number of tokens
(global $parse-ast-ptr s32 mut 0)   ; Pointer to AST output
(global $parse-ast-pos s32 mut 0)   ; Current write position in AST

;; === Helper: Read token at index ===

; Get token type at index
(fn parse-get-tok-type ((idx s32)) s32
  (i32.load8_u (i32.add (global.get $parse-tok-ptr) (i32.mul idx 9))))

; Get token start at index
(fn parse-get-tok-start ((idx s32)) s32
  (i32.load (i32.add (i32.add (global.get $parse-tok-ptr) 1) (i32.mul idx 9))))

; Get token length at index
(fn parse-get-tok-len ((idx s32)) s32
  (i32.load (i32.add (i32.add (global.get $parse-tok-ptr) 5) (i32.mul idx 9))))

;; === Parser helpers ===

; Get current token type
(fn parse-current-type () s32
  (if (i32.ge_s (global.get $parse-tok-idx) (global.get $parse-tok-count))
    (tok-eof)
    (parse-get-tok-type (global.get $parse-tok-idx))))

; Advance to next token
(fn parse-advance () s32
  (let (new-idx (i32.add (global.get $parse-tok-idx) 1))
    (let (_ (global.set $parse-tok-idx new-idx))
      new-idx)))

; Write byte to AST output
(fn parse-write-byte ((b s32)) s32
  (let (pos (i32.add (global.get $parse-ast-ptr) (global.get $parse-ast-pos)))
    (let (_ (i32.store8 pos b))
      (let (_ (global.set $parse-ast-pos (i32.add (global.get $parse-ast-pos) 1)))
        (global.get $parse-ast-pos)))))

; Write i32 to AST output
(fn parse-write-i32 ((v s32)) s32
  (let (pos (i32.add (global.get $parse-ast-ptr) (global.get $parse-ast-pos)))
    (let (_ (i32.store pos v))
      (let (_ (global.set $parse-ast-pos (i32.add (global.get $parse-ast-pos) 4)))
        (global.get $parse-ast-pos)))))

;; === Main parser functions ===

; Parse a single S-expression
; Returns: 1 on success, 0 on failure
(fn parse-expr () s32
  (let (tok-type (parse-current-type))
    (if (i32.eq tok-type (tok-lparen))
      ; List: parse ( items... )
      (parse-list)
      ; Atom: symbol, number, or string
      (if (i32.or
            (i32.eq tok-type (tok-symbol))
            (i32.or
              (i32.eq tok-type (tok-number))
              (i32.eq tok-type (tok-string))))
        (parse-atom)
        0))))  ; Error

; Parse an atom (symbol, number, or string)
(fn parse-atom () s32
  (let (tok-idx (global.get $parse-tok-idx))
    (let (_ (parse-advance))
      ; Write ATOM node: [type:1][token_idx:4]
      (let (_ (parse-write-byte (ast-atom)))
        (let (_ (parse-write-i32 tok-idx))
          1)))))  ; Success

; Parse a list ( expr* )
; We need to count children as we parse them
; Strategy: reserve space for count, parse children, patch count
(fn parse-list () s32
  ; Skip opening paren
  (let (_ (parse-advance))
    ; Write LIST node type
    (let (_ (parse-write-byte (ast-list)))
      ; Remember position for child count
      (let (count-pos (global.get $parse-ast-pos))
        ; Write placeholder count (0)
        (let (_ (parse-write-i32 0))
          ; Parse children and count them
          (let (count (parse-list-items 0))
            ; Patch the count
            (let (_ (i32.store (i32.add (global.get $parse-ast-ptr) count-pos) count))
              ; Write END marker
              (let (_ (parse-write-byte (ast-end)))
                1))))))))  ; Success

; Parse list items until )
; Returns count of items parsed
(fn parse-list-items ((count s32)) s32
  (let (tok-type (parse-current-type))
    (if (i32.eq tok-type (tok-rparen))
      ; End of list - skip ) and return count
      (let (_ (parse-advance))
        count)
      (if (i32.eq tok-type (tok-eof))
        ; Unexpected EOF
        count
        ; Parse an item and continue
        (let (_ (parse-expr))
          (parse-list-items (i32.add count 1)))))))

;; === Parser initialization ===

(fn parse-init ((tok-ptr s32) (tok-count s32) (ast-ptr s32)) s32
  (let (_ (global.set $parse-tok-ptr tok-ptr))
    (let (_ (global.set $parse-tok-count tok-count))
      (let (_ (global.set $parse-tok-idx 0))
        (let (_ (global.set $parse-ast-ptr ast-ptr))
          (let (_ (global.set $parse-ast-pos 0))
            0))))))

; Parse all input and return AST size
(fn parse-all () s32
  (let (_ (parse-expr))
    (global.get $parse-ast-pos)))

;; === Test helpers ===

; Include tokenizer primitives (simplified - just the test setup)

(fn write-byte ((offset s32) (byte s32)) s32
  (let (_ (i32.store8 offset byte))
    (i32.add offset 1)))

; Tokenizer globals
(global $tok-input-ptr s32 mut 0)
(global $tok-input-len s32 mut 0)
(global $tok-pos s32 mut 0)
(global $tok-output-ptr s32 mut 0)
(global $tok-output-pos s32 mut 0)

; Character checks
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

; Tokenizer functions
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
  (let (new-pos (i32.add (global.get $tok-pos) n))
    (let (_ (global.set $tok-pos new-pos))
      new-pos)))

(fn tok-emit ((type s32) (start s32) (len s32)) s32
  (let (out (i32.add (global.get $tok-output-ptr) (global.get $tok-output-pos)))
    (let (_ (i32.store8 out type))
      (let (_ (i32.store (i32.add out 1) start))
        (let (_ (i32.store (i32.add out 5) len))
          (let (_ (global.set $tok-output-pos (i32.add (global.get $tok-output-pos) 9)))
            (global.get $tok-output-pos)))))))

(fn tok-count () s32
  (i32.div_s (global.get $tok-output-pos) 9))

(fn tok-skip-whitespace () s32
  (if (tok-at-end) 0
    (let (c (tok-peek))
      (if (is-whitespace c)
        (let (_ (tok-advance 1))
          (tok-skip-whitespace))
        0))))

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

(fn tok-next () s32
  (let (_ (tok-skip-whitespace))
    (if (tok-at-end) 6
      (let (start (global.get $tok-pos))
        (let (c (tok-peek))
          (if (i32.eq c 40)
            (let (_ (tok-advance 1))
              (let (_ (tok-emit 0 start 1))
                0))
            (if (i32.eq c 41)
              (let (_ (tok-advance 1))
                (let (_ (tok-emit 1 start 1))
                  1))
              (if (is-digit c)
                (let (len (tok-scan-number))
                  (let (_ (tok-emit 3 start len))
                    3))
                (if (is-symbol-char c)
                  (let (len (tok-scan-symbol))
                    (let (_ (tok-emit 2 start len))
                      2))
                  7)))))))))

(fn tok-all () s32
  (let (type (tok-next))
    (if (i32.eq type 6) (tok-count)
      (if (i32.eq type 7) -1
        (tok-all)))))

;; === Test: Parse "(add 1 2)" ===

(fn setup-input () s32
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
                    9))))))))))

; Full test: tokenize and parse, return AST size
(fn test-parse () s32
  (let (_ (setup-input))
    (let (_ (tok-init 0 9))
      (let (_ (tok-output-init 256))
        (let (tok-count (tok-all))
          (let (_ (parse-init 256 tok-count 2048))
            (parse-all)))))))

; Read AST byte at offset
(fn get-ast-byte ((offset s32)) s32
  (let (_ (setup-input))
    (let (_ (tok-init 0 9))
      (let (_ (tok-output-init 256))
        (let (tok-count (tok-all))
          (let (_ (parse-init 256 tok-count 2048))
            (let (_ (parse-all))
              (i32.load8_u (i32.add 2048 offset)))))))))

; Read AST i32 at offset
(fn get-ast-i32 ((offset s32)) s32
  (let (_ (setup-input))
    (let (_ (tok-init 0 9))
      (let (_ (tok-output-init 256))
        (let (tok-count (tok-all))
          (let (_ (parse-init 256 tok-count 2048))
            (let (_ (parse-all))
              (i32.load (i32.add 2048 offset)))))))))

(export test-parse)
(export get-ast-byte)
(export get-ast-i32)
