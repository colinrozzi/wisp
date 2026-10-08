; Wisp Tokenizer - written in Wisp
; Part of the self-hosted compiler project

; ============================================================
; Token Type
; ============================================================

(variant token
  (lparen)                ; (
  (rparen)                ; )
  (number s32)            ; integer literal
  (symbol string)         ; identifier/symbol
  (str-lit string))       ; string literal "..."

; ============================================================
; Character Classification
; ============================================================

; Check if character is whitespace (space, tab, newline, carriage return)
(fn is-whitespace ((c s32)) s32
  (if (i32.eq c (i32.const 32))   ; space
    (i32.const 1)
    (if (i32.eq c (i32.const 9))  ; tab
      (i32.const 1)
      (if (i32.eq c (i32.const 10)) ; newline
        (i32.const 1)
        (if (i32.eq c (i32.const 13)) ; carriage return
          (i32.const 1)
          (i32.const 0))))))

; Check if character is a digit (0-9)
(fn is-digit ((c s32)) s32
  (if (i32.ge_s c (i32.const 48))  ; >= '0'
    (if (i32.le_s c (i32.const 57))  ; <= '9'
      (i32.const 1)
      (i32.const 0))
    (i32.const 0)))

; Check if character is a delimiter (whitespace or parenthesis)
(fn is-delimiter ((c s32)) s32
  (if (is-whitespace c)
    (i32.const 1)
    (if (i32.eq c (i32.const 40))  ; (
      (i32.const 1)
      (if (i32.eq c (i32.const 41))  ; )
        (i32.const 1)
        (if (i32.eq c (i32.const 59))  ; ; (comment)
          (i32.const 1)
          (if (i32.eq c (i32.const 34))  ; " (string)
            (i32.const 1)
            (i32.const 0)))))))

; Convert digit character to its numeric value
(fn digit-value ((c s32)) s32
  (i32.sub c (i32.const 48)))  ; c - '0'

; ============================================================
; Tokenizer State
; We use a record to track position during tokenization
; ============================================================

(record tokenizer-state
  (src string)       ; source string
  (pos s32)          ; current position
  (len s32))         ; source length

; Create initial tokenizer state
(fn make-tokenizer ((src string)) tokenizer-state
  (tokenizer-state src (i32.const 0) (string-len src)))

; Check if we've reached end of input
(fn at-end ((st tokenizer-state)) s32
  (i32.ge_s (tokenizer-state.pos st) (tokenizer-state.len st)))

; Get current character (returns -1 if at end)
(fn current-char ((st tokenizer-state)) s32
  (if (at-end st)
    (i32.const -1)
    (string-ref (tokenizer-state.src st) (tokenizer-state.pos st))))

; Advance position by 1
(fn advance ((st tokenizer-state)) tokenizer-state
  (tokenizer-state
    (tokenizer-state.src st)
    (i32.add (tokenizer-state.pos st) (i32.const 1))
    (tokenizer-state.len st)))

; ============================================================
; Skip Helpers
; ============================================================

; Skip whitespace, return new state
(fn skip-whitespace ((st tokenizer-state)) tokenizer-state
  (if (at-end st)
    st
    (let (c (current-char st))
      (if (is-whitespace c)
        (skip-whitespace (advance st))
        st))))

; Skip to end of line (for comments), return new state
(fn skip-to-eol ((st tokenizer-state)) tokenizer-state
  (if (at-end st)
    st
    (let (c (current-char st))
      (if (i32.eq c (i32.const 10))  ; newline
        (advance st)
        (skip-to-eol (advance st))))))

; Skip whitespace and comments
(fn skip-ignored ((st tokenizer-state)) tokenizer-state
  (let (st2 (skip-whitespace st))
    (if (at-end st2)
      st2
      (let (c (current-char st2))
        (if (i32.eq c (i32.const 59))  ; semicolon - start comment
          (skip-ignored (skip-to-eol st2))
          st2)))))

; ============================================================
; Number Parsing
; ============================================================

; Parse a number starting at current position
; Returns: (number-value, new-state) encoded as two values
; For simplicity, we'll return the state and use a global for the value

; Helper: accumulate digits into a number
(fn parse-number-acc ((st tokenizer-state) (acc s32)) tokenizer-state
  (if (at-end st)
    st
    (let (c (current-char st))
      (if (is-digit c)
        (parse-number-acc
          (advance st)
          (i32.add (i32.mul acc (i32.const 10)) (digit-value c)))
        st))))

; ============================================================
; Symbol Parsing
; ============================================================

; Find end of symbol (position after last symbol character)
(fn find-symbol-end ((st tokenizer-state)) s32
  (if (at-end st)
    (tokenizer-state.pos st)
    (let (c (current-char st))
      (if (is-delimiter c)
        (tokenizer-state.pos st)
        (find-symbol-end (advance st))))))

; ============================================================
; String Parsing
; ============================================================

; Find end of string literal (position of closing quote)
(fn find-string-end ((st tokenizer-state)) s32
  (if (at-end st)
    (tokenizer-state.pos st)  ; unterminated string
    (let (c (current-char st))
      (if (i32.eq c (i32.const 34))  ; closing quote
        (tokenizer-state.pos st)
        (if (i32.eq c (i32.const 92))  ; backslash - escape
          (find-string-end (advance (advance st)))  ; skip escaped char
          (find-string-end (advance st)))))))

; ============================================================
; Token Reading - returns (token, new-position) via a result record
; ============================================================

(record token-result
  (tok token)
  (new-pos s32))

; Parse a number starting at position, return value and end position
; Helper that accumulates into a value
(fn parse-number-value ((src string) (pos s32) (len s32) (acc s32)) token-result
  (if (i32.ge_s pos len)
    (token-result (number acc) pos)
    (let (c (string-ref src pos))
      (if (is-digit c)
        (parse-number-value src
          (i32.add pos (i32.const 1))
          len
          (i32.add (i32.mul acc (i32.const 10)) (digit-value c)))
        (token-result (number acc) pos)))))

; Read a number token
(fn read-number ((src string) (pos s32) (len s32)) token-result
  (parse-number-value src pos len (i32.const 0)))

; Read a symbol token
(fn read-symbol ((src string) (pos s32) (len s32)) token-result
  (let (start pos)
    (let (end (find-symbol-end-at src pos len))
      (let (sym-str (substring src start end))
        (token-result (symbol sym-str) end)))))

; Find end of symbol starting at pos
(fn find-symbol-end-at ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-delimiter c)
        pos
        (find-symbol-end-at src (i32.add pos (i32.const 1)) len)))))

; Read a string literal (pos points to opening quote)
(fn read-string-lit ((src string) (pos s32) (len s32)) token-result
  (let (start (i32.add pos (i32.const 1)))  ; skip opening quote
    (let (end (find-string-end-at src start len))
      (let (str-content (substring src start end))
        ; end points to closing quote, advance past it
        (token-result (str-lit str-content) (i32.add end (i32.const 1)))))))

; Find end of string literal (position of closing quote)
(fn find-string-end-at ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos  ; unterminated string
    (let (c (string-ref src pos))
      (if (i32.eq c (i32.const 34))  ; closing quote
        pos
        (if (i32.eq c (i32.const 92))  ; backslash
          (find-string-end-at src (i32.add pos (i32.const 2)) len)  ; skip escaped char
          (find-string-end-at src (i32.add pos (i32.const 1)) len))))))

; Read one token at position, return token and new position
(fn read-token ((src string) (pos s32) (len s32)) token-result
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 40))  ; '('
      (token-result (lparen) (i32.add pos (i32.const 1)))
      (if (i32.eq c (i32.const 41))  ; ')'
        (token-result (rparen) (i32.add pos (i32.const 1)))
        (if (i32.eq c (i32.const 34))  ; '"'
          (read-string-lit src pos len)
          (if (is-digit c)
            (read-number src pos len)
            ; Check for negative number
            (if (i32.eq c (i32.const 45))  ; '-'
              (if (i32.lt_s (i32.add pos (i32.const 1)) len)
                (let (next-c (string-ref src (i32.add pos (i32.const 1))))
                  (if (is-digit next-c)
                    ; It's a negative number - parse and negate
                    (let (result (read-number src (i32.add pos (i32.const 1)) len))
                      (let (tok (token-result.tok result))
                        (let (new-pos (token-result.new-pos result))
                          (match tok
                            ((number n) (token-result (number (i32.sub (i32.const 0) n)) new-pos))
                            ((lparen) result)
                            ((rparen) result)
                            ((symbol s) result)
                            ((str-lit s) result)))))
                    ; It's a symbol starting with -
                    (read-symbol src pos len)))
                (read-symbol src pos len))
              ; Regular symbol
              (read-symbol src pos len))))))))

; ============================================================
; Skip position helpers (using just position, not state record)
; ============================================================

; Skip whitespace starting at pos, return new pos
(fn skip-ws ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-whitespace c)
        (skip-ws src (i32.add pos (i32.const 1)) len)
        pos))))

; Skip to end of line
(fn skip-eol ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (i32.eq c (i32.const 10))  ; newline
        (i32.add pos (i32.const 1))
        (skip-eol src (i32.add pos (i32.const 1)) len)))))

; Skip whitespace and comments
(fn skip-ignored-at ((src string) (pos s32) (len s32)) s32
  (let (pos2 (skip-ws src pos len))
    (if (i32.ge_s pos2 len)
      pos2
      (let (c (string-ref src pos2))
        (if (i32.eq c (i32.const 59))  ; semicolon
          (skip-ignored-at src (skip-eol src pos2 len) len)
          pos2)))))

; ============================================================
; Main Tokenize Function
; ============================================================

; Tokenize helper: accumulate tokens into list
(fn tokenize-acc ((src string) (pos s32) (len s32) (tokens (list token))) (list token)
  (let (pos2 (skip-ignored-at src pos len))
    (if (i32.ge_s pos2 len)
      tokens  ; done
      (let (result (read-token src pos2 len))
        (let (new-tokens (list-push tokens (token-result.tok result)))
          (tokenize-acc src (token-result.new-pos result) len new-tokens))))))

; Main tokenize function: string -> list of tokens
(fn tokenize ((src string)) (list token)
  (tokenize-acc src (i32.const 0) (string-len src) (list-new token)))

; ============================================================
; Test Exports
; ============================================================

; Test: is-whitespace
(export (fn test-is-whitespace-space () s32
  (is-whitespace (i32.const 32))))  ; Expected: 1

(export (fn test-is-whitespace-a () s32
  (is-whitespace (i32.const 97))))  ; Expected: 0 ('a')

; Test: is-digit
(export (fn test-is-digit-0 () s32
  (is-digit (i32.const 48))))  ; Expected: 1 ('0')

(export (fn test-is-digit-9 () s32
  (is-digit (i32.const 57))))  ; Expected: 1 ('9')

(export (fn test-is-digit-a () s32
  (is-digit (i32.const 97))))  ; Expected: 0 ('a')

; Test: digit-value
(export (fn test-digit-value-0 () s32
  (digit-value (i32.const 48))))  ; Expected: 0

(export (fn test-digit-value-5 () s32
  (digit-value (i32.const 53))))  ; Expected: 5

; Test: tokenizer state
(export (fn test-make-tokenizer () s32
  (let (st (make-tokenizer "hello"))
    (tokenizer-state.len st))))  ; Expected: 5

(export (fn test-current-char () s32
  (let (st (make-tokenizer "hello"))
    (current-char st))))  ; Expected: 104 ('h')

(export (fn test-advance () s32
  (let (st (make-tokenizer "hello"))
    (let (st2 (advance st))
      (current-char st2)))))  ; Expected: 101 ('e')

; Test: skip-whitespace
(export (fn test-skip-whitespace () s32
  (let (st (make-tokenizer "  hello"))
    (let (st2 (skip-whitespace st))
      (current-char st2)))))  ; Expected: 104 ('h')

; Test: at-end
(export (fn test-at-end-false () s32
  (let (st (make-tokenizer "x"))
    (at-end st))))  ; Expected: 0

(export (fn test-at-end-true () s32
  (let (st (make-tokenizer ""))
    (at-end st))))  ; Expected: 1

; Test: find-symbol-end
(export (fn test-find-symbol-end () s32
  (let (st (make-tokenizer "hello world"))
    (find-symbol-end st))))  ; Expected: 5 (position after "hello")

; Test: is-delimiter
(export (fn test-is-delimiter-paren () s32
  (is-delimiter (i32.const 40))))  ; Expected: 1 ('(')

(export (fn test-is-delimiter-letter () s32
  (is-delimiter (i32.const 97))))  ; Expected: 0 ('a')

; ============================================================
; Tokenize Function Tests
; ============================================================

; Test: tokenize empty string
(export (fn test-tokenize-empty () s32
  (let (tokens (tokenize ""))
    (list-len tokens))))  ; Expected: 0

; Test: tokenize single lparen
(export (fn test-tokenize-lparen () s32
  (let (tokens (tokenize "("))
    (list-len tokens))))  ; Expected: 1

; Test: tokenize parens
(export (fn test-tokenize-parens () s32
  (let (tokens (tokenize "()"))
    (list-len tokens))))  ; Expected: 2

; Test: tokenize with whitespace
(export (fn test-tokenize-whitespace () s32
  (let (tokens (tokenize "  (  )  "))
    (list-len tokens))))  ; Expected: 2

; Test: tokenize number
(export (fn test-tokenize-number () s32
  (let (tokens (tokenize "42"))
    (list-len tokens))))  ; Expected: 1

; Test: tokenize symbol
(export (fn test-tokenize-symbol () s32
  (let (tokens (tokenize "hello"))
    (list-len tokens))))  ; Expected: 1

; Test: tokenize expression
(export (fn test-tokenize-expr () s32
  (let (tokens (tokenize "(add 1 2)"))
    (list-len tokens))))  ; Expected: 5: ( add 1 2 )

; Test: tokenize with comment
(export (fn test-tokenize-comment () s32
  (let (tokens (tokenize "; comment\n()"))
    (list-len tokens))))  ; Expected: 2

; Test: check first token is lparen
(fn is-lparen-token ((t token)) s32
  (match t
    ((lparen) (i32.const 1))
    ((rparen) (i32.const 0))
    ((number n) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

(export (fn test-first-token-lparen () s32
  (let (tokens (tokenize "(hello)"))
    (let (first (list-get tokens (i32.const 0)))
      (is-lparen-token first)))))  ; Expected: 1

; Test: check number value
(fn get-number-value ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 0))
    ((number n) n)
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

(export (fn test-number-value () s32
  (let (tokens (tokenize "42"))
    (let (first (list-get tokens (i32.const 0)))
      (get-number-value first)))))  ; Expected: 42

; Test: get symbol name length
(fn get-symbol-len ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 0))
    ((number n) (i32.const 0))
    ((symbol s) (string-len s))
    ((str-lit s) (i32.const 0))))

(export (fn test-symbol-name () s32
  (let (tokens (tokenize "hello"))
    (let (first (list-get tokens (i32.const 0)))
      (get-symbol-len first)))))  ; Expected: 5

; Test: string literal
(fn get-str-lit-len ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 0))
    ((number n) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (string-len s))))

(export (fn test-string-literal () s32
  (let (tokens (tokenize "\"hello\""))
    (let (first (list-get tokens (i32.const 0)))
      (get-str-lit-len first)))))  ; Expected: 5

; Test: negative number
(export (fn test-negative-number () s32
  (let (tokens (tokenize "-42"))
    (let (first (list-get tokens (i32.const 0)))
      (get-number-value first)))))  ; Expected: -42

; Test: complex expression
(export (fn test-complex-expr () s32
  (let (tokens (tokenize "(fn add ((x s32)) s32 x)"))
    (list-len tokens))))  ; Expected: 11
