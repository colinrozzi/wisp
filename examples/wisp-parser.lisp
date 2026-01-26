; Wisp Parser - written in Wisp
; Part of the self-hosted compiler project
; Converts tokens to S-expression AST

; ============================================================
; Token Type (same as tokenizer)
; ============================================================

(variant token
  (lparen)                ; (
  (rparen)                ; )
  (number s32)            ; integer literal
  (symbol string)         ; identifier/symbol
  (str-lit string))       ; string literal "..."

; ============================================================
; S-Expression AST Type
; ============================================================

(variant sexpr
  (sym string)            ; symbol/identifier
  (num s32)               ; number literal
  (str string)            ; string literal
  (lst (list sexpr)))     ; list of s-expressions

; ============================================================
; Character Classification (from tokenizer)
; ============================================================

(fn is-whitespace ((c s32)) s32
  (if (i32.eq c (i32.const 32))
    (i32.const 1)
    (if (i32.eq c (i32.const 9))
      (i32.const 1)
      (if (i32.eq c (i32.const 10))
        (i32.const 1)
        (if (i32.eq c (i32.const 13))
          (i32.const 1)
          (i32.const 0))))))

(fn is-digit ((c s32)) s32
  (if (i32.ge_s c (i32.const 48))
    (if (i32.le_s c (i32.const 57))
      (i32.const 1)
      (i32.const 0))
    (i32.const 0)))

(fn is-delimiter ((c s32)) s32
  (if (is-whitespace c)
    (i32.const 1)
    (if (i32.eq c (i32.const 40))
      (i32.const 1)
      (if (i32.eq c (i32.const 41))
        (i32.const 1)
        (if (i32.eq c (i32.const 59))
          (i32.const 1)
          (if (i32.eq c (i32.const 34))
            (i32.const 1)
            (i32.const 0)))))))

(fn digit-value ((c s32)) s32
  (i32.sub c (i32.const 48)))

; ============================================================
; Tokenizer (from tokenizer)
; ============================================================

(record token-result
  (tok token)
  (new-pos s32))

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

(fn read-number ((src string) (pos s32) (len s32)) token-result
  (parse-number-value src pos len (i32.const 0)))

(fn find-symbol-end-at ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-delimiter c)
        pos
        (find-symbol-end-at src (i32.add pos (i32.const 1)) len)))))

(fn read-symbol ((src string) (pos s32) (len s32)) token-result
  (let (start pos)
    (let (end (find-symbol-end-at src pos len))
      (let (sym-str (substring src start end))
        (token-result (symbol sym-str) end)))))

(fn find-string-end-at ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (i32.eq c (i32.const 34))
        pos
        (if (i32.eq c (i32.const 92))
          (find-string-end-at src (i32.add pos (i32.const 2)) len)
          (find-string-end-at src (i32.add pos (i32.const 1)) len))))))

(fn read-string-lit ((src string) (pos s32) (len s32)) token-result
  (let (start (i32.add pos (i32.const 1)))
    (let (end (find-string-end-at src start len))
      (let (str-content (substring src start end))
        (token-result (str-lit str-content) (i32.add end (i32.const 1)))))))

(fn read-token ((src string) (pos s32) (len s32)) token-result
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 40))
      (token-result (lparen) (i32.add pos (i32.const 1)))
      (if (i32.eq c (i32.const 41))
        (token-result (rparen) (i32.add pos (i32.const 1)))
        (if (i32.eq c (i32.const 34))
          (read-string-lit src pos len)
          (if (is-digit c)
            (read-number src pos len)
            (if (i32.eq c (i32.const 45))
              (if (i32.lt_s (i32.add pos (i32.const 1)) len)
                (let (next-c (string-ref src (i32.add pos (i32.const 1))))
                  (if (is-digit next-c)
                    (let (result (read-number src (i32.add pos (i32.const 1)) len))
                      (let (tok (token-result.tok result))
                        (let (new-pos (token-result.new-pos result))
                          (match tok
                            ((number n) (token-result (number (i32.sub (i32.const 0) n)) new-pos))
                            ((lparen) result)
                            ((rparen) result)
                            ((symbol s) result)
                            ((str-lit s) result)))))
                    (read-symbol src pos len)))
                (read-symbol src pos len))
              (read-symbol src pos len))))))))

(fn skip-ws ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-whitespace c)
        (skip-ws src (i32.add pos (i32.const 1)) len)
        pos))))

(fn skip-eol ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (i32.eq c (i32.const 10))
        (i32.add pos (i32.const 1))
        (skip-eol src (i32.add pos (i32.const 1)) len)))))

(fn skip-ignored-at ((src string) (pos s32) (len s32)) s32
  (let (pos2 (skip-ws src pos len))
    (if (i32.ge_s pos2 len)
      pos2
      (let (c (string-ref src pos2))
        (if (i32.eq c (i32.const 59))
          (skip-ignored-at src (skip-eol src pos2 len) len)
          pos2)))))

(fn tokenize-acc ((src string) (pos s32) (len s32) (tokens (list token))) (list token)
  (let (pos2 (skip-ignored-at src pos len))
    (if (i32.ge_s pos2 len)
      tokens
      (let (result (read-token src pos2 len))
        (let (new-tokens (list-push tokens (token-result.tok result)))
          (tokenize-acc src (token-result.new-pos result) len new-tokens))))))

(fn tokenize ((src string)) (list token)
  (tokenize-acc src (i32.const 0) (string-len src) (list-new token)))

; ============================================================
; Parser
; ============================================================

; Parser result: returns parsed sexpr and new position in token list
(record parse-result
  (expr sexpr)
  (new-pos s32))

; Check if token is lparen
(fn is-lparen ((t token)) s32
  (match t
    ((lparen) (i32.const 1))
    ((rparen) (i32.const 0))
    ((number n) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

; Check if token is rparen
(fn is-rparen ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

; Convert a single token to sexpr (for atoms)
(fn token-to-sexpr ((t token)) sexpr
  (match t
    ((lparen) (sym "error-lparen"))
    ((rparen) (sym "error-rparen"))
    ((number n) (num n))
    ((symbol s) (sym s))
    ((str-lit s) (str s))))

; Parse one atom (non-list) at position
(fn parse-atom ((tokens (list token)) (pos s32)) parse-result
  (let (tok (list-get tokens pos))
    (parse-result (token-to-sexpr tok) (i32.add pos (i32.const 1)))))

; Parse list items - separate function for clarity
(fn parse-list-items ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          ; Nested list
          (let (nested (parse-list-items tokens (i32.add pos (i32.const 1)) len (list-new sexpr)))
            (let (new-items (list-push items (parse-result.expr nested)))
              (parse-list-items tokens (parse-result.new-pos nested) len new-items)))
          ; Atom
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-list-items tokens (parse-result.new-pos atom) len new-items))))))))

; Parse one s-expression at position
(fn parse-one ((tokens (list token)) (pos s32) (len s32)) parse-result
  (if (i32.ge_s pos len)
    (parse-result (sym "error-eof") pos)
    (let (tok (list-get tokens pos))
      (if (is-lparen tok)
        (parse-list-items tokens (i32.add pos (i32.const 1)) len (list-new sexpr))
        (parse-atom tokens pos)))))

; Parse all s-expressions from tokens
(fn parse-all-acc ((tokens (list token)) (pos s32) (len s32) (exprs (list sexpr))) (list sexpr)
  (if (i32.ge_s pos len)
    exprs
    (let (result (parse-one tokens pos len))
      (let (new-exprs (list-push exprs (parse-result.expr result)))
        (parse-all-acc tokens (parse-result.new-pos result) len new-exprs)))))

; Parse all s-expressions from a token list
(fn parse-all ((tokens (list token))) (list sexpr)
  (parse-all-acc tokens (i32.const 0) (list-len tokens) (list-new sexpr)))

; Parse a single s-expression from tokens (convenience)
(fn parse ((tokens (list token))) sexpr
  (let (result (parse-one tokens (i32.const 0) (list-len tokens)))
    (parse-result.expr result)))

; Full pipeline: source string -> sexpr
(fn read-sexpr ((src string)) sexpr
  (parse (tokenize src)))

; Full pipeline: source string -> list of sexprs
(fn read-all ((src string)) (list sexpr)
  (parse-all (tokenize src)))

; ============================================================
; S-Expression Utilities
; ============================================================

; Check what kind of sexpr we have
(fn is-sym ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 1))
    ((num n) (i32.const 0))
    ((str s) (i32.const 0))
    ((lst l) (i32.const 0))))

(fn is-num ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 1))
    ((str s) (i32.const 0))
    ((lst l) (i32.const 0))))

(fn is-str ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((str s) (i32.const 1))
    ((lst l) (i32.const 0))))

(fn is-lst ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((str s) (i32.const 0))
    ((lst l) (i32.const 1))))

; Get symbol string (returns empty string if not a symbol)
(fn get-sym ((e sexpr)) string
  (match e
    ((sym s) s)
    ((num n) "")
    ((str s) "")
    ((lst l) "")))

; Get number value (returns 0 if not a number)
(fn get-num ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) n)
    ((str s) (i32.const 0))
    ((lst l) (i32.const 0))))

; Get string value (returns empty string if not a string)
(fn get-str ((e sexpr)) string
  (match e
    ((sym s) "")
    ((num n) "")
    ((str s) s)
    ((lst l) "")))

; Get list items (returns empty list if not a list)
(fn get-lst ((e sexpr)) (list sexpr)
  (match e
    ((sym s) (list-new sexpr))
    ((num n) (list-new sexpr))
    ((str s) (list-new sexpr))
    ((lst l) l)))

; Get length of list (0 for non-lists)
(fn sexpr-list-len ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((str s) (i32.const 0))
    ((lst l) (list-len l))))

; ============================================================
; Test Exports
; ============================================================

; Test: parse empty string
(export (fn test-parse-empty () s32
  (let (exprs (read-all ""))
    (list-len exprs))))  ; Expected: 0

; Test: parse single number
(export (fn test-parse-number () s32
  (let (e (read-sexpr "42"))
    (is-num e))))  ; Expected: 1

; Test: parse number value
(export (fn test-parse-number-value () s32
  (let (e (read-sexpr "42"))
    (get-num e))))  ; Expected: 42

; Test: parse single symbol
(export (fn test-parse-symbol () s32
  (let (e (read-sexpr "hello"))
    (is-sym e))))  ; Expected: 1

; Test: parse symbol name length
(export (fn test-parse-symbol-len () s32
  (let (e (read-sexpr "hello"))
    (string-len (get-sym e)))))  ; Expected: 5

; Test: parse string literal
(export (fn test-parse-string () s32
  (let (e (read-sexpr "\"hello\""))
    (is-str e))))  ; Expected: 1

; Test: parse string content length
(export (fn test-parse-string-len () s32
  (let (e (read-sexpr "\"hello\""))
    (string-len (get-str e)))))  ; Expected: 5

; Test: parse empty list
(export (fn test-parse-empty-list () s32
  (let (e (read-sexpr "()"))
    (is-lst e))))  ; Expected: 1

; Test: parse empty list length
(export (fn test-parse-empty-list-len () s32
  (let (e (read-sexpr "()"))
    (sexpr-list-len e))))  ; Expected: 0

; Test: parse simple list
(export (fn test-parse-simple-list () s32
  (let (e (read-sexpr "(a b c)"))
    (sexpr-list-len e))))  ; Expected: 3

; Test: parse list with numbers
(export (fn test-parse-list-numbers () s32
  (let (e (read-sexpr "(1 2 3)"))
    (sexpr-list-len e))))  ; Expected: 3

; Test: get first element of list
(export (fn test-parse-list-first () s32
  (let (e (read-sexpr "(42 1 2)"))
    (let (items (get-lst e))
      (let (first (list-get items (i32.const 0)))
        (get-num first))))))  ; Expected: 42

; Test: parse nested list
(export (fn test-parse-nested () s32
  (let (e (read-sexpr "(a (b c))"))
    (sexpr-list-len e))))  ; Expected: 2

; Test: check nested list is a list
(export (fn test-parse-nested-inner () s32
  (let (e (read-sexpr "(a (b c))"))
    (let (items (get-lst e))
      (let (second (list-get items (i32.const 1)))
        (is-lst second))))))  ; Expected: 1

; Test: nested list inner length
(export (fn test-parse-nested-inner-len () s32
  (let (e (read-sexpr "(a (b c))"))
    (let (items (get-lst e))
      (let (second (list-get items (i32.const 1)))
        (sexpr-list-len second))))))  ; Expected: 2

; Test: parse function-like expression
(export (fn test-parse-fn-expr () s32
  (let (e (read-sexpr "(i32.add 1 2)"))
    (sexpr-list-len e))))  ; Expected: 3

; Test: first element is symbol
(export (fn test-parse-fn-first-sym () s32
  (let (e (read-sexpr "(i32.add 1 2)"))
    (let (items (get-lst e))
      (let (first (list-get items (i32.const 0)))
        (is-sym first))))))  ; Expected: 1

; Test: parse multiple expressions
(export (fn test-parse-multiple () s32
  (let (exprs (read-all "1 2 3"))
    (list-len exprs))))  ; Expected: 3

; Test: parse multiple sexprs
(export (fn test-parse-multiple-sexprs () s32
  (let (exprs (read-all "(a) (b) (c)"))
    (list-len exprs))))  ; Expected: 3

; Test: deeply nested
(export (fn test-parse-deep-nested () s32
  (let (e (read-sexpr "(((a)))"))
    (let (l1 (get-lst e))
      (let (e2 (list-get l1 (i32.const 0)))
        (let (l2 (get-lst e2))
          (let (e3 (list-get l2 (i32.const 0)))
            (sexpr-list-len e3))))))))  ; Expected: 1

; Test: parse wisp-like function definition
(export (fn test-parse-fn-def () s32
  (let (e (read-sexpr "(fn add ((x s32)) s32 x)"))
    (sexpr-list-len e))))  ; Expected: 5: fn add ((x s32)) s32 x

; Test: negative number parsing
(export (fn test-parse-negative () s32
  (let (e (read-sexpr "-42"))
    (get-num e))))  ; Expected: -42

; Test: symbol with dots
(export (fn test-parse-dotted-symbol () s32
  (let (e (read-sexpr "i32.add"))
    (string-len (get-sym e)))))  ; Expected: 7
