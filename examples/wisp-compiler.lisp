; Wisp Compiler - written in Wisp
; Self-hosted compiler: source -> WAT
; Combines tokenizer, parser, and code generator

; ============================================================
; Token Type
; ============================================================

(variant token
  (lparen)
  (rparen)
  (number s32)
  (float-lit string)   ; Float literal stored as string (e.g., "3.14159")
  (symbol string)
  (str-lit string))

; ============================================================
; S-Expression AST Type
; ============================================================

(variant sexpr
  (sym string)
  (num s32)
  (fnum string)  ; Float number stored as string for pass-through to WAT
  (str string)
  (lst (list sexpr)))

; ============================================================
; Type Metadata (for dynamic variant/record support)
; ============================================================

; A single variant case: name, tag number, has-payload flag
(record variant-case
  (case-name string)
  (case-tag s32)
  (case-has-payload s32))

; A variant definition: name and list of cases
(record variant-def
  (var-name string)
  (var-cases (list variant-case)))

; A record field: name and byte offset
(record record-field
  (field-name string)
  (field-offset s32))

; A record definition: name and list of fields
(record record-def
  (rec-name string)
  (rec-fields (list record-field)))

; An import definition: module, function name, params, return type
(record import-def
  (imp-module string)
  (imp-name string)
  (imp-params (list sexpr))
  (imp-ret-type string))

; A variable's inferred type, for the lexical type environment.
(record vartype
  (vt-name string)
  (vt-type string))

; A function's return type, for inferring call-result types.
(record fnsig
  (fs-name string)
  (fs-ret string))

; Compilation context: type definitions, imports, the lexical var-type environment
; (seeded from fn params, extended at let), and the global function signature table.
(record compile-ctx
  (ctx-variants (list variant-def))
  (ctx-records (list record-def))
  (ctx-imports (list import-def))
  (ctx-vartypes (list vartype))
  (ctx-sigs (list fnsig)))

; ============================================================
; Tokenizer
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

(record token-result
  (tok token)
  (new-pos s32))

; Find the end position of a number (integer or float)
; Handles: digits, optional decimal point, more digits
(fn find-number-end ((src string) (pos s32) (len s32) (seen-dot s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-digit c)
        (find-number-end src (i32.add pos (i32.const 1)) len seen-dot)
        (if (i32.and (i32.eq c (i32.const 46)) (i32.eq seen-dot (i32.const 0)))
          (find-number-end src (i32.add pos (i32.const 1)) len (i32.const 1))
          pos)))))

; Check if substring contains a decimal point
(fn has-decimal ((src string) (start s32) (end s32)) s32
  (if (i32.ge_s start end)
    (i32.const 0)
    (if (i32.eq (string-ref src start) (i32.const 46))
      (i32.const 1)
      (has-decimal src (i32.add start (i32.const 1)) end))))

; Parse integer part of a number string. Numbers are represented as s32; literals
; wider than 32 bits are truncated (a known gap vs the Rust compiler's full s64).
(fn parse-int-value ((src string) (pos s32) (end s32) (acc s32)) s32
  (if (i32.ge_s pos end)
    acc
    (let (c (string-ref src pos))
      (if (is-digit c)
        (parse-int-value src (i32.add pos (i32.const 1)) end
          (i32.add (i32.mul acc (i32.const 10)) (digit-value c)))
        acc))))

; Read a number - returns either (number s32) or (float-lit string)
(fn read-number ((src string) (pos s32) (len s32)) token-result
  (let (end (find-number-end src pos len (i32.const 0)))
    (if (has-decimal src pos end)
      ; Float: return as string for pass-through to WAT
      (token-result (float-lit (substring src pos end)) end)
      ; Integer: parse and return as s32
      (token-result (number (parse-int-value src pos end (i32.const 0))) end))))

(fn find-symbol-end ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-delimiter c)
        pos
        (find-symbol-end src (i32.add pos (i32.const 1)) len)))))

(fn read-symbol ((src string) (pos s32) (len s32)) token-result
  (let (end (find-symbol-end src pos len))
    (token-result (symbol (substring src pos end)) end)))

; Find the end of a string literal (position after closing quote)
(fn find-string-end ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (i32.eq c (i32.const 34))  ; found closing "
        (i32.add pos (i32.const 1))  ; return position after "
        (if (i32.eq c (i32.const 92))  ; backslash escape
          (find-string-end src (i32.add pos (i32.const 2)) len)  ; skip next char
          (find-string-end src (i32.add pos (i32.const 1)) len))))))

; Decode one backslash escape: `pos` is the index of the char after the backslash.
; Matches the Rust tokenizer: \n \t \r \" \\ map to their bytes; any other escape
; keeps the backslash verbatim (2 chars). (\xHH is not needed -- it never appears in a
; string literal in this source, only in comments.)
(fn escape-char ((s string) (pos s32)) string
  (let (c (string-ref s pos))
    (if (i32.eq c (i32.const 110)) "\n"       ; n -> newline
      (if (i32.eq c (i32.const 116)) "\t"     ; t -> tab
        (if (i32.eq c (i32.const 114)) "\r"   ; r -> carriage return
          (if (i32.eq c (i32.const 34)) "\""  ; " -> quote
            (if (i32.eq c (i32.const 92)) "\\" ; \ -> backslash
              ; unknown escape: keep the backslash and the char
              (substring s (i32.sub pos (i32.const 1)) (i32.add pos (i32.const 1))))))))))

; Decode backslash escapes in s[i, len), flushing literal runs since run-start in bulk
; (so cost is O(escapes), not O(n^2)). Recurses once per char, like find-string-end.
(fn decode-str-lit ((s string) (i s32) (len s32) (run-start s32) (acc string)) string
  (if (i32.ge_s i len)
    (string-append acc (substring s run-start len))
    (if (i32.eq (string-ref s i) (i32.const 92))  ; backslash
      (decode-str-lit s (i32.add i (i32.const 2)) len (i32.add i (i32.const 2))
        (string-append
          (string-append acc (substring s run-start i))
          (escape-char s (i32.add i (i32.const 1)))))
      (decode-str-lit s (i32.add i (i32.const 1)) len run-start acc))))

; Read a string literal, starting after the opening quote
(fn read-string-lit ((src string) (pos s32) (len s32)) token-result
  (let (start (i32.add pos (i32.const 1)))  ; skip opening "
    (let (end-pos (find-string-end src start len))
      (let (str-end (i32.sub end-pos (i32.const 1)))  ; exclude closing "
        (token-result (str-lit (decode-str-lit src start str-end start "")) end-pos)))))

; Char at pos, or -1 if past the end.
(fn peek-char ((src string) (pos s32) (len s32)) s32
  (if (i32.lt_s pos len) (string-ref src pos) (i32.const -1)))

(fn read-token ((src string) (pos s32) (len s32)) token-result
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 40))
      (token-result (lparen) (i32.add pos (i32.const 1)))
    (if (i32.eq c (i32.const 41))
      (token-result (rparen) (i32.add pos (i32.const 1)))
    ; Reader macros: ` -> quasiquote, , -> unquote, ,@ -> unquote-splice. Emitted as
    ; reserved-name symbol tokens; the parser wraps the following form. (A leading `
    ; or , cannot start a normal symbol, so these names are unambiguous.)
    (if (i32.eq c (i32.const 96))  ; `
      (token-result (symbol "`") (i32.add pos (i32.const 1)))
    (if (i32.eq c (i32.const 44))  ; ,
      (if (i32.eq (peek-char src (i32.add pos (i32.const 1)) len) (i32.const 64))  ; ,@
        (token-result (symbol ",@") (i32.add pos (i32.const 2)))
        (token-result (symbol ",") (i32.add pos (i32.const 1))))
    (if (i32.eq c (i32.const 34))  ; " - string literal
      (read-string-lit src pos len)
    (if (is-digit c)
      (read-number src pos len)
      (if (i32.eq c (i32.const 45))
        (if (i32.lt_s (i32.add pos (i32.const 1)) len)
          (let (next-c (string-ref src (i32.add pos (i32.const 1))))
            (if (is-digit next-c)
              (let (result (read-number src (i32.add pos (i32.const 1)) len))
                (match (token-result.tok result)
                  ((number n) (token-result (number (i32.sub (i32.const 0) n)) (token-result.new-pos result)))
                  ((float-lit f) (token-result (float-lit (string-append "-" f)) (token-result.new-pos result)))
                  ((lparen) result)
                  ((rparen) result)
                  ((symbol s) result)
                  ((str-lit s) result)))
              (read-symbol src pos len)))
          (read-symbol src pos len))
        (read-symbol src pos len))))))))))

(fn skip-ws ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-whitespace c)
        (skip-ws src (i32.add pos (i32.const 1)) len)
        (if (i32.eq c (i32.const 59))
          (skip-ws src (skip-to-eol src pos len) len)
          pos)))))

(fn skip-to-eol ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (i32.eq c (i32.const 10))
        (i32.add pos (i32.const 1))
        (skip-to-eol src (i32.add pos (i32.const 1)) len)))))

(fn tokenize-acc ((src string) (pos s32) (len s32) (tokens (list token))) (list token)
  (let (pos2 (skip-ws src pos len))
    (if (i32.ge_s pos2 len)
      tokens
      (let (result (read-token src pos2 len))
        (tokenize-acc src (token-result.new-pos result) len
          (list-push tokens (token-result.tok result)))))))

(fn tokenize ((src string)) (list token)
  (tokenize-acc src (i32.const 0) (string-len src) (list-new token)))

; ============================================================
; Parser
; ============================================================

(record parse-result
  (expr sexpr)
  (new-pos s32))

(fn is-lparen ((t token)) s32
  (match t
    ((lparen) (i32.const 1))
    ((rparen) (i32.const 0))
    ((number n) (i32.const 0))
    ((float-lit f) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

(fn is-rparen ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 0))
    ((float-lit f) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

(fn token-to-sexpr ((t token)) sexpr
  (match t
    ((lparen) (sym "error-lparen"))
    ((rparen) (sym "error-rparen"))
    ((number n) (num n))
    ((float-lit f) (fnum f))
    ((symbol s) (sym s))
    ((str-lit s) (str s))))

(fn parse-atom ((tokens (list token)) (pos s32)) parse-result
  (parse-result (token-to-sexpr (list-get tokens pos)) (i32.add pos (i32.const 1))))

; Reader-prefix token kind: 0 = `(quasiquote), 1 = ,(unquote), 2 = ,@(unquote-splice),
; -1 = not a reader prefix. (These are the reserved-name symbol tokens from read-token.)
(fn reader-prefix-kind ((t token)) s32
  (match t
    ((lparen) (i32.const -1))
    ((rparen) (i32.const -1))
    ((number n) (i32.const -1))
    ((float-lit f) (i32.const -1))
    ((str-lit s) (i32.const -1))
    ((symbol s)
      (if (string=? s "`") (i32.const 0)
        (if (string=? s ",") (i32.const 1)
          (if (string=? s ",@") (i32.const 2)
            (i32.const -1)))))))

; Wrap the form following a reader prefix: `x -> (quasiquote x), etc.
(fn wrap-reader ((kind s32) (inner sexpr)) sexpr
  (let (head (if (i32.eq kind (i32.const 0)) "quasiquote"
               (if (i32.eq kind (i32.const 1)) "unquote" "unquote-splice")))
    (lst (list-push (list-push (list-new sexpr) (sym head)) inner))))

(fn parse-one ((tokens (list token)) (pos s32) (len s32)) parse-result
  (if (i32.ge_s pos len)
    (parse-result (sym "error-eof") pos)
    (let (tok (list-get tokens pos))
      (if (is-lparen tok)
        (parse-list-items tokens (i32.add pos (i32.const 1)) len (list-new sexpr))
        (let (pk (reader-prefix-kind tok))
          (if (i32.ge_s pk (i32.const 0))
            (let (inner (parse-one tokens (i32.add pos (i32.const 1)) len))
              (parse-result (wrap-reader pk (parse-result.expr inner)) (parse-result.new-pos inner)))
            (parse-atom tokens pos)))))))

(fn parse-list-items ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (let (one (parse-one tokens pos len))
          (parse-list-items tokens (parse-result.new-pos one) len
            (list-push items (parse-result.expr one))))))))

(fn parse-all-acc ((tokens (list token)) (pos s32) (len s32) (exprs (list sexpr))) (list sexpr)
  (if (i32.ge_s pos len)
    exprs
    (let (result (parse-one tokens pos len))
      (parse-all-acc tokens (parse-result.new-pos result) len
        (list-push exprs (parse-result.expr result))))))

(fn parse-all ((tokens (list token))) (list sexpr)
  (parse-all-acc tokens (i32.const 0) (list-len tokens) (list-new sexpr)))

(fn read-all ((src string)) (list sexpr)
  (parse-all (tokenize src)))

; ============================================================
; S-Expression Utilities
; ============================================================

(fn is-sym ((e sexpr)) s32
  (match e ((sym s) (i32.const 1)) ((num n) (i32.const 0)) ((fnum f) (i32.const 0)) ((str s) (i32.const 0)) ((lst l) (i32.const 0))))

(fn is-num ((e sexpr)) s32
  (match e ((sym s) (i32.const 0)) ((num n) (i32.const 1)) ((fnum f) (i32.const 0)) ((str s) (i32.const 0)) ((lst l) (i32.const 0))))

(fn is-fnum ((e sexpr)) s32
  (match e ((sym s) (i32.const 0)) ((num n) (i32.const 0)) ((fnum f) (i32.const 1)) ((str s) (i32.const 0)) ((lst l) (i32.const 0))))

(fn is-lst ((e sexpr)) s32
  (match e ((sym s) (i32.const 0)) ((num n) (i32.const 0)) ((fnum f) (i32.const 0)) ((str s) (i32.const 0)) ((lst l) (i32.const 1))))

(fn is-str ((e sexpr)) s32
  (match e ((sym s) (i32.const 0)) ((num n) (i32.const 0)) ((fnum f) (i32.const 0)) ((str s) (i32.const 1)) ((lst l) (i32.const 0))))

(fn get-sym ((e sexpr)) string
  (match e ((sym s) s) ((num n) "") ((fnum f) "") ((str s) "") ((lst l) "")))

; Return the numeric value of a (num ...) sexpr.
(fn get-num ((e sexpr)) s32
  (match e ((sym s) (i32.const 0)) ((num n) n) ((fnum f) (i32.const 0)) ((str s) (i32.const 0)) ((lst l) (i32.const 0))))

; Get float number string
(fn get-fnum ((e sexpr)) string
  (match e ((sym s) "") ((num n) "") ((fnum f) f) ((str s) "") ((lst l) "")))

(fn get-str ((e sexpr)) string
  (match e ((sym s) "") ((num n) "") ((fnum f) "") ((str s) s) ((lst l) "")))

(fn get-lst ((e sexpr)) (list sexpr)
  (match e ((sym s) (list-new sexpr)) ((num n) (list-new sexpr)) ((fnum f) (list-new sexpr)) ((str s) (list-new sexpr)) ((lst l) l)))

; ============================================================
; Type Definition Collection
; ============================================================

; Parse a single variant case from sexpr: (case-name) or (case-name type)
(fn parse-variant-case ((case-expr sexpr) (tag s32)) variant-case
  (if (is-lst case-expr)
    (let (items (get-lst case-expr))
      (if (i32.gt_s (list-len items) (i32.const 0))
        (let (name-expr (list-get items (i32.const 0)))
          (if (is-sym name-expr)
            (let (case-name (get-sym name-expr))
              (let (has-payload (if (i32.gt_s (list-len items) (i32.const 1)) (i32.const 1) (i32.const 0)))
                (variant-case case-name tag has-payload)))
            (variant-case "error" tag (i32.const 0))))
        (variant-case "error" tag (i32.const 0))))
    (variant-case "error" tag (i32.const 0))))

; Parse variant cases starting at index
(fn parse-variant-cases ((items (list sexpr)) (idx s32) (len s32) (tag s32) (cases (list variant-case))) (list variant-case)
  (if (i32.ge_s idx len)
    cases
    (let (case-expr (list-get items idx))
      (let (parsed-case (parse-variant-case case-expr tag))
        (parse-variant-cases items (i32.add idx (i32.const 1)) len (i32.add tag (i32.const 1))
          (list-push cases parsed-case))))))

; Parse a variant declaration: (variant name (case1) (case2 type) ...)
(fn parse-variant-def ((items (list sexpr))) variant-def
  (if (i32.lt_s (list-len items) (i32.const 2))
    (variant-def "error" (list-new variant-case))
    (let (name-expr (list-get items (i32.const 1)))
      (if (is-sym name-expr)
        (let (var-name (get-sym name-expr))
          (let (cases (parse-variant-cases items (i32.const 2) (list-len items) (i32.const 0) (list-new variant-case)))
            (variant-def var-name cases)))
        (variant-def "error" (list-new variant-case))))))

; Parse a single record field from sexpr: (field-name type)
(fn parse-record-field ((field-expr sexpr) (offset s32)) record-field
  (if (is-lst field-expr)
    (let (items (get-lst field-expr))
      (if (i32.gt_s (list-len items) (i32.const 0))
        (let (name-expr (list-get items (i32.const 0)))
          (if (is-sym name-expr)
            (record-field (get-sym name-expr) offset)
            (record-field "error" offset)))
        (record-field "error" offset)))
    (record-field "error" offset)))

; Parse record fields starting at index
(fn parse-record-fields ((items (list sexpr)) (idx s32) (len s32) (offset s32) (fields (list record-field))) (list record-field)
  (if (i32.ge_s idx len)
    fields
    (let (field-expr (list-get items idx))
      (let (parsed-field (parse-record-field field-expr offset))
        (parse-record-fields items (i32.add idx (i32.const 1)) len (i32.add offset (i32.const 4))
          (list-push fields parsed-field))))))

; Parse a record declaration: (record name (field1 type1) (field2 type2) ...)
(fn parse-record-def ((items (list sexpr))) record-def
  (if (i32.lt_s (list-len items) (i32.const 2))
    (record-def "error" (list-new record-field))
    (let (name-expr (list-get items (i32.const 1)))
      (if (is-sym name-expr)
        (let (rec-name (get-sym name-expr))
          (let (fields (parse-record-fields items (i32.const 2) (list-len items) (i32.const 0) (list-new record-field)))
            (record-def rec-name fields)))
        (record-def "error" (list-new record-field))))))

; Collect all variant definitions from forms
(fn collect-variants-acc ((forms (list sexpr)) (idx s32) (len s32) (variants (list variant-def))) (list variant-def)
  (if (i32.ge_s idx len)
    variants
    (let (form (list-get forms idx))
      (if (is-lst form)
        (let (items (get-lst form))
          (if (i32.gt_s (list-len items) (i32.const 0))
            (let (head (list-get items (i32.const 0)))
              (if (is-sym head)
                (if (string=? (get-sym head) "variant")
                  (collect-variants-acc forms (i32.add idx (i32.const 1)) len
                    (list-push variants (parse-variant-def items)))
                  (collect-variants-acc forms (i32.add idx (i32.const 1)) len variants))
                (collect-variants-acc forms (i32.add idx (i32.const 1)) len variants)))
            (collect-variants-acc forms (i32.add idx (i32.const 1)) len variants)))
        (collect-variants-acc forms (i32.add idx (i32.const 1)) len variants)))))

(fn collect-variants ((forms (list sexpr))) (list variant-def)
  (collect-variants-acc forms (i32.const 0) (list-len forms) (list-new variant-def)))

; Prepend the built-in parametric variants (option, result) so some/none/ok/err and
; matching on them work with no explicit (variant ...) declaration. Tags/layout match
; the Rust compiler: option = [none=0, some=1], result = [ok=0, err=1]; payload (if any)
; lives at +4. User-declared variants come first in the list, so a user may shadow these.
(fn add-builtin-variants ((base (list variant-def))) (list variant-def)
  (let (opt-c0 (list-push (list-new variant-case) (variant-case "none" (i32.const 0) (i32.const 0))))
    (let (opt-cases (list-push opt-c0 (variant-case "some" (i32.const 1) (i32.const 1))))
      (let (res-c0 (list-push (list-new variant-case) (variant-case "ok" (i32.const 0) (i32.const 1))))
        (let (res-cases (list-push res-c0 (variant-case "err" (i32.const 1) (i32.const 1))))
          (let (with-opt (list-push base (variant-def "option" opt-cases)))
            (list-push with-opt (variant-def "result" res-cases))))))))

; Collect all record definitions from forms
(fn collect-records-acc ((forms (list sexpr)) (idx s32) (len s32) (records (list record-def))) (list record-def)
  (if (i32.ge_s idx len)
    records
    (let (form (list-get forms idx))
      (if (is-lst form)
        (let (items (get-lst form))
          (if (i32.gt_s (list-len items) (i32.const 0))
            (let (head (list-get items (i32.const 0)))
              (if (is-sym head)
                (if (string=? (get-sym head) "record")
                  (collect-records-acc forms (i32.add idx (i32.const 1)) len
                    (list-push records (parse-record-def items)))
                  (collect-records-acc forms (i32.add idx (i32.const 1)) len records))
                (collect-records-acc forms (i32.add idx (i32.const 1)) len records)))
            (collect-records-acc forms (i32.add idx (i32.const 1)) len records)))
        (collect-records-acc forms (i32.add idx (i32.const 1)) len records)))))

(fn collect-records ((forms (list sexpr))) (list record-def)
  (collect-records-acc forms (i32.const 0) (list-len forms) (list-new record-def)))

; Collect all import definitions from forms
; Import syntax: (import module-name func-name ((param type) ...) ret-type)
(fn parse-import-def ((items (list sexpr))) import-def
  (if (i32.lt_s (list-len items) (i32.const 5))
    (import-def "" "" (list-new sexpr) "")
    (let (mod-expr (list-get items (i32.const 1)))
      (let (name-expr (list-get items (i32.const 2)))
        (let (params-expr (list-get items (i32.const 3)))
          (let (ret-expr (list-get items (i32.const 4)))
            (if (is-sym mod-expr)
              (if (is-sym name-expr)
                (if (is-lst params-expr)
                  (if (is-sym ret-expr)
                    (import-def (get-sym mod-expr) (get-sym name-expr) (get-lst params-expr) (get-sym ret-expr))
                    (import-def "" "" (list-new sexpr) ""))
                  (import-def "" "" (list-new sexpr) ""))
                (import-def "" "" (list-new sexpr) ""))
              (import-def "" "" (list-new sexpr) ""))))))))

(fn collect-imports-acc ((forms (list sexpr)) (idx s32) (len s32) (imports (list import-def))) (list import-def)
  (if (i32.ge_s idx len)
    imports
    (let (form (list-get forms idx))
      (if (is-lst form)
        (let (items (get-lst form))
          (if (i32.gt_s (list-len items) (i32.const 0))
            (let (head (list-get items (i32.const 0)))
              (if (is-sym head)
                (if (string=? (get-sym head) "import")
                  (collect-imports-acc forms (i32.add idx (i32.const 1)) len
                    (list-push imports (parse-import-def items)))
                  (collect-imports-acc forms (i32.add idx (i32.const 1)) len imports))
                (collect-imports-acc forms (i32.add idx (i32.const 1)) len imports)))
            (collect-imports-acc forms (i32.add idx (i32.const 1)) len imports)))
        (collect-imports-acc forms (i32.add idx (i32.const 1)) len imports)))))

(fn collect-imports ((forms (list sexpr))) (list import-def)
  (collect-imports-acc forms (i32.const 0) (list-len forms) (list-new import-def)))

; ============================================================
; Type Lookup Functions
; ============================================================

; Find a variant case by name in a list of cases
(fn find-case-in-list ((cases (list variant-case)) (idx s32) (len s32) (name string)) variant-case
  (if (i32.ge_s idx len)
    (variant-case "" (i32.const -1) (i32.const 0))  ; not found
    (let (c (list-get cases idx))
      (if (string=? (variant-case.case-name c) name)
        c
        (find-case-in-list cases (i32.add idx (i32.const 1)) len name)))))

; Find a variant case across all variants
(fn find-case-in-variants ((variants (list variant-def)) (idx s32) (len s32) (name string)) variant-case
  (if (i32.ge_s idx len)
    (variant-case "" (i32.const -1) (i32.const 0))  ; not found
    (let (v (list-get variants idx))
      (let (cases (variant-def.var-cases v))
        (let (found (find-case-in-list cases (i32.const 0) (list-len cases) name))
          (if (i32.ge_s (variant-case.case-tag found) (i32.const 0))
            found
            (find-case-in-variants variants (i32.add idx (i32.const 1)) len name)))))))

; Find a record by name
(fn find-record ((records (list record-def)) (idx s32) (len s32) (name string)) record-def
  (if (i32.ge_s idx len)
    (record-def "" (list-new record-field))  ; not found
    (let (r (list-get records idx))
      (if (string=? (record-def.rec-name r) name)
        r
        (find-record records (i32.add idx (i32.const 1)) len name)))))

; Find a field in a record
(fn find-field ((fields (list record-field)) (idx s32) (len s32) (name string)) record-field
  (if (i32.ge_s idx len)
    (record-field "" (i32.const -1))  ; not found
    (let (f (list-get fields idx))
      (if (string=? (record-field.field-name f) name)
        f
        (find-field fields (i32.add idx (i32.const 1)) len name)))))

; Check if a name contains a dot (for field accessor like "record.field")
(fn contains-dot ((s string) (idx s32) (len s32)) s32
  (if (i32.ge_s idx len)
    (i32.const 0)
    (if (i32.eq (string-ref s idx) (i32.const 46))  ; 46 = '.'
      (i32.const 1)
      (contains-dot s (i32.add idx (i32.const 1)) len))))

(fn has-dot ((s string)) s32
  (contains-dot s (i32.const 0) (string-len s)))

; Find the position of the dot
(fn find-dot-pos ((s string) (idx s32) (len s32)) s32
  (if (i32.ge_s idx len)
    (i32.const -1)
    (if (i32.eq (string-ref s idx) (i32.const 46))
      idx
      (find-dot-pos s (i32.add idx (i32.const 1)) len))))

; Get the part before the dot
(fn get-before-dot ((s string)) string
  (let (pos (find-dot-pos s (i32.const 0) (string-len s)))
    (if (i32.lt_s pos (i32.const 0))
      s
      (substring s (i32.const 0) pos))))

; Get the part after the dot
(fn get-after-dot ((s string)) string
  (let (pos (find-dot-pos s (i32.const 0) (string-len s)))
    (if (i32.lt_s pos (i32.const 0))
      ""
      (substring s (i32.add pos (i32.const 1)) (string-len s)))))

; ============================================================
; Number to String
; ============================================================

(fn digit-to-string ((d s32)) string
  (if (i32.eq d (i32.const 0)) "0"
    (if (i32.eq d (i32.const 1)) "1"
      (if (i32.eq d (i32.const 2)) "2"
        (if (i32.eq d (i32.const 3)) "3"
          (if (i32.eq d (i32.const 4)) "4"
            (if (i32.eq d (i32.const 5)) "5"
              (if (i32.eq d (i32.const 6)) "6"
                (if (i32.eq d (i32.const 7)) "7"
                  (if (i32.eq d (i32.const 8)) "8"
                    "9"))))))))))

(fn i32-to-string-pos ((n s32) (acc string)) string
  (if (i32.eq n (i32.const 0))
    acc
    (let (digit (i32.rem_s n (i32.const 10)))
      (let (rest (i32.div_s n (i32.const 10)))
        (i32-to-string-pos rest (string-append (digit-to-string digit) acc))))))

(fn i32-to-string ((n s32)) string
  (if (i32.eq n (i32.const 0))
    "0"
    (if (i32.lt_s n (i32.const 0))
      (string-append "-" (i32-to-string-pos (i32.sub (i32.const 0) n) ""))
      (i32-to-string-pos n ""))))

; ============================================================
; Code Generator
; ============================================================

(fn is-wasm-instr ((s string)) s32
  (if (i32.lt_s (string-len s) (i32.const 4))
    (i32.const 0)
    (let (prefix (substring s (i32.const 0) (i32.const 4)))
      (if (string=? prefix "i32.") (i32.const 1)
        (if (string=? prefix "i64.") (i32.const 1)
          (if (string=? prefix "f32.") (i32.const 1)
            (if (string=? prefix "f64.") (i32.const 1)
              (i32.const 0))))))))

; Get variant constructor tag number using context
(fn constructor-tag-ctx ((ctx compile-ctx) (name string)) s32
  (let (variants (compile-ctx.ctx-variants ctx))
    (let (found (find-case-in-variants variants (i32.const 0) (list-len variants) name))
      (variant-case.case-tag found))))

; Check if a name is a variant constructor
(fn is-variant-constructor ((ctx compile-ctx) (name string)) s32
  (let (tag (constructor-tag-ctx ctx name))
    (if (i32.ge_s tag (i32.const 0)) (i32.const 1) (i32.const 0))))

; Get whether a variant constructor has a payload
(fn constructor-has-payload ((ctx compile-ctx) (name string)) s32
  (let (variants (compile-ctx.ctx-variants ctx))
    (let (found (find-case-in-variants variants (i32.const 0) (list-len variants) name))
      (variant-case.case-has-payload found))))

; Check if a name is a record constructor
(fn is-record-constructor ((ctx compile-ctx) (name string)) s32
  (let (records (compile-ctx.ctx-records ctx))
    (let (found (find-record records (i32.const 0) (list-len records) name))
      (if (i32.gt_s (string-len (record-def.rec-name found)) (i32.const 0))
        (i32.const 1)
        (i32.const 0)))))

; Get record definition by name
(fn get-record-def ((ctx compile-ctx) (name string)) record-def
  (let (records (compile-ctx.ctx-records ctx))
    (find-record records (i32.const 0) (list-len records) name)))

; Check if a name is a field accessor (record-name.field-name)
(fn is-field-accessor ((ctx compile-ctx) (name string)) s32
  (if (has-dot name)
    (let (rec-name (get-before-dot name))
      (let (field-name (get-after-dot name))
        (let (records (compile-ctx.ctx-records ctx))
          (let (rec (find-record records (i32.const 0) (list-len records) rec-name))
            (if (i32.gt_s (string-len (record-def.rec-name rec)) (i32.const 0))
              (let (fields (record-def.rec-fields rec))
                (let (f (find-field fields (i32.const 0) (list-len fields) field-name))
                  (if (i32.ge_s (record-field.field-offset f) (i32.const 0))
                    (i32.const 1)
                    (i32.const 0))))
              (i32.const 0))))))
    (i32.const 0)))

; Get field offset for a field accessor
(fn get-field-offset ((ctx compile-ctx) (name string)) s32
  (let (rec-name (get-before-dot name))
    (let (field-name (get-after-dot name))
      (let (records (compile-ctx.ctx-records ctx))
        (let (rec (find-record records (i32.const 0) (list-len records) rec-name))
          (let (fields (record-def.rec-fields rec))
            (let (f (find-field fields (i32.const 0) (list-len fields) field-name))
              (record-field.field-offset f))))))))

; Legacy fallback for hardcoded tags (kept for compatibility)
(fn constructor-tag ((name string)) s32
  (if (string=? name "lparen") (i32.const 0)
    (if (string=? name "rparen") (i32.const 1)
      (if (string=? name "number") (i32.const 2)
        (if (string=? name "symbol") (i32.const 3)
          (if (string=? name "str-lit") (i32.const 4)
            (if (string=? name "sym") (i32.const 0)
              (if (string=? name "num") (i32.const 1)
                (if (string=? name "str") (i32.const 2)
                  (if (string=? name "lst") (i32.const 3)
                    (i32.const -1)))))))))))

; Compile a bare number literal. Bare integers default to s32.
(fn compile-number ((n s32)) string
  (string-append "(i32.const " (string-append (i32-to-string n) ")")))

(fn compile-var ((name string)) string
  (string-append "(local.get $" (string-append name ")")))

; Generate code to store bytes of a string starting at offset
; Returns WAT code that stores bytes at (heap_ptr + offset)
; Uses tail recursion with accumulator to avoid stack overflow on long strings
; Emit the WAT that stores one byte of a string literal at (heap_ptr + offset).
(fn emit-string-byte ((byte s32) (offset s32)) string
  (string-append
    "global.get $__heap_ptr i32.const " (string-append
    (i32-to-string offset) (string-append
    " i32.add i32.const " (string-append
    (i32-to-string byte)
    " i32.store8 ")))))

; Emit store8 instructions for bytes [idx, end) of s, joining halves with
; divide-and-conquer so the output is assembled with balanced string-append
; (O(N log N) allocation) instead of a linear left-fold (O(N^2) -- which, at
; ~40 WAT bytes per source byte, blew past 1 GB on the 5 KB runtime literal).
; Stack depth is O(log N), so long literals do not overflow either.
(fn compile-string-bytes ((s string) (idx s32) (end s32) (offset s32)) string
  (let (n (i32.sub end idx))
    (if (i32.le_s n (i32.const 0))
      ""
      (if (i32.eq n (i32.const 1))
        (emit-string-byte (string-ref s idx) offset)
        (let (half (i32.div_s n (i32.const 2)))
          (let (mid (i32.add idx half))
            (string-append
              (compile-string-bytes s idx mid offset)
              (compile-string-bytes s mid end (i32.add offset half)))))))))

; Compile a string literal to WAT
; String layout: 4 bytes length + bytes
(fn compile-string ((s string)) string
  (let (len (string-len s))
    (let (total-size (i32.add (i32.const 4) len))
      ; Generate code that:
      ; 1. Pushes heap_ptr (the result pointer) on stack
      ; 2. Stores length at heap_ptr
      ; 3. Stores each byte
      ; 4. Updates heap_ptr
      (string-append
        "global.get $__heap_ptr "
        (string-append
          "global.get $__heap_ptr i32.const " (string-append
          (i32-to-string len) (string-append
          " i32.store " (string-append
          (compile-string-bytes s (i32.const 0) len (i32.const 4)) (string-append
          "global.get $__heap_ptr i32.const " (string-append
          (i32-to-string total-size)
          " i32.add global.set $__heap_ptr"))))))))))

(fn compile-args ((args (list sexpr)) (idx s32) (len s32) (acc string) (ctx compile-ctx)) string
  (if (i32.ge_s idx len)
    acc
    (let (arg (list-get args idx))
      (let (compiled (compile-expr arg ctx (i32.const 0)))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-args args (i32.add idx (i32.const 1)) len new-acc ctx))))))

; Check if instruction is a const (i32.const, i64.const, etc.)
(fn is-const-instr ((s string)) s32
  (if (string=? s "i32.const") (i32.const 1)
    (if (string=? s "i64.const") (i32.const 1)
      (if (string=? s "f32.const") (i32.const 1)
        (if (string=? s "f64.const") (i32.const 1)
          (i32.const 0))))))

(fn compile-wasm-call ((instr string) (args (list sexpr)) (ctx compile-ctx)) string
  (if (i32.eq (list-len args) (i32.const 0))
    (string-append "(" (string-append instr ")"))
    ; For const instructions, use the literal value directly
    (if (is-const-instr instr)
      (let (arg (list-get args (i32.const 0)))
        (if (is-num arg)
          (string-append "(" (string-append instr (string-append " " (string-append (i32-to-string (get-num arg)) ")"))))
          (if (is-fnum arg)
            ; Float literal - use string directly
            (string-append "(" (string-append instr (string-append " " (string-append (get-fnum arg) ")"))))
            (string-append "(" (string-append instr " (error: const expects number or float)")))))
      (let (compiled-args (compile-args args (i32.const 0) (list-len args) "" ctx))
        (string-append "(" (string-append instr (string-append " " (string-append compiled-args ")"))))))))

(fn compile-fn-call ((name string) (args (list sexpr)) (ctx compile-ctx) (is-tail s32)) string
  (let (call-instr (if (i32.eq is-tail (i32.const 1)) "return_call" "call"))
    (if (i32.eq (list-len args) (i32.const 0))
      (string-append "(" (string-append call-instr (string-append " $" (string-append name ")"))))
      (let (compiled-args (compile-args args (i32.const 0) (list-len args) "" ctx))
        (string-append "(" (string-append call-instr (string-append " $" (string-append name
          (string-append " " (string-append compiled-args ")"))))))))))


(fn build-args-list ((items (list sexpr)) (start s32) (len s32) (acc (list sexpr))) (list sexpr)
  (if (i32.le_s len (i32.const 0))
    acc
    (let (item (list-get items start))
      (build-args-list items (i32.add start (i32.const 1)) (i32.sub len (i32.const 1))
        (list-push acc item)))))

; Compile record constructor: (name field1 field2 ...)
(fn compile-record-construct ((ctx compile-ctx) (name string) (items (list sexpr))) string
  (let (rec (get-record-def ctx name))
    (let (fields (record-def.rec-fields rec))
      (let (num-fields (list-len fields))
        (let (compiled-fields (compile-record-fields items ctx (i32.const 1) num-fields ""))
          (string-append "(call $__make_record_"
            (string-append (i32-to-string num-fields)
              (string-append " " (string-append compiled-fields ")")))))))))

(fn compile-record-fields ((items (list sexpr)) (ctx compile-ctx) (idx s32) (remaining s32) (acc string)) string
  (if (i32.le_s remaining (i32.const 0))
    acc
    (let (field-wat (compile-expr (list-get items idx) ctx (i32.const 0)))
      (let (new-acc (if (i32.eq (string-len acc) (i32.const 0))
                      field-wat
                      (string-append acc (string-append " " field-wat))))
        (compile-record-fields items ctx (i32.add idx (i32.const 1)) (i32.sub remaining (i32.const 1)) new-acc)))))

; Compile record constructor within match body (uses compile-expr-sub)
(fn compile-record-construct-sub ((ctx compile-ctx) (name string) (items (list sexpr)) (binding-name string) (scrutinee-wat string)) string
  (let (rec (get-record-def ctx name))
    (let (fields (record-def.rec-fields rec))
      (let (num-fields (list-len fields))
        (let (compiled-fields (compile-record-fields-sub items ctx (i32.const 1) num-fields "" binding-name scrutinee-wat))
          (string-append "(call $__make_record_"
            (string-append (i32-to-string num-fields)
              (string-append " " (string-append compiled-fields ")")))))))))

(fn compile-record-fields-sub ((items (list sexpr)) (ctx compile-ctx) (idx s32) (remaining s32) (acc string) (binding-name string) (scrutinee-wat string)) string
  (if (i32.le_s remaining (i32.const 0))
    acc
    (let (field-wat (compile-expr-sub (list-get items idx) binding-name scrutinee-wat ctx (i32.const 0)))
      (let (new-acc (if (i32.eq (string-len acc) (i32.const 0))
                      field-wat
                      (string-append acc (string-append " " field-wat))))
        (compile-record-fields-sub items ctx (i32.add idx (i32.const 1)) (i32.sub remaining (i32.const 1)) new-acc binding-name scrutinee-wat)))))

; Tuple construction: (tuple v1 ... vN) -> a heap record of N positional fields (field i
; at +4*i), emitted as a call to the runtime's $__make_record_N. Matches the Rust
; compiler: tuples carry no type annotations and are construct-only (no element access or
; tuple match pattern). N must be 1..5 (the record makers the runtime provides).
(fn compile-tuple-args ((items (list sexpr)) (idx s32) (len s32) (acc string) (ctx compile-ctx)) string
  (if (i32.ge_s idx len)
    acc
    (compile-tuple-args items (i32.add idx (i32.const 1)) len
      (string-append acc (string-append " " (compile-expr (list-get items idx) ctx (i32.const 0)))) ctx)))

(fn compile-tuple ((items (list sexpr)) (ctx compile-ctx)) string
  (let (n (i32.sub (list-len items) (i32.const 1)))
    (string-append "(call $__make_record_"
      (string-append (i32-to-string n)
        (string-append (compile-tuple-args items (i32.const 1) (list-len items) "" ctx) ")")))))

; Same, inside a match arm body (threads the binding substitution).
(fn compile-tuple-args-sub ((items (list sexpr)) (idx s32) (len s32) (binding-name string) (scrutinee-wat string) (acc string) (ctx compile-ctx)) string
  (if (i32.ge_s idx len)
    acc
    (compile-tuple-args-sub items (i32.add idx (i32.const 1)) len binding-name scrutinee-wat
      (string-append acc (string-append " " (compile-expr-sub (list-get items idx) binding-name scrutinee-wat ctx (i32.const 0)))) ctx)))

(fn compile-tuple-sub ((items (list sexpr)) (binding-name string) (scrutinee-wat string) (ctx compile-ctx)) string
  (let (n (i32.sub (list-len items) (i32.const 1)))
    (string-append "(call $__make_record_"
      (string-append (i32-to-string n)
        (string-append (compile-tuple-args-sub items (i32.const 1) (list-len items) binding-name scrutinee-wat "" ctx) ")")))))

; Compile expression with optional variable substitution for match bindings
; If binding-name is non-empty and expr is a sym matching it, emit payload load
(fn compile-expr-sub ((expr sexpr) (binding-name string) (scrutinee-wat string) (ctx compile-ctx) (is-tail s32)) string
  (match expr
    ((num n) (compile-number n))
    ((fnum f) (string-append "(f64.const " (string-append f ")")))  ; Default to f64 for bare float literals
    ((sym s)
      (if (string=? binding-name "")
        (compile-var s)
        (if (string=? s binding-name)
          (string-append "(i32.load (i32.add " (string-append scrutinee-wat " (i32.const 4)))"))
          (compile-var s))))
    ((str s) (compile-string s))
    ((lst items) (compile-list-sub items binding-name scrutinee-wat ctx is-tail))))

; Compile list expression with substitution context
(fn compile-list-sub ((items (list sexpr)) (binding-name string) (scrutinee-wat string) (ctx compile-ctx) (is-tail s32)) string
  (if (i32.eq (list-len items) (i32.const 0))
    "()"
    (let (head (list-get items (i32.const 0)))
      (if (is-sym head)
        (let (name (get-sym head))
          ; Control forms (if/let/match) can nest inside match arms too
          (if (i32.eq (is-ctrl-form name) (i32.const 1))
            (compile-ctrl-sub items name binding-name scrutinee-wat ctx is-tail)
          ; Context-aware variant constructor (in match body)
          (if (is-variant-constructor ctx name)
            (if (i32.eq (constructor-has-payload ctx name) (i32.const 0))
              (string-append "(call $__make_variant_0 (i32.const " (string-append (i32-to-string (constructor-tag-ctx ctx name)) "))"))
              ; Payload is the LAST arg (see compile-expr): ignores option/result type annotations.
              (let (payload-wat (compile-expr-sub (list-get items (i32.sub (list-len items) (i32.const 1))) binding-name scrutinee-wat ctx (i32.const 0)))
                (string-append "(call $__make_variant_1 (i32.const " (string-append (i32-to-string (constructor-tag-ctx ctx name)) (string-append ") " (string-append payload-wat ")"))))))
            ; Context-aware record constructor (in match body)
            (if (is-record-constructor ctx name)
              (compile-record-construct-sub ctx name items binding-name scrutinee-wat)
              ; Context-aware field accessor (in match body)
              (if (is-field-accessor ctx name)
                (let (offset (get-field-offset ctx name))
                  (let (rec-wat (compile-expr-sub (list-get items (i32.const 1)) binding-name scrutinee-wat ctx (i32.const 0)))
                    (if (i32.eq offset (i32.const 0))
                      (string-append "(i32.load " (string-append rec-wat ")"))
                      (string-append "(i32.load (i32.add " (string-append rec-wat (string-append " (i32.const " (string-append (i32-to-string offset) ")))")))))))
                ; String/list builtins (also valid inside match arms)
                (if (i32.eq (is-builtin-call name) (i32.const 1))
                  (compile-builtin-sub name items binding-name scrutinee-wat ctx)
                ; WASM instruction or function call
                (if (i32.eq (is-begin-or-global name) (i32.const 1))
                  (compile-begin-or-global-sub name items binding-name scrutinee-wat ctx is-tail)
                (if (is-wasm-instr name)
                  (compile-wasm-call-sub name items binding-name scrutinee-wat ctx)
                  (if (string=? name "tuple")
                    (compile-tuple-sub items binding-name scrutinee-wat ctx)
                    (if (i32.eq (is-cast-head name) (i32.const 1))
                      (compile-cast-sub name (list-get items (i32.const 1)) binding-name scrutinee-wat ctx)
                      (compile-fn-call-sub name items binding-name scrutinee-wat ctx is-tail)))))))))))
        "(error)"))))

; Builtins that take compiled sub-expression args and appear in match arm bodies.
(fn is-builtin-call ((name string)) s32
  (if (string=? name "string-append") (i32.const 1)
    (if (string=? name "string=?") (i32.const 1)
      (if (string=? name "substring") (i32.const 1)
        (if (string=? name "string-len") (i32.const 1)
          (if (string=? name "string-ref") (i32.const 1)
            (if (string=? name "list-len") (i32.const 1)
              (if (string=? name "list-get") (i32.const 1)
                (if (string=? name "list-push") (i32.const 1)
                  (if (string=? name "list-new") (i32.const 1)
                    (i32.const 0)))))))))))

; Compile a string/list builtin call inside a match arm (args via compile-expr-sub).
(fn compile-builtin-sub ((name string) (items (list sexpr)) (binding-name string) (scrutinee-wat string) (ctx compile-ctx)) string
  (if (string=? name "list-new")
    "(call $__list_new)"
    (if (string=? name "string-len")
      (string-append "(i32.load " (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx) ")"))
      (if (string=? name "list-len")
        (string-append "(i32.load " (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx) ")"))
        (if (string=? name "string-ref")
          (string-append "(i32.load8_u (i32.add (i32.add " (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx)
            (string-append " (i32.const 4)) " (string-append (cbsub items (i32.const 2) binding-name scrutinee-wat ctx) "))"))))
          (if (string=? name "list-get")
            (string-append "(i32.load (i32.add (i32.load (i32.add " (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx)
              (string-append " (i32.const 8))) (i32.mul " (string-append (cbsub items (i32.const 2) binding-name scrutinee-wat ctx) " (i32.const 4))))"))))
            (if (string=? name "string-append")
              (string-append "(call $__string_append " (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx)
                (string-append " " (string-append (cbsub items (i32.const 2) binding-name scrutinee-wat ctx) ")"))))
              (if (string=? name "string=?")
                (string-append "(call $__string_eq " (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx)
                  (string-append " " (string-append (cbsub items (i32.const 2) binding-name scrutinee-wat ctx) ")"))))
                (if (string=? name "list-push")
                  (string-append "(call $__list_push " (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx)
                    (string-append " " (string-append (cbsub items (i32.const 2) binding-name scrutinee-wat ctx) ")"))))
                  ; substring
                  (string-append "(call $__substring " (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx)
                    (string-append " " (string-append (cbsub items (i32.const 2) binding-name scrutinee-wat ctx)
                      (string-append " " (string-append (cbsub items (i32.const 3) binding-name scrutinee-wat ctx) ")")))))))))))))))

; Short helper: compile the item at idx as a sub-expression (non-tail).
(fn cbsub ((items (list sexpr)) (idx s32) (binding-name string) (scrutinee-wat string) (ctx compile-ctx)) string
  (compile-expr-sub (list-get items idx) binding-name scrutinee-wat ctx (i32.const 0)))

; Control forms that can appear inside a match arm body.
(fn is-ctrl-form ((name string)) s32
  (if (string=? name "if") (i32.const 1)
    (if (string=? name "let") (i32.const 1)
      (if (string=? name "match") (i32.const 1)
        (i32.const 0)))))

; Compile if/let/match inside a match arm (sub-expressions keep the substitution
; context; a nested match delegates to the normal compiler for its own scrutinee).
(fn compile-ctrl-sub ((items (list sexpr)) (name string) (binding-name string) (scrutinee-wat string) (ctx compile-ctx) (is-tail s32)) string
  (if (string=? name "if")
    (string-append "(if (result i32) "
      (string-append (cbsub items (i32.const 1) binding-name scrutinee-wat ctx)
        (string-append " (then "
          (string-append (compile-expr-sub (list-get items (i32.const 2)) binding-name scrutinee-wat ctx is-tail)
            (string-append ") (else "
              (string-append (compile-expr-sub (list-get items (i32.const 3)) binding-name scrutinee-wat ctx is-tail) "))"))))))
    (if (string=? name "let")
      (let (binding (get-lst (list-get items (i32.const 1))))
        (string-append "(local.set $"
          (string-append (get-sym (list-get binding (i32.const 0)))
            (string-append " "
              (string-append (cbsub binding (i32.const 1) binding-name scrutinee-wat ctx)
                (string-append ") "
                  (compile-expr-sub (list-get items (i32.const 2)) binding-name scrutinee-wat
                    (with-var ctx (get-sym (list-get binding (i32.const 0))) (infer-type (list-get binding (i32.const 1)) ctx))
                    is-tail)))))))
      (compile-match items ctx is-tail))))

(fn compile-wasm-call-sub ((instr string) (items (list sexpr)) (binding-name string) (scrutinee-wat string) (ctx compile-ctx)) string
  (let (args (build-args-list items (i32.const 1) (i32.sub (list-len items) (i32.const 1)) (list-new sexpr)))
    (if (i32.eq (list-len args) (i32.const 0))
      (string-append "(" (string-append instr ")"))
      ; For const instructions, use the literal value directly
      (if (is-const-instr instr)
        (let (arg (list-get args (i32.const 0)))
          (if (is-num arg)
            (string-append "(" (string-append instr (string-append " " (string-append (i32-to-string (get-num arg)) ")"))))
            (if (is-fnum arg)
              ; Float literal - use string directly
              (string-append "(" (string-append instr (string-append " " (string-append (get-fnum arg) ")"))))
              (string-append "(" (string-append instr " (error: const expects number or float)")))))
        (let (compiled-args (compile-args-sub args (i32.const 0) (list-len args) "" binding-name scrutinee-wat ctx))
          (string-append "(" (string-append instr (string-append " " (string-append compiled-args ")")))))))))

(fn compile-fn-call-sub ((name string) (items (list sexpr)) (binding-name string) (scrutinee-wat string) (ctx compile-ctx) (is-tail s32)) string
  (let (args (build-args-list items (i32.const 1) (i32.sub (list-len items) (i32.const 1)) (list-new sexpr)))
    (let (compiled-args (compile-args-sub args (i32.const 0) (list-len args) "" binding-name scrutinee-wat ctx))
      (let (call-instr (if (i32.eq is-tail (i32.const 1)) "return_call" "call"))
        (if (i32.eq (list-len args) (i32.const 0))
          (string-append "(" (string-append call-instr (string-append " $" (string-append name ")"))))
          (string-append "(" (string-append call-instr (string-append " $" (string-append name
            (string-append " " (string-append compiled-args ")")))))))))))


(fn compile-args-sub ((args (list sexpr)) (idx s32) (len s32) (acc string) (binding-name string) (scrutinee-wat string) (ctx compile-ctx)) string
  (if (i32.ge_s idx len)
    acc
    (let (arg (list-get args idx))
      (let (compiled (compile-expr-sub arg binding-name scrutinee-wat ctx (i32.const 0)))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-args-sub args (i32.add idx (i32.const 1)) len new-acc binding-name scrutinee-wat ctx))))))

; Compile a single match case
; case is: ((constructor [binding]) body)
(fn compile-match-case ((case-expr sexpr) (scrutinee-wat string) (remaining-cases (list sexpr)) (case-idx s32) (num-cases s32) (ctx compile-ctx) (is-tail s32)) string
  (if (is-lst case-expr)
    (let (case-items (get-lst case-expr))
      (if (i32.ge_s (list-len case-items) (i32.const 2))
        (let (pattern (list-get case-items (i32.const 0)))
          (let (body (list-get case-items (i32.const 1)))
            (if (is-lst pattern)
              (let (pattern-items (get-lst pattern))
                (if (i32.gt_s (list-len pattern-items) (i32.const 0))
                  (let (constructor (list-get pattern-items (i32.const 0)))
                    (if (is-sym constructor)
                      (let (tag (constructor-tag-ctx ctx (get-sym constructor)))
                        (let (binding-name (if (i32.ge_s (list-len pattern-items) (i32.const 2))
                                             (let (binding-expr (list-get pattern-items (i32.const 1)))
                                               (if (is-sym binding-expr) (get-sym binding-expr) ""))
                                             ""))
                          (let (cond-wat (string-append "(i32.eq (i32.load " (string-append scrutinee-wat (string-append ") (i32.const " (string-append (i32-to-string tag) "))")))))
                            (let (body-wat (compile-expr-sub body binding-name scrutinee-wat ctx is-tail))
                              ; Next case is at index case-idx + 3 (skip 'match' at 0, scrutinee at 1, cases start at 2)
                              (let (else-wat (if (i32.ge_s (i32.add case-idx (i32.const 1)) num-cases)
                                               "(unreachable)"
                                               (compile-match-case (list-get remaining-cases (i32.add case-idx (i32.const 3))) scrutinee-wat remaining-cases (i32.add case-idx (i32.const 1)) num-cases ctx is-tail)))
                                (string-append "(if (result i32) " (string-append cond-wat (string-append " (then " (string-append body-wat (string-append ") (else " (string-append else-wat "))")))))))))))
                      "(error: pattern constructor not symbol)"))
                  "(error: empty pattern)"))
              "(error: pattern not list)")))
        "(error: case needs pattern and body)"))
    "(error: case not list)"))

; Compile match expression: (match scrutinee case1 case2 ...)
(fn compile-match ((items (list sexpr)) (ctx compile-ctx) (is-tail s32)) string
  (if (i32.lt_s (list-len items) (i32.const 3))
    "(error: match needs scrutinee and at least one case)"
    (let (scrutinee (list-get items (i32.const 1)))
      (let (scrutinee-wat (compile-expr scrutinee ctx (i32.const 0)))
        (let (first-case (list-get items (i32.const 2)))
          (let (num-cases (i32.sub (list-len items) (i32.const 2)))
            (compile-match-case first-case scrutinee-wat items (i32.const 0) num-cases ctx is-tail)))))))

; ============================================================
; begin / global.get / global.set  (special forms the compiler's own
; source uses, e.g. the i64 number helpers that smuggle an s64 out of a
; match through the $__temp_i64 global)
; ============================================================

(fn is-begin-or-global ((name string)) s32
  (if (string=? name "begin") (i32.const 1)
    (if (string=? name "global.get") (i32.const 1)
      (if (string=? name "global.set") (i32.const 1)
        (i32.const 0)))))

; A store instruction (i32.store, i64.store8, ...) leaves nothing on the stack.
(fn instr-is-store ((n string)) s32
  (if (i32.lt_s (string-len n) (i32.const 9))
    (i32.const 0)
    (if (string=? (substring n (i32.const 4) (i32.const 9)) "store")
      (i32.const 1)
      (i32.const 0))))

; Does this expr leave no value on the stack? begin uses this to decide
; whether a non-final expression must be dropped.
(fn is-void-expr ((e sexpr)) s32
  (if (is-lst e)
    (let (its (get-lst e))
      (if (i32.gt_s (list-len its) (i32.const 0))
        (let (h (list-get its (i32.const 0)))
          (if (is-sym h)
            (if (string=? (get-sym h) "global.set")
              (i32.const 1)
              (instr-is-store (get-sym h)))
            (i32.const 0)))
        (i32.const 0)))
    (i32.const 0)))

; Compile the non-final exprs of a begin, dropping any that leave a value.
(fn compile-begin-stmts ((items (list sexpr)) (idx s32) (last-idx s32) (ctx compile-ctx) (acc string)) string
  (if (i32.ge_s idx last-idx)
    acc
    (let (e (list-get items idx))
      (let (c (compile-expr e ctx (i32.const 0)))
        (let (stmt (if (i32.eq (is-void-expr e) (i32.const 1))
                     c
                     (string-append "(drop " (string-append c ")"))))
          (compile-begin-stmts items (i32.add idx (i32.const 1)) last-idx ctx
            (string-append acc (string-append stmt " "))))))))

; (begin e1 ... en) -> evaluate all in order, value is en (e1..e(n-1) dropped
; if they leave a value). Emitted as a bare instruction sequence -- no block --
; so the last expr's type (which may be i64) flows to the enclosing context.
(fn compile-begin ((items (list sexpr)) (ctx compile-ctx) (is-tail s32)) string
  (let (last-idx (i32.sub (list-len items) (i32.const 1)))
    (let (stmts (compile-begin-stmts items (i32.const 1) last-idx ctx ""))
      (string-append stmts (compile-expr (list-get items last-idx) ctx is-tail)))))

(fn compile-begin-or-global ((name string) (items (list sexpr)) (ctx compile-ctx) (is-tail s32)) string
  (if (string=? name "global.get")
    (string-append "(global.get " (string-append (get-sym (list-get items (i32.const 1))) ")"))
    (if (string=? name "global.set")
      (string-append "(global.set " (string-append (get-sym (list-get items (i32.const 1)))
        (string-append " " (string-append (compile-expr (list-get items (i32.const 2)) ctx (i32.const 0)) ")"))))
      (compile-begin items ctx is-tail))))

; sub variants: same forms, but inside match arms (thread binding/scrutinee)
(fn compile-begin-stmts-sub ((items (list sexpr)) (idx s32) (last-idx s32) (binding-name string) (scrutinee-wat string) (ctx compile-ctx) (acc string)) string
  (if (i32.ge_s idx last-idx)
    acc
    (let (e (list-get items idx))
      (let (c (compile-expr-sub e binding-name scrutinee-wat ctx (i32.const 0)))
        (let (stmt (if (i32.eq (is-void-expr e) (i32.const 1))
                     c
                     (string-append "(drop " (string-append c ")"))))
          (compile-begin-stmts-sub items (i32.add idx (i32.const 1)) last-idx binding-name scrutinee-wat ctx
            (string-append acc (string-append stmt " "))))))))

(fn compile-begin-sub ((items (list sexpr)) (binding-name string) (scrutinee-wat string) (ctx compile-ctx) (is-tail s32)) string
  (let (last-idx (i32.sub (list-len items) (i32.const 1)))
    (let (stmts (compile-begin-stmts-sub items (i32.const 1) last-idx binding-name scrutinee-wat ctx ""))
      (string-append stmts (compile-expr-sub (list-get items last-idx) binding-name scrutinee-wat ctx is-tail)))))

(fn compile-begin-or-global-sub ((name string) (items (list sexpr)) (binding-name string) (scrutinee-wat string) (ctx compile-ctx) (is-tail s32)) string
  (if (string=? name "global.get")
    (string-append "(global.get " (string-append (get-sym (list-get items (i32.const 1))) ")"))
    (if (string=? name "global.set")
      (string-append "(global.set " (string-append (get-sym (list-get items (i32.const 1)))
        (string-append " " (string-append (compile-expr-sub (list-get items (i32.const 2)) binding-name scrutinee-wat ctx (i32.const 0)) ")"))))
      (compile-begin-sub items binding-name scrutinee-wat ctx is-tail))))

(fn compile-expr ((expr sexpr) (ctx compile-ctx) (is-tail s32)) string
  (match expr
    ((num n) (compile-number n))
    ((fnum f) (string-append "(f64.const " (string-append f ")")))  ; Default to f64 for bare float literals
    ((sym s) (compile-var s))
    ((str s) (compile-string s))
    ((lst items) (compile-list items ctx is-tail))))

; ============================================================
; Type inference (Increment 1: scalars) -- see docs/changes/SELF-HOSTED-TYPES.md
; ============================================================

(fn is-cast-head ((name string)) s32
  (if (string=? name "s32") (i32.const 1)
    (if (string=? name "s64") (i32.const 1)
      (if (string=? name "f32") (i32.const 1)
        (if (string=? name "f64") (i32.const 1)
          (i32.const 0))))))

; A wasm comparison yields s32 regardless of operand width. Detected by the two
; chars after the `iNN.`/`fNN.` prefix (eq ne lt gt le ge; also covers eqz).
(fn is-comparison-instr ((name string)) s32
  (if (i32.lt_s (string-len name) (i32.const 6))
    (i32.const 0)
    (let (op (substring name (i32.const 4) (i32.const 6)))
      (if (string=? op "eq") (i32.const 1)
        (if (string=? op "ne") (i32.const 1)
          (if (string=? op "lt") (i32.const 1)
            (if (string=? op "gt") (i32.const 1)
              (if (string=? op "le") (i32.const 1)
                (if (string=? op "ge") (i32.const 1)
                  (i32.const 0))))))))))

(fn instr-result-type ((name string)) string
  (if (i32.eq (is-comparison-instr name) (i32.const 1))
    "s32"
    (if (i32.lt_s (string-len name) (i32.const 3))
      "s32"
      (let (p (substring name (i32.const 0) (i32.const 3)))
        (if (string=? p "i32") "s32"
          (if (string=? p "i64") "s64"
            (if (string=? p "f32") "f32"
              (if (string=? p "f64") "f64"
                "s32"))))))))

; --- Type environment (Increment 2) ---

; Look up a variable's type in the lexical env (default s32 if unbound).
(fn vartype-of-acc ((vts (list vartype)) (idx s32) (len s32) (name string)) string
  (if (i32.ge_s idx len) "s32"
    (if (string=? (vartype.vt-name (list-get vts idx)) name)
      (vartype.vt-type (list-get vts idx))
      (vartype-of-acc vts (i32.add idx (i32.const 1)) len name))))

(fn vartype-of ((ctx compile-ctx) (name string)) string
  (vartype-of-acc (compile-ctx.ctx-vartypes ctx) (i32.const 0) (list-len (compile-ctx.ctx-vartypes ctx)) name))

; Look up a function's return type (default s32 if unknown).
(fn sig-ret-of-acc ((sigs (list fnsig)) (idx s32) (len s32) (name string)) string
  (if (i32.ge_s idx len) "s32"
    (if (string=? (fnsig.fs-name (list-get sigs idx)) name)
      (fnsig.fs-ret (list-get sigs idx))
      (sig-ret-of-acc sigs (i32.add idx (i32.const 1)) len name))))

(fn sig-ret-of ((ctx compile-ctx) (name string)) string
  (sig-ret-of-acc (compile-ctx.ctx-sigs ctx) (i32.const 0) (list-len (compile-ctx.ctx-sigs ctx)) name))

; Copy a vartype list (list-push mutates in place, so with-var must not share the
; parent's list -- otherwise a binding would leak into sibling scopes).
(fn copy-vartypes ((src (list vartype)) (idx s32) (len s32) (acc (list vartype))) (list vartype)
  (if (i32.ge_s idx len) acc
    (copy-vartypes src (i32.add idx (i32.const 1)) len (list-push acc (list-get src idx)))))

; ctx extended with one (name,type) binding, over a fresh copy of the env.
(fn with-var ((ctx compile-ctx) (name string) (ty string)) compile-ctx
  (let (copied (copy-vartypes (compile-ctx.ctx-vartypes ctx) (i32.const 0) (list-len (compile-ctx.ctx-vartypes ctx)) (list-new vartype)))
    (compile-ctx (compile-ctx.ctx-variants ctx) (compile-ctx.ctx-records ctx) (compile-ctx.ctx-imports ctx)
      (list-push copied (vartype name ty)) (compile-ctx.ctx-sigs ctx))))

; ctx seeded with a function's parameter types (each param is (name type)).
; Canonical string of a type expr: a symbol as-is, a compound like (list s32) as "(list s32)".
(fn type-str-list ((items (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len) acc
    (let (s (type-str (list-get items idx)))
      (type-str-list items (i32.add idx (i32.const 1)) len
        (if (i32.eq (string-len acc) (i32.const 0)) s (string-append acc (string-append " " s)))))))

(fn type-str ((e sexpr)) string
  (if (is-sym e) (get-sym e)
    (if (is-lst e)
      (string-append "(" (string-append (type-str-list (get-lst e) (i32.const 0) (list-len (get-lst e)) "") ")"))
      "s32")))

(fn ctx-with-params ((ctx compile-ctx) (params (list sexpr)) (idx s32) (len s32)) compile-ctx
  (if (i32.ge_s idx len)
    ctx
    (let (p (get-lst (list-get params idx)))
      (ctx-with-params (with-var ctx (get-sym (list-get p (i32.const 0))) (type-str (list-get p (i32.const 1)))) params (i32.add idx (i32.const 1)) len))))

; --- Function signature table (for call-result inference) ---

(fn empty-fnsig () fnsig (fnsig "" "s32"))

; (fn name (params) ret body...) -> its signature.
(fn fn-items-sig ((items (list sexpr))) fnsig
  (if (i32.ge_s (list-len items) (i32.const 4))
    (let (nm (if (is-sym (list-get items (i32.const 1))) (get-sym (list-get items (i32.const 1))) ""))
      (let (rt (if (is-sym (list-get items (i32.const 3))) (get-sym (list-get items (i32.const 3))) "s32"))
        (fnsig nm rt)))
    (empty-fnsig)))

; Signature of a top-level form if it is a fn or (export (fn ...)); else empty.
(fn form-fnsig ((form sexpr)) fnsig
  (if (is-lst form)
    (let (items (get-lst form))
      (if (i32.ge_s (list-len items) (i32.const 1))
        (if (is-sym (list-get items (i32.const 0)))
          (let (h (get-sym (list-get items (i32.const 0))))
            (if (string=? h "fn")
              (fn-items-sig items)
              (if (string=? h "export")
                (if (i32.ge_s (list-len items) (i32.const 2))
                  (if (is-lst (list-get items (i32.const 1)))
                    (form-fnsig (list-get items (i32.const 1)))
                    (empty-fnsig))
                  (empty-fnsig))
                (empty-fnsig))))
          (empty-fnsig))
        (empty-fnsig)))
    (empty-fnsig)))

(fn collect-sigs-acc ((forms (list sexpr)) (idx s32) (len s32) (acc (list fnsig))) (list fnsig)
  (if (i32.ge_s idx len)
    acc
    (let (sig (form-fnsig (list-get forms idx)))
      (if (string=? (fnsig.fs-name sig) "")
        (collect-sigs-acc forms (i32.add idx (i32.const 1)) len acc)
        (collect-sigs-acc forms (i32.add idx (i32.const 1)) len (list-push acc sig))))))

(fn collect-sigs ((forms (list sexpr))) (list fnsig)
  (collect-sigs-acc forms (i32.const 0) (list-len forms) (list-new fnsig)))

; Infer a scalar type name for an expression. Uses the lexical var env for symbols and
; the signature table for call results. Used to pick numeric cast conversions.
(fn infer-type-list ((items (list sexpr)) (ctx compile-ctx)) string
  (if (i32.eq (list-len items) (i32.const 0))
    "s32"
    (let (head (list-get items (i32.const 0)))
      (if (is-sym head)
        (let (name (get-sym head))
          (if (i32.eq (is-cast-head name) (i32.const 1))
            name
            (if (string=? name "if")
              (infer-type (list-get items (i32.const 2)) ctx)
              (if (is-wasm-instr name)
                (instr-result-type name)
                ; a record constructor's type is the record name (needed for trait dispatch)
                (if (i32.eq (is-record-constructor ctx name) (i32.const 1))
                  name
                  ; list-get yields the list's element type (enables dispatch on elements)
                  (if (string=? name "list-get")
                    (type-str (list-elem-type (list-get items (i32.const 1)) ctx))
                    (sig-ret-of ctx name)))))))
        "s32"))))

(fn infer-type ((e sexpr) (ctx compile-ctx)) string
  (match e
    ((sym s) (vartype-of ctx s))
    ((num n) "s32")
    ((fnum f) "f64")
    ((str s) "string")
    ((lst items) (infer-type-list items ctx))))

; The WAT op converting a value of type `src` to type `target` (both scalar, differing).
(fn conversion-op ((target string) (src string)) string
  (if (string=? target "s32")
    (if (string=? src "s64") "i32.wrap_i64"
      (if (string=? src "f32") "i32.trunc_f32_s"
        (if (string=? src "f64") "i32.trunc_f64_s" "nop")))
    (if (string=? target "s64")
      (if (string=? src "s32") "i64.extend_i32_s"
        (if (string=? src "f32") "i64.trunc_f32_s"
          (if (string=? src "f64") "i64.trunc_f64_s" "nop")))
      (if (string=? target "f32")
        (if (string=? src "s32") "f32.convert_i32_s"
          (if (string=? src "s64") "f32.convert_i64_s"
            (if (string=? src "f64") "f32.demote_f64" "nop")))
        (if (string=? target "f64")
          (if (string=? src "s32") "f64.convert_i32_s"
            (if (string=? src "s64") "f64.convert_i64_s"
              (if (string=? src "f32") "f64.promote_f32" "nop")))
          "nop")))))

; Numeric cast (sNN|fNN expr): emit the conversion for inferred-source -> target,
; or the bare value when they already match.
(fn compile-cast ((target string) (expr sexpr) (ctx compile-ctx)) string
  (let (src (infer-type expr ctx))
    (let (inner (compile-expr expr ctx (i32.const 0)))
      (if (string=? src target)
        inner
        (string-append "(" (string-append (conversion-op target src) (string-append " " (string-append inner ")"))))))))

(fn compile-cast-sub ((target string) (expr sexpr) (binding-name string) (scrutinee-wat string) (ctx compile-ctx)) string
  (let (src (infer-type expr ctx))
    (let (inner (compile-expr-sub expr binding-name scrutinee-wat ctx (i32.const 0)))
      (if (string=? src target)
        inner
        (string-append "(" (string-append (conversion-op target src) (string-append " " (string-append inner ")"))))))))

(fn compile-list ((items (list sexpr)) (ctx compile-ctx) (is-tail s32)) string
  (if (i32.eq (list-len items) (i32.const 0))
    "()"
    (let (head (list-get items (i32.const 0)))
      (if (is-sym head)
        (let (name (get-sym head))
          (if (string=? name "if")
            (if (i32.lt_s (list-len items) (i32.const 4))
              "(error: if needs 3 arguments)"
              (let (cond-expr (list-get items (i32.const 1)))
                (let (then-expr (list-get items (i32.const 2)))
                  (let (else-expr (list-get items (i32.const 3)))
                    (let (cond-wat (compile-expr cond-expr ctx (i32.const 0)))
                      (let (then-wat (compile-expr then-expr ctx is-tail))
                        (let (else-wat (compile-expr else-expr ctx is-tail))
                          (string-append "(if (result i32) "
                            (string-append cond-wat
                              (string-append " (then "
                                (string-append then-wat
                                  (string-append ") (else "
                                    (string-append else-wat "))")))))))))))))
            (if (string=? name "let")
              (if (i32.lt_s (list-len items) (i32.const 3))
                "(error: let needs binding and body)"
                (let (binding (list-get items (i32.const 1)))
                  (let (body (list-get items (i32.const 2)))
                    (if (is-lst binding)
                      (let (binding-items (get-lst binding))
                        (if (i32.lt_s (list-len binding-items) (i32.const 2))
                          "(error: let binding needs name and value)"
                          (let (name-expr (list-get binding-items (i32.const 0)))
                            (let (value-expr (list-get binding-items (i32.const 1)))
                              (if (is-sym name-expr)
                                (let (var-name (get-sym name-expr))
                                  (let (value-wat (compile-expr value-expr ctx (i32.const 0)))
                                    (let (body-wat (compile-expr body (with-var ctx var-name (infer-type value-expr ctx)) is-tail))
                                      (string-append "(local.set $"
                                        (string-append var-name
                                          (string-append " "
                                            (string-append value-wat
                                              (string-append ") " body-wat))))))))
                                "(error: let binding name must be symbol)")))))
                      "(error: let binding must be a list)"))))
              ; Match expression
              (if (string=? name "match")
                (compile-match items ctx is-tail)
                ; Built-in: string-len -> (i32.load ptr)
                (if (string=? name "string-len")
                  (let (arg (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                    (string-append "(i32.load " (string-append arg ")")))
                  ; Built-in: list-len -> (i32.load ptr)
                  (if (string=? name "list-len")
                    (let (arg (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                      (string-append "(i32.load " (string-append arg ")")))
                  ; Built-in: string-ref -> (i32.load8_u (i32.add (i32.add s 4) i))
                  (if (string=? name "string-ref")
                    (let (s-wat (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                      (let (i-wat (compile-expr (list-get items (i32.const 2)) ctx (i32.const 0)))
                        (string-append "(i32.load8_u (i32.add (i32.add "
                          (string-append s-wat
                            (string-append " (i32.const 4)) " (string-append i-wat "))"))))))
                    ; Built-in: list-get
                    (if (string=? name "list-get")
                      (let (lst-wat (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                        (let (idx-wat (compile-expr (list-get items (i32.const 2)) ctx (i32.const 0)))
                          (string-append "(i32.load (i32.add (i32.load (i32.add "
                            (string-append lst-wat
                              (string-append " (i32.const 8))) (i32.mul " (string-append idx-wat " (i32.const 4))))"))))))
                      ; Built-in: string-append -> (call $__string_append a b)
                      (if (string=? name "string-append")
                        (let (a-wat (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                          (let (b-wat (compile-expr (list-get items (i32.const 2)) ctx (i32.const 0)))
                            (string-append "(call $__string_append "
                              (string-append a-wat
                                (string-append " " (string-append b-wat ")"))))))
                        ; Built-in: string=? -> (call $__string_eq a b)
                        (if (string=? name "string=?")
                          (let (a-wat (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                            (let (b-wat (compile-expr (list-get items (i32.const 2)) ctx (i32.const 0)))
                              (string-append "(call $__string_eq "
                                (string-append a-wat
                                  (string-append " " (string-append b-wat ")"))))))
                          ; Built-in: substring -> (call $__substring s start end)
                          (if (string=? name "substring")
                            (let (s-wat (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                              (let (start-wat (compile-expr (list-get items (i32.const 2)) ctx (i32.const 0)))
                                (let (end-wat (compile-expr (list-get items (i32.const 3)) ctx (i32.const 0)))
                                  (string-append "(call $__substring "
                                    (string-append s-wat
                                      (string-append " " (string-append start-wat
                                        (string-append " " (string-append end-wat ")")))))))))
                          ; Built-in: list-new -> (call $__list_new)
                          (if (string=? name "list-new")
                            "(call $__list_new)"
                            ; Built-in: list-push -> (call $__list_push lst item)
                            (if (string=? name "list-push")
                              (let (lst-wat (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                                (let (item-wat (compile-expr (list-get items (i32.const 2)) ctx (i32.const 0)))
                                  (string-append "(call $__list_push "
                                    (string-append lst-wat
                                      (string-append " " (string-append item-wat ")"))))))
                              ; Context-aware variant constructor
                              (if (is-variant-constructor ctx name)
                                (if (i32.eq (constructor-has-payload ctx name) (i32.const 0))
                                  (string-append "(call $__make_variant_0 (i32.const " (string-append (i32-to-string (constructor-tag-ctx ctx name)) "))"))
                                  ; Payload is the LAST arg: a variant case carries at most one
                                  ; payload, so for a normal case that is items[1]; for the built-in
                                  ; option/result constructors the Rust type annotations precede it
                                  ; (e.g. (some T v), (ok T E v)) and are ignored here.
                                  (let (payload-wat (compile-expr (list-get items (i32.sub (list-len items) (i32.const 1))) ctx (i32.const 0)))
                                    (string-append "(call $__make_variant_1 (i32.const " (string-append (i32-to-string (constructor-tag-ctx ctx name)) (string-append ") " (string-append payload-wat ")"))))))
                                ; Context-aware record constructor
                                (if (is-record-constructor ctx name)
                                  (compile-record-construct ctx name items)
                                  ; Context-aware field accessor
                                  (if (is-field-accessor ctx name)
                                    (let (offset (get-field-offset ctx name))
                                      (let (rec-wat (compile-expr (list-get items (i32.const 1)) ctx (i32.const 0)))
                                        (if (i32.eq offset (i32.const 0))
                                          (string-append "(i32.load " (string-append rec-wat ")"))
                                          (string-append "(i32.load (i32.add " (string-append rec-wat (string-append " (i32.const " (string-append (i32-to-string offset) ")))")))))))
                                    ; Regular function call or WASM instruction
                                    (let (args (build-args-list items (i32.const 1) (i32.sub (list-len items) (i32.const 1)) (list-new sexpr)))
                                      (if (i32.eq (is-begin-or-global name) (i32.const 1))
                                        (compile-begin-or-global name items ctx is-tail)
                                      (if (is-wasm-instr name)
                                        (compile-wasm-call name args ctx)
                                        (if (string=? name "tuple")
                                          (compile-tuple items ctx)
                                          (if (i32.eq (is-cast-head name) (i32.const 1))
                                            (compile-cast name (list-get items (i32.const 1)) ctx)
                                            (compile-fn-call name args ctx is-tail))))))))))))))))))))))
        "(error: list head not symbol)"))))

; ============================================================
; Import WAT Generation
; ============================================================

; Compile a single import parameter: (x s32) -> "(param $x i32)"
(fn compile-import-param ((param sexpr)) string
  (if (is-lst param)
    (let (items (get-lst param))
      (if (i32.ge_s (list-len items) (i32.const 2))
        (let (name-expr (list-get items (i32.const 0)))
          (let (type-expr (list-get items (i32.const 1)))
            (if (is-sym name-expr)
              (let (wat-type (if (is-sym type-expr) (type-to-wat-imp (get-sym type-expr)) "i32"))
                (string-append "(param $" (string-append (get-sym name-expr)
                  (string-append " " (string-append wat-type ")")))))
              "(error)")))
        "(error)"))
    "(error)"))

; Convert type name to WAT type (standalone, before type-to-wat is defined)
(fn type-to-wat-imp ((t string)) string
  (if (string=? t "s32") "i32"
    (if (string=? t "s64") "i64"
      (if (string=? t "f32") "f32"
        (if (string=? t "f64") "f64"
          "i32")))))

; Compile import parameters list
(fn compile-import-params ((params (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (param (list-get params idx))
      (let (compiled (compile-import-param param))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-import-params params (i32.add idx (i32.const 1)) len new-acc))))))

; Compile a single import to WAT declaration
; -> "  (import \"module\" \"name\" (func $name (param $x i32) ... (result i32)))\n"
(fn compile-import ((imp import-def)) string
  (let (mod-name (import-def.imp-module imp))
    (let (func-name (import-def.imp-name imp))
      (let (params (import-def.imp-params imp))
        (let (ret-type (import-def.imp-ret-type imp))
          (let (params-wat (compile-import-params params (i32.const 0) (list-len params) ""))
            (let (result-wat (string-append "(result " (string-append (type-to-wat-imp ret-type) ")")))
              (let (params-section (if (i32.gt_s (list-len params) (i32.const 0))
                                     (string-append " " params-wat)
                                     ""))
                (string-append "  (import \"" (string-append mod-name
                  (string-append "\" \"" (string-append func-name
                    (string-append "\" (func $" (string-append func-name
                      (string-append params-section
                        (string-append " " (string-append result-wat "))\n")))))))))))))))))

; Compile all imports to WAT declarations
(fn compile-imports ((imports (list import-def)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (imp (list-get imports idx))
      (let (compiled (compile-import imp))
        (compile-imports imports (i32.add idx (i32.const 1)) len
          (string-append acc compiled))))))

; ============================================================
; Function Compilation
; ============================================================

(fn type-to-wat ((t string)) string
  (if (string=? t "s32") "i32"
    (if (string=? t "s64") "i64"
      (if (string=? t "f32") "f32"
        (if (string=? t "f64") "f64"
          "i32")))))

(fn compile-param ((param sexpr)) string
  (if (is-lst param)
    (let (items (get-lst param))
      (if (i32.ge_s (list-len items) (i32.const 2))
        (let (name-expr (list-get items (i32.const 0)))
          (let (type-expr (list-get items (i32.const 1)))
            (if (is-sym name-expr)
              ; Type can be symbol (s32) or list ((list token)) - compound types compile to i32
              (let (wat-type (if (is-sym type-expr) (type-to-wat (get-sym type-expr)) "i32"))
                (string-append "(param $" (string-append (get-sym name-expr)
                  (string-append " " (string-append wat-type ")")))))
              "(error)")))
        "(error)"))
    "(error)"))

(fn compile-params ((params (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (param (list-get params idx))
      (let (compiled (compile-param param))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-params params (i32.add idx (i32.const 1)) len new-acc))))))

; Collect all let-binding names from an expression
(fn collect-locals ((expr sexpr) (acc (list string))) (list string)
  (if (is-lst expr)
    (let (items (get-lst expr))
      (if (i32.gt_s (list-len items) (i32.const 0))
        (let (head (list-get items (i32.const 0)))
          (if (is-sym head)
            (if (string=? (get-sym head) "let")
              ; (let (name value) body) - add name and recurse into value and body
              (if (i32.ge_s (list-len items) (i32.const 3))
                (let (binding (list-get items (i32.const 1)))
                  (if (is-lst binding)
                    (let (binding-items (get-lst binding))
                      (if (i32.ge_s (list-len binding-items) (i32.const 2))
                        (let (name-expr (list-get binding-items (i32.const 0)))
                          (if (is-sym name-expr)
                            (let (name (get-sym name-expr))
                              (let (value-expr (list-get binding-items (i32.const 1)))
                                (let (body-expr (list-get items (i32.const 2)))
                                  (let (acc2 (list-push acc name))
                                    (let (acc3 (collect-locals value-expr acc2))
                                      (collect-locals body-expr acc3))))))
                            acc))
                        acc))
                    acc))
                acc)
              (if (string=? (get-sym head) "if")
                ; (if cond then else) - recurse into all three
                (if (i32.ge_s (list-len items) (i32.const 4))
                  (let (cond-expr (list-get items (i32.const 1)))
                    (let (then-expr (list-get items (i32.const 2)))
                      (let (else-expr (list-get items (i32.const 3)))
                        (let (acc2 (collect-locals cond-expr acc))
                          (let (acc3 (collect-locals then-expr acc2))
                            (collect-locals else-expr acc3))))))
                  acc)
                (if (string=? (get-sym head) "match")
                  ; (match scrutinee case1 case2 ...) - recurse into scrutinee and case bodies
                  (if (i32.ge_s (list-len items) (i32.const 3))
                    (let (scrutinee (list-get items (i32.const 1)))
                      (let (acc2 (collect-locals scrutinee acc))
                        (collect-locals-match-cases items (i32.const 2) (list-len items) acc2)))
                    acc)
                  ; Other list form - recurse into all elements
                  (collect-locals-list items (i32.const 0) (list-len items) acc))))
            ; Head not a symbol - recurse into all elements
            (collect-locals-list items (i32.const 0) (list-len items) acc)))
        acc))
    acc))

; Collect locals from match cases
(fn collect-locals-match-cases ((items (list sexpr)) (idx s32) (len s32) (acc (list string))) (list string)
  (if (i32.ge_s idx len)
    acc
    (let (case-expr (list-get items idx))
      (if (is-lst case-expr)
        (let (case-items (get-lst case-expr))
          (if (i32.ge_s (list-len case-items) (i32.const 2))
            (let (pattern (list-get case-items (i32.const 0)))
              (let (body (list-get case-items (i32.const 1)))
                ; Add binding from pattern if exists
                (let (acc2 (if (is-lst pattern)
                             (let (pat-items (get-lst pattern))
                               (if (i32.ge_s (list-len pat-items) (i32.const 2))
                                 (let (binding-expr (list-get pat-items (i32.const 1)))
                                   (if (is-sym binding-expr)
                                     (list-push acc (get-sym binding-expr))
                                     acc))
                                 acc))
                             acc))
                  (let (acc3 (collect-locals body acc2))
                    (collect-locals-match-cases items (i32.add idx (i32.const 1)) len acc3)))))
            (collect-locals-match-cases items (i32.add idx (i32.const 1)) len acc)))
        (collect-locals-match-cases items (i32.add idx (i32.const 1)) len acc)))))

; Collect locals from a list of expressions
(fn collect-locals-list ((items (list sexpr)) (idx s32) (len s32) (acc (list string))) (list string)
  (if (i32.ge_s idx len)
    acc
    (let (item (list-get items idx))
      (let (acc2 (collect-locals item acc))
        (collect-locals-list items (i32.add idx (i32.const 1)) len acc2)))))

; Is `name` present in names[0, idx)? Used to skip duplicate local declarations
; (the same binding name can appear in several match arms / let bodies, but WAT
; requires each local identifier to be unique within a function).
(fn name-seen-before ((names (list string)) (idx s32) (name string)) s32
  (name-seen-loop names (i32.const 0) idx name))

(fn name-seen-loop ((names (list string)) (i s32) (idx s32) (name string)) s32
  (if (i32.ge_s i idx)
    (i32.const 0)
    (if (string=? (list-get names i) name)
      (i32.const 1)
      (name-seen-loop names (i32.add i (i32.const 1)) idx name))))

; Is `items` a (let (NAME value) body) that binds NAME == `name`?
(fn is-let-binding-of ((items (list sexpr)) (name string)) s32
  (if (i32.ge_s (list-len items) (i32.const 3))
    (if (is-sym (list-get items (i32.const 0)))
      (if (string=? (get-sym (list-get items (i32.const 0))) "let")
        (if (is-lst (list-get items (i32.const 1)))
          (let (b (get-lst (list-get items (i32.const 1))))
            (if (i32.ge_s (list-len b) (i32.const 2))
              (if (is-sym (list-get b (i32.const 0)))
                (if (string=? (get-sym (list-get b (i32.const 0))) name) (i32.const 1) (i32.const 0))
                (i32.const 0))
              (i32.const 0)))
          (i32.const 0))
        (i32.const 0))
      (i32.const 0))
    (i32.const 0)))

; Search `expr` for the `let` that binds `name` and return its value's inferred type;
; "" if not found. Used to declare each local with the right WAT type (Increment 2.5).
; Inference uses the function-level ctx (params + signatures); a value referencing an
; inner let var falls back to s32 -- a pointer/i32 case in practice.
(fn find-local-type-list ((name string) (items (list sexpr)) (idx s32) (len s32) (ctx compile-ctx)) string
  (if (i32.ge_s idx len)
    ""
    (let (r (find-local-type name (list-get items idx) ctx))
      (if (string=? r "")
        (find-local-type-list name items (i32.add idx (i32.const 1)) len ctx)
        r))))

(fn find-local-type ((name string) (expr sexpr) (ctx compile-ctx)) string
  (if (is-lst expr)
    (let (items (get-lst expr))
      (if (i32.eq (is-let-binding-of items name) (i32.const 1))
        (infer-type (list-get (get-lst (list-get items (i32.const 1))) (i32.const 1)) ctx)
        (find-local-type-list name items (i32.const 0) (list-len items) ctx)))
    ""))

; Generate local declarations, de-duplicating repeats. Each local's WAT type is inferred
; from the value of its `let` binding (default i32 when unknown / pointer-shaped).
(fn gen-locals ((names (list string)) (idx s32) (len s32) (acc string) (body sexpr) (ctx compile-ctx)) string
  (if (i32.ge_s idx len)
    acc
    (let (name (list-get names idx))
      (if (i32.eq (name-seen-before names idx name) (i32.const 1))
        (gen-locals names (i32.add idx (i32.const 1)) len acc body ctx)
        (let (tyname (find-local-type name body ctx))
          (let (watty (if (string=? tyname "") "i32" (type-to-wat tyname)))
            (let (decl (string-append "(local $" (string-append name (string-append " " (string-append watty ")")))))
              (let (new-acc (if (i32.eq (string-len acc) (i32.const 0))
                              decl
                              (string-append acc (string-append " " decl))))
                (gen-locals names (i32.add idx (i32.const 1)) len new-acc body ctx)))))))))

; Compile (fn name ((params...)) ret-type body)
(fn compile-fn-def ((items (list sexpr)) (ctx compile-ctx)) string
  (if (i32.lt_s (list-len items) (i32.const 5))
    "(error: fn needs name, params, return type, and body)"
    (let (name-expr (list-get items (i32.const 1)))
      (let (params-expr (list-get items (i32.const 2)))
        (let (ret-type-expr (list-get items (i32.const 3)))
          (let (body-expr (list-get items (i32.const 4)))
            (if (is-sym name-expr)
              (if (is-lst params-expr)
                ; Return type can be a symbol (s32) or a list ((list token))
                ; Lists/records/variants all compile to i32 pointers
                (let (name (get-sym name-expr))
                  (let (params (get-lst params-expr))
                    (let (ret-type (if (is-sym ret-type-expr) (get-sym ret-type-expr) "s32"))
                      (let (params-wat (compile-params params (i32.const 0) (list-len params) ""))
                        ; Collect local variables from the body
                        (let (locals (collect-locals body-expr (list-new string)))
                          (let (pctx (ctx-with-params ctx params (i32.const 0) (list-len params)))
                          (let (locals-wat (gen-locals locals (i32.const 0) (list-len locals) "" body-expr pctx))
                            (let (body-wat (compile-expr body-expr pctx (i32.const 1)))
                              (let (result-wat (string-append "(result " (string-append (type-to-wat ret-type) ")")))
                                ; Include locals after result type if any
                                (let (locals-section (if (i32.gt_s (list-len locals) (i32.const 0))
                                                       (string-append " " locals-wat)
                                                       ""))
                                  (string-append "  (func $" (string-append name
                                    (string-append " " (string-append params-wat
                                      (string-append " " (string-append result-wat
                                        (string-append locals-section
                                          (string-append "\n    " (string-append body-wat ")")))))))))))))))))))
                "(error: params)")
              "(error: name)")))))))

; Compile (export (fn ...)) or (export "alias" (fn ...))
(fn compile-export ((items (list sexpr)) (ctx compile-ctx)) string
  (if (i32.lt_s (list-len items) (i32.const 2))
    "(error: export needs body)"
    ; Check for aliased export: (export "alias" (fn ...)) - 3 items
    (if (i32.ge_s (list-len items) (i32.const 3))
      (if (is-str (list-get items (i32.const 1)))
        (let (export-name (get-str (list-get items (i32.const 1))))
          (let (fn-expr (list-get items (i32.const 2)))
            (if (is-lst fn-expr)
              (let (fn-items (get-lst fn-expr))
                (if (i32.gt_s (list-len fn-items) (i32.const 1))
                  (let (head (list-get fn-items (i32.const 0)))
                    (if (is-sym head)
                      (if (string=? (get-sym head) "fn")
                        (let (fn-name (get-sym (list-get fn-items (i32.const 1))))
                          (let (fn-wat (compile-fn-def fn-items ctx))
                            (string-append fn-wat
                              (string-append "\n  (export \""
                                (string-append export-name
                                  (string-append "\" (func $"
                                    (string-append fn-name "))")))))))
                        "(error: expected fn)")
                      "(error: expected symbol)"))
                  "(error: empty fn body)"))
              "(error: expected fn list)")))
        ; Fall through to standard export
        (compile-export-simple items ctx))
      ; Standard export: (export (fn ...)) - 2 items
      (compile-export-simple items ctx))))

; Compile standard (export (fn ...)) without alias
(fn compile-export-simple ((items (list sexpr)) (ctx compile-ctx)) string
  (let (body-expr (list-get items (i32.const 1)))
    (if (is-lst body-expr)
      (let (body-items (get-lst body-expr))
        (if (i32.gt_s (list-len body-items) (i32.const 1))
          (let (head (list-get body-items (i32.const 0)))
            (if (is-sym head)
              (if (string=? (get-sym head) "fn")
                (let (fn-name (get-sym (list-get body-items (i32.const 1))))
                  (let (fn-wat (compile-fn-def body-items ctx))
                    (string-append fn-wat
                      (string-append "\n  (export \""
                        (string-append fn-name
                          (string-append "\" (func $"
                            (string-append fn-name "))")))))))
                "(error: expected fn)")
              "(error: expected symbol)"))
          "(error: empty body)"))
      "(error: expected list)")))

; ============================================================
; Data Segment Helpers
; ============================================================

; Convert a nibble (0-15) to a single hex character string
(fn nibble-to-hex ((n s32)) string
  (substring "0123456789abcdef" n (i32.add n (i32.const 1))))

; Convert a byte (0-255) to a 2-character hex string
(fn byte-to-hex ((b s32)) string
  (string-append (nibble-to-hex (i32.shr_u b (i32.const 4)))
                 (nibble-to-hex (i32.and b (i32.const 15)))))

; Escape raw data string for WAT data segment format
; Handles \xHH sequences from tokenizer, hex-encodes other bytes
(fn escape-data-for-wat ((s string) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (b (string-ref s idx))
      (if (i32.and
            (i32.eq b (i32.const 92))
            (i32.ge_s (i32.sub len idx) (i32.const 4)))
        ; Might be \xHH escape
        (if (i32.eq (string-ref s (i32.add idx (i32.const 1))) (i32.const 120))
          ; \xHH - pass through the two hex digits as \HH
          (escape-data-for-wat s (i32.add idx (i32.const 4)) len
            (string-append acc
              (string-append "\\"
                (substring s (i32.add idx (i32.const 2)) (i32.add idx (i32.const 4))))))
          ; Backslash but not \x - hex encode the backslash
          (escape-data-for-wat s (i32.add idx (i32.const 1)) len
            (string-append acc "\\5c")))
        ; Regular byte - hex encode it
        (escape-data-for-wat s (i32.add idx (i32.const 1)) len
          (string-append acc (string-append "\\" (byte-to-hex b))))))))

; Compile a single (data offset "bytes") form to WAT
(fn compile-one-data ((items (list sexpr))) string
  (let (offset-val (get-num (list-get items (i32.const 1))))
    (let (raw-str (get-str (list-get items (i32.const 2))))
      (let (escaped (escape-data-for-wat raw-str (i32.const 0) (string-len raw-str) ""))
        (string-append "  (data (i32.const "
          (string-append (i32-to-string offset-val)
            (string-append ") \""
              (string-append escaped "\")\n"))))))))

; Collect and compile all data segments from top-level forms
(fn compile-data-segments ((forms (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (form (list-get forms idx))
      (let (new-acc
        (if (is-lst form)
          (let (items (get-lst form))
            (if (i32.gt_s (list-len items) (i32.const 0))
              (if (is-sym (list-get items (i32.const 0)))
                (if (string=? (get-sym (list-get items (i32.const 0))) "data")
                  (string-append acc (compile-one-data items))
                  acc)
                acc)
              acc))
          acc))
        (compile-data-segments forms (i32.add idx (i32.const 1)) len new-acc)))))

; Helper to compile based on form name
(fn compile-by-name ((name string) (items (list sexpr)) (ctx compile-ctx)) string
  (if (string=? name "fn")
    (compile-fn-def items ctx)
    (if (string=? name "export")
      (compile-export items ctx)
      (if (string=? name "variant")
        ""
        (if (string=? name "record")
          ""
          (if (string=? name "import")
            ""
            (if (string=? name "data")
              ""
              (if (string=? name "global")
                (compile-global items)
                "(error: unknown form)"))))))))

; Compile a top-level global declaration:
;   (global $name type mut|const init)  ->  (global $name (mut T) (T.const v))
; The name symbol already carries its leading $; init is assumed to be a number.
(fn compile-global ((items (list sexpr)) ) string
  (let (name (get-sym (list-get items (i32.const 1))))
    (let (ty (type-to-wat-imp (get-sym (list-get items (i32.const 2)))))
      (let (mutability (get-sym (list-get items (i32.const 3))))
        (let (init-val (i32-to-string (get-num (list-get items (i32.const 4)))))
          (let (init-wat (string-append "(" (string-append ty (string-append ".const " (string-append init-val ")")))))
            (let (type-decl (if (string=? mutability "mut")
                              (string-append "(mut " (string-append ty ")"))
                              ty))
              (string-append "  (global " (string-append name (string-append " "
                (string-append type-decl (string-append " " (string-append init-wat ")\n")))))))))))))

; Compile a top-level form
(fn compile-toplevel ((form sexpr) (ctx compile-ctx)) string
  (if (is-lst form)
    (compile-toplevel-list (get-lst form) ctx)
    "(error: not list)"))

(fn compile-toplevel-list ((items (list sexpr)) (ctx compile-ctx)) string
  (if (i32.gt_s (list-len items) (i32.const 0))
    (compile-toplevel-head items (list-get items (i32.const 0)) ctx)
    "(error: empty)"))

(fn compile-toplevel-head ((items (list sexpr)) (head sexpr) (ctx compile-ctx)) string
  (if (is-sym head)
    (compile-by-name (get-sym head) items ctx)
    "(error: not symbol)"))

; Compile multiple top-level forms, joining them with newlines. Uses
; divide-and-conquer so the ~150 KB module body is assembled with balanced
; string-append (O(N log F)) rather than a linear left-fold (O(N*F)). The
; outer signature (idx=0, len) is kept for callers; [idx, len) is the range.
(fn compile-toplevels ((forms (list sexpr)) (idx s32) (len s32) (acc string) (ctx compile-ctx)) string
  (compile-toplevels-range forms idx len ctx))

(fn compile-toplevels-range ((forms (list sexpr)) (start s32) (end s32) (ctx compile-ctx)) string
  (let (n (i32.sub end start))
    (if (i32.le_s n (i32.const 0))
      ""
      (if (i32.eq n (i32.const 1))
        (compile-toplevel (list-get forms start) ctx)
        (let (mid (i32.add start (i32.div_s n (i32.const 2))))
          (string-append
            (compile-toplevels-range forms start mid ctx)
            (string-append "\n"
              (compile-toplevels-range forms mid end ctx))))))))

; Runtime helpers for string, list, and variant operations
(fn get-runtime () string
  "  (global $__heap_ptr (mut i32) (i32.const 49152))\n  (func $__string_append (param $a i32) (param $b i32) (result i32) (local $la i32) (local $lb i32) (local $tot i32) (local $ptr i32) local.get $a i32.load local.set $la local.get $b i32.load local.set $lb local.get $la local.get $lb i32.add local.set $tot global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 local.get $tot i32.add i32.add global.set $__heap_ptr local.get $ptr local.get $tot i32.store local.get $ptr i32.const 4 i32.add local.get $a i32.const 4 i32.add local.get $la memory.copy local.get $ptr i32.const 4 i32.add local.get $la i32.add local.get $b i32.const 4 i32.add local.get $lb memory.copy local.get $ptr)\n  (func $__string_eq (param $a i32) (param $b i32) (result i32) (local $la i32) (local $lb i32) (local $i i32) local.get $a i32.load local.set $la local.get $b i32.load local.set $lb block (result i32) local.get $la local.get $lb i32.ne if (result i32) i32.const 0 else i32.const 0 local.set $i block (result i32) loop local.get $i local.get $la i32.ge_u if i32.const 1 br 2 end local.get $a i32.const 4 i32.add local.get $i i32.add i32.load8_u local.get $b i32.const 4 i32.add local.get $i i32.add i32.load8_u i32.ne if i32.const 0 br 3 end local.get $i i32.const 1 i32.add local.set $i br 0 end i32.const 1 end end end)\n  (func $__substring (param $s i32) (param $start i32) (param $end i32) (result i32) (local $new_len i32) (local $ptr i32) local.get $end local.get $start i32.sub local.set $new_len global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 local.get $new_len i32.add i32.add global.set $__heap_ptr local.get $ptr local.get $new_len i32.store local.get $ptr i32.const 4 i32.add local.get $s i32.const 4 i32.add local.get $start i32.add local.get $new_len memory.copy local.get $ptr)\n  (func $__list_new (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 12 i32.add global.set $__heap_ptr local.get $ptr i32.const 0 i32.store local.get $ptr i32.const 4 i32.add i32.const 0 i32.store local.get $ptr i32.const 8 i32.add i32.const 0 i32.store local.get $ptr)\n  (func $__list_push (param $lst i32) (param $item i32) (result i32) (local $len i32) (local $cap i32) (local $data i32) (local $new_cap i32) (local $new_data i32) local.get $lst i32.load local.set $len local.get $lst i32.const 4 i32.add i32.load local.set $cap local.get $lst i32.const 8 i32.add i32.load local.set $data local.get $len local.get $cap i32.lt_s if local.get $data local.get $len i32.const 4 i32.mul i32.add local.get $item i32.store local.get $lst local.get $len i32.const 1 i32.add i32.store else local.get $cap i32.const 0 i32.eq if (result i32) i32.const 4 else local.get $cap i32.const 2 i32.mul end local.set $new_cap global.get $__heap_ptr local.set $new_data global.get $__heap_ptr local.get $new_cap i32.const 4 i32.mul i32.add global.set $__heap_ptr local.get $new_data local.get $data local.get $len i32.const 4 i32.mul memory.copy local.get $new_data local.get $len i32.const 4 i32.mul i32.add local.get $item i32.store local.get $lst local.get $len i32.const 1 i32.add i32.store local.get $lst i32.const 4 i32.add local.get $new_cap i32.store local.get $lst i32.const 8 i32.add local.get $new_data i32.store end local.get $lst)\n  (func $__make_variant_0 (param $tag i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 i32.add global.set $__heap_ptr local.get $ptr local.get $tag i32.store local.get $ptr)\n  (func $__make_variant_1 (param $tag i32) (param $payload i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 8 i32.add global.set $__heap_ptr local.get $ptr local.get $tag i32.store local.get $ptr i32.const 4 i32.add local.get $payload i32.store local.get $ptr)\n  (func $__make_record_2 (param $f0 i32) (param $f1 i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 8 i32.add global.set $__heap_ptr local.get $ptr local.get $f0 i32.store local.get $ptr i32.const 4 i32.add local.get $f1 i32.store local.get $ptr)\n  (func $__make_record_3 (param $f0 i32) (param $f1 i32) (param $f2 i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 12 i32.add global.set $__heap_ptr local.get $ptr local.get $f0 i32.store local.get $ptr i32.const 4 i32.add local.get $f1 i32.store local.get $ptr i32.const 8 i32.add local.get $f2 i32.store local.get $ptr)\n  (func $__make_record_1 (param $f0 i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 i32.add global.set $__heap_ptr local.get $ptr local.get $f0 i32.store local.get $ptr)\n  (func $__make_record_4 (param $f0 i32) (param $f1 i32) (param $f2 i32) (param $f3 i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 16 i32.add global.set $__heap_ptr local.get $ptr local.get $f0 i32.store local.get $ptr i32.const 4 i32.add local.get $f1 i32.store local.get $ptr i32.const 8 i32.add local.get $f2 i32.store local.get $ptr i32.const 12 i32.add local.get $f3 i32.store local.get $ptr)\n  (func $__make_record_5 (param $f0 i32) (param $f1 i32) (param $f2 i32) (param $f3 i32) (param $f4 i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 20 i32.add global.set $__heap_ptr local.get $ptr local.get $f0 i32.store local.get $ptr i32.const 4 i32.add local.get $f1 i32.store local.get $ptr i32.const 8 i32.add local.get $f2 i32.store local.get $ptr i32.const 12 i32.add local.get $f3 i32.store local.get $ptr i32.const 16 i32.add local.get $f4 i32.store local.get $ptr)\n")

; ============================================================
; Macro expansion (unhygienic, with quasiquotation)
; ============================================================
; defmacro + quasiquote/unquote/unquote-splice, matching the Rust compiler. Runs on the
; sexpr tree before codegen: collect defmacros, drop them, expand every other form.
; Reader sugar is already desugared by the parser: `x -> (quasiquote x), ,x -> (unquote x),
; ,@x -> (unquote-splice x).

(record macro-def
  (mac-name string)
  (mac-params (list sexpr))   ; parameter symbols
  (mac-template sexpr))       ; template (usually a quasiquote form)

(record subst
  (sub-name string)
  (sub-val sexpr))

; Is `form` a list whose head is the symbol `name`?
(fn form-has-head ((form sexpr) (name string)) s32
  (if (is-lst form)
    (let (items (get-lst form))
      (if (i32.ge_s (list-len items) (i32.const 1))
        (if (is-sym (list-get items (i32.const 0)))
          (if (string=? (get-sym (list-get items (i32.const 0))) name) (i32.const 1) (i32.const 0))
          (i32.const 0))
        (i32.const 0)))
    (i32.const 0)))

(fn is-defmacro-form ((form sexpr)) s32
  (if (i32.eq (form-has-head form "defmacro") (i32.const 1))
    (if (i32.ge_s (list-len (get-lst form)) (i32.const 4)) (i32.const 1) (i32.const 0))
    (i32.const 0)))

(fn collect-macros-acc ((forms (list sexpr)) (idx s32) (len s32) (acc (list macro-def))) (list macro-def)
  (if (i32.ge_s idx len)
    acc
    (let (form (list-get forms idx))
      (if (i32.eq (is-defmacro-form form) (i32.const 1))
        (let (items (get-lst form))
          (collect-macros-acc forms (i32.add idx (i32.const 1)) len
            (list-push acc (macro-def
              (get-sym (list-get items (i32.const 1)))
              (get-lst (list-get items (i32.const 2)))
              (list-get items (i32.const 3))))))
        (collect-macros-acc forms (i32.add idx (i32.const 1)) len acc)))))

(fn collect-macros ((forms (list sexpr))) (list macro-def)
  (collect-macros-acc forms (i32.const 0) (list-len forms) (list-new macro-def)))

; Find a macro by name; returns a sentinel with name "" if not found.
(fn find-macro ((macros (list macro-def)) (idx s32) (len s32) (name string)) macro-def
  (if (i32.ge_s idx len)
    (macro-def "" (list-new sexpr) (sym ""))
    (let (m (list-get macros idx))
      (if (string=? (macro-def.mac-name m) name)
        m
        (find-macro macros (i32.add idx (i32.const 1)) len name)))))

(fn subst-contains ((subs (list subst)) (idx s32) (len s32) (name string)) s32
  (if (i32.ge_s idx len) (i32.const 0)
    (if (string=? (subst.sub-name (list-get subs idx)) name) (i32.const 1)
      (subst-contains subs (i32.add idx (i32.const 1)) len name))))

(fn subst-get ((subs (list subst)) (idx s32) (len s32) (name string)) sexpr
  (if (i32.ge_s idx len) (sym name)
    (if (string=? (subst.sub-name (list-get subs idx)) name) (subst.sub-val (list-get subs idx))
      (subst-get subs (i32.add idx (i32.const 1)) len name))))

; Bind macro params to call arguments: param i <- items[i+1] (items[0] is the macro name).
(fn bind-params ((params (list sexpr)) (items (list sexpr)) (pidx s32) (plen s32) (acc (list subst))) (list subst)
  (if (i32.ge_s pidx plen)
    acc
    (bind-params params items (i32.add pidx (i32.const 1)) plen
      (list-push acc (subst
        (get-sym (list-get params pidx))
        (list-get items (i32.add pidx (i32.const 1))))))))

; Append every element of `spliced` (a list) to acc; if not a list, push it.
(fn append-items ((acc (list sexpr)) (items (list sexpr)) (idx s32) (len s32)) (list sexpr)
  (if (i32.ge_s idx len) acc
    (append-items (list-push acc (list-get items idx)) items (i32.add idx (i32.const 1)) len)))

(fn append-sexprs ((acc (list sexpr)) (spliced sexpr)) (list sexpr)
  (if (is-lst spliced)
    (append-items acc (get-lst spliced) (i32.const 0) (list-len (get-lst spliced)))
    (list-push acc spliced)))

; Evaluate an unquoted expression: a bare symbol that is a macro param is replaced by its
; bound argument; anything else is evaluated as a template. Used for both ,x and ,@x.
(fn qq-value ((x sexpr) (subs (list subst))) sexpr
  (if (is-sym x)
    (if (i32.eq (subst-contains subs (i32.const 0) (list-len subs) (get-sym x)) (i32.const 1))
      (subst-get subs (i32.const 0) (list-len subs) (get-sym x))
      x)
    (eval-qq x subs)))

; Evaluate a quasiquote template with substitutions, returning a new sexpr.
(fn eval-qq ((tmpl sexpr) (subs (list subst))) sexpr
  (if (i32.eq (form-has-head tmpl "unquote") (i32.const 1))
    (qq-value (list-get (get-lst tmpl) (i32.const 1)) subs)
    (if (i32.eq (form-has-head tmpl "quasiquote") (i32.const 1))
      (lst (list-push (list-push (list-new sexpr) (sym "quasiquote"))
             (eval-qq (list-get (get-lst tmpl) (i32.const 1)) subs)))
      (if (is-lst tmpl)
        (lst (eval-qq-list (get-lst tmpl) (i32.const 0) (list-len (get-lst tmpl)) subs (list-new sexpr)))
        tmpl))))

; Evaluate the elements of a list template, splicing any unquote-splice elements.
(fn eval-qq-list ((items (list sexpr)) (idx s32) (len s32) (subs (list subst)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len)
    acc
    (let (item (list-get items idx))
      (if (i32.eq (form-has-head item "unquote-splice") (i32.const 1))
        (eval-qq-list items (i32.add idx (i32.const 1)) len subs
          (append-sexprs acc (qq-value (list-get (get-lst item) (i32.const 1)) subs)))
        (eval-qq-list items (i32.add idx (i32.const 1)) len subs
          (list-push acc (eval-qq item subs)))))))

; Strip a leading quasiquote wrapper from a macro template.
(fn unwrap-qq ((tmpl sexpr)) sexpr
  (if (i32.eq (form-has-head tmpl "quasiquote") (i32.const 1))
    (list-get (get-lst tmpl) (i32.const 1))
    tmpl))

(fn expand-list ((items (list sexpr)) (idx s32) (len s32) (macros (list macro-def)) (depth s32) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len)
    acc
    (expand-list items (i32.add idx (i32.const 1)) len macros depth
      (list-push acc (expand-form (list-get items idx) macros depth)))))

; Expand macro calls in `form`, re-expanding results (fixpoint; depth-capped at 100).
(fn expand-form ((form sexpr) (macros (list macro-def)) (depth s32)) sexpr
  (if (i32.gt_s depth (i32.const 100))
    form
    (if (is-lst form)
      (let (items (get-lst form))
        (if (i32.eq (list-len items) (i32.const 0))
          form
          (let (head (list-get items (i32.const 0)))
            (if (is-sym head)
              (let (m (find-macro macros (i32.const 0) (list-len macros) (get-sym head)))
                (if (string=? (macro-def.mac-name m) "")
                  (lst (expand-list items (i32.const 0) (list-len items) macros depth (list-new sexpr)))
                  (expand-form
                    (eval-qq (unwrap-qq (macro-def.mac-template m))
                      (bind-params (macro-def.mac-params m) items (i32.const 0) (list-len (macro-def.mac-params m)) (list-new subst)))
                    macros (i32.add depth (i32.const 1)))))
              (lst (expand-list items (i32.const 0) (list-len items) macros depth (list-new sexpr)))))))
      form)))

(fn expand-all-acc ((forms (list sexpr)) (idx s32) (len s32) (macros (list macro-def)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len)
    acc
    (let (form (list-get forms idx))
      (if (i32.eq (is-defmacro-form form) (i32.const 1))
        (expand-all-acc forms (i32.add idx (i32.const 1)) len macros acc)
        (expand-all-acc forms (i32.add idx (i32.const 1)) len macros
          (list-push acc (expand-form form macros (i32.const 0))))))))

(fn expand-all ((forms (list sexpr)) (macros (list macro-def))) (list sexpr)
  (expand-all-acc forms (i32.const 0) (list-len forms) macros (list-new sexpr)))

; ============================================================
; Generics: monomorphization (Increment 3) -- see docs/changes/SELF-HOSTED-TYPES.md
; ============================================================
; A source-to-source pass (after macros, before codegen): collect generic templates
; (fns with a (where ...) clause), drop them, and for each call to a template emit a
; specialized monomorphic copy with a mangled name, rewriting the call. Transitive +
; deduped. Unconstrained type params only (trait bounds => Increment 4). Type args are
; inferred from bare type-parameter params (x T) via infer-type. If there are no
; templates the pass is the identity (so the generic-free self-source is untouched).

(record gtemplate
  (gt-name string)
  (gt-params (list sexpr))
  (gt-ret sexpr)
  (gt-body sexpr)
  (gt-typarams (list string)))

(record spec
  (sp-name string)
  (sp-funcargs (list string))
  (sp-args (list string)))

(fn str-list-contains ((xs (list string)) (idx s32) (len s32) (s string)) s32
  (if (i32.ge_s idx len) (i32.const 0)
    (if (string=? (list-get xs idx) s) (i32.const 1)
      (str-list-contains xs (i32.add idx (i32.const 1)) len s))))

(fn str-index ((xs (list string)) (idx s32) (len s32) (s string)) s32
  (if (i32.ge_s idx len) (i32.const -1)
    (if (string=? (list-get xs idx) s) idx
      (str-index xs (i32.add idx (i32.const 1)) len s))))

; --- Traits & instances (Increment 4) ---
; (trait (Name T) (fn method (params) ret) ...) declares an interface; (instance (Name
; Type) (fn method (params) ret body) ...) implements it. A trait-method call resolves to
; the instance fn for the first argument's inferred type -- which works both directly and
; inside a specialized generic body (whose args are concretely typed after substitution).

(record tdef
  (td-name string)
  (td-methods (list string)))

(record idef
  (id-trait string)
  (id-type string)
  (id-methods (list sexpr)))

; Method names declared in a trait body: each item[2..] is (fn method (params) ret).
(fn collect-method-names ((items (list sexpr)) (idx s32) (len s32) (acc (list string))) (list string)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (form-has-head (list-get items idx) "fn") (i32.const 1))
      (collect-method-names items (i32.add idx (i32.const 1)) len (list-push acc (get-sym (list-get (get-lst (list-get items idx)) (i32.const 1)))))
      (collect-method-names items (i32.add idx (i32.const 1)) len acc))))

(fn parse-tdef ((form sexpr)) tdef
  (let (items (get-lst form))
    (tdef
      (get-sym (list-get (get-lst (list-get items (i32.const 1))) (i32.const 0)))
      (collect-method-names items (i32.const 2) (list-len items) (list-new string)))))

(fn collect-tdefs-acc ((forms (list sexpr)) (idx s32) (len s32) (acc (list tdef))) (list tdef)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (form-has-head (list-get forms idx) "trait") (i32.const 1))
      (collect-tdefs-acc forms (i32.add idx (i32.const 1)) len (list-push acc (parse-tdef (list-get forms idx))))
      (collect-tdefs-acc forms (i32.add idx (i32.const 1)) len acc))))

(fn collect-tdefs ((forms (list sexpr))) (list tdef)
  (collect-tdefs-acc forms (i32.const 0) (list-len forms) (list-new tdef)))

; Instance method fn forms are items[2..].
(fn collect-method-forms ((items (list sexpr)) (idx s32) (len s32) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (form-has-head (list-get items idx) "fn") (i32.const 1))
      (collect-method-forms items (i32.add idx (i32.const 1)) len (list-push acc (list-get items idx)))
      (collect-method-forms items (i32.add idx (i32.const 1)) len acc))))

(fn parse-idef ((form sexpr)) idef
  (let (items (get-lst form))
    (let (head (get-lst (list-get items (i32.const 1))))
      (idef
        (get-sym (list-get head (i32.const 0)))
        (if (is-sym (list-get head (i32.const 1))) (get-sym (list-get head (i32.const 1))) "s32")
        (collect-method-forms items (i32.const 2) (list-len items) (list-new sexpr))))))

(fn collect-idefs-acc ((forms (list sexpr)) (idx s32) (len s32) (acc (list idef))) (list idef)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (form-has-head (list-get forms idx) "instance") (i32.const 1))
      (collect-idefs-acc forms (i32.add idx (i32.const 1)) len (list-push acc (parse-idef (list-get forms idx))))
      (collect-idefs-acc forms (i32.add idx (i32.const 1)) len acc))))

(fn collect-idefs ((forms (list sexpr))) (list idef)
  (collect-idefs-acc forms (i32.const 0) (list-len forms) (list-new idef)))

; The trait that declares `method`, or "" if none.
(fn method-trait ((tdefs (list tdef)) (idx s32) (len s32) (method string)) string
  (if (i32.ge_s idx len) ""
    (if (i32.eq (str-list-contains (tdef.td-methods (list-get tdefs idx)) (i32.const 0) (list-len (tdef.td-methods (list-get tdefs idx))) method) (i32.const 1))
      (tdef.td-name (list-get tdefs idx))
      (method-trait tdefs (i32.add idx (i32.const 1)) len method))))

; trait--method--type
(fn mangle-instance ((trait string) (method string) (ty string)) string
  (string-append trait (string-append "--" (string-append method (string-append "--" ty)))))

; --- Deriving (Increment 5): (derive Eq Type) -> a generated Eq instance ---
; Reflect the record's fields and emit (instance (Eq Type) (fn eq ((a Type)(b Type)) s32
; <and of per-field i32.eq>)). Fields are 4-byte i32 slots, so i32.eq suits every field.
(fn sx2 ((a sexpr) (b sexpr)) sexpr
  (lst (list-push (list-push (list-new sexpr) a) b)))

(fn sx3 ((a sexpr) (b sexpr) (c sexpr)) sexpr
  (lst (list-push (list-push (list-push (list-new sexpr) a) b) c)))

; (i32.eq (Type.field a) (Type.field b))
(fn field-cmp ((type string) (fname string)) sexpr
  (let (acc (string-append type (string-append "." fname)))
    (sx3 (sym "i32.eq") (sx2 (sym acc) (sym "a")) (sx2 (sym acc) (sym "b")))))

; Right-fold per-field comparisons with i32.and; (i32.const 1) when there are no fields.
(fn eq-body ((type string) (fields (list record-field)) (idx s32) (len s32)) sexpr
  (if (i32.ge_s idx len)
    (sx2 (sym "i32.const") (num (i32.const 1)))
    (let (cmp (field-cmp type (record-field.field-name (list-get fields idx))))
      (if (i32.eq idx (i32.sub len (i32.const 1)))
        cmp
        (sx3 (sym "i32.and") cmp (eq-body type fields (i32.add idx (i32.const 1)) len))))))

(fn gen-eq-instance ((type string) (fields (list record-field))) sexpr
  (let (plist (list-push (list-push (list-new sexpr) (sx2 (sym "a") (sym type))) (sx2 (sym "b") (sym type))))
    (sx3 (sym "instance") (sx2 (sym "Eq") (sym type))
      (make-fn-form "eq" plist (sym "s32") (eq-body type fields (i32.const 0) (list-len fields))))))

(fn expand-derive-form ((form sexpr) (records (list record-def))) sexpr
  (if (i32.eq (form-has-head form "derive") (i32.const 1))
    (let (items (get-lst form))
      (if (if (string=? (get-sym (list-get items (i32.const 1))) "Eq") (i32.const 1) (i32.const 0))
        (let (type (get-sym (list-get items (i32.const 2))))
          (gen-eq-instance type (record-def.rec-fields (find-record records (i32.const 0) (list-len records) type))))
        form))
    form))

(fn expand-derives-acc ((forms (list sexpr)) (idx s32) (len s32) (records (list record-def)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (expand-derives-acc forms (i32.add idx (i32.const 1)) len records (list-push acc (expand-derive-form (list-get forms idx) records)))))

(fn expand-derives ((forms (list sexpr))) (list sexpr)
  (expand-derives-acc forms (i32.const 0) (list-len forms) (collect-records forms) (list-new sexpr)))

; A param (name (-> arg... ret)) is a function parameter (a compile-time function value).
(fn is-func-param ((p sexpr)) s32
  (if (is-lst p)
    (let (items (get-lst p))
      (if (i32.ge_s (list-len items) (i32.const 2))
        (form-has-head (list-get items (i32.const 1)) "->")
        (i32.const 0)))
    (i32.const 0)))

(fn has-func-param ((params (list sexpr)) (idx s32) (len s32)) s32
  (if (i32.ge_s idx len) (i32.const 0)
    (if (i32.eq (is-func-param (list-get params idx)) (i32.const 1)) (i32.const 1)
      (has-func-param params (i32.add idx (i32.const 1)) len))))

; A fn is a generic template iff it has a (where ...) clause (items[4]) or a function param.
(fn fn-has-where ((items (list sexpr)) ) s32
  (if (i32.ge_s (list-len items) (i32.const 6))
    (form-has-head (list-get items (i32.const 4)) "where")
    (i32.const 0)))

(fn is-generic-fn ((form sexpr)) s32
  (if (i32.eq (form-has-head form "fn") (i32.const 1))
    (let (items (get-lst form))
      (if (i32.ge_s (list-len items) (i32.const 5))
        (if (i32.eq (fn-has-where items) (i32.const 1)) (i32.const 1)
          (has-func-param (get-lst (list-get items (i32.const 2))) (i32.const 0) (list-len (get-lst (list-get items (i32.const 2))))))
        (i32.const 0)))
    (i32.const 0)))

; Names of the function params (for inlining); positions of func args in a call.
(fn func-param-names ((params (list sexpr)) (idx s32) (len s32) (acc (list string))) (list string)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (is-func-param (list-get params idx)) (i32.const 1))
      (func-param-names params (i32.add idx (i32.const 1)) len (list-push acc (get-sym (list-get (get-lst (list-get params idx)) (i32.const 0)))))
      (func-param-names params (i32.add idx (i32.const 1)) len acc))))

(fn drop-func-params ((params (list sexpr)) (idx s32) (len s32) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (is-func-param (list-get params idx)) (i32.const 1))
      (drop-func-params params (i32.add idx (i32.const 1)) len acc)
      (drop-func-params params (i32.add idx (i32.const 1)) len (list-push acc (list-get params idx))))))

; Function arguments at a call: the symbol passed at each function-param position.
(fn call-funcargs ((params (list sexpr)) (items (list sexpr)) (idx s32) (len s32) (acc (list string))) (list string)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (is-func-param (list-get params idx)) (i32.const 1))
      (call-funcargs params items (i32.add idx (i32.const 1)) len (list-push acc (get-sym (list-get items (i32.add idx (i32.const 1))))))
      (call-funcargs params items (i32.add idx (i32.const 1)) len acc))))

; Rewrite a template call's value args, dropping those at function-param positions.
(fn keep-nonfunc-args ((params (list sexpr)) (items (list sexpr)) (idx s32) (len s32) (ctx compile-ctx) (ts (list gtemplate)) (tdefs (list tdef)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (is-func-param (list-get params idx)) (i32.const 1))
      (keep-nonfunc-args params items (i32.add idx (i32.const 1)) len ctx ts tdefs acc)
      (keep-nonfunc-args params items (i32.add idx (i32.const 1)) len ctx ts tdefs (list-push acc (rewrite-expr (list-get items (i32.add idx (i32.const 1))) ctx ts tdefs))))))

; Add the type-arg symbols of a constraint list (Trait T U...) to acc (deduped).
(fn add-constraint-tparams ((acc (list string)) (items (list sexpr)) (idx s32) (len s32)) (list string)
  (if (i32.ge_s idx len) acc
    (if (is-sym (list-get items idx))
      (if (i32.eq (str-list-contains acc (i32.const 0) (list-len acc) (get-sym (list-get items idx))) (i32.const 1))
        (add-constraint-tparams acc items (i32.add idx (i32.const 1)) len)
        (add-constraint-tparams (list-push acc (get-sym (list-get items idx))) items (i32.add idx (i32.const 1)) len))
      (add-constraint-tparams acc items (i32.add idx (i32.const 1)) len))))

; Type parameters from a (where ...) clause: bare symbols plus the type args of each
; trait-bound entry (Trait T ...), deduped.
(fn where-typarams ((w (list sexpr)) (idx s32) (len s32) (acc (list string))) (list string)
  (if (i32.ge_s idx len) acc
    (let (e (list-get w idx))
      (if (is-sym e)
        (if (i32.eq (str-list-contains acc (i32.const 0) (list-len acc) (get-sym e)) (i32.const 1))
          (where-typarams w (i32.add idx (i32.const 1)) len acc)
          (where-typarams w (i32.add idx (i32.const 1)) len (list-push acc (get-sym e))))
        (if (is-lst e)
          (where-typarams w (i32.add idx (i32.const 1)) len (add-constraint-tparams acc (get-lst e) (i32.const 1) (list-len (get-lst e))))
          (where-typarams w (i32.add idx (i32.const 1)) len acc))))))

(fn parse-template ((form sexpr)) gtemplate
  (let (items (get-lst form))
    (if (i32.eq (fn-has-where items) (i32.const 1))
      ; (fn name params ret (where ...) body)
      (gtemplate
        (get-sym (list-get items (i32.const 1)))
        (get-lst (list-get items (i32.const 2)))
        (list-get items (i32.const 3))
        (list-get items (i32.const 5))
        (where-typarams (get-lst (list-get items (i32.const 4))) (i32.const 1) (list-len (get-lst (list-get items (i32.const 4)))) (list-new string)))
      ; HOF-only (no where): (fn name params ret body); no type params
      (gtemplate
        (get-sym (list-get items (i32.const 1)))
        (get-lst (list-get items (i32.const 2)))
        (list-get items (i32.const 3))
        (list-get items (i32.const 4))
        (list-new string)))))

(fn collect-templates-acc ((forms (list sexpr)) (idx s32) (len s32) (acc (list gtemplate))) (list gtemplate)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (is-generic-fn (list-get forms idx)) (i32.const 1))
      (collect-templates-acc forms (i32.add idx (i32.const 1)) len (list-push acc (parse-template (list-get forms idx))))
      (collect-templates-acc forms (i32.add idx (i32.const 1)) len acc))))

(fn collect-templates ((forms (list sexpr))) (list gtemplate)
  (collect-templates-acc forms (i32.const 0) (list-len forms) (list-new gtemplate)))

(fn find-template ((ts (list gtemplate)) (idx s32) (len s32) (name string)) gtemplate
  (if (i32.ge_s idx len)
    (gtemplate "" (list-new sexpr) (sym "") (sym "") (list-new string))
    (if (string=? (gtemplate.gt-name (list-get ts idx)) name)
      (list-get ts idx)
      (find-template ts (i32.add idx (i32.const 1)) len name))))

; base--arg1--arg2...
(fn mangle ((base string) (args (list string)) (idx s32) (len s32)) string
  (if (i32.ge_s idx len) base
    (mangle (string-append base (string-append "--" (list-get args idx))) args (i32.add idx (i32.const 1)) len)))

; Parse a canonical type string ("s32" / "(list s32)") back to a type sexpr.
(fn parse-type-str ((s string)) sexpr
  (if (i32.le_s (string-len s) (i32.const 0))
    (sym "s32")
    (let (forms (read-all s))
      (if (i32.gt_s (list-len forms) (i32.const 0))
        (list-get forms (i32.const 0))
        (sym "s32")))))

; Structured type of an expression (for compound-type-param unification). Handles list
; construction and variables; other forms fall back to the scalar inferrer.
; Element type of a list-valued expression ((list E) -> E), else s32.
(fn list-elem-type ((list-expr sexpr) (ctx compile-ctx)) sexpr
  (let (lt (infer-type-sexpr list-expr ctx))
    (if (if (is-lst lt) (i32.eq (list-len (get-lst lt)) (i32.const 2)) (i32.const 0))
      (list-get (get-lst lt) (i32.const 1))
      (sym "s32"))))

(fn infer-type-sexpr-list ((items (list sexpr)) (ctx compile-ctx)) sexpr
  (if (i32.eq (list-len items) (i32.const 0))
    (sym "s32")
    (let (head (list-get items (i32.const 0)))
      (if (is-sym head)
        (if (string=? (get-sym head) "list-new")
          (sx2 (sym "list") (list-get items (i32.const 1)))
          (if (string=? (get-sym head) "list-push")
            (infer-type-sexpr (list-get items (i32.const 1)) ctx)
            (if (string=? (get-sym head) "list-get")
              (list-elem-type (list-get items (i32.const 1)) ctx)
              (parse-type-str (infer-type-list items ctx)))))
        (sym "s32")))))

(fn infer-type-sexpr ((e sexpr) (ctx compile-ctx)) sexpr
  (match e
    ((sym s) (parse-type-str (vartype-of ctx s)))
    ((num n) (sym "s32"))
    ((fnum f) (sym "f64"))
    ((str s) (sym "string"))
    ((lst items) (infer-type-sexpr-list items ctx))))

; Unify a type-pattern against a concrete type, returning tvar's binding string ("" if
; the pattern does not mention tvar). Structural: (list T) vs (list s32) binds T=s32.
(fn find-binding-list ((ps (list sexpr)) (cs (list sexpr)) (idx s32) (len s32) (tvar string)) string
  (if (i32.ge_s idx len) ""
    (let (b (find-binding (list-get ps idx) (list-get cs idx) tvar))
      (if (string=? b "") (find-binding-list ps cs (i32.add idx (i32.const 1)) len tvar) b))))

(fn find-binding ((pat sexpr) (conc sexpr) (tvar string)) string
  (if (is-sym pat)
    (if (string=? (get-sym pat) tvar) (type-str conc) "")
    (if (if (is-lst pat) (is-lst conc) (i32.const 0))
      (let (pl (get-lst pat))
        (let (cl (get-lst conc))
          (find-binding-list pl cl (i32.const 0)
            (if (i32.lt_s (list-len pl) (list-len cl)) (list-len pl) (list-len cl)) tvar)))
      "")))

; Concrete type for one type var: unify each param's type pattern against its arg's type.
(fn infer-one-typearg ((params (list sexpr)) (items (list sexpr)) (pidx s32) (plen s32) (ctx compile-ctx) (tv string)) string
  (if (i32.ge_s pidx plen) "s32"
    (let (b (find-binding (list-get (get-lst (list-get params pidx)) (i32.const 1)) (infer-type-sexpr (list-get items (i32.add pidx (i32.const 1))) ctx) tv))
      (if (string=? b "") (infer-one-typearg params items (i32.add pidx (i32.const 1)) plen ctx tv) b))))

; Concrete type args for a template call, in typaram order.
(fn infer-typeargs ((t gtemplate) (items (list sexpr)) (ctx compile-ctx) (idx s32) (len s32) (acc (list string))) (list string)
  (if (i32.ge_s idx len) acc
    (infer-typeargs t items ctx (i32.add idx (i32.const 1)) len
      (list-push acc (infer-one-typearg (gtemplate.gt-params t) items (i32.const 0) (list-len (gtemplate.gt-params t)) ctx (list-get (gtemplate.gt-typarams t) idx))))))

; Substitute type vars -> concretes throughout an sexpr (syntactic).
(fn subst-tv-list ((items (list sexpr)) (idx s32) (len s32) (tps (list string)) (cs (list string)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (subst-tv-list items (i32.add idx (i32.const 1)) len tps cs (list-push acc (subst-type-vars (list-get items idx) tps cs)))))

(fn subst-type-vars ((e sexpr) (tps (list string)) (cs (list string))) sexpr
  (match e
    ((sym s) (let (i (str-index tps (i32.const 0) (list-len tps) s))
               (if (i32.ge_s i (i32.const 0)) (sym (list-get cs i)) e)))
    ((num n) e)
    ((fnum f) e)
    ((str s) e)
    ((lst items) (lst (subst-tv-list items (i32.const 0) (list-len items) tps cs (list-new sexpr))))))

; Build a 5-element (fn name (params) ret body) form.
(fn make-fn-form ((mname string) (params (list sexpr)) (ret sexpr) (body sexpr)) sexpr
  (lst (list-push (list-push (list-push (list-push (list-push (list-new sexpr)
    (sym "fn")) (sym mname)) (lst params)) ret) body)))

; --- Rewrite template calls to mangled names and trait-method calls to instances ---
(fn rewrite-args ((items (list sexpr)) (idx s32) (len s32) (ctx compile-ctx) (ts (list gtemplate)) (tdefs (list tdef)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (rewrite-args items (i32.add idx (i32.const 1)) len ctx ts tdefs (list-push acc (rewrite-expr (list-get items idx) ctx ts tdefs)))))

(fn rewrite-expr ((e sexpr) (ctx compile-ctx) (ts (list gtemplate)) (tdefs (list tdef))) sexpr
  (if (is-lst e)
    (let (items (get-lst e))
      (if (i32.gt_s (list-len items) (i32.const 0))
        (let (head (list-get items (i32.const 0)))
          (if (is-sym head)
            (let (t (find-template ts (i32.const 0) (list-len ts) (get-sym head)))
              (if (string=? (gtemplate.gt-name t) "")
                ; not a template -- maybe a trait method, dispatched on the first arg's type
                (let (tr (method-trait tdefs (i32.const 0) (list-len tdefs) (get-sym head)))
                  (if (string=? tr "")
                    (lst (rewrite-args items (i32.const 0) (list-len items) ctx ts tdefs (list-new sexpr)))
                    (lst (rewrite-args items (i32.const 1) (list-len items) ctx ts tdefs
                      (list-push (list-new sexpr) (sym (mangle-instance tr (get-sym head) (infer-type (list-get items (i32.const 1)) ctx))))))))
                (let (targs (infer-typeargs t items ctx (i32.const 0) (list-len (gtemplate.gt-typarams t)) (list-new string)))
                  (let (fargs (call-funcargs (gtemplate.gt-params t) items (i32.const 0) (list-len (gtemplate.gt-params t)) (list-new string)))
                    (lst (keep-nonfunc-args (gtemplate.gt-params t) items (i32.const 0) (list-len (gtemplate.gt-params t)) ctx ts tdefs
                      (list-push (list-new sexpr) (sym (mangle (mangle (get-sym head) fargs (i32.const 0) (list-len fargs)) targs (i32.const 0) (list-len targs))))))))))
            (lst (rewrite-args items (i32.const 0) (list-len items) ctx ts tdefs (list-new sexpr)))))
        e))
    e))

; --- Collect the specializations needed by an expression ---
(fn collect-specs-args ((items (list sexpr)) (idx s32) (len s32) (ctx compile-ctx) (ts (list gtemplate)) (acc (list spec))) (list spec)
  (if (i32.ge_s idx len) acc
    (collect-specs-args items (i32.add idx (i32.const 1)) len ctx ts (collect-specs-expr (list-get items idx) ctx ts acc))))

(fn collect-specs-expr ((e sexpr) (ctx compile-ctx) (ts (list gtemplate)) (acc (list spec))) (list spec)
  (if (is-lst e)
    (let (items (get-lst e))
      (if (i32.gt_s (list-len items) (i32.const 0))
        (let (head (list-get items (i32.const 0)))
          (if (is-sym head)
            (let (t (find-template ts (i32.const 0) (list-len ts) (get-sym head)))
              (if (string=? (gtemplate.gt-name t) "")
                (collect-specs-args items (i32.const 0) (list-len items) ctx ts acc)
                (collect-specs-args items (i32.const 1) (list-len items) ctx ts
                  (list-push acc (spec (get-sym head)
                    (call-funcargs (gtemplate.gt-params t) items (i32.const 0) (list-len (gtemplate.gt-params t)) (list-new string))
                    (infer-typeargs t items ctx (i32.const 0) (list-len (gtemplate.gt-typarams t)) (list-new string)))))))
            (collect-specs-args items (i32.const 0) (list-len items) ctx ts acc)))
        acc))
    acc))

(fn append-specs ((worklist (list spec)) (specs (list spec)) (idx s32) (len s32)) (list spec)
  (if (i32.ge_s idx len) worklist
    (append-specs (list-push worklist (list-get specs idx)) specs (i32.add idx (i32.const 1)) len)))

; Drain the worklist: specialize each not-yet-emitted spec, append transitive specs.
; worklist is grown in place via list-push, so (list-len worklist) re-reads the new length.
(fn drain ((worklist (list spec)) (widx s32) (ts (list gtemplate)) (tdefs (list tdef)) (ctx0 compile-ctx) (emitted (list string)) (output (list sexpr))) (list sexpr)
  (if (i32.ge_s widx (list-len worklist))
    output
    (let (sp (list-get worklist widx))
      (let (mname (mangle (mangle (spec.sp-name sp) (spec.sp-funcargs sp) (i32.const 0) (list-len (spec.sp-funcargs sp))) (spec.sp-args sp) (i32.const 0) (list-len (spec.sp-args sp))))
        (if (i32.eq (str-list-contains emitted (i32.const 0) (list-len emitted) mname) (i32.const 1))
          (drain worklist (i32.add widx (i32.const 1)) ts tdefs ctx0 emitted output)
          (let (t (find-template ts (i32.const 0) (list-len ts) (spec.sp-name sp)))
            (let (sparams (subst-tv-list (gtemplate.gt-params t) (i32.const 0) (list-len (gtemplate.gt-params t)) (gtemplate.gt-typarams t) (spec.sp-args sp) (list-new sexpr)))
              (let (fparams (drop-func-params sparams (i32.const 0) (list-len sparams) (list-new sexpr)))
                (let (sret (subst-type-vars (gtemplate.gt-ret t) (gtemplate.gt-typarams t) (spec.sp-args sp)))
                  ; type-substitute, then inline function params (name -> passed fn name)
                  (let (ibody (subst-type-vars (subst-type-vars (gtemplate.gt-body t) (gtemplate.gt-typarams t) (spec.sp-args sp)) (func-param-names (gtemplate.gt-params t) (i32.const 0) (list-len (gtemplate.gt-params t)) (list-new string)) (spec.sp-funcargs sp)))
                    (let (pctx (ctx-with-params ctx0 fparams (i32.const 0) (list-len fparams)))
                      (let (newspecs (collect-specs-expr ibody pctx ts (list-new spec)))
                        (let (grown (append-specs worklist newspecs (i32.const 0) (list-len newspecs)))
                          (let (rbody (rewrite-expr ibody pctx ts tdefs))
                            (drain grown (i32.add widx (i32.const 1)) ts tdefs ctx0
                              (list-push emitted mname)
                              (list-push output (make-fn-form mname fparams sret rbody)))))))))))))))))

; --- Top-level form handling ---
; Drop generic templates, trait decls, and instance decls (all handled separately).
(fn is-dropped-form ((form sexpr)) s32
  (if (i32.eq (is-generic-fn form) (i32.const 1)) (i32.const 1)
    (if (i32.eq (form-has-head form "trait") (i32.const 1)) (i32.const 1)
      (form-has-head form "instance"))))

(fn drop-templates ((forms (list sexpr)) (idx s32) (len s32) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (if (i32.eq (is-dropped-form (list-get forms idx)) (i32.const 1))
      (drop-templates forms (i32.add idx (i32.const 1)) len acc)
      (drop-templates forms (i32.add idx (i32.const 1)) len (list-push acc (list-get forms idx))))))

(fn collect-specs-form ((form sexpr) (ctx0 compile-ctx) (ts (list gtemplate))) (list spec)
  (if (i32.eq (form-has-head form "fn") (i32.const 1))
    (let (items (get-lst form))
      (if (i32.ge_s (list-len items) (i32.const 5))
        (let (params (get-lst (list-get items (i32.const 2))))
          (collect-specs-expr (list-get items (i32.const 4)) (ctx-with-params ctx0 params (i32.const 0) (list-len params)) ts (list-new spec)))
        (list-new spec)))
    (if (i32.eq (form-has-head form "export") (i32.const 1))
      (if (if (i32.ge_s (list-len (get-lst form)) (i32.const 2)) (is-lst (list-get (get-lst form) (i32.const 1))) (i32.const 0))
        (collect-specs-form (list-get (get-lst form) (i32.const 1)) ctx0 ts)
        (list-new spec))
      (list-new spec))))

(fn collect-specs-forms ((forms (list sexpr)) (idx s32) (len s32) (ctx0 compile-ctx) (ts (list gtemplate)) (acc (list spec))) (list spec)
  (if (i32.ge_s idx len) acc
    (collect-specs-forms forms (i32.add idx (i32.const 1)) len ctx0 ts
      (append-specs acc (collect-specs-form (list-get forms idx) ctx0 ts) (i32.const 0) (list-len (collect-specs-form (list-get forms idx) ctx0 ts))))))

(fn rewrite-form ((form sexpr) (ctx0 compile-ctx) (ts (list gtemplate)) (tdefs (list tdef))) sexpr
  (if (i32.eq (form-has-head form "fn") (i32.const 1))
    (let (items (get-lst form))
      (if (i32.ge_s (list-len items) (i32.const 5))
        (let (params (get-lst (list-get items (i32.const 2))))
          (make-fn-form (get-sym (list-get items (i32.const 1))) params (list-get items (i32.const 3))
            (rewrite-expr (list-get items (i32.const 4)) (ctx-with-params ctx0 params (i32.const 0) (list-len params)) ts tdefs)))
        form))
    (if (i32.eq (form-has-head form "export") (i32.const 1))
      (if (if (i32.ge_s (list-len (get-lst form)) (i32.const 2)) (is-lst (list-get (get-lst form) (i32.const 1))) (i32.const 0))
        (lst (list-push (list-push (list-new sexpr) (sym "export")) (rewrite-form (list-get (get-lst form) (i32.const 1)) ctx0 ts tdefs)))
        form)
      form)))

(fn rewrite-forms ((forms (list sexpr)) (idx s32) (len s32) (ctx0 compile-ctx) (ts (list gtemplate)) (tdefs (list tdef)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (rewrite-forms forms (i32.add idx (i32.const 1)) len ctx0 ts tdefs (list-push acc (rewrite-form (list-get forms idx) ctx0 ts tdefs)))))

; Emit each instance's methods as monomorphic fns named trait--method--type, with bodies
; rewritten (so trait/generic calls inside them resolve too).
(fn emit-idef-methods ((methods (list sexpr)) (idx s32) (len s32) (trait string) (ty string) (ctx0 compile-ctx) (ts (list gtemplate)) (tdefs (list tdef)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (let (m (get-lst (list-get methods idx)))
      (let (params (get-lst (list-get m (i32.const 2))))
        (emit-idef-methods methods (i32.add idx (i32.const 1)) len trait ty ctx0 ts tdefs
          (list-push acc (make-fn-form (mangle-instance trait (get-sym (list-get m (i32.const 1))) ty) params (list-get m (i32.const 3))
            (rewrite-expr (list-get m (i32.const 4)) (ctx-with-params ctx0 params (i32.const 0) (list-len params)) ts tdefs))))))))

(fn emit-instances ((idefs (list idef)) (idx s32) (len s32) (ctx0 compile-ctx) (ts (list gtemplate)) (tdefs (list tdef)) (acc (list sexpr))) (list sexpr)
  (if (i32.ge_s idx len) acc
    (let (i (list-get idefs idx))
      (emit-instances idefs (i32.add idx (i32.const 1)) len ctx0 ts tdefs
        (emit-idef-methods (idef.id-methods i) (i32.const 0) (list-len (idef.id-methods i)) (idef.id-trait i) (idef.id-type i) ctx0 ts tdefs acc)))))

(fn append-forms ((a (list sexpr)) (b (list sexpr)) (idx s32) (len s32)) (list sexpr)
  (if (i32.ge_s idx len) a
    (append-forms (list-push a (list-get b idx)) b (i32.add idx (i32.const 1)) len)))

(fn monomorphize ((forms0 (list sexpr))) (list sexpr)
  (let (forms (expand-derives forms0))
    (let (templates (collect-templates forms))
    (let (idefs (collect-idefs forms))
      (if (if (i32.eq (list-len templates) (i32.const 0)) (i32.eq (list-len idefs) (i32.const 0)) (i32.const 0))
        forms
        (let (tdefs (collect-tdefs forms))
          (let (ctx0 (compile-ctx (add-builtin-variants (collect-variants forms)) (collect-records forms) (collect-imports forms) (list-new vartype) (collect-sigs forms)))
            (let (retained (drop-templates forms (i32.const 0) (list-len forms) (list-new sexpr)))
              (let (seeds (collect-specs-forms retained (i32.const 0) (list-len retained) ctx0 templates (list-new spec)))
                (let (specialized (drain seeds (i32.const 0) templates tdefs ctx0 (list-new string) (list-new sexpr)))
                  (let (instmethods (emit-instances idefs (i32.const 0) (list-len idefs) ctx0 templates tdefs (list-new sexpr)))
                    (append-forms
                      (append-forms (rewrite-forms retained (i32.const 0) (list-len retained) ctx0 templates tdefs (list-new sexpr)) specialized (i32.const 0) (list-len specialized))
                      instmethods (i32.const 0) (list-len instmethods)))))))))))))

; Compile source to WAT module
(fn compile ((src string)) string
  (let (raw-forms (read-all src))
    (let (forms (monomorphize (expand-all raw-forms (collect-macros raw-forms))))
    (let (variants (add-builtin-variants (collect-variants forms)))
      (let (records (collect-records forms))
        (let (imports (collect-imports forms))
          (let (ctx (compile-ctx variants records imports (list-new vartype) (collect-sigs forms)))
            (let (body (compile-toplevels forms (i32.const 0) (list-len forms) "" ctx))
              (let (runtime (get-runtime))
                (let (import-wat (compile-imports imports (i32.const 0) (list-len imports) ""))
                  (let (data-wat (compile-data-segments forms (i32.const 0) (list-len forms) ""))
                    (string-append "(module\n"
                      (string-append import-wat
                        (string-append "  (memory (export \"memory\") 1)\n"
                          (string-append data-wat
                            (string-append runtime
                              (string-append body "\n)")))))))))))))))))

; ============================================================
; Test Exports
; ============================================================

; Test: compile a simple identity function
(export (fn test-compile-identity () s32
  (let (src "(fn identity ((x s32)) s32 x)")
    (let (wat (compile src))
      (if (i32.gt_s (string-len wat) (i32.const 50))
        (i32.const 1)
        (i32.const 0))))))

; Test: compile factorial
(export (fn test-compile-factorial () s32
  (let (src "(fn factorial ((n s32)) s32 (if (i32.le_s n (i32.const 1)) (i32.const 1) (i32.mul n (factorial (i32.sub n (i32.const 1))))))")
    (let (wat (compile src))
      (if (i32.gt_s (string-len wat) (i32.const 100))
        (i32.const 1)
        (i32.const 0))))))

; Get compiled WAT for identity function
(export (fn get-identity-wat () string
  (compile "(fn identity ((x s32)) s32 x)")))

; Get compiled WAT for factorial
(export (fn get-factorial-wat () string
  (compile "(export (fn factorial ((n s32)) s32 (if (i32.le_s n (i32.const 1)) (i32.const 1) (i32.mul n (factorial (i32.sub n (i32.const 1)))))))")))

; Bootstrap: compile arbitrary source code
(export (fn compile-source ((src string)) string
  (compile src)))
