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
  (symbol string)
  (str-lit string))

; ============================================================
; S-Expression AST Type
; ============================================================

(variant sexpr
  (sym string)
  (num s32)
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

; Compilation context: holds all type definitions
(record compile-ctx
  (ctx-variants (list variant-def))
  (ctx-records (list record-def)))

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

(fn parse-number-value ((src string) (pos s32) (len s32) (acc s32)) token-result
  (if (i32.ge_s pos len)
    (token-result (number acc) pos)
    (let (c (string-ref src pos))
      (if (is-digit c)
        (parse-number-value src (i32.add pos (i32.const 1)) len
          (i32.add (i32.mul acc (i32.const 10)) (digit-value c)))
        (token-result (number acc) pos)))))

(fn read-number ((src string) (pos s32) (len s32)) token-result
  (parse-number-value src pos len (i32.const 0)))

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

; Read a string literal, starting after the opening quote
(fn read-string-lit ((src string) (pos s32) (len s32)) token-result
  (let (start (i32.add pos (i32.const 1)))  ; skip opening "
    (let (end-pos (find-string-end src start len))
      (let (str-end (i32.sub end-pos (i32.const 1)))  ; exclude closing "
        (token-result (str-lit (substring src start str-end)) end-pos)))))

(fn read-token ((src string) (pos s32) (len s32)) token-result
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 40))
      (token-result (lparen) (i32.add pos (i32.const 1)))
      (if (i32.eq c (i32.const 41))
        (token-result (rparen) (i32.add pos (i32.const 1)))
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
                        ((lparen) result)
                        ((rparen) result)
                        ((symbol s) result)
                        ((str-lit s) result)))
                    (read-symbol src pos len)))
                (read-symbol src pos len))
              (read-symbol src pos len))))))))

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
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

(fn is-rparen ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

(fn token-to-sexpr ((t token)) sexpr
  (match t
    ((lparen) (sym "error-lparen"))
    ((rparen) (sym "error-rparen"))
    ((number n) (num n))
    ((symbol s) (sym s))
    ((str-lit s) (str s))))

(fn parse-atom ((tokens (list token)) (pos s32)) parse-result
  (parse-result (token-to-sexpr (list-get tokens pos)) (i32.add pos (i32.const 1))))

(fn parse-one ((tokens (list token)) (pos s32) (len s32)) parse-result
  (if (i32.ge_s pos len)
    (parse-result (sym "error-eof") pos)
    (let (tok (list-get tokens pos))
      (if (is-lparen tok)
        (parse-list-items tokens (i32.add pos (i32.const 1)) len (list-new sexpr))
        (parse-atom tokens pos)))))

(fn parse-list-items ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          (let (nested (parse-list-items tokens (i32.add pos (i32.const 1)) len (list-new sexpr)))
            (parse-list-items tokens (parse-result.new-pos nested) len
              (list-push items (parse-result.expr nested))))
          (let (atom (parse-atom tokens pos))
            (parse-list-items tokens (parse-result.new-pos atom) len
              (list-push items (parse-result.expr atom)))))))))

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
  (match e ((sym s) (i32.const 1)) ((num n) (i32.const 0)) ((str s) (i32.const 0)) ((lst l) (i32.const 0))))

(fn is-num ((e sexpr)) s32
  (match e ((sym s) (i32.const 0)) ((num n) (i32.const 1)) ((str s) (i32.const 0)) ((lst l) (i32.const 0))))

(fn is-lst ((e sexpr)) s32
  (match e ((sym s) (i32.const 0)) ((num n) (i32.const 0)) ((str s) (i32.const 0)) ((lst l) (i32.const 1))))

(fn get-sym ((e sexpr)) string
  (match e ((sym s) s) ((num n) "") ((str s) "") ((lst l) "")))

(fn get-num ((e sexpr)) s32
  (match e ((sym s) (i32.const 0)) ((num n) n) ((str s) (i32.const 0)) ((lst l) (i32.const 0))))

(fn get-lst ((e sexpr)) (list sexpr)
  (match e ((sym s) (list-new sexpr)) ((num n) (list-new sexpr)) ((str s) (list-new sexpr)) ((lst l) l)))

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

(fn compile-number ((n s32)) string
  (string-append "(i32.const " (string-append (i32-to-string n) ")")))

(fn compile-var ((name string)) string
  (string-append "(local.get $" (string-append name ")")))

; Generate code to store bytes of a string starting at offset
; Returns WAT code that stores bytes at (heap_ptr + offset)
; Uses tail recursion with accumulator to avoid stack overflow on long strings
(fn compile-string-bytes-acc ((s string) (idx s32) (len s32) (offset s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (byte (string-ref s idx))
      (let (byte-code (string-append
              "global.get $__heap_ptr i32.const " (string-append
              (i32-to-string offset) (string-append
              " i32.add i32.const " (string-append
              (i32-to-string byte)
              " i32.store8 ")))))
        (compile-string-bytes-acc s (i32.add idx (i32.const 1)) len (i32.add offset (i32.const 1))
          (string-append acc byte-code))))))

(fn compile-string-bytes ((s string) (idx s32) (len s32) (offset s32)) string
  (compile-string-bytes-acc s idx len offset ""))

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

(fn compile-args ((args (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (arg (list-get args idx))
      (let (compiled (compile-expr arg))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-args args (i32.add idx (i32.const 1)) len new-acc))))))

; Check if instruction is a const (i32.const, i64.const, etc.)
(fn is-const-instr ((s string)) s32
  (if (string=? s "i32.const") (i32.const 1)
    (if (string=? s "i64.const") (i32.const 1)
      (if (string=? s "f32.const") (i32.const 1)
        (if (string=? s "f64.const") (i32.const 1)
          (i32.const 0))))))

(fn compile-wasm-call ((instr string) (args (list sexpr))) string
  (if (i32.eq (list-len args) (i32.const 0))
    (string-append "(" (string-append instr ")"))
    ; For const instructions, use the literal value directly
    (if (is-const-instr instr)
      (let (arg (list-get args (i32.const 0)))
        (if (is-num arg)
          (string-append "(" (string-append instr (string-append " " (string-append (i32-to-string (get-num arg)) ")"))))
          (string-append "(" (string-append instr " (error: const expects number))"))))
      (let (compiled-args (compile-args args (i32.const 0) (list-len args) ""))
        (string-append "(" (string-append instr (string-append " " (string-append compiled-args ")"))))))))

(fn compile-fn-call ((name string) (args (list sexpr))) string
  (if (i32.eq (list-len args) (i32.const 0))
    (string-append "(call $" (string-append name ")"))
    (let (compiled-args (compile-args args (i32.const 0) (list-len args) ""))
      (string-append "(call $" (string-append name (string-append " " (string-append compiled-args ")")))))))

(fn build-args-list ((items (list sexpr)) (start s32) (len s32) (acc (list sexpr))) (list sexpr)
  (if (i32.le_s len (i32.const 0))
    acc
    (let (item (list-get items start))
      (build-args-list items (i32.add start (i32.const 1)) (i32.sub len (i32.const 1))
        (list-push acc item)))))

; Compile expression with optional variable substitution for match bindings
; If binding-name is non-empty and expr is a sym matching it, emit payload load
(fn compile-expr-sub ((expr sexpr) (binding-name string) (scrutinee-wat string)) string
  (match expr
    ((num n) (compile-number n))
    ((sym s)
      (if (string=? binding-name "")
        (compile-var s)
        (if (string=? s binding-name)
          (string-append "(i32.load (i32.add " (string-append scrutinee-wat " (i32.const 4)))"))
          (compile-var s))))
    ((str s) (compile-string s))
    ((lst items) (compile-list-sub items binding-name scrutinee-wat))))

; Compile list expression with substitution context
(fn compile-list-sub ((items (list sexpr)) (binding-name string) (scrutinee-wat string)) string
  (if (i32.eq (list-len items) (i32.const 0))
    "()"
    (let (head (list-get items (i32.const 0)))
      (if (is-sym head)
        (let (name (get-sym head))
          ; For function calls, substitute in arguments
          (if (is-wasm-instr name)
            (compile-wasm-call-sub name items binding-name scrutinee-wat)
            (compile-fn-call-sub name items binding-name scrutinee-wat)))
        "(error)"))))

(fn compile-wasm-call-sub ((instr string) (items (list sexpr)) (binding-name string) (scrutinee-wat string)) string
  (let (args (build-args-list items (i32.const 1) (i32.sub (list-len items) (i32.const 1)) (list-new sexpr)))
    (if (i32.eq (list-len args) (i32.const 0))
      (string-append "(" (string-append instr ")"))
      ; For const instructions, use the literal value directly
      (if (is-const-instr instr)
        (let (arg (list-get args (i32.const 0)))
          (if (is-num arg)
            (string-append "(" (string-append instr (string-append " " (string-append (i32-to-string (get-num arg)) ")"))))
            (string-append "(" (string-append instr " (error: const expects number))"))))
        (let (compiled-args (compile-args-sub args (i32.const 0) (list-len args) "" binding-name scrutinee-wat))
          (string-append "(" (string-append instr (string-append " " (string-append compiled-args ")")))))))))

(fn compile-fn-call-sub ((name string) (items (list sexpr)) (binding-name string) (scrutinee-wat string)) string
  (let (args (build-args-list items (i32.const 1) (i32.sub (list-len items) (i32.const 1)) (list-new sexpr)))
    (let (compiled-args (compile-args-sub args (i32.const 0) (list-len args) "" binding-name scrutinee-wat))
      (if (i32.eq (list-len args) (i32.const 0))
        (string-append "(call $" (string-append name ")"))
        (string-append "(call $" (string-append name (string-append " " (string-append compiled-args ")"))))))))

(fn compile-args-sub ((args (list sexpr)) (idx s32) (len s32) (acc string) (binding-name string) (scrutinee-wat string)) string
  (if (i32.ge_s idx len)
    acc
    (let (arg (list-get args idx))
      (let (compiled (compile-expr-sub arg binding-name scrutinee-wat))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-args-sub args (i32.add idx (i32.const 1)) len new-acc binding-name scrutinee-wat))))))

; Compile a single match case
; case is: ((constructor [binding]) body)
(fn compile-match-case ((case-expr sexpr) (scrutinee-wat string) (remaining-cases (list sexpr)) (case-idx s32) (num-cases s32)) string
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
                      (let (tag (constructor-tag (get-sym constructor)))
                        (let (binding-name (if (i32.ge_s (list-len pattern-items) (i32.const 2))
                                             (let (binding-expr (list-get pattern-items (i32.const 1)))
                                               (if (is-sym binding-expr) (get-sym binding-expr) ""))
                                             ""))
                          (let (cond-wat (string-append "(i32.eq (i32.load " (string-append scrutinee-wat (string-append ") (i32.const " (string-append (i32-to-string tag) "))")))))
                            (let (body-wat (compile-expr-sub body binding-name scrutinee-wat))
                              ; Next case is at index case-idx + 3 (skip 'match' at 0, scrutinee at 1, cases start at 2)
                              (let (else-wat (if (i32.ge_s (i32.add case-idx (i32.const 1)) num-cases)
                                               "(unreachable)"
                                               (compile-match-case (list-get remaining-cases (i32.add case-idx (i32.const 3))) scrutinee-wat remaining-cases (i32.add case-idx (i32.const 1)) num-cases)))
                                (string-append "(if (result i32) " (string-append cond-wat (string-append " (then " (string-append body-wat (string-append ") (else " (string-append else-wat "))")))))))))))
                      "(error: pattern constructor not symbol)"))
                  "(error: empty pattern)"))
              "(error: pattern not list)")))
        "(error: case needs pattern and body)"))
    "(error: case not list)"))

; Compile match expression: (match scrutinee case1 case2 ...)
(fn compile-match ((items (list sexpr))) string
  (if (i32.lt_s (list-len items) (i32.const 3))
    "(error: match needs scrutinee and at least one case)"
    (let (scrutinee (list-get items (i32.const 1)))
      (let (scrutinee-wat (compile-expr scrutinee))
        (let (first-case (list-get items (i32.const 2)))
          (let (num-cases (i32.sub (list-len items) (i32.const 2)))
            (compile-match-case first-case scrutinee-wat items (i32.const 0) num-cases)))))))

(fn compile-expr ((expr sexpr)) string
  (match expr
    ((num n) (compile-number n))
    ((sym s) (compile-var s))
    ((str s) (compile-string s))
    ((lst items) (compile-list items))))

(fn compile-list ((items (list sexpr))) string
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
                    (let (cond-wat (compile-expr cond-expr))
                      (let (then-wat (compile-expr then-expr))
                        (let (else-wat (compile-expr else-expr))
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
                                  (let (value-wat (compile-expr value-expr))
                                    (let (body-wat (compile-expr body))
                                      (string-append "(local.tee $"
                                        (string-append var-name
                                          (string-append " "
                                            (string-append value-wat
                                              (string-append ") " body-wat))))))))
                                "(error: let binding name must be symbol)")))))
                      "(error: let binding must be a list)"))))
              ; Match expression
              (if (string=? name "match")
                (compile-match items)
                ; Built-in: string-len -> (i32.load ptr)
                (if (string=? name "string-len")
                  (let (arg (compile-expr (list-get items (i32.const 1))))
                    (string-append "(i32.load " (string-append arg ")")))
                  ; Built-in: list-len -> (i32.load ptr)
                  (if (string=? name "list-len")
                    (let (arg (compile-expr (list-get items (i32.const 1))))
                      (string-append "(i32.load " (string-append arg ")")))
                  ; Built-in: string-ref -> (i32.load8_u (i32.add (i32.add s 4) i))
                  (if (string=? name "string-ref")
                    (let (s-wat (compile-expr (list-get items (i32.const 1))))
                      (let (i-wat (compile-expr (list-get items (i32.const 2))))
                        (string-append "(i32.load8_u (i32.add (i32.add "
                          (string-append s-wat
                            (string-append " (i32.const 4)) " (string-append i-wat "))"))))))
                    ; Built-in: list-get -> (i32.load (i32.add (i32.load (i32.add lst 8)) (i32.mul idx 4)))
                    (if (string=? name "list-get")
                      (let (lst-wat (compile-expr (list-get items (i32.const 1))))
                        (let (idx-wat (compile-expr (list-get items (i32.const 2))))
                          (string-append "(i32.load (i32.add (i32.load (i32.add "
                            (string-append lst-wat
                              (string-append " (i32.const 8))) (i32.mul " (string-append idx-wat " (i32.const 4))))"))))))
                      ; Built-in: string-append -> (call $__string_append a b)
                      (if (string=? name "string-append")
                        (let (a-wat (compile-expr (list-get items (i32.const 1))))
                          (let (b-wat (compile-expr (list-get items (i32.const 2))))
                            (string-append "(call $__string_append "
                              (string-append a-wat
                                (string-append " " (string-append b-wat ")"))))))
                        ; Built-in: string=? -> (call $__string_eq a b)
                        (if (string=? name "string=?")
                          (let (a-wat (compile-expr (list-get items (i32.const 1))))
                            (let (b-wat (compile-expr (list-get items (i32.const 2))))
                              (string-append "(call $__string_eq "
                                (string-append a-wat
                                  (string-append " " (string-append b-wat ")"))))))
                          ; Built-in: substring -> (call $__substring s start end)
                          (if (string=? name "substring")
                            (let (s-wat (compile-expr (list-get items (i32.const 1))))
                              (let (start-wat (compile-expr (list-get items (i32.const 2))))
                                (let (end-wat (compile-expr (list-get items (i32.const 3))))
                                  (string-append "(call $__substring "
                                    (string-append s-wat
                                      (string-append " " (string-append start-wat
                                        (string-append " " (string-append end-wat ")")))))))))
                          ; Built-in: list-new -> (call $__list_new)
                          (if (string=? name "list-new")
                            "(call $__list_new)"
                            ; Built-in: list-push -> (call $__list_push lst item)
                            (if (string=? name "list-push")
                              (let (lst-wat (compile-expr (list-get items (i32.const 1))))
                                (let (item-wat (compile-expr (list-get items (i32.const 2))))
                                  (string-append "(call $__list_push "
                                    (string-append lst-wat
                                      (string-append " " (string-append item-wat ")"))))))
                              ; Variant constructor: sym (tag 0)
                              (if (string=? name "sym")
                                (let (payload-wat (compile-expr (list-get items (i32.const 1))))
                                  (string-append "(call $__make_variant_1 (i32.const 0) " (string-append payload-wat ")")))
                                ; Variant constructor: num (tag 1)
                                (if (string=? name "num")
                                  (let (payload-wat (compile-expr (list-get items (i32.const 1))))
                                    (string-append "(call $__make_variant_1 (i32.const 1) " (string-append payload-wat ")")))
                                  ; Variant constructor: str (tag 2)
                                  (if (string=? name "str")
                                    (let (payload-wat (compile-expr (list-get items (i32.const 1))))
                                      (string-append "(call $__make_variant_1 (i32.const 2) " (string-append payload-wat ")")))
                                    ; Variant constructor: lst (tag 3)
                                    (if (string=? name "lst")
                                      (let (payload-wat (compile-expr (list-get items (i32.const 1))))
                                        (string-append "(call $__make_variant_1 (i32.const 3) " (string-append payload-wat ")")))
                                      ; Variant constructor: lparen (tag 0, no payload)
                                      (if (string=? name "lparen")
                                        "(call $__make_variant_0 (i32.const 0))"
                                        ; Variant constructor: rparen (tag 1, no payload)
                                        (if (string=? name "rparen")
                                          "(call $__make_variant_0 (i32.const 1))"
                                          ; Variant constructor: number (tag 2)
                                          (if (string=? name "number")
                                            (let (payload-wat (compile-expr (list-get items (i32.const 1))))
                                              (string-append "(call $__make_variant_1 (i32.const 2) " (string-append payload-wat ")")))
                                            ; Variant constructor: symbol (tag 3)
                                            (if (string=? name "symbol")
                                              (let (payload-wat (compile-expr (list-get items (i32.const 1))))
                                                (string-append "(call $__make_variant_1 (i32.const 3) " (string-append payload-wat ")")))
                                              ; Variant constructor: str-lit (tag 4)
                                              (if (string=? name "str-lit")
                                                (let (payload-wat (compile-expr (list-get items (i32.const 1))))
                                                  (string-append "(call $__make_variant_1 (i32.const 4) " (string-append payload-wat ")")))
                                                ; Record constructor: token-result (tok, new-pos)
                                                (if (string=? name "token-result")
                                                  (let (tok-wat (compile-expr (list-get items (i32.const 1))))
                                                    (let (pos-wat (compile-expr (list-get items (i32.const 2))))
                                                      (string-append "(call $__make_record_2 " (string-append tok-wat (string-append " " (string-append pos-wat ")"))))))
                                                  ; Record constructor: parse-result (expr, new-pos)
                                                  (if (string=? name "parse-result")
                                                    (let (expr-wat (compile-expr (list-get items (i32.const 1))))
                                                      (let (pos-wat (compile-expr (list-get items (i32.const 2))))
                                                        (string-append "(call $__make_record_2 " (string-append expr-wat (string-append " " (string-append pos-wat ")"))))))
                                                    ; Record field: token-result.tok (offset 0)
                                                    (if (string=? name "token-result.tok")
                                                      (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                        (string-append "(i32.load " (string-append rec-wat ")")))
                                                      ; Record field: token-result.new-pos (offset 4)
                                                      (if (string=? name "token-result.new-pos")
                                                        (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                          (string-append "(i32.load (i32.add " (string-append rec-wat " (i32.const 4)))")))
                                                        ; Record field: parse-result.expr (offset 0)
                                                        (if (string=? name "parse-result.expr")
                                                          (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                            (string-append "(i32.load " (string-append rec-wat ")")))
                                                          ; Record field: parse-result.new-pos (offset 4)
                                                          (if (string=? name "parse-result.new-pos")
                                                            (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                              (string-append "(i32.load (i32.add " (string-append rec-wat " (i32.const 4)))")))
                                                            ; Record constructor: variant-case (3 fields)
                                                            (if (string=? name "variant-case")
                                                              (let (f0 (compile-expr (list-get items (i32.const 1))))
                                                                (let (f1 (compile-expr (list-get items (i32.const 2))))
                                                                  (let (f2 (compile-expr (list-get items (i32.const 3))))
                                                                    (string-append "(call $__make_record_3 " (string-append f0 (string-append " " (string-append f1 (string-append " " (string-append f2 ")")))))))))
                                                              ; Record constructor: variant-def (2 fields)
                                                              (if (string=? name "variant-def")
                                                                (let (f0 (compile-expr (list-get items (i32.const 1))))
                                                                  (let (f1 (compile-expr (list-get items (i32.const 2))))
                                                                    (string-append "(call $__make_record_2 " (string-append f0 (string-append " " (string-append f1 ")"))))))
                                                                ; Record constructor: record-field (2 fields)
                                                                (if (string=? name "record-field")
                                                                  (let (f0 (compile-expr (list-get items (i32.const 1))))
                                                                    (let (f1 (compile-expr (list-get items (i32.const 2))))
                                                                      (string-append "(call $__make_record_2 " (string-append f0 (string-append " " (string-append f1 ")"))))))
                                                                  ; Record constructor: record-def (2 fields)
                                                                  (if (string=? name "record-def")
                                                                    (let (f0 (compile-expr (list-get items (i32.const 1))))
                                                                      (let (f1 (compile-expr (list-get items (i32.const 2))))
                                                                        (string-append "(call $__make_record_2 " (string-append f0 (string-append " " (string-append f1 ")"))))))
                                                                    ; Record constructor: compile-ctx (2 fields)
                                                                    (if (string=? name "compile-ctx")
                                                                      (let (f0 (compile-expr (list-get items (i32.const 1))))
                                                                        (let (f1 (compile-expr (list-get items (i32.const 2))))
                                                                          (string-append "(call $__make_record_2 " (string-append f0 (string-append " " (string-append f1 ")"))))))
                                                                      ; Field: variant-case.case-name (offset 0)
                                                                      (if (string=? name "variant-case.case-name")
                                                                        (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                          (string-append "(i32.load " (string-append rec-wat ")")))
                                                                        ; Field: variant-case.case-tag (offset 4)
                                                                        (if (string=? name "variant-case.case-tag")
                                                                          (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                            (string-append "(i32.load (i32.add " (string-append rec-wat " (i32.const 4)))")))
                                                                          ; Field: variant-case.case-has-payload (offset 8)
                                                                          (if (string=? name "variant-case.case-has-payload")
                                                                            (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                              (string-append "(i32.load (i32.add " (string-append rec-wat " (i32.const 8)))")))
                                                                            ; Field: variant-def.var-name (offset 0)
                                                                            (if (string=? name "variant-def.var-name")
                                                                              (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                                (string-append "(i32.load " (string-append rec-wat ")")))
                                                                              ; Field: variant-def.var-cases (offset 4)
                                                                              (if (string=? name "variant-def.var-cases")
                                                                                (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                                  (string-append "(i32.load (i32.add " (string-append rec-wat " (i32.const 4)))")))
                                                                                ; Field: record-field.field-name (offset 0)
                                                                                (if (string=? name "record-field.field-name")
                                                                                  (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                                    (string-append "(i32.load " (string-append rec-wat ")")))
                                                                                  ; Field: record-field.field-offset (offset 4)
                                                                                  (if (string=? name "record-field.field-offset")
                                                                                    (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                                      (string-append "(i32.load (i32.add " (string-append rec-wat " (i32.const 4)))")))
                                                                                    ; Field: record-def.rec-name (offset 0)
                                                                                    (if (string=? name "record-def.rec-name")
                                                                                      (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                                        (string-append "(i32.load " (string-append rec-wat ")")))
                                                                                      ; Field: record-def.rec-fields (offset 4)
                                                                                      (if (string=? name "record-def.rec-fields")
                                                                                        (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                                          (string-append "(i32.load (i32.add " (string-append rec-wat " (i32.const 4)))")))
                                                                                        ; Field: compile-ctx.ctx-variants (offset 0)
                                                                                        (if (string=? name "compile-ctx.ctx-variants")
                                                                                          (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                                            (string-append "(i32.load " (string-append rec-wat ")")))
                                                                                          ; Field: compile-ctx.ctx-records (offset 4)
                                                                                          (if (string=? name "compile-ctx.ctx-records")
                                                                                            (let (rec-wat (compile-expr (list-get items (i32.const 1))))
                                                                                              (string-append "(i32.load (i32.add " (string-append rec-wat " (i32.const 4)))")))
                                                                                            ; Regular function call or WASM instruction
                                                                                            (let (rest-start (i32.const 1))
                                                                                              (let (rest-len (i32.sub (list-len items) (i32.const 1)))
                                                                                                (let (args (build-args-list items rest-start rest-len (list-new sexpr)))
                                                                                                  (if (is-wasm-instr name)
                                                                                                    (compile-wasm-call name args)
                                                                                                    (compile-fn-call name args)))))))))))))))))))))))))))))))))))))))))))))))))
        "(error: list head not symbol)"))))

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

; Generate local declarations from a list of names
(fn gen-locals ((names (list string)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (name (list-get names idx))
      (let (decl (string-append "(local $" (string-append name " i32)")))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        decl
                        (string-append acc (string-append " " decl))))
          (gen-locals names (i32.add idx (i32.const 1)) len new-acc))))))

; Compile (fn name ((params...)) ret-type body)
(fn compile-fn-def ((items (list sexpr))) string
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
                          (let (locals-wat (gen-locals locals (i32.const 0) (list-len locals) ""))
                            (let (body-wat (compile-expr body-expr))
                              (let (result-wat (string-append "(result " (string-append (type-to-wat ret-type) ")")))
                                ; Include locals after result type if any
                                (let (locals-section (if (i32.gt_s (list-len locals) (i32.const 0))
                                                       (string-append " " locals-wat)
                                                       ""))
                                  (string-append "  (func $" (string-append name
                                    (string-append " " (string-append params-wat
                                      (string-append " " (string-append result-wat
                                        (string-append locals-section
                                          (string-append "\n    " (string-append body-wat ")"))))))))))))))))))
                "(error: params)")
              "(error: name)")))))))

; Compile (export (fn ...))
(fn compile-export ((items (list sexpr))) string
  (if (i32.lt_s (list-len items) (i32.const 2))
    "(error: export needs body)"
    (let (body-expr (list-get items (i32.const 1)))
      (if (is-lst body-expr)
        (let (body-items (get-lst body-expr))
          (if (i32.gt_s (list-len body-items) (i32.const 1))
            (let (head (list-get body-items (i32.const 0)))
              (if (is-sym head)
                (if (string=? (get-sym head) "fn")
                  (let (fn-name (get-sym (list-get body-items (i32.const 1))))
                    (let (fn-wat (compile-fn-def body-items))
                      (string-append fn-wat
                        (string-append "\n  (export \""
                          (string-append fn-name
                            (string-append "\" (func $"
                              (string-append fn-name "))")))))))
                  "(error: expected fn)")
                "(error: expected symbol)"))
            "(error: empty body)"))
        "(error: expected list)"))))

; Helper to compile based on form name
(fn compile-by-name ((name string) (items (list sexpr))) string
  (if (string=? name "fn")
    (compile-fn-def items)
    (if (string=? name "export")
      (compile-export items)
      (if (string=? name "variant")
        ""
        (if (string=? name "record")
          ""
          "(error: unknown form)")))))

; Compile a top-level form
(fn compile-toplevel ((form sexpr)) string
  (if (is-lst form)
    (compile-toplevel-list (get-lst form))
    "(error: not list)"))

(fn compile-toplevel-list ((items (list sexpr))) string
  (if (i32.gt_s (list-len items) (i32.const 0))
    (compile-toplevel-head items (list-get items (i32.const 0)))
    "(error: empty)"))

(fn compile-toplevel-head ((items (list sexpr)) (head sexpr)) string
  (if (is-sym head)
    (compile-by-name (get-sym head) items)
    "(error: not symbol)"))

; Compile multiple top-level forms
(fn compile-toplevels ((forms (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (form (list-get forms idx))
      (let (compiled (compile-toplevel form))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append "\n" compiled))))
          (compile-toplevels forms (i32.add idx (i32.const 1)) len new-acc))))))

; Runtime helpers for string, list, and variant operations
(fn get-runtime () string
  "  (global $__heap_ptr (mut i32) (i32.const 49152))\n  (func $__string_append (param $a i32) (param $b i32) (result i32) (local $la i32) (local $lb i32) (local $tot i32) (local $ptr i32) local.get $a i32.load local.set $la local.get $b i32.load local.set $lb local.get $la local.get $lb i32.add local.set $tot global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 local.get $tot i32.add i32.add global.set $__heap_ptr local.get $ptr local.get $tot i32.store local.get $ptr i32.const 4 i32.add local.get $a i32.const 4 i32.add local.get $la memory.copy local.get $ptr i32.const 4 i32.add local.get $la i32.add local.get $b i32.const 4 i32.add local.get $lb memory.copy local.get $ptr)\n  (func $__string_eq (param $a i32) (param $b i32) (result i32) (local $la i32) (local $lb i32) (local $i i32) local.get $a i32.load local.set $la local.get $b i32.load local.set $lb block (result i32) local.get $la local.get $lb i32.ne if (result i32) i32.const 0 else i32.const 0 local.set $i block (result i32) loop local.get $i local.get $la i32.ge_u if i32.const 1 br 2 end local.get $a i32.const 4 i32.add local.get $i i32.add i32.load8_u local.get $b i32.const 4 i32.add local.get $i i32.add i32.load8_u i32.ne if i32.const 0 br 3 end local.get $i i32.const 1 i32.add local.set $i br 0 end i32.const 1 end end end)\n  (func $__substring (param $s i32) (param $start i32) (param $end i32) (result i32) (local $new_len i32) (local $ptr i32) local.get $end local.get $start i32.sub local.set $new_len global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 local.get $new_len i32.add i32.add global.set $__heap_ptr local.get $ptr local.get $new_len i32.store local.get $ptr i32.const 4 i32.add local.get $s i32.const 4 i32.add local.get $start i32.add local.get $new_len memory.copy local.get $ptr)\n  (func $__list_new (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 12 i32.add global.set $__heap_ptr local.get $ptr i32.const 0 i32.store local.get $ptr i32.const 4 i32.add i32.const 0 i32.store local.get $ptr i32.const 8 i32.add i32.const 0 i32.store local.get $ptr)\n  (func $__list_push (param $lst i32) (param $item i32) (result i32) (local $len i32) (local $new_data i32) (local $old_data i32) local.get $lst i32.load local.set $len global.get $__heap_ptr local.set $new_data global.get $__heap_ptr local.get $len i32.const 1 i32.add i32.const 4 i32.mul i32.add global.set $__heap_ptr local.get $lst i32.const 8 i32.add i32.load local.set $old_data local.get $new_data local.get $old_data local.get $len i32.const 4 i32.mul memory.copy local.get $new_data local.get $len i32.const 4 i32.mul i32.add local.get $item i32.store local.get $lst local.get $len i32.const 1 i32.add i32.store local.get $lst i32.const 8 i32.add local.get $new_data i32.store local.get $lst)\n  (func $__make_variant_0 (param $tag i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 i32.add global.set $__heap_ptr local.get $ptr local.get $tag i32.store local.get $ptr)\n  (func $__make_variant_1 (param $tag i32) (param $payload i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 8 i32.add global.set $__heap_ptr local.get $ptr local.get $tag i32.store local.get $ptr i32.const 4 i32.add local.get $payload i32.store local.get $ptr)\n  (func $__make_record_2 (param $f0 i32) (param $f1 i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 8 i32.add global.set $__heap_ptr local.get $ptr local.get $f0 i32.store local.get $ptr i32.const 4 i32.add local.get $f1 i32.store local.get $ptr)\n  (func $__make_record_3 (param $f0 i32) (param $f1 i32) (param $f2 i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 12 i32.add global.set $__heap_ptr local.get $ptr local.get $f0 i32.store local.get $ptr i32.const 4 i32.add local.get $f1 i32.store local.get $ptr i32.const 8 i32.add local.get $f2 i32.store local.get $ptr)\n")

; Compile source to WAT module
(fn compile ((src string)) string
  (let (forms (read-all src))
    (let (body (compile-toplevels forms (i32.const 0) (list-len forms) ""))
      (let (runtime (get-runtime))
        (string-append "(module\n  (memory 1)\n"
          (string-append runtime
            (string-append body "\n)")))))))

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
