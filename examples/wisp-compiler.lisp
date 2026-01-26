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

(fn read-token ((src string) (pos s32) (len s32)) token-result
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 40))
      (token-result (lparen) (i32.add pos (i32.const 1)))
      (if (i32.eq c (i32.const 41))
        (token-result (rparen) (i32.add pos (i32.const 1)))
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
            (read-symbol src pos len)))))))

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

; Get variant constructor tag number
; token: lparen=0, rparen=1, number=2, symbol=3, str-lit=4
; sexpr: sym=0, num=1, str=2, lst=3
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

(fn compile-args ((args (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (arg (list-get args idx))
      (let (compiled (compile-expr arg))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-args args (i32.add idx (i32.const 1)) len new-acc))))))

(fn compile-wasm-call ((instr string) (args (list sexpr))) string
  (if (i32.eq (list-len args) (i32.const 0))
    (string-append "(" (string-append instr ")"))
    (let (compiled-args (compile-args args (i32.const 0) (list-len args) ""))
      (string-append "(" (string-append instr (string-append " " (string-append compiled-args ")")))))))

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
    ((str s) "(error: strings not supported)")
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
    (let (compiled-args (compile-args-sub args (i32.const 0) (list-len args) "" binding-name scrutinee-wat))
      (if (i32.eq (list-len args) (i32.const 0))
        (string-append "(" (string-append instr ")"))
        (string-append "(" (string-append instr (string-append " " (string-append compiled-args ")"))))))))

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
                              (let (else-wat (if (i32.ge_s (i32.add case-idx (i32.const 1)) num-cases)
                                               "(unreachable)"
                                               (compile-match-case (list-get remaining-cases (i32.add case-idx (i32.const 1))) scrutinee-wat remaining-cases (i32.add case-idx (i32.const 1)) num-cases)))
                                (string-append "(if (result i32) " (string-append cond-wat (string-append " (then " (string-append body-wat (string-append ") (else " (string-append else-wat ")")))))))))))
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
    ((str s) "(error: strings not supported)")
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
                                                            ; Regular function call or WASM instruction
                                                            (let (rest-start (i32.const 1))
                                                              (let (rest-len (i32.sub (list-len items) (i32.const 1)))
                                                                (let (args (build-args-list items rest-start rest-len (list-new sexpr)))
                                                                  (if (is-wasm-instr name)
                                                                    (compile-wasm-call name args)
                                                                    (compile-fn-call name args)))))))))))))))))))))))))))))))))
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
              (if (is-sym type-expr)
                (string-append "(param $" (string-append (get-sym name-expr)
                  (string-append " " (string-append (type-to-wat (get-sym type-expr)) ")"))))
                "(error)")
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
                (if (is-sym ret-type-expr)
                  (let (name (get-sym name-expr))
                    (let (params (get-lst params-expr))
                      (let (ret-type (get-sym ret-type-expr))
                        (let (params-wat (compile-params params (i32.const 0) (list-len params) ""))
                          (let (body-wat (compile-expr body-expr))
                            (let (result-wat (string-append "(result " (string-append (type-to-wat ret-type) ")")))
                              (string-append "  (func $" (string-append name
                                (string-append " " (string-append params-wat
                                  (string-append " " (string-append result-wat
                                    (string-append "\n    " (string-append body-wat ")"))))))))))))))
                  "(error: ret type)")
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

; Compile a top-level form
(fn compile-toplevel ((form sexpr)) string
  (if (is-lst form)
    (let (items (get-lst form))
      (if (i32.gt_s (list-len items) (i32.const 0))
        (let (head (list-get items (i32.const 0)))
          (if (is-sym head)
            (let (name (get-sym head))
              (if (string=? name "fn")
                (compile-fn-def items)
                (if (string=? name "export")
                  (compile-export items)
                  ; Skip variant and record definitions - constructors are hardcoded
                  (if (string=? name "variant")
                    ""
                    (if (string=? name "record")
                      ""
                      "(error: unknown form)")))))
            "(error: not symbol)"))
        "(error: empty)"))
    "(error: not list)"))

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
  "  (global $__heap_ptr (mut i32) (i32.const 49152))\n  (func $__string_append (param $a i32) (param $b i32) (result i32) (local $la i32) (local $lb i32) (local $tot i32) (local $ptr i32) local.get $a i32.load local.set $la local.get $b i32.load local.set $lb local.get $la local.get $lb i32.add local.set $tot global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 local.get $tot i32.add i32.add global.set $__heap_ptr local.get $ptr local.get $tot i32.store local.get $ptr i32.const 4 i32.add local.get $a i32.const 4 i32.add local.get $la memory.copy local.get $ptr i32.const 4 i32.add local.get $la i32.add local.get $b i32.const 4 i32.add local.get $lb memory.copy local.get $ptr)\n  (func $__string_eq (param $a i32) (param $b i32) (result i32) (local $la i32) (local $lb i32) (local $i i32) local.get $a i32.load local.set $la local.get $b i32.load local.set $lb block (result i32) local.get $la local.get $lb i32.ne if (result i32) i32.const 0 else i32.const 0 local.set $i block (result i32) loop local.get $i local.get $la i32.ge_u if i32.const 1 br 2 end local.get $a i32.const 4 i32.add local.get $i i32.add i32.load8_u local.get $b i32.const 4 i32.add local.get $i i32.add i32.load8_u i32.ne if i32.const 0 br 3 end local.get $i i32.const 1 i32.add local.set $i br 0 end i32.const 1 end end end)\n  (func $__substring (param $s i32) (param $start i32) (param $end i32) (result i32) (local $new_len i32) (local $ptr i32) local.get $end local.get $start i32.sub local.set $new_len global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 local.get $new_len i32.add i32.add global.set $__heap_ptr local.get $ptr local.get $new_len i32.store local.get $ptr i32.const 4 i32.add local.get $s i32.const 4 i32.add local.get $start i32.add local.get $new_len memory.copy local.get $ptr)\n  (func $__list_new (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 12 i32.add global.set $__heap_ptr local.get $ptr i32.const 0 i32.store local.get $ptr i32.const 4 i32.add i32.const 0 i32.store local.get $ptr i32.const 8 i32.add i32.const 0 i32.store local.get $ptr)\n  (func $__list_push (param $lst i32) (param $item i32) (result i32) (local $len i32) (local $new_data i32) (local $old_data i32) local.get $lst i32.load local.set $len global.get $__heap_ptr local.set $new_data global.get $__heap_ptr local.get $len i32.const 1 i32.add i32.const 4 i32.mul i32.add global.set $__heap_ptr local.get $lst i32.const 8 i32.add i32.load local.set $old_data local.get $new_data local.get $old_data local.get $len i32.const 4 i32.mul memory.copy local.get $new_data local.get $len i32.const 4 i32.mul i32.add local.get $item i32.store local.get $lst local.get $len i32.const 1 i32.add i32.store local.get $lst i32.const 8 i32.add local.get $new_data i32.store local.get $lst)\n  (func $__make_variant_0 (param $tag i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 4 i32.add global.set $__heap_ptr local.get $ptr local.get $tag i32.store local.get $ptr)\n  (func $__make_variant_1 (param $tag i32) (param $payload i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 8 i32.add global.set $__heap_ptr local.get $ptr local.get $tag i32.store local.get $ptr i32.const 4 i32.add local.get $payload i32.store local.get $ptr)\n  (func $__make_record_2 (param $f0 i32) (param $f1 i32) (result i32) (local $ptr i32) global.get $__heap_ptr local.set $ptr global.get $__heap_ptr i32.const 8 i32.add global.set $__heap_ptr local.get $ptr local.get $f0 i32.store local.get $ptr i32.const 4 i32.add local.get $f1 i32.store local.get $ptr)\n")

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
