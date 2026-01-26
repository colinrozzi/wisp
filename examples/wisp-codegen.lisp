; Wisp Code Generator - written in Wisp
; Part of the self-hosted compiler project
; Generates WAT (WebAssembly Text) from S-expression AST

; ============================================================
; Include Parser (which includes Tokenizer)
; ============================================================

; Token Type
(variant token
  (lparen)
  (rparen)
  (number s32)
  (symbol string)
  (str-lit string))

; S-Expression AST Type
(variant sexpr
  (sym string)
  (num s32)
  (str string)
  (lst (list sexpr)))

; ============================================================
; Number to String Conversion
; ============================================================

; Convert a single digit (0-9) to its string representation
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

; Convert positive integer to string (recursive helper)
(fn i32-to-string-pos ((n s32) (acc string)) string
  (if (i32.eq n (i32.const 0))
    acc
    (let (digit (i32.rem_s n (i32.const 10)))
      (let (rest (i32.div_s n (i32.const 10)))
        (i32-to-string-pos rest (string-append (digit-to-string digit) acc))))))

; Convert s32 to string
(fn i32-to-string ((n s32)) string
  (if (i32.eq n (i32.const 0))
    "0"
    (if (i32.lt_s n (i32.const 0))
      (string-append "-" (i32-to-string-pos (i32.sub (i32.const 0) n) ""))
      (i32-to-string-pos n ""))))

; ============================================================
; S-Expression Utilities
; ============================================================

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

(fn is-lst ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((str s) (i32.const 0))
    ((lst l) (i32.const 1))))

(fn get-sym ((e sexpr)) string
  (match e
    ((sym s) s)
    ((num n) "")
    ((str s) "")
    ((lst l) "")))

(fn get-num ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) n)
    ((str s) (i32.const 0))
    ((lst l) (i32.const 0))))

(fn get-lst ((e sexpr)) (list sexpr)
  (match e
    ((sym s) (list-new sexpr))
    ((num n) (list-new sexpr))
    ((str s) (list-new sexpr))
    ((lst l) l)))

; ============================================================
; WASM Instruction Recognition
; ============================================================

; Check if a symbol is a WASM instruction (starts with i32., i64., f32., f64.)
(fn is-wasm-instr ((s string)) s32
  (if (i32.lt_s (string-len s) (i32.const 4))
    (i32.const 0)
    (let (prefix (substring s (i32.const 0) (i32.const 4)))
      (if (string=? prefix "i32.")
        (i32.const 1)
        (if (string=? prefix "i64.")
          (i32.const 1)
          (if (string=? prefix "f32.")
            (i32.const 1)
            (if (string=? prefix "f64.")
              (i32.const 1)
              (i32.const 0))))))))

; ============================================================
; Code Generator
; ============================================================

; Compile a number literal to WAT
(fn compile-number ((n s32)) string
  (string-append "(i32.const " (string-append (i32-to-string n) ")")))

; Compile a variable reference to WAT
(fn compile-var ((name string)) string
  (string-append "(local.get $" (string-append name ")")))

; Forward declaration for mutual recursion
; (compile-expr is used by compile-args which is used by compile-call)

; Compile a list of expressions, joining with spaces
(fn compile-args ((args (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (arg (list-get args idx))
      (let (compiled (compile-expr arg))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-args args (i32.add idx (i32.const 1)) len new-acc))))))

; Compile a WASM instruction call
(fn compile-wasm-call ((instr string) (args (list sexpr))) string
  (let (compiled-args (compile-args args (i32.const 0) (list-len args) ""))
    (string-append "(" (string-append instr (string-append " " (string-append compiled-args ")"))))))

; Compile a function call
(fn compile-fn-call ((name string) (args (list sexpr))) string
  (let (compiled-args (compile-args args (i32.const 0) (list-len args) ""))
    (string-append "(call $" (string-append name (string-append " " (string-append compiled-args ")"))))))

; Build a list from items[start..start+len]
(fn build-args-list ((items (list sexpr)) (start s32) (len s32) (acc (list sexpr))) (list sexpr)
  (if (i32.le_s len (i32.const 0))
    acc
    (let (item (list-get items start))
      (build-args-list items (i32.add start (i32.const 1)) (i32.sub len (i32.const 1)) (list-push acc item)))))

; Main expression compiler - handles all cases including special forms
; Note: compile-expr and compile-list are mutually recursive
(fn compile-expr ((expr sexpr)) string
  (match expr
    ((num n) (compile-number n))
    ((sym s) (compile-var s))
    ((str s) "(error: strings not supported)")
    ((lst items) (compile-list items))))

; Compile a list expression (could be special form, WASM instr or function call)
; This is defined after compile-expr so it can call it for sub-expressions
(fn compile-list ((items (list sexpr))) string
  (if (i32.eq (list-len items) (i32.const 0))
    "()"
    (let (head (list-get items (i32.const 0)))
      (if (is-sym head)
        (let (name (get-sym head))
          ; Check for special forms first
          (if (string=? name "if")
            ; Inline if compilation: (if cond then else)
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
              ; Inline let compilation: (let (name value) body)
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
                                              (string-append ") "
                                                body-wat))))))))
                                "(error: let binding name must be symbol)")))))
                      "(error: let binding must be a list)"))))
              ; Regular call (not special form)
              (let (rest-start (i32.const 1))
                (let (rest-len (i32.sub (list-len items) (i32.const 1)))
                  (let (args (build-args-list items rest-start rest-len (list-new sexpr)))
                    (if (is-wasm-instr name)
                      (compile-wasm-call name args)
                      (compile-fn-call name args))))))))
        ; Head is not a symbol - compile as nested expression
        "(error: list head not symbol)"))))

; ============================================================
; Function Definition Compilation
; ============================================================

; Map Wisp type to WAT type
(fn type-to-wat ((t string)) string
  (if (string=? t "s32")
    "i32"
    (if (string=? t "s64")
      "i64"
      (if (string=? t "f32")
        "f32"
        (if (string=? t "f64")
          "f64"
          "i32")))))

; Compile a single parameter: (name type) -> "(param $name type)"
(fn compile-param ((param sexpr)) string
  (if (is-lst param)
    (let (items (get-lst param))
      (if (i32.ge_s (list-len items) (i32.const 2))
        (let (name-expr (list-get items (i32.const 0)))
          (let (type-expr (list-get items (i32.const 1)))
            (if (is-sym name-expr)
              (if (is-sym type-expr)
                (let (name (get-sym name-expr))
                  (let (type-name (get-sym type-expr))
                    (string-append "(param $"
                      (string-append name
                        (string-append " "
                          (string-append (type-to-wat type-name) ")"))))))
                "(error: param type not symbol)")
              "(error: param name not symbol)")))
        "(error: param needs name and type)"))
    "(error: param must be list)"))

; Compile parameters list
(fn compile-params ((params (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (param (list-get params idx))
      (let (compiled (compile-param param))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-params params (i32.add idx (i32.const 1)) len new-acc))))))

; Compile a function definition: (fn name ((p1 t1) ...) ret-type body)
; Output: (func $name (param $p1 t1) ... (result ret-type) body)
(fn compile-fn ((items (list sexpr))) string
  ; items[0] = "fn", items[1] = name, items[2] = params, items[3] = ret-type, items[4] = body
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
                              (string-append "(func $"
                                (string-append name
                                  (string-append " "
                                    (string-append params-wat
                                      (string-append " "
                                        (string-append result-wat
                                          (string-append "\n    "
                                            (string-append body-wat ")"))))))))))))))
                  "(error: return type not symbol)")
                "(error: params must be list)")
              "(error: fn name not symbol)")))))))

; Compile a top-level form (currently just fn)
(fn compile-toplevel ((form sexpr)) string
  (if (is-lst form)
    (let (items (get-lst form))
      (if (i32.gt_s (list-len items) (i32.const 0))
        (let (head (list-get items (i32.const 0)))
          (if (is-sym head)
            (let (name (get-sym head))
              (if (string=? name "fn")
                (compile-fn items)
                "(error: unknown top-level form)"))
            "(error: top-level must start with symbol)"))
        "(error: empty top-level form)"))
    "(error: top-level must be list)"))

; ============================================================
; Test Exports
; ============================================================

; Test: i32-to-string for 0
(export (fn test-i32-to-string-zero () s32
  (if (string=? (i32-to-string (i32.const 0)) "0")
    (i32.const 1)
    (i32.const 0))))

; Test: i32-to-string for positive
(export (fn test-i32-to-string-pos () s32
  (if (string=? (i32-to-string (i32.const 42)) "42")
    (i32.const 1)
    (i32.const 0))))

; Test: i32-to-string for negative
(export (fn test-i32-to-string-neg () s32
  (if (string=? (i32-to-string (i32.const -123)) "-123")
    (i32.const 1)
    (i32.const 0))))

; Test: compile number literal
(export (fn test-compile-number () s32
  (if (string=? (compile-number (i32.const 42)) "(i32.const 42)")
    (i32.const 1)
    (i32.const 0))))

; Test: compile variable reference
(export (fn test-compile-var () s32
  (if (string=? (compile-var "x") "(local.get $x)")
    (i32.const 1)
    (i32.const 0))))

; Test: is-wasm-instr for i32.add
(export (fn test-is-wasm-instr-yes () s32
  (is-wasm-instr "i32.add")))

; Test: is-wasm-instr for regular symbol
(export (fn test-is-wasm-instr-no () s32
  (if (is-wasm-instr "foo")
    (i32.const 0)
    (i32.const 1))))

; Test: compile simple WASM expression
(export (fn test-compile-wasm-expr () s32
  (let (expr (lst (list-push (list-push (list-new sexpr) (sym "i32.const")) (num (i32.const 42)))))
    (if (string=? (compile-expr expr) "(i32.const 42)")
      (i32.const 1)
      (i32.const 0)))))

; Test: compile nested WASM expression
(export (fn test-compile-nested () s32
  ; (i32.add 1 2)
  (let (one (num (i32.const 1)))
    (let (two (num (i32.const 2)))
      (let (add-sym (sym "i32.add"))
        (let (items (list-push (list-push (list-push (list-new sexpr) add-sym) one) two))
          (let (expr (lst items))
            (if (string=? (compile-expr expr) "(i32.add (i32.const 1) (i32.const 2))")
              (i32.const 1)
              (i32.const 0)))))))))
