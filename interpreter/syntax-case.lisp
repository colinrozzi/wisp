; The compiler's small procedural transformer language, executed only during
; expansion. Syntax, computed integers, booleans, and repetition lists stay distinct.
(variant sc-value
  (sc-syntax value) (sc-int s64) (sc-bool s32)
  (sc-list (list sc-value)) (sc-failure string))
(record sc-binding (name string) (item sc-value))
(define-syntax sc-case
  (syntax-rules (else)
    ((_ expr arm ... (else fallback))
      (match expr arm ...
        ((sc-syntax ignored-syntax) fallback)
        ((sc-int ignored-number) fallback)
        ((sc-bool ignored-bool) fallback)
        ((sc-list ignored-list) fallback)
        ((sc-failure ignored-error) fallback)))))
(fn sc-failed? ((v sc-value)) s32
  (sc-case v ((sc-failure message) 1) (else 0)))
(fn sc-prefix? ((name string)) s32
  (i32.or (i32.or (string=? name "syntax") (string=? name "quasisyntax"))
    (i32.or (string=? name "unsyntax") (string=? name "unsyntax-splice"))))
(fn sc-lookup ((name string) (env (list sc-binding)) (index s32)) sc-value
  (if (i32.lt_s index 0) (sc-failure "unknown transformer binding")
    (let (entry (list-get env index))
      (if (string=? name (sc-binding.name entry)) (sc-binding.item entry)
        (sc-lookup name env (i32.sub index 1))))))
(fn sc-extend ((env (list sc-binding)) (entry sc-binding)) (list sc-binding)
  (list-push (sc-copy-env env 0 (list-new sc-binding)) entry))
(fn sc-copy-env ((env (list sc-binding)) (index s32) (out (list sc-binding))) (list sc-binding)
  (if (i32.ge_s index (list-len env)) out
    (sc-copy-env env (i32.add index 1) (list-push out (list-get env index)))))
(fn sc-syntax-value ((v sc-value) (scope string)) value
  (sc-case v
    ((sc-syntax expr) expr)
    ((sc-int n) (integer-literal n))
    ((sc-bool b) (sr-introduce (if b "#t" "#f") scope))
    ((sc-failure message) (failure message))
    (else (failure "repeated syntax requires unsyntax-splice"))))
(fn sc-wrap ((v value)) sc-value
  (value-case v ((failure message) (sc-failure message)) (else (sc-syntax v))))

; Declaration validation happens before any macro or ordinary definition is published.
(fn collect-syntax-case ((parts (list value))) value
  (if (i32.ne (list-len parts) 3) (failure "define-syntax expects a name and transformer")
    (let (name (symbol-name (list-get parts 1)))
      (let (body (items-of (list-get parts 2)))
        (if (i32.or (string=? name "") (macro-reserved? name)) (failure "invalid syntax-case macro name")
          (if (i32.lt_s (list-len body) 3) (failure "syntax-case-lambda expects one parameter and clauses")
            (let (params (items-of (list-get body 1)))
              (if (i32.ne (list-len params) 1) (failure "syntax-case-lambda expects one parameter")
                (if (i32.eq (symbol? (list-get params 0)) 0) (failure "syntax-case parameter must be a symbol")
                  (let (param (symbol-name (list-get params 0)))
                    (if (string=? (form-head (list-get body 2)) "syntax-case")
                      (sc-collect-wrapper name param body)
                      (sc-publish name param (list-new value) (copy-list body 2 (list-new value))))))))))))))
(fn sc-collect-wrapper ((name string) (param string) (body (list value))) value
  (let (wrapper (items-of (list-get body 2)))
    (if (i32.or (i32.ne (list-len body) 3) (i32.lt_s (list-len wrapper) 4))
      (failure "syntax-case expects its input, literals, and clauses")
      (if (i32.eq (string=? (symbol-name (list-get wrapper 1)) param) 0)
        (failure "syntax-case input must be the transformer parameter")
        (if (i32.eq (sequence? (list-get wrapper 2)) 0) (failure "syntax-case expects a literal list")
          (let (literals (items-of (list-get wrapper 2)))
            (let (checked (check-params literals 0))
              (if (failed? checked) checked
                (if (i32.or (sr-symbol-in? "..." literals 0) (sr-symbol-in? "_" literals 0))
                  (failure "reserved syntax-case literal")
                  (sc-publish name param literals (copy-list wrapper 3 (list-new value))))))))))))
(fn sc-publish ((name string) (param string) (literals (list value)) (clauses (list value))) value
  (let (checked (sc-check-clauses clauses 0 literals name (list-new value)))
    (if (failed? checked) checked
      (begin
        (global.set $pending-macros (list-push (global.get $pending-macros)
          (binding name (sequence (list-push (list-push (list-push (list-push (list-new value)
            (symbol "syntax-case")) (symbol param)) (sequence literals)) checked)))))
        (nil)))))
(fn sc-check-clauses ((clauses (list value)) (index s32) (literals (list value)) (name string) (out (list value))) value
  (if (i32.ge_s index (list-len clauses)) (sequence out)
    (let (parts (items-of (list-get clauses index)))
      (if (i32.or (i32.lt_s (list-len parts) 2) (i32.gt_s (list-len parts) 3))
        (failure "syntax-case clause expects pattern, optional guard, and expression")
        (let (pattern (list-get parts 0))
          (if (i32.or (sr-repeated? (items-of pattern) 0)
                (i32.eq (i32.or (string=? (form-head pattern) "_") (string=? (form-head pattern) name)) 0))
            (failure "clause pattern must start with _ or its macro name")
            (begin
              (global.set $sr-vars (list-new value))
              (let (checked (sr-check-pattern pattern literals name 0))
                (if (failed? checked) checked
                  (let (valid (sc-check-expressions parts 1))
                    (if (failed? valid) valid
                      (sc-check-clauses clauses (i32.add index 1) literals name
                        (list-push out (sequence (list-push (list-push (list-push (list-push (list-new value)
                          pattern) (if (i32.eq (list-len parts) 3) (list-get parts 1) (integer-literal 1s64)))
                          (list-get parts (i32.sub (list-len parts) 1))) (sequence (global.get $sr-vars)))))))))))))))))
(fn sc-check-expressions ((items (list value)) (index s32)) value
  (if (i32.ge_s index (list-len items)) (nil)
    (let (v (sc-check-expr (list-get items index)))
      (if (failed? v) v (sc-check-expressions items (i32.add index 1))))))
(fn sc-check-expr ((expr value)) value
  (if (i32.eq (sequence? expr) 0) (nil)
    (let (items (items-of expr))
      (if (i32.eq (list-len items) 0) (nil)
        (let (name (form-head expr))
          (if (string=? name "") (failure "transformer application expects a symbol")
            (if (sc-prefix? name)
              (if (i32.ne (list-len items) 2) (failure "syntax prefix expects one expression")
                (if (i32.or (string=? name "unsyntax") (string=? name "unsyntax-splice"))
                  (failure "unsyntax requires quasisyntax") (nil)))
              (if (string=? name "if")
                (if (i32.eq (list-len items) 4) (sc-check-expressions items 1) (failure "transformer if expects three arguments"))
                (if (string=? name "let")
                  (if (i32.ne (list-len items) 3) (failure "transformer let expects a binding and body")
                    (let (pair (items-of (list-get items 1)))
                      (if (i32.ne (list-len pair) 2) (failure "transformer let expects (name expression)")
                        (if (i32.eq (symbol? (list-get pair 0)) 0) (failure "transformer let expects a symbol")
                          (let (checked (sc-check-expr (list-get pair 1)))
                            (if (failed? checked) checked (sc-check-expr (list-get items 2))))))))
                  (if (if (sc-unary? name) (i32.ne (list-len items) 2) 0)
                    (failure "transformer builtin expects one argument")
                    (sc-check-expressions items 1)))))))))))

(fn sc-capture ((v value) (rank s32)) sc-value
  (if (i32.eq rank 0) (sc-syntax v)
    (sc-list (sc-capture-items (items-of v) 0 (i32.sub rank 1) (list-new sc-value)))))
(fn sc-capture-items ((items (list value)) (index s32) (rank s32) (out (list sc-value))) (list sc-value)
  (if (i32.ge_s index (list-len items)) out
    (sc-capture-items items (i32.add index 1) rank (list-push out (sc-capture (list-get items index) rank)))))
(fn sc-captures ((captures (list binding)) (vars (list value)) (index s32) (out (list sc-binding))) (list sc-binding)
  (if (i32.ge_s index (list-len captures)) out
    (let (entry (list-get captures index))
      (sc-captures captures vars (i32.add index 1)
        (list-push out (sc-binding (binding.name entry)
          (sc-capture (binding.item entry) (sr-rank (binding.name entry) vars 0))))))))
(fn expand-syntax-case ((definition value) (input value)) value
  (let (parts (items-of definition))
    (begin
      (global.set $syntax-id (i32.add (global.get $syntax-id) 1))
      (sc-try-clauses (items-of (list-get parts 3)) 0 (items-of (list-get parts 2))
        (symbol-name (list-get parts 1)) input
        (string-append " syntax:" (string-append (show (integer (global.get $syntax-id))) ":"))))))
(fn sc-try-clauses ((clauses (list value)) (index s32) (literals (list value)) (param string) (input value) (scope string)) value
  (if (i32.ge_s index (list-len clauses)) (failure (string-append "no matching syntax-case clause: " (form-head input)))
    (let (parts (items-of (list-get clauses index)))
      (let (matched (sr-match-pattern (list-get parts 0) input literals (form-head input)))
        (if (sr-match.ok matched)
          (let (env (sc-captures (sr-match.bindings matched) (items-of (list-get parts 3)) 0
                      (list-push (list-new sc-binding) (sc-binding param (sc-syntax input)))))
            (let (guard (sc-eval (list-get parts 1) env scope 0))
              (if (sc-failed? guard) (sc-syntax-value guard scope)
                (if (sc-case guard ((sc-bool b) b) (else 1))
                  (sc-case (sc-eval (list-get parts 2) env scope 0)
                    ((sc-syntax result) result)
                    ((sc-failure message) (failure message))
                    (else (failure "syntax-case transformer must return syntax")))
                  (sc-try-clauses clauses (i32.add index 1) literals param input scope)))))
          (sc-try-clauses clauses (i32.add index 1) literals param input scope))))))

(fn sc-eval ((expr value) (env (list sc-binding)) (scope string) (depth s32)) sc-value
  (if (i32.ge_s depth 100) (sc-failure "transformer nesting limit")
    (if (i32.ge_s (global.get $macro-steps) 10000) (sc-failure "macro expansion step limit")
      (begin
        (global.set $macro-steps (i32.add (global.get $macro-steps) 1))
        (if (symbol? expr)
          (let (v (sc-lookup (symbol-name expr) env (i32.sub (list-len env) 1)))
            (if (sc-failed? v) (sc-syntax (sr-introduce (symbol-name expr) scope)) v))
          (value-case expr
            ((integer-literal n) (sc-int n))
            ((wide-integer n) (sc-int n))
            ((sequence items)
              (if (list-len items) (sc-eval-form items env scope (i32.add depth 1)) (sc-syntax expr)))
            (else (sc-syntax expr))))))))
(fn sc-eval-form ((items (list value)) (env (list sc-binding)) (scope string) (depth s32)) sc-value
  (let (name (symbol-name (list-get items 0)))
    (if (string=? name "") (sc-failure "transformer application expects a symbol")
      (if (i32.or (string=? name "syntax") (string=? name "quasisyntax"))
        (if (i32.ne (list-len items) 2) (sc-failure "syntax prefix expects one expression")
          (sc-wrap (sc-template (list-get items 1) env scope depth 1 (string=? name "quasisyntax"))))
        (if (string=? name "if")
          (if (i32.ne (list-len items) 4) (sc-failure "transformer if expects three arguments")
            (let (condition (sc-eval (list-get items 1) env scope depth))
              (if (sc-failed? condition) condition
                (sc-eval (list-get items (if (sc-truthy? condition) 2 3)) env scope depth))))
          (if (string=? name "let")
            (if (i32.ne (list-len items) 3) (sc-failure "transformer let expects a binding and body")
              (let (pair (items-of (list-get items 1)))
                (if (i32.ne (list-len pair) 2) (sc-failure "transformer let expects (name expression)")
                  (if (i32.eq (symbol? (list-get pair 0)) 0) (sc-failure "transformer let expects a symbol")
                    (let (v (sc-eval (list-get pair 1) env scope depth))
                      (if (sc-failed? v) v
                        (sc-eval (list-get items 2) (sc-extend env (sc-binding (symbol-name (list-get pair 0)) v)) scope depth)))))))
            (if (sc-prefix? name) (sc-failure "unsyntax requires quasisyntax")
              (sc-eval-args name items 1 env scope depth (list-new sc-value)))))))))
(fn sc-eval-args ((name string) (items (list value)) (index s32) (env (list sc-binding)) (scope string) (depth s32) (out (list sc-value))) sc-value
  (if (i32.ge_s index (list-len items)) (sc-apply name out scope)
    (let (v (sc-eval (list-get items index) env scope depth))
      (if (sc-failed? v) v
        (sc-eval-args name items (i32.add index 1) env scope depth (list-push out v))))))
(fn sc-truthy? ((v sc-value)) s32
  (sc-case v ((sc-bool b) b) ((sc-int n) (i64.ne n 0)) (else 1)))
(fn sc-unary? ((name string)) s32
  (i32.or (i32.or (string=? name "identifier?") (string=? name "number?"))
    (i32.or (i32.or (string=? name "integer?") (string=? name "not"))
      (i32.or (string=? name "syntax->datum") (string=? name "syntax-error")))))
(fn sc-number ((v sc-value)) sc-value
  (sc-case v
    ((sc-int n) v)
    ((sc-syntax expr)
      (value-case expr
        ((integer-literal n) (sc-int n)) ((wide-integer n) (sc-int n))
        (else (sc-failure "transformer arithmetic expects integers"))))
    (else (sc-failure "transformer arithmetic expects integers"))))
(fn sc-apply ((name string) (args (list sc-value)) (scope string)) sc-value
  (if (sc-unary? name)
    (if (i32.ne (list-len args) 1) (sc-failure "transformer builtin expects one argument")
      (sc-apply-unary name (list-get args 0)))
    (if (i32.or (string=? name "+") (string=? name "-"))
      (sc-arithmetic name args 0 0s64)
      (if (i32.or (string=? name "and") (string=? name "or"))
        (sc-bool (sc-boolean-fold name args 0 (string=? name "and")))
        (sc-wrap (sc-application args 0 scope (list-push (list-new value) (sr-introduce name scope))))))))
(fn sc-apply-unary ((name string) (v sc-value)) sc-value
  (if (string=? name "identifier?") (sc-bool (sc-case v ((sc-syntax expr) (symbol? expr)) (else 0)))
    (if (string=? name "integer?") (sc-bool (i32.eq (sc-failed? (sc-number v)) 0))
      (if (string=? name "number?")
        (sc-bool (if (sc-failed? (sc-number v))
          (sc-case v ((sc-syntax expr) (value-case expr ((single n) 1) ((double n) 1) (else 0))) (else 0)) 1))
        (if (string=? name "not") (sc-bool (i32.eq (sc-truthy? v) 0))
          (if (string=? name "syntax-error")
            (sc-failure (sc-case v ((sc-syntax expr)
              (value-case expr ((text message) message) (else (if (symbol? expr) (symbol-name expr) "syntax error"))))
              (else "syntax error")))
            (sc-case v
              ((sc-syntax expr)
                (let (n (sc-number v))
                  (if (sc-failed? n) (if (symbol? expr) (sc-syntax (symbol (symbol-name expr))) v) n)))
              (else v))))))))
(fn sc-arithmetic ((name string) (args (list sc-value)) (index s32) (acc s64)) sc-value
  (if (i32.ge_s index (list-len args)) (sc-int acc)
    (sc-case (sc-number (list-get args index))
      ((sc-int n)
        (sc-arithmetic name args (i32.add index 1)
          (if (string=? name "+") (i64.add acc n)
            (if (i32.and (i32.eq index 0) (i32.gt_s (list-len args) 1)) n (i64.sub acc n)))))
      (else (sc-failure "transformer arithmetic expects integers")))))
(fn sc-boolean-fold ((name string) (args (list sc-value)) (index s32) (acc s32)) s32
  (if (i32.ge_s index (list-len args)) acc
    (sc-boolean-fold name args (i32.add index 1)
      (if (string=? name "and") (i32.and acc (sc-truthy? (list-get args index)))
        (i32.or acc (sc-truthy? (list-get args index)))))))
(fn sc-application ((args (list sc-value)) (index s32) (scope string) (out (list value))) value
  (if (i32.ge_s index (list-len args)) (sequence out)
    (let (v (sc-syntax-value (list-get args index) scope))
      (if (failed? v) v (sc-application args (i32.add index 1) scope (list-push out v))))))

; Syntax templates substitute variables automatically; active unsyntax evaluates
; transformer expressions. All traversal shares the expansion step budget.
(fn sc-template ((expr value) (env (list sc-binding)) (scope string) (depth s32) (level s32) (quasi s32)) value
  (if (i32.ge_s depth 100) (failure "transformer nesting limit")
    (if (i32.ge_s (global.get $macro-steps) 10000) (failure "macro expansion step limit")
      (begin
        (global.set $macro-steps (i32.add (global.get $macro-steps) 1))
        (if (symbol? expr)
          (let (v (sc-lookup (symbol-name expr) env (i32.sub (list-len env) 1)))
            (if (sc-failed? v) (sr-introduce (symbol-name expr) scope) (sc-syntax-value v scope)))
          (if (sequence? expr)
            (let (name (form-head expr))
              (let (items (items-of expr))
                (if (if quasi (i32.or (string=? name "quasisyntax")
                      (i32.or (string=? name "unsyntax") (string=? name "unsyntax-splice"))) 0)
                  (if (i32.ne (list-len items) 2) (failure "syntax prefix expects one expression")
                    (if (string=? name "quasisyntax")
                      (prefixed name (sc-template (list-get items 1) env scope (i32.add depth 1) (i32.add level 1) quasi))
                      (if (i32.eq level 1)
                        (if (string=? name "unsyntax-splice") (failure "unsyntax-splice requires a list position")
                          (sc-syntax-value (sc-eval (list-get items 1) env scope (i32.add depth 1)) scope))
                        (prefixed name (sc-template (list-get items 1) env scope (i32.add depth 1) (i32.sub level 1) quasi)))))
                  (sc-template-items items 0 env scope (i32.add depth 1) level quasi (list-new value)))))
            expr))))))
(fn sc-template-items ((items (list value)) (index s32) (env (list sc-binding)) (scope string) (depth s32) (level s32) (quasi s32) (out (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence out)
    (let (item (list-get items index))
      (let (splice (i32.and (i32.and quasi (i32.eq level 1)) (string=? (form-head item) "unsyntax-splice")))
        (let (v (if splice
                  (if (i32.ne (list-len (items-of item)) 2) (failure "syntax prefix expects one expression")
                    (sc-splice (sc-eval (list-get (items-of item) 1) env scope depth) scope))
                  (sc-template item env scope depth level quasi)))
          (if (failed? v) v
            (sc-template-items items (i32.add index 1) env scope depth level quasi
              (if splice (copy-list (items-of v) 0 out) (list-push out v)))))))))
(fn sc-splice ((v sc-value) (scope string)) value
  (sc-case v
    ((sc-list items) (sc-application items 0 scope (list-new value)))
    ((sc-syntax expr) (if (sequence? expr) expr (failure "unsyntax-splice expects a syntax list")))
    ((sc-failure message) (failure message))
    (else (failure "unsyntax-splice expects a syntax list"))))
