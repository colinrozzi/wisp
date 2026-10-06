; Read signature metadata without evaluating expressions. This lets later
; arguments guide earlier constants/methods while effects still run left to right.
(fn callable-type ((v value)) value
  (value-case v
    ((typed-function params result body) (function-type (schema-types params 0 (list-new value)) result))
    ((checked-function target params result) (function-type params result))
    ((constructor name id case-name types) (function-type types (symbol name)))
    ((field-reader name id index)
      (function-type (list-push (list-new value) (symbol name))
        (field-type (list-get (named-type.schema (list-get (global.get $types) id)) index))))
    (else (nil))))
(fn callable? ((v value)) s32
  (value-case v
    ((closure params body env) 1) ((typed-function params result body) 1)
    ((generic-function params result body vars constraints) 1)
    ((checked-function target params result) 1) ((trait-method trait method) 1)
    ((builtin name) 1) ((constructor name id case-name types) 1) ((field-reader name id index) 1)
    (else 0)))
(fn callable-compatible? ((v value) (ty value)) s32
  (if (i32.eq (callable? v) 0) 0
    (let (actual (callable-type v))
      (if (unknown-type? actual)
        (value-case v
          ((closure params body env) (i32.eq (list-len params) (list-len (function-params ty))))
          ((generic-function params result body vars constraints)
            (let (solution (unify-type (function-type (schema-types params 0 (list-new value)) result) ty vars (empty-solution)))
              (i32.eq (string-len (type-solution.error solution)) 0)))
          ((trait-method trait method)
            (i32.eq (failed? (resolve-method trait method (function-params ty) (symbol-name (function-result ty)))) 0))
          (else 1))
        (same-type? actual ty)))))
(fn checked-argument ((v value) (ty value)) value
  (if (function-type? ty)
    (value-case v ((checked-function target params result) v)
      (else (checked-function v (function-params ty) (function-result ty)))) v))
(fn argument-value-type ((v value)) value
  (let (ty (value-type v)) (if (failed? ty) (callable-type v) ty)))
(fn argument-value-types ((args (list value)) (index s32) (out (list value))) (list value)
  (if (i32.ge_s index (list-len args)) out
    (argument-value-types args (i32.add index 1) (list-push out (argument-value-type (list-get args index))))))
(fn peek-value ((expr value) (env (list binding))) value
  (value-case expr
    ((symbol name) (lookup name env)) ((identifier name key) (lookup-identifier name key env))
    (else (failure "not a binding"))))
(fn peek-type ((expr value) (env (list binding)) (depth s32)) value
  (if (i32.ge_s depth 32) (nil)
    (if (symbol? expr) (argument-value-type (peek-value expr env))
      (value-case expr
        ((integer-literal n) (nil))
        ((sequence items) (peek-form-type items env (i32.add depth 1)))
        (else (argument-value-type expr))))))
(fn peek-form-type ((items (list value)) (env (list binding)) (depth s32)) value
  (let (special (peek-special-type items env depth))
    (if (unknown-type? special) (peek-call-type items env depth) special)))
(fn peek-special-type ((items (list value)) (env (list binding)) (depth s32)) value
  (if (i32.lt_s (list-len items) 2) (nil)
    (let (name (symbol-name (list-get items 0)))
      (if (string=? name "begin") (peek-type (list-get items (i32.sub (list-len items) 1)) env depth)
        (if (string=? name "global.get")
          (let (index (global-index (symbol-name (list-get items 1))))
            (if (i32.ge_s index 0) (global-binding.type (list-get (global.get $session-globals) index)) (nil)))
          (if (i32.or (string=? name "list-push") (string=? name "list-get"))
            (let (ty (peek-type (list-get items 1) env depth))
              (if (string=? (form-head ty) "list")
                (if (string=? name "list-push") ty (list-get (items-of ty) 1)) (nil)))
            (nil)))))))
(fn peek-call-type ((items (list value)) (env (list binding)) (depth s32)) value
  (if (i32.eq (list-len items) 0) (nil)
    (let (name (symbol-name (list-get items 0)))
      (if (i32.and (i32.eq (list-len items) 2) (string=? name "list-new")) (unary-type "list" (list-get items 1))
        (if (numeric-type? name) (symbol name)
          (if (numeric-primitive? name) (primitive-result-type name)
            (if (i32.and (i32.eq (list-len items) 3) (string=? (symbol-name (list-get items 1)) ":")) (list-get items 2)
              (let (callee (peek-value (list-get items 0) env))
                (let (actual (callable-type callee))
                  (if (function-type? actual) (function-result actual)
                    (value-case callee
                      ((generic-function params result body vars constraints)
                        (let (state (infer-generic params result vars (peek-types items 1 env depth (list-new value)) ""))
                          (let (ret (substitute-type result (type-solution.bindings state)))
                            (if (known-type? ret "") ret (nil)))))
                      ((trait-method trait method)
                        (let (selected (resolve-method trait method (peek-types items 1 env depth (list-new value)) ""))
                          (let (ty (callable-type selected)) (if (function-type? ty) (function-result ty) (nil)))))
                      (else (nil)))))))))))))
(fn float-comparison? ((name string)) s32
  (if (i32.eq (string-len name) 6)
    (let (op (substring name 4 6))
      (i32.and (float-type? (substring name 0 3))
        (i32.or (i32.or (string=? op "eq") (string=? op "ne"))
          (i32.or (i32.or (string=? op "lt") (string=? op "gt"))
            (i32.or (string=? op "le") (string=? op "ge")))))) 0))
(fn primitive-result-type ((name string)) value
  (if (i32.or (i32-comparison? name) (i32.or (i64-comparison? name) (float-comparison? name))) (symbol "s32")
    (if (i32.ge_s (string-len name) 3)
      (let (prefix (substring name 0 3))
        (if (string=? prefix "i32") (symbol "s32")
          (if (string=? prefix "i64") (symbol "s64")
            (if (string=? prefix "f32") (symbol "f32") (if (string=? prefix "f64") (symbol "f64") (nil)))))) (nil))))
(fn peek-types ((items (list value)) (index s32) (env (list binding)) (depth s32) (out (list value))) (list value)
  (if (i32.ge_s index (list-len items)) out
    (peek-types items (i32.add index 1) env depth (list-push out (peek-type (list-get items index) env depth)))))
(fn merged-argument-types ((hints (list value)) (args (list value)) (index s32) (out (list value))) (list value)
  (if (i32.ge_s index (list-len hints)) out
    (merged-argument-types hints args (i32.add index 1)
      (list-push out (if (i32.lt_s index (list-len args)) (argument-value-type (list-get args index)) (list-get hints index))))))
(fn inferred-argument-type ((callee value) (actuals (list value)) (index s32) (expected string)) string
  (value-case callee
    ((generic-function params result body vars constraints)
      (if (i32.ge_s index (list-len params)) ""
        (let (state (infer-generic params result vars actuals expected))
          (let (ty (substitute-type (field-type (list-get params index)) (type-solution.bindings state)))
            (if (known-type? ty "") (symbol-name ty) "")))))
    ((trait-method trait method)
      (let (candidates (trait-candidates trait method actuals expected 0 (list-new value)))
        (if (i32.eq (list-len candidates) 0) ""
          (common-argument-type candidates 1 index (argument-type (list-get candidates 0) index)))))
    (else "")))
(fn common-argument-type ((candidates (list value)) (index s32) (param s32) (expected string)) string
  (if (i32.ge_s index (list-len candidates)) expected
    (if (string=? expected (argument-type (list-get candidates index) param))
      (common-argument-type candidates (i32.add index 1) param expected) "")))
(fn eval-inferred-args ((items (list value)) (index s32) (env (list binding)) (depth s32) (args (list value)) (callee value) (hints (list value)) (expected string)) value
  (if (i32.ge_s index (list-len items)) (sequence args)
    (let (hint (inferred-argument-type callee (merged-argument-types hints args 0 (list-new value)) (i32.sub index 1) expected))
      (let (v (eval-expected (list-get items index) env depth 0
                (if (string-len hint) hint (call-argument-type callee args (i32.sub index 1)))))
        (if (failed? v) v
          (eval-inferred-args items (i32.add index 1) env depth (list-push args v) callee hints expected))))))
(fn needs-inference? ((callee value)) s32
  (value-case callee ((generic-function params result body vars constraints) 1) ((trait-method trait method) 1) (else 0)))

; Wasm s32 operands also accept u8. Preserve a known byte-producing signature
; during generic inference; the primitive converts the value at its boundary.
(fn operand-hint ((callee value) (expr value) (env (list binding)) (hint string)) string
  (value-case callee
    ((builtin name)
      (if (i32.and (string=? hint "s32") (i32.and (numeric-primitive? name) (i32.eq (numeric-type? name) 0)))
        (if (string=? (symbol-name (peek-type expr env 0)) "u8") "u8" hint) hint))
    (else hint)))
