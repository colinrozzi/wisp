; Runtime type inference: solve signatures from arguments, then evaluate one body.
; No Wasm specialization or global type-variable environment is needed.
(record type-solution (error string) (bindings (list binding)))
(fn empty-solution () type-solution (type-solution "" (list-new binding)))
(fn type-variable? ((ty value) (vars (list value))) s32
  (if (symbol? ty) (sr-symbol-in? (symbol-name ty) vars 0) 0))
(fn unknown-type? ((ty value)) s32
  (if (sequence? ty) (i32.eq (list-len (items-of ty)) 0) 0))
(fn function-type? ((ty value)) s32 (string=? (form-head ty) "->"))
(fn function-type ((params (list value)) (result value)) value
  (sequence (list-push (copy-list params 0 (list-push (list-new value) (symbol "->"))) result)))
(fn function-params ((ty value)) (list value)
  (type-slice (items-of ty) 1 (i32.sub (list-len (items-of ty)) 1) (list-new value)))
(fn type-slice ((items (list value)) (index s32) (end s32) (out (list value))) (list value)
  (if (i32.ge_s index end) out
    (type-slice items (i32.add index 1) end (list-push out (list-get items index)))))
(fn function-result ((ty value)) value
  (list-get (items-of ty) (i32.sub (list-len (items-of ty)) 1)))
(fn generic-type? ((ty value) (vars (list value))) s32
  (if (symbol? ty) (i32.or (type-variable? ty vars) (known-type? ty ""))
    (let (parts (items-of ty))
      (let (name (form-head ty))
        (if (if (i32.or (string=? name "list") (string=? name "option")) (i32.eq (list-len parts) 2)
              (if (string=? name "result") (i32.eq (list-len parts) 3)
                (if (string=? name "tuple") (i32.ge_s (list-len parts) 1)
                  (if (string=? name "->") (i32.ge_s (list-len parts) 2) 0))))
          (generic-types? parts 1 vars) 0)))))
(fn generic-types? ((types (list value)) (index s32) (vars (list value))) s32
  (if (i32.ge_s index (list-len types)) 1
    (if (generic-type? (list-get types index) vars) (generic-types? types (i32.add index 1) vars) 0)))
(fn substitute-type ((ty value) (bindings (list binding))) value
  (if (symbol? ty)
    (let (found (lookup-local (symbol-name ty) bindings (i32.sub (list-len bindings) 1)))
      (if (failed? found) ty found))
    (if (sequence? ty) (sequence (substitute-types (items-of ty) 0 bindings (list-new value))) ty)))
(fn substitute-types ((types (list value)) (index s32) (bindings (list binding)) (out (list value))) (list value)
  (if (i32.ge_s index (list-len types)) out
    (substitute-types types (i32.add index 1) bindings (list-push out (substitute-type (list-get types index) bindings)))))
(fn unify-type ((pattern value) (actual value) (vars (list value)) (state type-solution)) type-solution
  (if (i32.or (string-len (type-solution.error state)) (unknown-type? actual)) state
    (if (type-variable? pattern vars)
      (let (bindings (type-solution.bindings state))
        (let (old (lookup-local (symbol-name pattern) bindings (i32.sub (list-len bindings) 1)))
          (if (failed? old) (type-solution "" (extend-env bindings (binding (symbol-name pattern) actual)))
            (if (same-type? old actual) state (type-solution "inconsistent type arguments" bindings)))))
      (if (i32.and (sequence? pattern) (sequence? actual))
        (unify-types (items-of pattern) (items-of actual) 0 vars state)
        (if (same-type? pattern actual) state (type-solution "argument type does not match signature" (type-solution.bindings state)))))))
(fn unify-types ((patterns (list value)) (actuals (list value)) (index s32) (vars (list value)) (state type-solution)) type-solution
  (if (i32.ne (list-len patterns) (list-len actuals)) (type-solution "wrong number of type arguments" (type-solution.bindings state))
    (if (i32.ge_s index (list-len patterns)) state
      (unify-types patterns actuals (i32.add index 1) vars
        (unify-type (list-get patterns index) (list-get actuals index) vars state)))))
(fn solved-vars? ((vars (list value)) (index s32) (bindings (list binding))) s32
  (if (i32.ge_s index (list-len vars)) 1
    (let (v (lookup-local (symbol-name (list-get vars index)) bindings (i32.sub (list-len bindings) 1)))
      (if (failed? v) 0 (solved-vars? vars (i32.add index 1) bindings)))))

; Normalize fn syntax for ordinary functions, generic functions, and instances.
(fn fn-shape ((items (list value)) (signature s32)) value
  (if (i32.lt_s (list-len items) (if signature 4 5)) (failure "incomplete function declaration")
    (let (ri (if (string=? (symbol-name (list-get items 3)) ":") 4 3))
      (if (i32.ge_s ri (list-len items)) (failure "missing function return type")
        (let (wi (i32.add ri 1))
          (let (has-where (if (i32.lt_s wi (list-len items)) (string=? (form-head (list-get items wi)) "where") 0))
            (if (i32.ne (list-len items) (i32.add ri (if signature 1 (i32.add 2 has-where))))
              (failure "invalid function declaration")
              (if (i32.eq (i32.and (symbol? (list-get items 1)) (sequence? (list-get items 2))) 0)
                (failure "invalid function name or parameters")
                (sequence (list-push (list-push (list-push (list-push (list-push (list-new value)
                  (list-get items 1)) (list-get items 2)) (list-get items ri))
                  (if has-where (list-get items wi) (nil)))
                  (if signature (nil) (list-get items (i32.sub (list-len items) 1)))))))))))))
(fn normalize-generic-fields ((fields (list value)) (index s32) (vars (list value)) (out (list value))) value
  (if (i32.ge_s index (list-len fields)) (sequence out)
    (let (parts (items-of (list-get fields index)))
      (if (i32.eq (i32.or (i32.eq (list-len parts) 2)
            (if (i32.eq (list-len parts) 3) (string=? (symbol-name (list-get parts 1)) ":") 0)) 0)
        (failure "expected typed function parameter")
        (let (name (list-get parts 0))
          (let (ty (list-get parts (i32.sub (list-len parts) 1)))
            (if (i32.eq (i32.and (symbol? name) (generic-type? ty vars)) 0) (failure "invalid parameter name or type")
              (if (field-present? out (binding-key name) 0 1) (failure "duplicate parameter")
                (normalize-generic-fields fields (i32.add index 1) vars (list-push out (sr-pair name ty)))))))))))
(fn where-vars ((entries (list value)) (index s32) (out (list value))) value
  (if (i32.ge_s index (list-len entries)) (sequence out)
    (let (entry (list-get entries index))
      (if (symbol? entry)
        (if (known-type? entry "") (failure "type parameter must not name a concrete type")
          (where-vars entries (i32.add index 1)
            (if (sr-symbol-in? (symbol-name entry) out 0) out (list-push out entry))))
        (let (parts (items-of entry))
          (let (trait (trait-index (form-head entry)))
            (if (i32.lt_s trait 0) (failure "unknown trait in where clause")
              (if (i32.ne (list-len parts) (i32.add 1 (list-len (trait-definition.vars (list-get (global.get $traits) trait)))))
                (failure "wrong number of trait type parameters")
                (let (vars (where-bare-vars parts 1 out))
                  (if (failed? vars) vars (where-vars entries (i32.add index 1) (items-of vars))))))))))))
(fn where-bare-vars ((parts (list value)) (index s32) (out (list value))) value
  (if (i32.ge_s index (list-len parts)) (sequence out)
    (let (v (list-get parts index))
      (if (i32.eq (symbol? v) 0) (failure "where constraints expect type parameters")
        (if (known-type? v "") (failure "where constraint expects a type parameter")
          (where-bare-vars parts (i32.add index 1)
            (if (sr-symbol-in? (symbol-name v) out 0) out (list-push out v))))))))
(fn declare-function ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "fn is only supported at top level")
    (let (shape (fn-shape items 0))
      (if (failed? shape) shape
        (let (parts (items-of shape))
          (let (where (list-get parts 3))
            (let (vars (where-vars (items-of where) 1 (list-new value)))
              (if (failed? vars) vars
                (if (if (list-len (items-of where)) (i32.eq (list-len (items-of vars)) 0) 0)
                  (failure "generic function needs type parameters")
                  (let (params (normalize-generic-fields (items-of (list-get parts 1)) 0 (items-of vars) (list-new value)))
                    (if (failed? params) params
                      (if (i32.eq (generic-type? (list-get parts 2) (items-of vars)) 0) (failure "unsupported return type")
                        (publish (binding-key (list-get parts 0))
                          (if (list-len (items-of where))
                            (generic-function (items-of params) (list-get parts 2) (list-get parts 4) (items-of vars)
                              (copy-list (items-of where) 1 (list-new value)))
                            (typed-function (items-of params) (list-get parts 2) (list-get parts 4))))))))))))))))
(fn specialize-fields ((fields (list value)) (index s32) (bindings (list binding)) (out (list value))) (list value)
  (if (i32.ge_s index (list-len fields)) out
    (specialize-fields fields (i32.add index 1) bindings
      (list-push out (sr-pair (list-get (items-of (list-get fields index)) 0) (substitute-type (field-type (list-get fields index)) bindings))))))

; Replace type positions only; lexical variables and quoted data keep their names.
(fn specialize-body ((expr value) (bindings (list binding))) value
  (if (sequence? expr)
    (let (name (form-head expr))
      (if (i32.or (string=? name "quote") (string=? name "quasiquote")) expr
        (sequence (specialize-body-items (items-of expr) 0 name bindings (list-new value))))) expr))
(fn specialize-body-items ((items (list value)) (index s32) (name string) (bindings (list binding)) (out (list value))) (list value)
  (if (i32.ge_s index (list-len items)) out
    (let (item (list-get items index))
      (let (type-position
        (if (i32.or (string=? name "list-new") (i32.or (string=? name "some") (string=? name "none"))) (i32.eq index 1)
          (if (i32.or (string=? name "ok") (string=? name "err")) (i32.or (i32.eq index 1) (i32.eq index 2))
            (if (if (i32.eq (list-len items) 3) (string=? (symbol-name (list-get items 1)) ":") 0) (i32.eq index 2) 0))))
        (let (next (if type-position (substitute-type item bindings)
          (if (i32.and (string=? name "lambda") (i32.eq index 1)) item
            (if (i32.and (string=? name "match") (i32.ge_s index 2)) (specialize-pair item bindings)
              (if (i32.and (string=? name "let") (i32.eq index 1))
                (specialize-let item bindings) (specialize-body item bindings))))))
          (specialize-body-items items (i32.add index 1) name bindings (list-push out next)))))))
(fn specialize-let ((pair value) (bindings (list binding))) value
  (let (parts (items-of pair))
    (if (if (i32.eq (list-len parts) 4) (string=? (symbol-name (list-get parts 1)) ":") 0)
      (sequence (list-push (list-push (list-push (list-push (list-new value) (list-get parts 0)) (list-get parts 1))
        (substitute-type (list-get parts 2) bindings)) (specialize-body (list-get parts 3) bindings)))
      (specialize-pair pair bindings))))
(fn specialize-pair ((pair value) (bindings (list binding))) value
  (let (parts (items-of pair))
    (if (i32.eq (list-len parts) 2)
      (sr-pair (list-get parts 0) (specialize-body (list-get parts 1) bindings)) pair)))
(fn infer-generic ((params (list value)) (result value) (vars (list value)) (actuals (list value)) (expected string)) type-solution
  (unify-types (schema-types params 0 (list-new value)) actuals 0 vars
    (if (string-len expected) (unify-type result (symbol expected) vars (empty-solution)) (empty-solution))))
(fn apply-generic ((params (list value)) (result value) (body value) (vars (list value)) (constraints (list value)) (args (list value)) (depth s32) (expected string)) value
  (let (state (infer-generic params result vars (argument-value-types args 0 (list-new value)) expected))
    (if (string-len (type-solution.error state)) (failure (type-solution.error state))
      (let (bindings (type-solution.bindings state))
        (if (i32.eq (solved-vars? vars 0 bindings) 0) (failure "cannot infer all type parameters")
          (let (dictionary (constraint-methods constraints 0 bindings (list-new binding)))
            (if (string-len (type-solution.error dictionary)) (failure (type-solution.error dictionary))
              (let (concrete (specialize-fields params 0 bindings (list-new value)))
                (let (checked (check-arguments (schema-types concrete 0 (list-new value)) args 0))
                  (if (failed? checked) checked
                    (let (ret (substitute-type result bindings))
                      (require-type (eval-expected (specialize-body body bindings)
                        (typed-bindings concrete args 0 (type-solution.bindings dictionary)) depth 0 (symbol-name ret)) ret))))))))))))
