; Traits and instances are session declarations. Validate whole declarations
; before publishing methods or implementations; instances have concrete types.
(record trait-definition (name string) (vars (list value)) (methods (list value)))
(record trait-implementation (name string) (types (list value)) (methods (list binding)))
(global $traits (list trait-definition) mut 0)
(global $instances (list trait-implementation) mut 0)
(fn trait-index ((name string)) s32 (find-trait name 0))
(fn find-trait ((name string) (index s32)) s32
  (if (i32.ge_s index (list-len (global.get $traits))) -1
    (if (string=? name (trait-definition.name (list-get (global.get $traits) index))) index
      (find-trait name (i32.add index 1)))))
(fn instance-index ((name string) (types (list value)) (index s32)) s32
  (if (i32.ge_s index (list-len (global.get $instances))) -1
    (let (entry (list-get (global.get $instances) index))
      (if (i32.and (string=? name (trait-implementation.name entry)) (same-types? types (trait-implementation.types entry) 0)) index
        (instance-index name types (i32.add index 1))))))
(fn trait-method-schema ((methods (list value)) (name string) (index s32)) value
  (if (i32.ge_s index (list-len methods)) (failure "unknown trait method")
    (let (entry (list-get methods index))
      (if (string=? name (symbol-name (list-get (items-of entry) 0))) entry
        (trait-method-schema methods name (i32.add index 1))))))
(fn method-owner ((name string) (index s32)) s32
  (if (i32.ge_s index (list-len (global.get $traits))) -1
    (if (failed? (trait-method-schema (trait-definition.methods (list-get (global.get $traits) index)) name 0))
      (method-owner name (i32.add index 1)) index)))
(fn declare-trait ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "trait is only supported at top level")
    (if (i32.lt_s (list-len items) 3) (failure "trait expects a header and method signatures")
      (let (header (items-of (list-get items 1)))
        (if (i32.lt_s (list-len header) 2) (failure "trait expects a name and type parameters")
          (let (name (symbol-name (list-get header 0)))
            (let (vars (copy-list header 1 (list-new value)))
              (let (checked (check-params vars 0))
                (if (failed? checked) checked
                  (if (i32.or (string=? name "") (i32.ge_s (trait-index name) 0)) (failure "invalid or already declared trait")
                    (let (valid (where-bare-vars header 1 (list-new value)))
                      (if (failed? valid) valid
                        (let (methods (trait-signatures items 2 vars (list-new value)))
                          (if (failed? methods) methods
                            (begin
                              (global.set $traits (list-push (global.get $traits) (trait-definition name vars (items-of methods))))
                              (publish-trait-methods name (items-of methods) 0))))))))))))))))
(fn trait-signatures ((items (list value)) (index s32) (vars (list value)) (out (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence out)
    (let (item (list-get items index))
      (if (i32.eq (string=? (form-head item) "fn") 0) (failure "trait expects function signatures")
        (let (shape (fn-shape (items-of item) 1))
          (if (failed? shape) shape
            (let (parts (items-of shape))
              (let (name (symbol-name (list-get parts 0)))
                (if (i32.or (i32.ge_s (method-owner name 0) 0) (i32.eq (failed? (trait-method-schema out name 0)) 0))
                  (failure "trait method name is already declared")
                  (let (params (normalize-generic-fields (items-of (list-get parts 1)) 0 vars (list-new value)))
                    (if (failed? params) params
                      (if (i32.eq (generic-type? (list-get parts 2) vars) 0) (failure "invalid trait return type")
                        (trait-signatures items (i32.add index 1) vars
                          (list-push out (sequence (list-push (list-push (list-push (list-new value)
                            (list-get parts 0)) params) (list-get parts 2)))))))))))))))))
(fn publish-trait-methods ((name string) (methods (list value)) (index s32)) value
  (if (i32.ge_s index (list-len methods)) (nil)
    (let (method (symbol-name (list-get (items-of (list-get methods index)) 0)))
      (begin (publish method (trait-method name method))
        (publish-trait-methods name methods (i32.add index 1))))))
(fn declare-instance ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "instance is only supported at top level")
    (if (i32.lt_s (list-len items) 3) (failure "instance expects a trait header and methods")
      (let (header (items-of (list-get items 1)))
        (let (index (trait-index (form-head (list-get items 1))))
          (if (i32.lt_s index 0) (failure "unknown trait in instance")
            (let (trait (list-get (global.get $traits) index))
              (let (types (copy-list header 1 (list-new value)))
                (if (i32.ne (list-len types) (list-len (trait-definition.vars trait))) (failure "wrong number of instance types")
                  (if (i32.eq (generic-types? types 0 (list-new value)) 0) (failure "instance types must be concrete")
                    (if (i32.ge_s (instance-index (trait-definition.name trait) types 0) 0) (failure "instance is already declared")
                      (let (bindings (bind-type-vars (trait-definition.vars trait) types 0 (list-new binding)))
                        (let (methods (instance-methods items 2 trait bindings (type-solution "" (list-new binding))))
                          (if (string-len (type-solution.error methods)) (failure (type-solution.error methods))
                            (if (i32.ne (list-len (type-solution.bindings methods)) (list-len (trait-definition.methods trait)))
                              (failure "instance is missing trait methods")
                              (begin
                                (global.set $instances (list-push (global.get $instances)
                                  (trait-implementation (trait-definition.name trait) types (type-solution.bindings methods))))
                                (nil)))))))))))))))))
(fn bind-type-vars ((vars (list value)) (types (list value)) (index s32) (out (list binding))) (list binding)
  (if (i32.ge_s index (list-len vars)) out
    (bind-type-vars vars types (i32.add index 1)
      (list-push out (binding (symbol-name (list-get vars index)) (list-get types index))))))
(fn instance-methods ((items (list value)) (index s32) (trait trait-definition) (bindings (list binding)) (out type-solution)) type-solution
  (if (i32.or (string-len (type-solution.error out)) (i32.ge_s index (list-len items))) out
    (let (item (list-get items index))
      (if (i32.eq (string=? (form-head item) "fn") 0) (type-solution "instance expects function definitions" (list-new binding))
        (let (shape (fn-shape (items-of item) 0))
          (if (failed? shape) (type-solution "invalid instance method" (list-new binding))
            (let (parts (items-of shape))
              (let (name (symbol-name (list-get parts 0)))
                (let (schema (trait-method-schema (trait-definition.methods trait) name 0))
                  (if (failed? schema) (type-solution "unknown method in instance" (list-new binding))
                    (if (i32.eq (failed? (lookup-local name (type-solution.bindings out) (i32.sub (list-len (type-solution.bindings out)) 1))) 0)
                      (type-solution "duplicate instance method" (list-new binding))
                      (let (method (checked-instance-method parts (items-of schema) bindings))
                        (if (failed? method) (type-solution (show method) (list-new binding))
                          (instance-methods items (i32.add index 1) trait bindings
                            (type-solution "" (list-push (type-solution.bindings out) (binding name method)))))))))))))))))
(fn checked-instance-method ((parts (list value)) (schema (list value)) (bindings (list binding))) value
  (if (list-len (items-of (list-get parts 3))) (failure "instance methods must be concrete")
    (let (params (normalize-generic-fields (items-of (list-get parts 1)) 0 (list-new value) (list-new value)))
      (if (failed? params) params
        (let (expected (substitute-type (function-type (schema-types (items-of (list-get schema 1)) 0 (list-new value)) (list-get schema 2)) bindings))
          (let (actual (function-type (schema-types (items-of params) 0 (list-new value)) (list-get parts 2)))
            (if (same-type? expected actual) (typed-function (items-of params) (list-get parts 2) (list-get parts 4))
              (failure "instance method signature does not match trait"))))))))
(fn constraint-methods ((constraints (list value)) (index s32) (bindings (list binding)) (out (list binding))) type-solution
  (if (i32.ge_s index (list-len constraints)) (type-solution "" out)
    (let (constraint (list-get constraints index))
      (if (symbol? constraint) (constraint-methods constraints (i32.add index 1) bindings out)
        (let (parts (items-of constraint))
          (let (types (substitute-types (copy-list parts 1 (list-new value)) 0 bindings (list-new value)))
            (let (name (form-head constraint))
              (let (found (instance-index name types 0))
                (if (i32.lt_s found 0) (type-solution "missing required trait instance" out)
                  (constraint-methods constraints (i32.add index 1) bindings
                    (dictionary-methods name (trait-implementation.methods (list-get (global.get $instances) found)) 0 out)))))))))))
(fn dictionary-methods ((trait string) (methods (list binding)) (index s32) (out (list binding))) (list binding)
  (if (i32.ge_s index (list-len methods)) out
    (let (entry (list-get methods index))
      (let (name (binding.name entry))
        (let (old (lookup-local name out (i32.sub (list-len out) 1)))
          (let (method (if (failed? old) (binding.item entry)
                        (if (same-type? (callable-type old) (callable-type (binding.item entry))) old (trait-method trait name))))
            (dictionary-methods trait methods (i32.add index 1)
              (list-push (list-push out (binding name method)) (binding (string-append " trait:" name) method)))))))))

; Resolve direct method calls from known arguments and the expected scalar return.
(fn trait-candidates ((trait string) (method string) (actuals (list value)) (expected string) (index s32) (out (list value))) (list value)
  (if (i32.ge_s index (list-len (global.get $instances))) out
    (let (entry (list-get (global.get $instances) index))
      (let (implementation (if (string=? trait (trait-implementation.name entry))
                             (lookup-local method (trait-implementation.methods entry) (i32.sub (list-len (trait-implementation.methods entry)) 1))
                             (failure "different trait")))
        (trait-candidates trait method actuals expected (i32.add index 1)
          (if (failed? implementation) out
            (value-case implementation
              ((typed-function params result body)
                (let (state (infer-generic params result (list-new value) actuals expected))
                  (if (string-len (type-solution.error state)) out (list-push out implementation))))
              (else out))))))))
(fn resolve-method ((trait string) (method string) (actuals (list value)) (expected string)) value
  (let (candidates (trait-candidates trait method actuals expected 0 (list-new value)))
    (if (i32.eq (list-len candidates) 1) (list-get candidates 0)
      (failure (if (list-len candidates) "ambiguous trait method" "no matching trait instance")))))
