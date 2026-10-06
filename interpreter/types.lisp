; Declarations and runtime type checks for the initial compiled-source subset.
(global $types (list named-type) mut 0)

(fn find-type-index ((name string) (index s32)) s32
  (if (i32.lt_s index 0) -1
    (if (string=? name (named-type.name (list-get (global.get $types) index))) index
      (find-type-index name (i32.sub index 1)))))

(fn type-index ((name string)) s32
  (find-type-index name (i32.sub (list-len (global.get $types)) 1)))

(fn known-type? ((ty value) (self string)) s32
  (value-case ty
    ((symbol name)
      (i32.or (i32.or (numeric-type? name) (string=? name "string"))
        (i32.or (string=? name self) (i32.ge_s (type-index name) 0))))
    ((sequence parts) (known-compound-type? parts self))
    (else 0)))

(fn value-has-type? ((v value) (ty value)) s32
  (let (name (symbol-name ty))
    (value-case v
      ((integer n) (string=? name "s32"))
      ((wide-integer n) (string=? name "s64"))
      ((single n) (string=? name "f32"))
      ((double n) (string=? name "f64"))
      ((typed-list element items) (same-type? (unary-type "list" element) ty))
      ((compound actual case-name fields) (same-type? actual ty))
      ((text s) (string=? name "string"))
      ((aggregate type-name id case-name fields)
        (let (index (type-index name))
          (if (i32.lt_s index 0) 0
            (i32.eq id (named-type.id (list-get (global.get $types) index))))))
      (else 0))))

(fn require-type ((v value) (ty value)) value
  (if (failed? v) v
    (if (value-has-type? v ty) v
      (failure (string-append "expected " (show ty))))))

; Both (name type) and (name : type) declarations normalize to (name type).
(fn normalize-field ((field value) (self string)) value
  (let (parts (items-of field))
    (if (i32.or (i32.eq (list-len parts) 2)
          (if (i32.eq (list-len parts) 3) (string=? (symbol-name (list-get parts 1)) ":") 0))
      (let (name (list-get parts 0))
        (let (ty (list-get parts (i32.sub (list-len parts) 1)))
          (if (i32.and (symbol? name) (known-type? ty self))
            (sequence (list-push (list-push (list-new value) name) ty))
            (failure "invalid name or unsupported type in declaration"))))
      (failure "expected (name type) or (name : type)"))))

(fn field-name ((field value)) string (symbol-name (list-get (items-of field) 0)))
(fn field-type ((field value)) value (list-get (items-of field) 1))

(fn field-present? ((fields (list value)) (name string) (index s32)) s32
  (if (i32.ge_s index (list-len fields)) 0
    (if (string=? (field-name (list-get fields index)) name) 1
      (field-present? fields name (i32.add index 1)))))

(fn normalize-fields ((fields (list value)) (index s32) (out (list value)) (self string)) value
  (if (i32.ge_s index (list-len fields)) (sequence out)
    (let (field (normalize-field (list-get fields index) self))
      (if (failed? field) field
        (if (field-present? out (field-name field) 0) (failure "duplicate field or parameter")
          (normalize-fields fields (i32.add index 1) (list-push out field) self))))))

(fn publish ((name string) (v value)) value
  (begin (global.set $bindings (list-push (global.get $bindings) (binding name v))) v))

(fn eval-fn ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "fn is only supported at top level")
    (if (i32.or (i32.eq (list-len items) 5)
          (if (i32.eq (list-len items) 6) (string=? (symbol-name (list-get items 3)) ":") 0))
      (let (name (list-get items 1))
        (let (params (list-get items 2))
          (let (result (list-get items (i32.sub (list-len items) 2)))
            (if (i32.and (symbol? name) (i32.and (sequence? params) (known-type? result "")))
              (let (checked (normalize-fields (items-of params) 0 (list-new value) ""))
                (if (failed? checked) checked
                  (publish (symbol-name name)
                    (typed-function (items-of checked) result (list-get items (i32.sub (list-len items) 1))))))
              (failure "invalid function name, parameters, or return type")))))
      (failure "fn expects name, typed parameters, return type, and body"))))

(fn check-arguments ((types (list value)) (args (list value)) (index s32)) value
  (if (i32.ne (list-len types) (list-len args)) (failure "wrong number of arguments")
    (if (i32.ge_s index (list-len args)) (nil)
      (let (checked (require-type (list-get args index) (list-get types index)))
        (if (failed? checked) checked (check-arguments types args (i32.add index 1)))))))

(fn schema-types ((fields (list value)) (index s32) (out (list value))) (list value)
  (if (i32.ge_s index (list-len fields)) out
    (schema-types fields (i32.add index 1) (list-push out (field-type (list-get fields index))))))

(fn typed-bindings ((params (list value)) (args (list value)) (index s32) (env (list binding))) (list binding)
  (if (i32.ge_s index (list-len params)) env
    (typed-bindings params args (i32.add index 1)
      (list-push env (binding (field-name (list-get params index)) (list-get args index))))))

(fn apply-typed ((params (list value)) (result value) (body value) (args (list value)) (depth s32)) value
  (let (checked (check-arguments (schema-types params 0 (list-new value)) args 0))
    (if (failed? checked) checked
      (require-type (eval-expected body (typed-bindings params args 0 (list-new binding)) depth 0 (symbol-name result)) result))))

(fn add-accessors ((name string) (id s32) (fields (list value)) (index s32)) value
  (if (i32.ge_s index (list-len fields)) (nil)
    (begin
      (publish (string-append name (string-append "." (field-name (list-get fields index))))
        (field-reader name id index))
      (add-accessors name id fields (i32.add index 1)))))

(fn declaration-tail ((items (list value))) (list value)
  (copy-list items 2 (list-new value)))

(fn eval-record ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "record is only supported at top level")
    (if (i32.lt_s (list-len items) 2) (failure "record expects a name")
      (let (name (symbol-name (list-get items 1)))
        (if (i32.or (string=? name "") (known-type? (symbol name) ""))
          (failure "invalid or already defined type name")
          (let (checked (normalize-fields (declaration-tail items) 0 (list-new value) name))
            (if (failed? checked) checked
              (let (fields (items-of checked))
                (let (id (list-len (global.get $types)))
                  (begin
                    (global.set $types (list-push (global.get $types) (named-type name id 0 fields)))
                    (publish name (constructor name id name (schema-types fields 0 (list-new value))))
                    (add-accessors name id fields 0)))))))))))

(fn validate-case-types ((items (list value)) (index s32) (self string)) s32
  (if (i32.ge_s index (list-len items)) 1
    (if (known-type? (list-get items index) self)
      (validate-case-types items (i32.add index 1) self) 0)))

(fn case-present? ((cases (list value)) (name string) (end s32)) s32
  (if (i32.lt_s end 0) 0
    (if (string=? (symbol-name (list-get (items-of (list-get cases end)) 0)) name) 1
      (case-present? cases name (i32.sub end 1)))))

(fn validate-cases ((cases (list value)) (index s32) (self string)) value
  (if (i32.ge_s index (list-len cases)) (nil)
    (let (parts (items-of (list-get cases index)))
      (if (i32.eq (list-len parts) 0) (failure "variant expects nonempty case declarations")
        (let (name (symbol-name (list-get parts 0)))
          (if (i32.or (string=? name "") (case-present? cases name (i32.sub index 1)))
            (failure "invalid or duplicate variant case")
            (if (validate-case-types parts 1 self) (validate-cases cases (i32.add index 1) self)
              (failure "unsupported variant payload type"))))))))

(fn add-constructors ((name string) (id s32) (cases (list value)) (index s32)) value
  (if (i32.ge_s index (list-len cases)) (nil)
    (let (parts (items-of (list-get cases index)))
      (let (case-name (symbol-name (list-get parts 0)))
        (begin
          (publish case-name (constructor name id case-name (copy-list parts 1 (list-new value))))
          (add-constructors name id cases (i32.add index 1)))))))

(fn eval-variant ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "variant is only supported at top level")
    (if (i32.lt_s (list-len items) 3) (failure "variant expects a name and cases")
      (let (name (symbol-name (list-get items 1)))
        (if (i32.or (string=? name "") (known-type? (symbol name) ""))
          (failure "invalid or already defined type name")
          (let (cases (declaration-tail items))
            (let (checked (validate-cases cases 0 name))
              (if (failed? checked) checked
                (let (id (list-len (global.get $types)))
                  (begin
                    (global.set $types (list-push (global.get $types) (named-type name id 1 cases)))
                    (add-constructors name id cases 0)))))))))))

(fn apply-constructor ((name string) (id s32) (case-name string) (types (list value)) (args (list value))) value
  (let (checked (check-arguments types args 0))
    (if (failed? checked) checked (aggregate name id case-name args))))

(fn apply-field ((name string) (id s32) (index s32) (args (list value))) value
  (if (i32.ne (list-len args) 1) (failure "field access expects one argument")
    (value-case (list-get args 0)
      ((aggregate actual-name actual-id case-name fields)
        (if (i32.eq id actual-id) (list-get fields index)
          (failure (string-append "expected record " name))))
      (else (failure (string-append "expected record " name))))))

(fn eval-export ((items (list value)) (env (list binding)) (depth s32) (top s32)) value
  (if (i32.eq top 0) (failure "export is only supported at top level")
    (if (i32.eq (list-len items) 2)
      (let (item (list-get items 1))
        (if (symbol? item) (nil)
          (let (parts (items-of item))
            (if (i32.eq (list-len parts) 0) (failure "export expects a function")
              (if (string=? (symbol-name (list-get parts 0)) "fn")
                (eval-fn parts top) (failure "export expects a function"))))))
      (if (i32.eq (list-len items) 3)
        (value-case (list-get items 1)
          ((text alias)
            (let (parts (items-of (list-get items 2)))
              (if (i32.eq (list-len parts) 0) (failure "export expects a function")
                (if (string=? (symbol-name (list-get parts 0)) "fn")
                  (eval-fn parts top) (failure "export expects a function")))))
          (else (failure "export alias must be a string")))
        (failure "invalid export")))))
