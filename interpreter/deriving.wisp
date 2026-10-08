; Derived equality is an ordinary checked trait instance. Its body captures
; accessor/primitive values so later session redefinitions cannot change it.
(fn derive-call2 ((callee value) (a value) (b value)) value
  (sequence (list-push (list-push (list-push (list-new value) callee) a) b)))
(fn derive-equality-op ((ty value)) string
  (let (name (symbol-name ty))
    (if (i32.or (string=? name "s32") (string=? name "u8")) "i32.eq"
      (if (string=? name "s64") "i64.eq"
        (if (string=? name "f32") "f32.eq" (if (string=? name "f64") "f64.eq" ""))))))
(fn derive-equality-body ((record named-type) (index s32) (out value)) value
  (if (i32.ge_s index (list-len (named-type.schema record))) out
    (let (field (list-get (named-type.schema record) index))
      (let (op (derive-equality-op (field-type field)))
        (if (string=? op "")
          (failure (string-append "cannot derive Eq for non-scalar field: " (field-name field)))
          (let (accessor (field-reader (named-type.name record) (named-type.id record) index))
            (let (comparison (derive-call2 (builtin op)
                    (sr-pair accessor (symbol "a")) (sr-pair accessor (symbol "b"))))
              (derive-equality-body record (i32.add index 1)
                (derive-call2 (builtin "i32.and") out comparison)))))))))
(fn derive-method-name () string
  (let (index (trait-index "Eq"))
    (if (i32.lt_s index 0) "="
      (let (methods (trait-definition.methods (list-get (global.get $traits) index)))
        (if (failed? (trait-method-schema methods "=" 0)) "eq" "=")))))
(fn eval-derive ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "derive is only supported at top level")
    (if (i32.ne (list-len items) 3) (failure "derive expects (derive Eq Type)")
      (if (i32.eq (string=? (symbol-name (list-get items 1)) "Eq") 0)
        (failure "cannot derive trait (supported: Eq)")
        (let (index (type-index (symbol-name (list-get items 2))))
          (if (i32.lt_s index 0) (failure "derive Eq expects a declared record")
            (let (record (list-get (global.get $types) index))
              (if (named-type.variant? record) (failure "derive Eq expects a record, not a variant")
                (let (body (derive-equality-body record 0 (integer 1)))
                  (if (failed? body) body
                    (let (ty (symbol (named-type.name record)))
                      (let (params (sr-pair (sr-pair (symbol "a") ty) (sr-pair (symbol "b") ty)))
                        (let (method (sequence (list-push (list-push (list-push (list-push (list-push (list-new value)
                                      (symbol "fn")) (symbol (derive-method-name))) params) (symbol "s32")) body)))
                          (declare-instance (items-of (derive-call2 (symbol "instance") (sr-pair (symbol "Eq") ty) method)) top))))))))))))))
