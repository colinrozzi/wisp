; Compound types are structural; named record/variant types retain their identity.
(fn unary-type ((name string) (element value)) value
  (sequence (list-push (list-push (list-new value) (symbol name)) element)))
(fn result-type ((ok-type value) (err-type value)) value
  (sequence (list-push (list-push (list-push (list-new value) (symbol "result")) ok-type) err-type)))
(fn known-compound-type? ((parts (list value)) (self string)) s32
  (if (i32.lt_s (list-len parts) 2) 0
    (let (name (symbol-name (list-get parts 0)))
      (if (i32.or (string=? name "list") (string=? name "option"))
        (if (i32.eq (list-len parts) 2) (known-type? (list-get parts 1) self) 0)
        (if (string=? name "result")
          (if (i32.eq (list-len parts) 3) (validate-case-types parts 1 self) 0)
          (if (string=? name "tuple") (validate-case-types parts 1 self) 0))))))
(fn same-types? ((left (list value)) (right (list value)) (index s32)) s32
  (if (i32.ne (list-len left) (list-len right)) 0
    (if (i32.ge_s index (list-len left)) 1
      (if (same-type? (list-get left index) (list-get right index))
        (same-types? left right (i32.add index 1)) 0))))
(fn same-type? ((left value) (right value)) s32
  (if (symbol? left) (if (symbol? right) (string=? (symbol-name left) (symbol-name right)) 0)
    (value-case left
      ((sequence parts)
        (value-case right ((sequence other) (same-types? parts other 0)) (else 0)))
      (else 0))))

(fn value-type ((v value)) value
  (value-case v
    ((integer n) (symbol "s32"))
    ((wide-integer n) (symbol "s64"))
    ((single n) (symbol "f32"))
    ((double n) (symbol "f64"))
    ((text s) (symbol "string"))
    ((aggregate name id case-name fields) (symbol name))
    ((typed-list element items) (unary-type "list" element))
    ((compound ty case-name fields) ty)
    (else (failure "value has no supported compiled type"))))
(fn tuple-types ((items (list value)) (index s32) (out (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence out)
    (let (ty (value-type (list-get items index)))
      (if (failed? ty) ty (tuple-types items (i32.add index 1) (list-push out ty))))))

(fn collection-form? ((name string)) s32
  (i32.or (string=? name "list-new")
    (i32.or (i32.or (string=? name "some") (string=? name "none"))
      (i32.or (string=? name "ok") (string=? name "err")))))
(fn collection-builtin? ((name string)) s32
  (i32.or (string=? name "tuple")
    (i32.or (string=? name "list-push") (i32.or (string=? name "list-get") (string=? name "list-len")))))
(fn eval-collection-form ((name string) (items (list value)) (env (list binding)) (depth s32)) value
  (let (empty (i32.or (string=? name "list-new") (string=? name "none")))
    (let (result (i32.or (string=? name "ok") (string=? name "err")))
      (if (i32.ne (list-len items) (if empty 2 (if result 4 3)))
        (failure "wrong number of constructor arguments")
        (let (first (list-get items 1))
          (if (i32.eq (known-type? first "") 0) (failure "unsupported element or payload type")
            (if (string=? name "list-new") (typed-list first (list-new value))
              (let (ty (if result (result-type first (list-get items 2)) (unary-type "option" first)))
                (if (i32.eq (known-type? ty "") 0) (failure "unsupported result type")
                  (if empty (compound ty name (list-new value))
                    (let (payload-type (if (string=? name "err") (list-get items 2) first))
                      (let (payload (require-type
                        (eval-expected (list-get items (i32.sub (list-len items) 1)) env depth 0 (symbol-name payload-type)) payload-type))
                        (if (failed? payload) payload (compound ty name (list-push (list-new value) payload)))))))))))))))

; The second list-push argument can adopt the list's element type when it is
; a literal. Stored values retain their types. Evaluation remains left-to-right.
(fn call-argument-type ((callee value) (args (list value)) (index s32)) string
  (value-case callee
    ((builtin name)
      (if (i32.and (string=? name "list-push") (i32.eq index 1))
        (value-case (list-get args 0)
          ((typed-list element items) (symbol-name element))
          (else ""))
        (argument-type callee index)))
    (else (argument-type callee index))))
(fn apply-collection ((name string) (args (list value))) value
  (if (string=? name "tuple")
    (if (i32.eq (list-len args) 0) (failure "tuple expects at least one value")
      (let (ty (tuple-types args 0 (list-push (list-new value) (symbol "tuple"))))
        (if (failed? ty) ty (compound ty "tuple" args))))
    (if (i32.ne (list-len args) (if (string=? name "list-len") 1 2))
      (failure "wrong number of list arguments")
      (value-case (list-get args 0)
        ((typed-list element items)
          (if (string=? name "list-len") (integer (list-len items))
            (if (string=? name "list-push")
              (let (checked (require-type (list-get args 1) element))
                (if (failed? checked) checked (typed-list element (list-push items checked))))
              (value-case (list-get args 1)
                ((integer index)
                  (if (i32.and (i32.ge_s index 0) (i32.lt_s index (list-len items)))
                    (list-get items index) (failure "list index out of bounds")))
                (else (failure "list index must be s32"))))))
        (else (failure "expected typed list"))))))

; Reuse the existing variant arm validation and lexical binding machinery.
(fn compound-cases ((ty value)) (list value)
  (let (parts (items-of ty))
    (let (name (symbol-name (list-get parts 0)))
      (if (string=? name "option")
        (list-push (list-push (list-new value)
          (unary-type "some" (list-get parts 1)))
          (sequence (list-push (list-new value) (symbol "none"))))
        (if (string=? name "result")
          (list-push (list-push (list-new value) (unary-type "ok" (list-get parts 1)))
            (unary-type "err" (list-get parts 2)))
          (list-new value))))))

(fn compound-type-label ((ty value) (case-name string)) string
  (let (parts (items-of ty))
    (if (string=? case-name "tuple") ""
      (let (first (string-append " " (show (list-get parts 1))))
        (if (string=? (symbol-name (list-get parts 0)) "option") first
          (string-append first (string-append " " (show (list-get parts 2)))))))))

(fn show-compound ((ty value) (case-name string) (fields (list value)) (depth s32)) string
  (let (types (compound-type-label ty case-name))
    (show-list-at fields 0
      (string-append "(" (string-append case-name (string-append types (if (list-len fields) " " "")))) depth)))
