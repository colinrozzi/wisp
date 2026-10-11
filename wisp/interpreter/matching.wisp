; Match a declared variant and bind its payload in a fresh lexical environment.
(fn find-case ((cases (list value)) (name string) (index s32)) (list value)
  (if (i32.ge_s index (list-len cases)) (list-new value)
    (let (parts (items-of (list-get cases index)))
      (if (string=? name (symbol-name (list-get parts 0))) parts
        (find-case cases name (i32.add index 1))))))

(fn validate-arms ((arms (list value)) (index s32) (cases (list value))) value
  (if (i32.ge_s index (list-len arms)) (nil)
    (let (parts (items-of (list-get arms index)))
      (if (i32.ne (list-len parts) 2) (failure "match expects ((case bindings...) body) arms")
        (let (pattern (items-of (list-get parts 0)))
          (if (i32.eq (list-len pattern) 0) (failure "match expects a case pattern")
            (let (case (find-case cases (symbol-name (list-get pattern 0)) 0))
              (if (i32.eq (list-len case) 0) (failure "unknown variant case in match")
                (if (i32.ne (list-len case) (list-len pattern)) (failure "wrong number of pattern bindings")
                  (let (checked (check-params (copy-list pattern 1 (list-new value)) 0))
                    (if (failed? checked) checked
                      (validate-arms arms (i32.add index 1) cases))))))))))))

(fn match-arms ((arms (list value)) (index s32) (case-name string) (fields (list value)) (env (list binding)) (depth s32) (expected string)) value
  (if (i32.ge_s index (list-len arms)) (failure "no matching variant case")
    (let (parts (items-of (list-get arms index)))
      (let (pattern (items-of (list-get parts 0)))
        (if (string=? (symbol-name (list-get pattern 0)) case-name)
          (eval-expected (list-get parts 1)
            (bind-args (copy-list pattern 1 (list-new value)) fields 0 (copy-env env 0 (list-new binding))) depth 0 expected)
          (match-arms arms (i32.add index 1) case-name fields env depth expected))))))

(fn eval-match ((items (list value)) (env (list binding)) (depth s32) (expected string)) value
  (if (i32.lt_s (list-len items) 3) (failure "match expects a value and arms")
    (let (v (eval (list-get items 1) env depth 0))
      (value-case v
        ((failure message) v)
        ((compound ty case-name fields)
          (let (cases (compound-cases ty))
            (if (i32.eq (list-len cases) 0) (failure "match expects an option, result, or variant")
              (let (arms (declaration-tail items))
                (let (checked (validate-arms arms 0 cases))
                  (if (failed? checked) checked (match-arms arms 0 case-name fields env depth expected)))))))
        ((aggregate name id case-name fields)
          (let (ty (list-get (global.get $types) id))
            (let (arms (declaration-tail items))
              (let (cases (if (named-type.variant? ty) (named-type.schema ty) (record-cases name (named-type.schema ty))))
                (let (checked (validate-arms arms 0 cases))
                  (if (failed? checked) checked (match-arms arms 0 case-name fields env depth expected)))))))
        (else (failure "match expects an option, result, variant, or record"))))))

; A record matches a single synthetic case named after the record, carrying its
; field types in order: (record-name field-type ...). So (match r ((Rec a b) …))
; destructures a record the same way a one-case variant would.
(fn record-field-types ((schema (list value)) (index s32) (acc (list value))) (list value)
  (if (i32.ge_s index (list-len schema)) acc
    (record-field-types schema (i32.add index 1) (list-push acc (field-type (list-get schema index))))))
(fn record-cases ((name string) (schema (list value))) (list value)
  (list-push (list-new value)
    (sequence (record-field-types schema 0 (list-push (list-new value) (symbol name))))))
