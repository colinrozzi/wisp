; Explicit globals are separate from lexical bindings and top-level define.
; An empty contents list represents the compiler's zero-initialized pointer
; globals until a real typed value is assigned; raw pointers are never exposed.
(record global-binding (name string) (type value) (mutable s32) (contents (list value)))
(global $session-globals (list global-binding) mut 0)

(fn global-name? ((v value)) s32
  (let (name (symbol-name v))
    (if (string-len name) (i32.eq (string-ref name 0) 36) 0)))
(fn find-global ((name string) (index s32)) s32
  (if (i32.lt_s index 0) -1
    (if (string=? name (global-binding.name (list-get (global.get $session-globals) index))) index
      (find-global name (i32.sub index 1)))))
(fn global-index ((name string)) s32
  (find-global name (i32.sub (list-len (global.get $session-globals)) 1)))
(fn global-form? ((name string)) s32
  (i32.or (string=? name "global") (i32.or (string=? name "global.get") (string=? name "global.set"))))

(fn global-initial-value ((ty value) (n s64)) value
  (let (name (symbol-name ty))
    (if (numeric-type? name)
      (if (if (string=? name "s32") (i32.or (i64.lt_s n -2147483648) (i64.gt_s n 4294967295)) 0)
        (failure "global initializer out of s32 range")
        (sequence (list-push (list-new value) (numeric-cast name (wide-integer n)))))
      (if (i64.eq n 0) (sequence (list-new value))
        (failure "non-numeric global initializer must be zero")))))
(fn global-initializer ((ty value) (expr value)) value
  (value-case expr
    ((integer-literal n) (global-initial-value ty n))
    ((wide-integer n) (global-initial-value ty n))
    (else (failure "global initializer must be an integer constant"))))
(fn declare-global ((items (list value)) (top s32)) value
  (if (i32.eq top 0) (failure "global is only supported at top level")
    (if (i32.or (i32.eq (list-len items) 5)
          (if (i32.eq (list-len items) 6) (string=? (symbol-name (list-get items 2)) ":") 0))
      (let (name (list-get items 1))
        (let (ty (list-get items (i32.sub (list-len items) 3)))
          (let (mode (symbol-name (list-get items (i32.sub (list-len items) 2))))
            (if (i32.eq (global-name? name) 0) (failure "global name must start with $")
              (if (i32.ge_s (global-index (symbol-name name)) 0) (failure "global is already declared")
                (if (i32.eq (known-type? ty "") 0) (failure "unsupported global type")
                  (if (i32.eq (i32.or (string=? mode "mut") (string=? mode "const")) 0)
                    (failure "global mutability must be mut or const")
                    (let (initial (global-initializer ty (list-get items (i32.sub (list-len items) 1))))
                      (if (failed? initial) initial
                        (begin
                          (global.set $session-globals (list-push (global.get $session-globals)
                            (global-binding (symbol-name name) ty (string=? mode "mut") (items-of initial))))
                          (nil)))))))))))
      (failure "global expects name, type, mutability, and initializer"))))

(fn replace-global ((entries (list global-binding)) (index s32) (target s32) (replacement global-binding) (out (list global-binding))) (list global-binding)
  (if (i32.ge_s index (list-len entries)) out
    (replace-global entries (i32.add index 1) target replacement
      (list-push out (if (i32.eq index target) replacement (list-get entries index))))))
(fn eval-global-access ((name string) (items (list value)) (env (list binding)) (depth s32)) value
  (if (i32.ne (list-len items) (if (string=? name "global.get") 2 3))
    (failure "wrong number of global arguments")
    (if (i32.eq (global-name? (list-get items 1)) 0) (failure "global name must start with $")
      (let (index (global-index (symbol-name (list-get items 1))))
        (if (i32.lt_s index 0) (failure "unknown global")
          (let (entry (list-get (global.get $session-globals) index))
            (if (string=? name "global.get")
              (if (list-len (global-binding.contents entry)) (list-get (global-binding.contents entry) 0)
                (failure "global has not been initialized with a typed value"))
              (if (i32.eq (global-binding.mutable entry) 0) (failure "cannot set immutable global")
                (let (ty (global-binding.type entry))
                  (let (v (require-type (eval-expected (list-get items 2) env depth 0 (symbol-name ty)) ty))
                    (if (failed? v) v
                      (begin
                        (global.set $session-globals
                          (replace-global (global.get $session-globals) 0 index
                            (global-binding (global-binding.name entry) ty 1 (list-push (list-new value) v)) (list-new global-binding)))
                        v))))))))))))
