; Classic template macros. Expansion consumes syntax, never evaluated arguments.
; The session table is published only after a whole input expands successfully.
(global $session-macros (list binding) mut 0)
(global $pending-macros (list binding) mut 0)
(global $macro-steps s32 mut 0)

(fn form-head ((v value)) string
  (let (parts (items-of v))
    (if (list-len parts) (symbol-name (list-get parts 0)) "")))
(fn prefixed ((name string) (v value)) value
  (if (failed? v) v
    (sequence (list-push (list-push (list-new value) (symbol name)) v))))
(fn quotation-prefix? ((name string)) s32
  (i32.or (string=? name "quasiquote")
    (i32.or (string=? name "unquote") (string=? name "unquote-splice"))))
(fn macro-reserved? ((name string)) s32
  (i32.or (quotation-prefix? name)
    (i32.or (string=? name "quote")
      (i32.or (string=? name "defmacro")
        (i32.or (string=? name "define-syntax") (string=? name "include"))))))

(fn collect-session-macros ((forms (list value)) (index s32)) value
  (if (i32.ge_s index (list-len forms)) (nil)
    (let (form (list-get forms index))
      (let (checked (if (string=? (form-head form) "defmacro")
                       (collect-session-macro (items-of form)) (nil)))
        (if (failed? checked) checked
          (collect-session-macros forms (i32.add index 1)))))))
(fn collect-session-macro ((parts (list value))) value
  (if (i32.ne (list-len parts) 4) (failure "defmacro expects name, parameters, and template")
    (let (name (symbol-name (list-get parts 1)))
      (if (i32.eq (string-len name) 0) (failure "defmacro expects a symbol name")
        (if (macro-reserved? name) (failure "reserved macro name")
          (value-case (list-get parts 2)
            ((sequence params)
              (let (checked (check-params params 0))
                (if (failed? checked) checked
                  (begin
                    (global.set $pending-macros (list-push (global.get $pending-macros)
                      (binding name (sequence (list-push (list-push (list-new value)
                        (sequence params)) (list-get parts 3))))))
                    (nil)))))
            (else (failure "defmacro expects a parameter list"))))))))

; mode=1 substitutes macro parameters; mode=0 evaluates ordinary Lisp unquotes.
; Nesting tracks which commas belong to the current quasiquote.
(fn qq-unquote ((expr value) (env (list binding)) (depth s32) (mode s32)) value
  (if mode
    (value-case expr
      ((symbol name)
        (let (v (lookup-local name env (i32.sub (list-len env) 1)))
          (if (failed? v) expr v)))
      (else (qq-walk expr env depth 1 mode)))
    (eval expr env depth 0)))
(fn qq-walk ((expr value) (env (list binding)) (depth s32) (level s32) (mode s32)) value
  (if (i32.ge_s depth 128) (failure "quasiquote nesting limit")
    (let (name (form-head expr))
      (if (quotation-prefix? name)
        (let (parts (items-of expr))
          (if (i32.ne (list-len parts) 2) (failure "quotation prefix expects one expression")
            (let (inner (list-get parts 1))
              (if (string=? name "quasiquote")
                (prefixed name (qq-walk inner env (i32.add depth 1) (i32.add level 1) mode))
                (if (i32.eq level 1)
                  (if (string=? name "unquote-splice") (failure "unquote-splice requires a list position")
                    (qq-unquote inner env (i32.add depth 1) mode))
                  (prefixed name (qq-walk inner env (i32.add depth 1) (i32.sub level 1) mode)))))))
        (value-case expr
          ((sequence items) (qq-items items 0 env (i32.add depth 1) level mode (list-new value)))
          (else (if mode expr (quote-value expr))))))))
(fn qq-items ((items (list value)) (index s32) (env (list binding)) (depth s32) (level s32) (mode s32) (out (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence out)
    (let (item (list-get items index))
      (let (splice (i32.and (i32.eq level 1) (string=? (form-head item) "unquote-splice")))
        (let (v (if splice
                  (if (i32.ne (list-len (items-of item)) 2) (failure "quotation prefix expects one expression")
                    (qq-unquote (list-get (items-of item) 1) env depth mode))
                  (qq-walk item env depth level mode)))
          (if (failed? v) v
            (if splice
              (value-case v
                ((sequence entries) (qq-items items (i32.add index 1) env depth level mode (copy-list entries 0 out)))
                (else (failure "unquote-splice expects a Lisp list")))
              (qq-items items (i32.add index 1) env depth level mode (list-push out v)))))))))

(fn macro-template ((definition value) (args (list value))) value
  (let (parts (items-of definition))
    (let (params (items-of (list-get parts 0)))
      (if (i32.ne (list-len args) (i32.add (list-len params) 1)) (failure "wrong number of macro arguments")
        (let (template (list-get parts 1))
          (let (inner (if (string=? (form-head template) "quasiquote")
                         (if (i32.eq (list-len (items-of template)) 2) (list-get (items-of template) 1)
                           (failure "quasiquote expects one expression")) template))
            (if (failed? inner) inner
              (qq-walk inner (macro-bind params args 0 (list-new binding)) 0 1 1))))))))
(fn macro-bind ((params (list value)) (args (list value)) (index s32) (out (list binding))) (list binding)
  (if (i32.ge_s index (list-len params)) out
    (macro-bind params args (i32.add index 1)
      (list-push out (binding (symbol-name (list-get params index)) (list-get args (i32.add index 1)))))))

(fn expand-macro-form ((form value) (depth s32)) value
  (if (i32.ge_s depth 100) (failure "macro expansion nesting limit")
    (if (i32.ge_s (global.get $macro-steps) 10000) (failure "macro expansion step limit")
      (begin
        (global.set $macro-steps (i32.add (global.get $macro-steps) 1))
        (let (name (form-head form))
          (if (string=? name "quote") form
            (if (string=? name "quasiquote") (expand-quoted form depth 0)
              (if (string=? name "defmacro") (failure "defmacro is only supported as a top-level source directive")
                (let (definition (lookup-local name (global.get $pending-macros)
                                  (i32.sub (list-len (global.get $pending-macros)) 1)))
                  (if (failed? definition)
                    (value-case form
                      ((sequence items) (expand-macro-items items 0 (i32.add depth 1) (list-new value)))
                      (else form))
                    (let (expanded (macro-template definition (items-of form)))
                      (if (failed? expanded) expanded (expand-macro-form expanded (i32.add depth 1))))))))))))))
(fn expand-macro-items ((items (list value)) (index s32) (depth s32) (out (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence out)
    (let (v (expand-macro-form (list-get items index) depth))
      (if (failed? v) v
        (expand-macro-items items (i32.add index 1) depth (list-push out v))))))

; Quoted data is opaque to macros; only active commas contain executable code.
(fn expand-quoted ((form value) (depth s32) (level s32)) value
  (if (i32.ge_s depth 100) (failure "macro expansion nesting limit")
    (let (name (form-head form))
      (if (quotation-prefix? name)
        (let (parts (items-of form))
          (if (i32.ne (list-len parts) 2) (failure "quotation prefix expects one expression")
            (let (inner (list-get parts 1))
              (if (string=? name "quasiquote")
                (prefixed name (expand-quoted inner (i32.add depth 1) (i32.add level 1)))
                (if (i32.eq level 1) (prefixed name (expand-macro-form inner (i32.add depth 1)))
                  (prefixed name (expand-quoted inner (i32.add depth 1) (i32.sub level 1))))))))
        (value-case form
          ((sequence items) (expand-quoted-items items 0 depth level (list-new value)))
          (else form))))))
(fn expand-quoted-items ((items (list value)) (index s32) (depth s32) (level s32) (out (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence out)
    (let (v (expand-quoted (list-get items index) (i32.add depth 1) level))
      (if (failed? v) v
        (expand-quoted-items items (i32.add index 1) depth level (list-push out v))))))
(fn expand-macro-top ((forms (list value)) (index s32) (out (list value))) value
  (if (i32.ge_s index (list-len forms)) (sequence out)
    (let (form (list-get forms index))
      (if (string=? (form-head form) "defmacro") (expand-macro-top forms (i32.add index 1) out)
        (let (v (expand-macro-form form 0))
          (if (failed? v) v
            (expand-macro-top forms (i32.add index 1) (list-push out v))))))))
(fn prepare-macros ((forms (list value))) value
  (begin
    (global.set $macro-steps 0)
    (global.set $pending-macros (copy-env (global.get $session-macros) 0 (list-new binding)))
    (let (checked (collect-session-macros forms 0))
      (if (failed? checked) checked
        (let (expanded (expand-macro-top forms 0 (list-new value)))
          (if (failed? expanded) expanded
            (begin (global.set $session-macros (global.get $pending-macros)) expanded)))))))
