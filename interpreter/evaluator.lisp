; A persistent Lisp interpreter written in Wisp. Compile this module once and
; call evaluate for each input. No code generation happens during evaluation.
(include "reader.lisp")
(include "printer.lisp")

(global $started s32 mut 0)
(global $bindings (list binding) mut 0)
(global $steps s32 mut 0)

(fn nil () value (sequence (list-new value)))

(fn lookup-local ((name string) (env (list binding)) (index s32)) value
  (if (i32.lt_s index 0) (failure (string-append "unbound symbol: " name))
    (let (entry (list-get env index))
      (if (string=? name (binding.name entry)) (binding.item entry)
        (lookup-local name env (i32.sub index 1))))))

(fn builtin? ((name string)) s32
  (i32.or
    (i32.or (i32.or (string=? name "+") (string=? name "-"))
      (i32.or (string=? name "*") (string=? name "/")))
    (i32.or (i32.or (string=? name "=") (string=? name "<"))
      (i32.or (string=? name "list")
        (i32.or (string=? name "car") (i32.or (string=? name "cdr") (string=? name "cons")))))))

(fn lookup ((name string) (env (list binding))) value
  (let (local (lookup-local name env (i32.sub (list-len env) 1)))
    (if (failed? local)
      (let (root (global.get $bindings))
        (let (found (lookup-local name root (i32.sub (list-len root) 1)))
          (if (failed? found)
            (if (builtin? name) (builtin name)
              (if (string=? name "nil") (nil) found))
            found)))
      local)))

; Wisp list-push mutates its list. Copy before extending a lexical environment
; so sibling calls and nested lets cannot change an existing closure's bindings.
(fn copy-env ((env (list binding)) (index s32) (out (list binding))) (list binding)
  (if (i32.ge_s index (list-len env)) out
    (copy-env env (i32.add index 1) (list-push out (list-get env index)))))

(fn extend-env ((env (list binding)) (entry binding)) (list binding)
  (list-push (copy-env env 0 (list-new binding)) entry))

; Locals are captured by value in immutable environment lists. Top-level names
; are looked up in the current session globals, supporting recursion/redefinition.
(fn eval ((expr value) (env (list binding)) (depth s32) (top s32)) value
  (if (i32.ge_s depth 128) (failure "evaluation nesting limit")
    (if (i32.ge_s (global.get $steps) 10000) (failure "evaluation step limit")
      (begin
        (global.set $steps (i32.add (global.get $steps) 1))
        (match expr
          ((symbol name) (lookup name env))
          ((sequence items)
            (if (i32.eq (list-len items) 0) expr
              (eval-form items env (i32.add depth 1) top)))
          ((integer ignored-n) expr)
          ((text ignored-s) expr)
          ((closure ignored-params ignored-body ignored-env) expr)
          ((builtin ignored-builtin) expr)
          ((failure ignored-message) expr))))))

(fn eval-form ((items (list value)) (env (list binding)) (depth s32) (top s32)) value
  (match (list-get items 0)
    ((symbol name)
      (if (string=? name "quote")
        (if (i32.eq (list-len items) 2) (list-get items 1) (failure "quote expects one argument"))
        (if (string=? name "if") (eval-if items env depth)
          (if (string=? name "define") (eval-define items env depth top)
            (if (string=? name "lambda") (eval-lambda items env)
              (if (string=? name "let") (eval-let items env depth)
                (if (string=? name "begin") (eval-body items 1 env depth top (nil))
                  (eval-call items env depth))))))))
    ((integer ignored-n) (eval-call items env depth))
    ((text ignored-s) (eval-call items env depth))
    ((sequence ignored-items) (eval-call items env depth))
    ((closure ignored-params ignored-body ignored-env) (eval-call items env depth))
    ((builtin ignored-builtin) (eval-call items env depth))
    ((failure ignored-message) (eval-call items env depth))))

(fn truthy? ((v value)) s32
  (match v
    ((integer n) (i32.ne n 0))
    ((sequence items) (i32.ne (list-len items) 0))
    ((text ignored-s) 1)
    ((symbol ignored-name) 1)
    ((closure ignored-params ignored-body ignored-env) 1)
    ((builtin ignored-builtin) 1)
    ((failure ignored-message) 1)))

(fn eval-if ((items (list value)) (env (list binding)) (depth s32)) value
  (if (i32.ne (list-len items) 4) (failure "if expects condition, then, else")
    (let (condition (eval (list-get items 1) env depth 0))
      (if (failed? condition) condition
        (eval (list-get items (if (truthy? condition) 2 3)) env depth 0)))))

(fn eval-define ((items (list value)) (env (list binding)) (depth s32) (top s32)) value
  (if (i32.eq top 0) (failure "define is only supported at top level")
    (if (i32.ne (list-len items) 3) (failure "define expects name and expression")
      (match (list-get items 1)
        ((symbol name)
          (let (v (eval (list-get items 2) env depth 0))
            (if (failed? v) v
              (begin
                (global.set $bindings (list-push (global.get $bindings) (binding name v)))
                v))))
        ((integer ignored-n) (failure "define expects a symbol"))
        ((text ignored-s) (failure "define expects a symbol"))
        ((sequence ignored-items) (failure "define expects a symbol"))
        ((closure ignored-params ignored-body ignored-env) (failure "define expects a symbol"))
        ((builtin ignored-builtin) (failure "define expects a symbol"))
        ((failure ignored-message) (failure "define expects a symbol"))))))

(fn has-name? ((params (list value)) (end s32) (name string)) s32
  (if (i32.lt_s end 0) 0
    (match (list-get params end)
      ((symbol previous)
        (if (string=? name previous) 1 (has-name? params (i32.sub end 1) name)))
      ((integer ignored-n) 0)
      ((text ignored-s) 0)
      ((sequence ignored-items) 0)
      ((closure ignored-params ignored-body ignored-env) 0)
      ((builtin ignored-builtin) 0)
      ((failure ignored-message) 0))))

(fn check-params ((params (list value)) (index s32)) value
  (if (i32.ge_s index (list-len params)) (nil)
    (match (list-get params index)
      ((symbol name)
        (if (has-name? params (i32.sub index 1) name) (failure "duplicate parameter")
          (check-params params (i32.add index 1))))
      ((integer ignored-n) (failure "lambda parameters must be symbols"))
      ((text ignored-s) (failure "lambda parameters must be symbols"))
      ((sequence ignored-items) (failure "lambda parameters must be symbols"))
      ((closure ignored-params ignored-body ignored-env) (failure "lambda parameters must be symbols"))
      ((builtin ignored-builtin) (failure "lambda parameters must be symbols"))
      ((failure ignored-message) (failure "lambda parameters must be symbols")))))

(fn eval-lambda ((items (list value)) (env (list binding))) value
  (if (i32.ne (list-len items) 3) (failure "lambda expects parameters and body")
    (match (list-get items 1)
      ((sequence params)
        (let (checked (check-params params 0))
          (if (failed? checked) checked (closure params (list-get items 2) env))))
      ((integer ignored-n) (failure "lambda expects a parameter list"))
      ((text ignored-s) (failure "lambda expects a parameter list"))
      ((symbol ignored-name) (failure "lambda expects a parameter list"))
      ((closure ignored-params ignored-body ignored-env) (failure "lambda expects a parameter list"))
      ((builtin ignored-builtin) (failure "lambda expects a parameter list"))
      ((failure ignored-message) (failure "lambda expects a parameter list")))))

; Use Wisp's existing single-binding let syntax: (let (name value) body).
(fn eval-let ((items (list value)) (env (list binding)) (depth s32)) value
  (if (i32.ne (list-len items) 3) (failure "let expects a binding and body")
    (match (list-get items 1)
      ((sequence pair)
        (if (i32.ne (list-len pair) 2) (failure "let expects (name value)")
          (match (list-get pair 0)
            ((symbol name)
              (let (v (eval (list-get pair 1) env depth 0))
                (if (failed? v) v
                  (eval (list-get items 2) (extend-env env (binding name v)) depth 0))))
            ((integer ignored-n) (failure "let expects a symbol"))
            ((text ignored-s) (failure "let expects a symbol"))
            ((sequence ignored-items) (failure "let expects a symbol"))
            ((closure ignored-params ignored-body ignored-env) (failure "let expects a symbol"))
            ((builtin ignored-builtin) (failure "let expects a symbol"))
            ((failure ignored-message) (failure "let expects a symbol")))))
      ((integer ignored-n) (failure "let expects (name value)"))
      ((text ignored-s) (failure "let expects (name value)"))
      ((symbol ignored-name) (failure "let expects (name value)"))
      ((closure ignored-params ignored-body ignored-env) (failure "let expects (name value)"))
      ((builtin ignored-builtin) (failure "let expects (name value)"))
      ((failure ignored-message) (failure "let expects (name value)")))))

(fn eval-body ((items (list value)) (index s32) (env (list binding)) (depth s32) (top s32) (last value)) value
  (if (i32.ge_s index (list-len items)) last
    (let (v (eval (list-get items index) env depth top))
      (if (failed? v) v
        (eval-body items (i32.add index 1) env depth top v)))))

(fn eval-args ((items (list value)) (index s32) (env (list binding)) (depth s32) (args (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence args)
    (let (v (eval (list-get items index) env depth 0))
      (if (failed? v) v
        (eval-args items (i32.add index 1) env depth (list-push args v))))))

(fn bind-args ((params (list value)) (args (list value)) (index s32) (env (list binding))) (list binding)
  (if (i32.ge_s index (list-len params)) env
    (match (list-get params index)
      ((symbol name)
        (bind-args params args (i32.add index 1) (list-push env (binding name (list-get args index)))))
      ((integer ignored-n) env)
      ((text ignored-s) env)
      ((sequence ignored-items) env)
      ((closure ignored-params ignored-body ignored-env) env)
      ((builtin ignored-builtin) env)
      ((failure ignored-message) env))))

(fn eval-call ((items (list value)) (env (list binding)) (depth s32)) value
  (let (callee (eval (list-get items 0) env depth 0))
    (if (failed? callee) callee
      (let (arguments (eval-args items 1 env depth (list-new value)))
        (match arguments
          ((failure message) arguments)
          ((sequence args)
            (match callee
              ((closure params body captured)
                (if (i32.ne (list-len params) (list-len args)) (failure "wrong number of arguments")
                  (eval body (bind-args params args 0 (copy-env captured 0 (list-new binding))) depth 0)))
              ((builtin name) (apply-builtin name args))
              ((integer ignored-n) (failure "value is not callable"))
              ((text ignored-s) (failure "value is not callable"))
              ((symbol ignored-name) (failure "value is not callable"))
              ((sequence ignored-items) (failure "value is not callable"))
              ((failure ignored-message) (failure "value is not callable"))))
          ((integer ignored-n) (failure "invalid arguments"))
          ((text ignored-s) (failure "invalid arguments"))
          ((symbol ignored-name) (failure "invalid arguments"))
          ((closure ignored-params ignored-body ignored-env) (failure "invalid arguments"))
          ((builtin ignored-builtin) (failure "invalid arguments")))))))

(fn numeric-op ((name string) (a s32) (b s32)) value
  (if (string=? name "+") (integer (i32.add a b))
    (if (string=? name "-") (integer (i32.sub a b))
      (if (string=? name "*") (integer (i32.mul a b))
        (if (string=? name "=") (integer (i32.eq a b))
          (if (string=? name "<") (integer (i32.lt_s a b))
            (if (i32.eq b 0) (failure "division by zero")
              (if (i32.and (i32.eq a -2147483648) (i32.eq b -1))
                (failure "division overflow")
                (integer (i32.div_s a b))))))))))

(fn copy-list ((items (list value)) (index s32) (out (list value))) (list value)
  (if (i32.ge_s index (list-len items)) out
    (copy-list items (i32.add index 1) (list-push out (list-get items index)))))

(fn apply-builtin ((name string) (args (list value))) value
  (if (string=? name "list") (sequence args)
    (if (i32.or (string=? name "car") (string=? name "cdr"))
      (if (i32.ne (list-len args) 1) (failure "car/cdr expects one argument")
        (match (list-get args 0)
          ((sequence items)
            (if (i32.eq (list-len items) 0) (failure "car/cdr expects a nonempty list")
              (if (string=? name "car") (list-get items 0)
                (sequence (copy-list items 1 (list-new value))))))
          ((integer ignored-n) (failure "car/cdr expects a list"))
          ((text ignored-s) (failure "car/cdr expects a list"))
          ((symbol ignored-name) (failure "car/cdr expects a list"))
          ((closure ignored-params ignored-body ignored-env) (failure "car/cdr expects a list"))
          ((builtin ignored-builtin) (failure "car/cdr expects a list"))
          ((failure ignored-message) (failure "car/cdr expects a list"))))
      (if (i32.ne (list-len args) 2) (failure "expected two arguments")
        (if (string=? name "cons")
          (match (list-get args 1)
            ((sequence items) (sequence (copy-list items 0 (list-push (list-new value) (list-get args 0)))))
            ((integer ignored-n) (failure "cons expects a list as its second argument"))
            ((text ignored-s) (failure "cons expects a list as its second argument"))
            ((symbol ignored-name) (failure "cons expects a list as its second argument"))
            ((closure ignored-params ignored-body ignored-env) (failure "cons expects a list as its second argument"))
            ((builtin ignored-builtin) (failure "cons expects a list as its second argument"))
            ((failure ignored-message) (failure "cons expects a list as its second argument")))
          (match (list-get args 0)
            ((integer a)
              (match (list-get args 1)
                ((integer b) (numeric-op name a b))
                ((text ignored-s) (failure "expected integer arguments"))
                ((symbol ignored-name) (failure "expected integer arguments"))
                ((sequence ignored-items) (failure "expected integer arguments"))
                ((closure ignored-params ignored-body ignored-env) (failure "expected integer arguments"))
                ((builtin ignored-builtin) (failure "expected integer arguments"))
                ((failure ignored-message) (failure "expected integer arguments"))))
            ((text ignored-s) (failure "expected integer arguments"))
            ((symbol ignored-name) (failure "expected integer arguments"))
            ((sequence ignored-items) (failure "expected integer arguments"))
            ((closure ignored-params ignored-body ignored-env) (failure "expected integer arguments"))
            ((builtin ignored-builtin) (failure "expected integer arguments"))
            ((failure ignored-message) (failure "expected integer arguments"))))))))

; A minimal local REPL boundary: source in, printed value (or diagnostic) out.
; Actual values and closures stay in the session heap. A Theater adapter can
; route the same text through actor I/O later, without changing the evaluator.
(export (fn evaluate ((source string)) string
  (begin
    (if (global.get $started) 0
      (begin (global.set $bindings (list-new binding)) (global.set $started 1) 0))
    (global.set $steps 0)
    (if (i32.gt_s (string-len source) 4096) "error: input exceeds 4096 bytes"
      (let (forms (read-forms source 0 (list-new value)))
        (match forms
          ((sequence items) (show (eval-body items 0 (list-new binding) 0 1 (nil))))
          ((integer ignored-n) (show forms))
          ((text ignored-s) (show forms))
          ((symbol ignored-name) (show forms))
          ((closure ignored-params ignored-body ignored-env) (show forms))
          ((builtin ignored-builtin) (show forms))
          ((failure ignored-message) (show forms))))))))
