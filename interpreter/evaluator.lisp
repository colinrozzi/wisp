; A persistent Lisp interpreter written in Wisp. Compile this module once and
; call evaluate for each input. No code generation happens during evaluation.
(include "reader.lisp")
(include "printer.lisp")
(include "types.lisp")
(include "matching.lisp")
(include "primitives.lisp")
(include "integers.lisp")
(include "decimal.lisp")
(include "floats.lisp")
(include "collections.lisp")
(include "globals.lisp")
(include "loading.lisp")
(include "macros.lisp")

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
            (if (if (builtin? name) 1 (if (collection-builtin? name) 1 (if (string-primitive? name) 1 (numeric-primitive? name)))) (builtin name)
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
  (eval-expected expr env depth top ""))

; Only syntax literals adopt an expected type. Values retrieved from bindings
; keep their actual type, even when a caller expects a wider integer.
(fn eval-expected ((expr value) (env (list binding)) (depth s32) (top s32) (expected string)) value
  (if (i32.ge_s depth 128) (failure "evaluation nesting limit")
    (if (i32.ge_s (global.get $steps) 10000) (failure "evaluation step limit")
      (begin
        (global.set $steps (i32.add (global.get $steps) 1))
        (value-case expr
          ((integer-literal n) (resolve-integer n expected))
          ((symbol name) (lookup name env))
          ((sequence items)
            (if (i32.eq (list-len items) 0) expr
              (eval-form items env (i32.add depth 1) top expected)))
          (else expr))))))

(fn eval-form ((items (list value)) (env (list binding)) (depth s32) (top s32) (expected string)) value
  (if (if (i32.eq (list-len items) 3) (string=? (symbol-name (list-get items 1)) ":") 0)
    (eval-numeric-ascription items env depth)
    (if (quotation-prefix? (symbol-name (list-get items 0)))
      (if (string=? (symbol-name (list-get items 0)) "quasiquote")
        (if (i32.eq (list-len items) 2)
          (qq-walk (list-get items 1) env depth 1 0)
          (failure "quasiquote expects one expression"))
        (failure "unquote is only supported inside quasiquote"))
      (eval-named-form items env depth top expected))))

(fn eval-named-form ((items (list value)) (env (list binding)) (depth s32) (top s32) (expected string)) value
  (value-case (list-get items 0)
    ((symbol name)
      (if (string=? name "quote")
        (if (i32.eq (list-len items) 2) (quote-value (list-get items 1)) (failure "quote expects one argument"))
        (if (string=? name "if") (eval-if items env depth expected)
          (if (string=? name "define") (eval-define items env depth top)
            (if (string=? name "lambda") (eval-lambda items env)
              (if (string=? name "let") (eval-let items env depth expected)
                (if (string=? name "begin") (eval-body items 1 env depth top (nil))
                  (eval-declaration-or-call name items env depth top))))))))
    (else (eval-call items env depth))))

(fn truthy? ((v value)) s32
  (value-case v
    ((integer n) (i32.ne n 0))
    ((wide-integer n) (i64.ne n 0))
    ((single n) (f32.ne n 0f32))
    ((double n) (f64.ne n 0.0))
    ((sequence items) (i32.ne (list-len items) 0))
    (else 1)))

(fn eval-declaration-or-call ((name string) (items (list value)) (env (list binding)) (depth s32) (top s32)) value
  (if (global-form? name)
    (if (string=? name "global") (declare-global items top) (eval-global-access name items env depth))
    (if (collection-form? name) (eval-collection-form name items env depth)
      (eval-declaration name items env depth top))))

(fn eval-declaration ((name string) (items (list value)) (env (list binding)) (depth s32) (top s32)) value
  (if (string=? name "include") (failure "include is only supported as a top-level source directive")
    (eval-named-declaration name items env depth top)))

(fn eval-named-declaration ((name string) (items (list value)) (env (list binding)) (depth s32) (top s32)) value
  (if (string=? name "fn") (eval-fn items top)
    (if (string=? name "record") (eval-record items top)
      (if (string=? name "variant") (eval-variant items top)
        (if (string=? name "match") (eval-match items env depth)
          (if (string=? name "export") (eval-export items env depth top)
            (eval-call items env depth)))))))

(fn eval-if ((items (list value)) (env (list binding)) (depth s32) (expected string)) value
  (if (i32.ne (list-len items) 4) (failure "if expects condition, then, else")
    (let (condition (eval (list-get items 1) env depth 0))
      (if (failed? condition) condition
        (eval-expected (list-get items (if (truthy? condition) 2 3)) env depth 0 expected)))))

(fn eval-define ((items (list value)) (env (list binding)) (depth s32) (top s32)) value
  (if (i32.eq top 0) (failure "define is only supported at top level")
    (if (i32.ne (list-len items) 3) (failure "define expects name and expression")
      (value-case (list-get items 1)
        ((symbol name)
          (let (v (eval (list-get items 2) env depth 0))
            (if (failed? v) v
              (begin
                (global.set $bindings (list-push (global.get $bindings) (binding name v)))
                v))))
        (else (failure "define expects a symbol"))))))

(fn has-name? ((params (list value)) (end s32) (name string)) s32
  (if (i32.lt_s end 0) 0
    (value-case (list-get params end)
      ((symbol previous)
        (if (string=? name previous) 1 (has-name? params (i32.sub end 1) name)))
      (else 0))))

(fn check-params ((params (list value)) (index s32)) value
  (if (i32.ge_s index (list-len params)) (nil)
    (value-case (list-get params index)
      ((symbol name)
        (if (has-name? params (i32.sub index 1) name) (failure "duplicate parameter")
          (check-params params (i32.add index 1))))
      (else (failure "lambda parameters must be symbols")))))

(fn eval-lambda ((items (list value)) (env (list binding))) value
  (if (i32.ne (list-len items) 3) (failure "lambda expects parameters and body")
    (value-case (list-get items 1)
      ((sequence params)
        (let (checked (check-params params 0))
          (if (failed? checked) checked (closure params (list-get items 2) env))))
      (else (failure "lambda expects a parameter list")))))

; Support both (let (name value) body) and (let (name : type value) body).
(fn eval-let ((items (list value)) (env (list binding)) (depth s32) (expected string)) value
  (if (i32.ne (list-len items) 3) (failure "let expects a binding and body")
    (value-case (list-get items 1)
      ((sequence pair) (eval-let-binding pair (list-get items 2) env depth expected))
      (else (failure "let expects (name value)")))))

(fn eval-let-binding ((pair (list value)) (body value) (env (list binding)) (depth s32) (expected string)) value
  (if (i32.or (i32.eq (list-len pair) 2)
        (if (i32.eq (list-len pair) 4)
          (i32.and (string=? (symbol-name (list-get pair 1)) ":") (known-type? (list-get pair 2) "")) 0))
    (value-case (list-get pair 0)
      ((symbol name)
        (let (raw (eval-expected (list-get pair (i32.sub (list-len pair) 1)) env depth 0
                    (if (i32.eq (list-len pair) 4) (symbol-name (list-get pair 2)) "")))
          (let (v (if (i32.eq (list-len pair) 4) (require-type raw (list-get pair 2)) raw))
            (if (failed? v) v (eval-expected body (extend-env env (binding name v)) depth 0 expected)))))
      (else (failure "let expects a symbol")))
    (failure "invalid let binding or unsupported type")))

(fn eval-body ((items (list value)) (index s32) (env (list binding)) (depth s32) (top s32) (last value)) value
  (if (i32.ge_s index (list-len items)) last
    (let (v (eval (list-get items index) env depth top))
      (if (failed? v) v
        (eval-body items (i32.add index 1) env depth top v)))))

(fn eval-args ((items (list value)) (index s32) (env (list binding)) (depth s32) (args (list value)) (callee value)) value
  (if (i32.ge_s index (list-len items)) (sequence args)
    (let (v (eval-expected (list-get items index) env depth 0 (call-argument-type callee args (i32.sub index 1))))
      (if (failed? v) v
        (eval-args items (i32.add index 1) env depth (list-push args v) callee)))))

(fn bind-args ((params (list value)) (args (list value)) (index s32) (env (list binding))) (list binding)
  (if (i32.ge_s index (list-len params)) env
    (value-case (list-get params index)
      ((symbol name)
        (bind-args params args (i32.add index 1) (list-push env (binding name (list-get args index)))))
      (else env))))

(fn eval-call ((items (list value)) (env (list binding)) (depth s32)) value
  (let (callee (eval (list-get items 0) env depth 0))
    (if (failed? callee) callee
      (let (arguments (eval-args items 1 env depth (list-new value) callee))
        (value-case arguments
          ((failure message) arguments)
          ((sequence args)
            (value-case callee
              ((closure params body captured)
                (if (i32.ne (list-len params) (list-len args)) (failure "wrong number of arguments")
                  (eval body (bind-args params args 0 (copy-env captured 0 (list-new binding))) depth 0)))
              ((builtin name)
                (if (collection-builtin? name) (apply-collection name args)
                  (if (numeric-primitive? name) (apply-numeric-primitive name args)
                    (if (string-primitive? name) (apply-string name args) (apply-builtin name args)))))
              ((typed-function params result body) (apply-typed params result body args depth))
              ((constructor name id case-name types) (apply-constructor name id case-name types args))
              ((field-reader name id index) (apply-field name id index args))
              (else (failure "value is not callable"))))
          (else (failure "invalid arguments")))))))

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
        (value-case (list-get args 0)
          ((sequence items)
            (if (i32.eq (list-len items) 0) (failure "car/cdr expects a nonempty list")
              (if (string=? name "car") (list-get items 0)
                (sequence (copy-list items 1 (list-new value))))))
          (else (failure "car/cdr expects a list"))))
      (if (i32.ne (list-len args) 2) (failure "expected two arguments")
        (if (string=? name "cons")
          (value-case (list-get args 1)
            ((sequence items) (sequence (copy-list items 0 (list-push (list-new value) (list-get args 0)))))
            (else (failure "cons expects a list as its second argument")))
          (apply-numeric-builtin name (list-get args 0) (list-get args 1)))))))

; A minimal local REPL boundary: source in, printed value (or diagnostic) out.
; Actual values and closures stay in the session heap. A Theater adapter can
; route the same text through actor I/O later, without changing the evaluator.
(export (fn evaluate ((source string)) string
  (evaluate-from source "")))

; base is the canonical source file, or empty for interactive input. Each input
; gets its own include-once set, matching one compiler expansion graph.
(export (fn evaluate-from ((source string) (base string)) string
  (begin
    (if (global.get $started) 0
      (begin
        (global.set $bindings (list-new binding))
        (global.set $types (list-new named-type))
        (global.set $session-globals (list-new global-binding))
        (global.set $session-macros (list-new binding))
        (global.set $started 1) 0))
    (global.set $steps 0)
    (global.set $include-seen (list-new string))
    (if (string-len base) (begin (global.set $include-seen (list-push (global.get $include-seen) base)) 0) 0)
    (if (i32.gt_s (string-len source) (if (string-len base) 65536 4096)) "error: source exceeds input limit"
      (let (forms (expand-source source base 0))
        (value-case forms
          ((sequence items)
            (let (expanded (prepare-macros items))
              (value-case expanded
                ((sequence ready) (show (eval-body ready 0 (list-new binding) 0 1 (nil))))
                (else (show expanded)))))
          (else (show forms))))))))
