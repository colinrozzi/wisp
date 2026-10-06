; Syntax and runtime data share values. Named aggregates carry a session-local
; nominal type ID; constructors/accessors preserve it across calls. Only syntax
; uses integer-literal; evaluation/quotation resolves it to an actual integer.
(variant value
  (integer s32)
  (wide-integer s64)
  (integer-literal s64)
  (single f32)
  (double f64)
  (text string)
  (symbol string)
  (sequence (list value))
  (closure (list value) value (list binding))
  (builtin string)
  (failure string)
  (typed-function (list value) value value)
  (aggregate string s32 string (list value))
  (constructor string s32 string (list value))
  (field-reader string s32 s32))

(record binding (name string) (item value))
(record read-result (item value) (next s32))
(record named-type (name string) (id s32) (variant? s32) (schema (list value)))

; Keep exhaustive fallback cases in one place. Earlier matching cases win;
; syntax-rules keeps fallback bindings from capturing names in the caller.
(define-syntax value-case
  (syntax-rules (else)
    ((_ expr arm ... (else fallback))
      (match expr
        arm ...
        ((integer ignored-n) fallback)
        ((wide-integer ignored-wide) fallback)
        ((integer-literal ignored-literal) fallback)
        ((single ignored-single) fallback)
        ((double ignored-double) fallback)
        ((text ignored-s) fallback)
        ((symbol ignored-name) fallback)
        ((sequence ignored-items) fallback)
        ((closure ignored-params ignored-body ignored-env) fallback)
        ((builtin ignored-builtin) fallback)
        ((failure ignored-message) fallback)
        ((typed-function ignored-params ignored-result ignored-body) fallback)
        ((aggregate ignored-type ignored-id ignored-case ignored-fields) fallback)
        ((constructor ignored-type ignored-id ignored-case ignored-schema) fallback)
        ((field-reader ignored-type ignored-id ignored-index) fallback)))))

(fn failed? ((v value)) s32
  (value-case v ((failure message) 1) (else 0)))

(fn symbol-name ((v value)) string
  (value-case v ((symbol name) name) (else "")))

(fn items-of ((v value)) (list value)
  (value-case v ((sequence items) items) (else (list-new value))))

(fn symbol? ((v value)) s32
  (value-case v ((symbol name) 1) (else 0)))

(fn sequence? ((v value)) s32
  (value-case v ((sequence items) 1) (else 0)))
