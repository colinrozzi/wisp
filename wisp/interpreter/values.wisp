; Syntax and runtime data share values. Named aggregates carry a session-local
; nominal type ID; constructors/accessors preserve it across calls. Only syntax
; uses integer-literal; evaluation/quotation resolves it to an actual integer.
(variant value
  (integer s32)
  (byte-value s32)
  (wide-integer s64)
  (integer-literal s64)
  (single f32)
  (double f64)
  (typed-list value (list value))
  ; (list u8) packed: one buffer ([len][data], a string's layout) instead of a
  ; boxed byte-value per byte. Programs see a (list u8) either way; it is what
  ; the host's byte arrays decode to and string->bytes gives (0157).
  (bytes string)
  (compound value string (list value))
  (text string)
  (symbol string)
  (identifier string string)
  (sequence (list value))
  (closure (list value) value (list binding) string)
  (builtin string)
  (failure string)
  (typed-function (list value) value value)
  (generic-function (list value) value value (list value) (list value))
  (trait-method string string)
  (checked-function value (list value) value)
  (aggregate string s32 string (list value))
  (constructor string s32 string (list value))
  (field-reader string s32 s32)
  (boolean s32)
  (u64-value s64)
  (u32-value s32)
  (u16-value s32)
  (open-variant string string s32 (list value))
  (vector-value (list value))
  (map-value hash-table))

(record binding (name string) (item value))
; A hash map's state (containers.wisp). Its lists change in place.
(record hash-table (keys (list value)) (vals (list value)) (hashes (list s32)) (slots (list s32)) (counts (list s32)))
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
        ((byte-value ignored-byte) fallback)
        ((wide-integer ignored-wide) fallback)
        ((integer-literal ignored-literal) fallback)
        ((single ignored-single) fallback)
        ((double ignored-double) fallback)
        ((typed-list ignored-element ignored-list) fallback)
        ((bytes ignored-bytes) fallback)
        ((compound ignored-compound-type ignored-tag ignored-payload) fallback)
        ((text ignored-s) fallback)
        ((symbol ignored-name) fallback)
        ((identifier ignored-name ignored-key) fallback)
        ((sequence ignored-items) fallback)
        ((closure ignored-params ignored-body ignored-env ignored-home) fallback)
        ((builtin ignored-builtin) fallback)
        ((failure ignored-message) fallback)
        ((typed-function ignored-params ignored-result ignored-body) fallback)
        ((generic-function ignored-params ignored-result ignored-body ignored-vars ignored-constraints) fallback)
        ((trait-method ignored-trait ignored-method) fallback)
        ((checked-function ignored-target ignored-params ignored-result) fallback)
        ((aggregate ignored-type ignored-id ignored-case ignored-fields) fallback)
        ((constructor ignored-type ignored-id ignored-case ignored-schema) fallback)
        ((field-reader ignored-type ignored-id ignored-index) fallback)
        ((boolean ignored-bool) fallback)
        ((u64-value ignored-u64) fallback)
        ((u32-value ignored-u32) fallback)
        ((u16-value ignored-u16) fallback)
        ((open-variant ignored-tn ignored-cn ignored-tag ignored-fields) fallback)
        ((vector-value ignored-vector) fallback)
        ((map-value ignored-map) fallback)))))

(fn failed? ((v value)) s32
  (value-case v ((failure message) 1) (else 0)))

(fn symbol-name ((v value)) string
  (value-case v ((symbol name) name) ((identifier name key) name) (else "")))

(fn items-of ((v value)) (list value)
  (value-case v ((sequence items) items) (else (list-new value))))

; Packed bytes (the `bytes` variant): its length, byte i, and its items as the
; (list u8) of byte-values a typed-list would hold, for code that walks one.
(fn bytes-len ((b string)) s32 (i32.load (string-addr b)))
(fn bytes-ref ((b string) (i s32)) value
  (byte-value (i32.load8_u (i32.add (i32.add (string-addr b) (i32.const 4)) i))))
(fn bytes-items-from ((b string) (i s32) (n s32) (acc (list value))) (list value)
  (if (i32.ge_s i n) acc (bytes-items-from b (i32.add i (i32.const 1)) n (list-push acc (bytes-ref b i)))))
(fn bytes-items ((b string)) (list value) (bytes-items-from b (i32.const 0) (bytes-len b) (list-new value)))
; Bytes FROM .. the end of b, as a new buffer (cdr's copy).
(fn copy-bytes-in ((dst s32) (src s32) (i s32) (n s32)) s32
  (if (i32.ge_s i n) dst
    (begin (i32.store8 (i32.add dst i) (i32.load8_u (i32.add src i)))
           (copy-bytes-in dst src (i32.add i (i32.const 1)) n))))
(fn bytes-from ((b string) (from s32)) string
  (let (n (i32.sub (bytes-len b) from))
    (let (buf (heap-alloc (i32.add n (i32.const 4))))
      (begin
        (i32.store buf n)
        (copy-bytes-in (i32.add buf (i32.const 4)) (i32.add (i32.add (string-addr b) (i32.const 4)) from) (i32.const 0) n)
        (let (s (string-from-addr buf)) (begin (heap-free buf) s))))))
; A list value's items whichever kind it is: a sequence's or typed-list's own,
; packed bytes unpacked; none for anything else.
(fn list-items-any ((v value)) (list value)
  (value-case v
    ((sequence items) items)
    ((typed-list element items) items)
    ((bytes b) (bytes-items b))
    (else (list-new value))))

(fn symbol? ((v value)) s32
  (value-case v ((symbol name) 1) ((identifier name key) 1) (else 0)))

(fn sequence? ((v value)) s32
  (value-case v ((sequence items) 1) (else 0)))

; Syntax-rules identifiers retain a private binding key until quotation/evaluation.
(fn binding-key ((v value)) string
  (value-case v ((symbol name) name) ((identifier name key) key) (else "")))
