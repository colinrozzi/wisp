; Declarative macros. Captures are trees of syntax lists, indexed by ellipsis rank.
; Introduced identifiers carry fresh keys; substitutions preserve caller keys.
(global $syntax-id s32 mut 0)
(global $sr-vars (list value) mut 0)
(record sr-match (ok s32) (bindings (list binding)))

(fn sr-pair ((a value) (b value)) value
  (sequence (list-push (list-push (list-new value) a) b)))
(fn sr-symbol-in? ((name string) (items (list value)) (index s32)) s32
  (if (i32.ge_s index (list-len items)) 0
    (if (string=? name (symbol-name (list-get items index))) 1
      (sr-symbol-in? name items (i32.add index 1)))))
(fn sr-literal? ((name string) (literals (list value)) (macro-name string)) s32
  (i32.or (string=? name macro-name) (sr-symbol-in? name literals 0)))
(fn sr-rank ((name string) (vars (list value)) (index s32)) s32
  (if (i32.ge_s index (list-len vars)) -1
    (let (parts (items-of (list-get vars index)))
      (if (string=? name (symbol-name (list-get parts 0)))
        (value-case (list-get parts 1) ((integer n) n) (else -1))
        (sr-rank name vars (i32.add index 1))))))
(fn sr-repeated? ((items (list value)) (index s32)) s32
  (if (i32.lt_s (i32.add index 1) (list-len items))
    (string=? (symbol-name (list-get items (i32.add index 1))) "...") 0))

(fn sr-check-pattern ((pattern value) (literals (list value)) (name string) (rank s32)) value
  (if (symbol? pattern)
    (let (p (symbol-name pattern))
      (if (string=? p "...") (failure "misplaced pattern ellipsis")
        (if (i32.or (string=? p "_") (sr-literal? p literals name)) (nil)
          (if (i32.ge_s (sr-rank p (global.get $sr-vars) 0) 0) (failure "duplicate pattern variable")
            (begin
              (global.set $sr-vars (list-push (global.get $sr-vars) (sr-pair (symbol p) (integer rank))))
              (nil))))))
    (if (sequence? pattern) (sr-check-pattern-items (items-of pattern) 0 literals name rank 0) (nil))))
(fn sr-check-pattern-items ((items (list value)) (index s32) (literals (list value)) (name string) (rank s32) (seen s32)) value
  (if (i32.ge_s index (list-len items)) (nil)
    (let (repeated (sr-repeated? items index))
      (if (i32.and seen repeated) (failure "one ellipsis group is allowed per pattern list")
        (let (v (sr-check-pattern (list-get items index) literals name (i32.add rank repeated)))
          (if (failed? v) v
            (sr-check-pattern-items items (i32.add index (i32.add 1 repeated)) literals name rank (i32.or seen repeated))))))))
(fn sr-check-template ((template value) (rank s32)) value
  (if (symbol? template)
    (let (name (symbol-name template))
      (if (string=? name "...") (failure "misplaced template ellipsis")
        (if (i32.gt_s (sr-rank name (global.get $sr-vars) 0) rank)
          (failure "pattern variable requires more template ellipses") (nil))))
    (if (sequence? template) (sr-check-template-items (items-of template) 0 rank) (nil))))
(fn sr-check-template-items ((items (list value)) (index s32) (rank s32)) value
  (if (i32.ge_s index (list-len items)) (nil)
    (let (repeated (sr-repeated? items index))
      (let (checked (sr-check-template (list-get items index) (i32.add rank repeated)))
        (if (failed? checked) checked
          (if (if repeated (i32.eq (sr-has-driver? (list-get items index) (global.get $sr-vars) rank) 0) 0)
            (failure "template ellipsis needs a repeated pattern variable")
            (sr-check-template-items items (i32.add index (i32.add 1 repeated)) rank)))))))
(fn sr-has-driver? ((template value) (vars (list value)) (rank s32)) s32
  (if (symbol? template) (i32.gt_s (sr-rank (symbol-name template) vars 0) rank)
    (sr-driver-items? (items-of template) 0 vars rank)))
(fn sr-driver-items? ((items (list value)) (index s32) (vars (list value)) (rank s32)) s32
  (if (i32.ge_s index (list-len items)) 0
    (if (sr-has-driver? (list-get items index) vars rank) 1
      (sr-driver-items? items (i32.add index 1) vars rank))))

(fn sr-check-rules ((rules (list value)) (index s32) (literals (list value)) (name string) (out (list value))) value
  (if (i32.ge_s index (list-len rules)) (sequence out)
    (let (parts (items-of (list-get rules index)))
      (if (i32.ne (list-len parts) 2) (failure "syntax-rules expects (pattern template) rules")
        (let (pattern (list-get parts 0))
          (if (i32.or (sr-repeated? (items-of pattern) 0)
                (i32.eq (i32.or (string=? (form-head pattern) "_") (string=? (form-head pattern) name)) 0))
            (failure "rule pattern must start with _ or its macro name")
            (begin
              (global.set $sr-vars (list-new value))
              (let (checked (sr-check-pattern pattern literals name 0))
                (if (failed? checked) checked
                  (let (valid (sr-check-template (list-get parts 1) 0))
                    (if (failed? valid) valid
                      (sr-check-rules rules (i32.add index 1) literals name
                        (list-push out (sequence (list-push (list-push (list-push (list-new value)
                          pattern) (list-get parts 1)) (sequence (global.get $sr-vars)))))))))))))))))
(fn collect-syntax-rules ((parts (list value))) value
  (if (i32.ne (list-len parts) 3) (failure "define-syntax expects a name and syntax-rules")
    (let (name (symbol-name (list-get parts 1)))
      (let (body (items-of (list-get parts 2)))
        (if (i32.or (string=? name "") (macro-reserved? name)) (failure "invalid syntax-rules macro name")
          (if (i32.lt_s (list-len body) 3) (failure "syntax-rules expects literals and at least one rule")
            (if (i32.eq (string=? (symbol-name (list-get body 0)) "syntax-rules") 0)
              (failure "only syntax-rules transformers are supported")
              (if (i32.eq (sequence? (list-get body 1)) 0) (failure "syntax-rules expects a literal list")
                (let (literals (items-of (list-get body 1)))
                  (let (checked (check-params literals 0))
                    (if (failed? checked) checked
                      (if (i32.or (sr-symbol-in? "..." literals 0) (sr-symbol-in? "_" literals 0))
                        (failure "reserved syntax-rules literal")
                        (let (rules (sr-check-rules (copy-list body 2 (list-new value)) 0 literals name (list-new value)))
                          (if (failed? rules) rules
                            (begin
                              (global.set $pending-macros (list-push (global.get $pending-macros)
                                (binding name (sequence (list-push (list-push (list-push (list-new value)
                                  (symbol "syntax-rules")) (sequence literals)) rules)))))
                              (nil))))))))))))))))

; Pattern validation makes names unique, so merging captures needs no overwrite.
(fn sr-merge ((a sr-match) (b sr-match)) sr-match
  (if (i32.and (sr-match.ok a) (sr-match.ok b))
    (sr-match 1 (copy-env (sr-match.bindings b) 0 (copy-env (sr-match.bindings a) 0 (list-new binding))))
    (sr-match 0 (list-new binding))))
(fn sr-match-pattern ((pattern value) (input value) (literals (list value)) (name string)) sr-match
  (if (symbol? pattern)
    (let (p (symbol-name pattern))
      (if (string=? p "_") (sr-match 1 (list-new binding))
        (if (sr-literal? p literals name)
          (sr-match (i32.and (symbol? input) (string=? p (symbol-name input))) (list-new binding))
          (sr-match 1 (list-push (list-new binding) (binding p input))))))
    (if (sequence? pattern)
      (if (sequence? input)
        (sr-match-items (items-of pattern) (items-of input) 0 0 literals name)
        (sr-match 0 (list-new binding)))
      (sr-match (string=? (show pattern) (show input)) (list-new binding)))))
(fn sr-match-items ((patterns (list value)) (inputs (list value)) (pi s32) (ii s32) (literals (list value)) (name string)) sr-match
  (if (i32.ge_s pi (list-len patterns))
    (sr-match (i32.eq ii (list-len inputs)) (list-new binding))
    (if (sr-repeated? patterns pi)
      (let (count (i32.sub (i32.sub (list-len inputs) ii) (i32.sub (list-len patterns) (i32.add pi 2))))
        (if (i32.lt_s count 0) (sr-match 0 (list-new binding))
          (sr-merge
            (sr-match-repeat (list-get patterns pi) inputs ii (i32.add ii count) literals name
              (sr-empty (list-get patterns pi) literals name (list-new binding)))
            (sr-match-items patterns inputs (i32.add pi 2) (i32.add ii count) literals name))))
      (if (i32.ge_s ii (list-len inputs)) (sr-match 0 (list-new binding))
        (let (one (sr-match-pattern (list-get patterns pi) (list-get inputs ii) literals name))
          (if (sr-match.ok one)
            (sr-merge one (sr-match-items patterns inputs (i32.add pi 1) (i32.add ii 1) literals name))
            one))))))
(fn sr-empty ((pattern value) (literals (list value)) (name string) (out (list binding))) (list binding)
  (if (symbol? pattern)
    (let (p (symbol-name pattern))
      (if (i32.or (i32.or (string=? p "_") (string=? p "...")) (sr-literal? p literals name)) out
        (list-push out (binding p (nil)))))
    (sr-empty-items (items-of pattern) 0 literals name out)))
(fn sr-empty-items ((items (list value)) (index s32) (literals (list value)) (name string) (out (list binding))) (list binding)
  (if (i32.ge_s index (list-len items)) out
    (sr-empty-items items (i32.add index 1) literals name (sr-empty (list-get items index) literals name out))))
(fn sr-accumulate ((out (list binding)) (one (list binding)) (index s32) (next (list binding))) (list binding)
  (if (i32.ge_s index (list-len out)) next
    (let (entry (list-get out index))
      (sr-accumulate out one (i32.add index 1)
        (list-push next (binding (binding.name entry)
          (sequence (list-push (items-of (binding.item entry))
            (lookup-local (binding.name entry) one (i32.sub (list-len one) 1))))))))))
(fn sr-match-repeat ((pattern value) (inputs (list value)) (index s32) (end s32) (literals (list value)) (name string) (out (list binding))) sr-match
  (if (i32.ge_s index end) (sr-match 1 out)
    (let (one (sr-match-pattern pattern (list-get inputs index) literals name))
      (if (sr-match.ok one)
        (sr-match-repeat pattern inputs (i32.add index 1) end literals name
          (sr-accumulate out (sr-match.bindings one) 0 (list-new binding)))
        one))))

(fn sr-select ((capture value) (path (list value)) (index s32) (rank s32)) value
  (if (i32.ge_s index rank) capture
    (value-case (list-get path index)
      ((integer n)
        (let (items (items-of capture))
          (if (i32.ge_s n (list-len items)) (failure "inconsistent ellipsis lengths")
            (sr-select (list-get items n) path (i32.add index 1) rank))))
      (else (failure "invalid ellipsis index")))))
(fn sr-count ((template value) (vars (list value)) (captures (list binding)) (path (list value))) s32
  (if (symbol? template)
    (let (name (symbol-name template))
      (if (i32.gt_s (sr-rank name vars 0) (list-len path))
        (let (v (sr-select (lookup-local name captures (i32.sub (list-len captures) 1)) path 0 (list-len path)))
          (if (failed? v) -2 (list-len (items-of v)))) -1))
    (sr-count-items (items-of template) 0 vars captures path -1)))
(fn sr-count-items ((items (list value)) (index s32) (vars (list value)) (captures (list binding)) (path (list value)) (count s32)) s32
  (if (i32.ge_s index (list-len items)) count
    (let (n (sr-count (list-get items index) vars captures path))
      (if (i32.or (i32.eq n -2) (if (i32.and (i32.ge_s n 0) (i32.ge_s count 0)) (i32.ne n count) 0)) -2
        (sr-count-items items (i32.add index 1) vars captures path (if (i32.ge_s n 0) n count))))))
(fn sr-introduce ((name string) (scope string)) value
  (identifier name (string-append scope name)))
(fn sr-template ((template value) (vars (list value)) (captures (list binding)) (path (list value)) (scope string)) value
  (if (i32.ge_s (global.get $macro-steps) 10000) (failure "macro expansion step limit")
    (begin
      (global.set $macro-steps (i32.add (global.get $macro-steps) 1))
      (if (symbol? template)
        (let (name (symbol-name template))
          (let (rank (sr-rank name vars 0))
            (if (i32.lt_s rank 0) (sr-introduce name scope)
              (if (i32.gt_s rank (list-len path)) (failure "pattern variable requires more template ellipses")
                (sr-select (lookup-local name captures (i32.sub (list-len captures) 1)) path 0 rank)))))
        (if (sequence? template)
          (sr-template-items (items-of template) 0 vars captures path scope (list-new value)) template)))))
(fn sr-template-items ((items (list value)) (index s32) (vars (list value)) (captures (list binding)) (path (list value)) (scope string) (out (list value))) value
  (if (i32.ge_s index (list-len items)) (sequence out)
    (let (repeated (sr-repeated? items index))
      (let (v (if repeated
                (let (count (sr-count (list-get items index) vars captures path))
                  (if (i32.lt_s count 0) (failure "inconsistent ellipsis lengths or missing repeated variable")
                    (sr-template-repeat (list-get items index) vars captures path scope 0 count (list-new value))))
                (sr-template (list-get items index) vars captures path scope)))
        (if (failed? v) v
          (sr-template-items items (i32.add index (i32.add 1 repeated)) vars captures path scope
            (if repeated (copy-list (items-of v) 0 out) (list-push out v))))))))
(fn sr-template-repeat ((template value) (vars (list value)) (captures (list binding)) (path (list value)) (scope string) (index s32) (count s32) (out (list value))) value
  (if (i32.ge_s index count) (sequence out)
    (let (v (sr-template template vars captures (list-push (copy-list path 0 (list-new value)) (integer index)) scope))
      (if (failed? v) v
        (sr-template-repeat template vars captures path scope (i32.add index 1) count (list-push out v))))))
(fn expand-syntax-rules ((definition value) (input value)) value
  (let (parts (items-of definition))
    (begin
      (global.set $syntax-id (i32.add (global.get $syntax-id) 1))
      (sr-try-rules (items-of (list-get parts 2)) 0 (items-of (list-get parts 1)) (form-head input) input
        (string-append " syntax:" (string-append (show (integer (global.get $syntax-id))) ":"))))))
(fn sr-try-rules ((rules (list value)) (index s32) (literals (list value)) (name string) (input value) (scope string)) value
  (if (i32.ge_s index (list-len rules)) (failure (string-append "no matching syntax rule: " name))
    (let (parts (items-of (list-get rules index)))
      (let (matched (sr-match-pattern (list-get parts 0) input literals name))
        (if (sr-match.ok matched)
          (sr-template (list-get parts 1) (items-of (list-get parts 2)) (sr-match.bindings matched) (list-new value) scope)
          (sr-try-rules rules (i32.add index 1) literals name input scope))))))
