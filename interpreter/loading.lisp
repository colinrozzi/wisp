; File I/O belongs to the host. Reading, include expansion, and evaluation remain
; in Wisp. Resolve first so duplicate/cyclic paths do not read the file again.
(import wisp-source resolve-path ((base string) (path string)) (result string string))
(import wisp-source read-source ((path string)) (result string string))
(global $include-seen (list string) mut 0)

(fn included? ((path string) (index s32)) s32
  (if (i32.ge_s index (list-len (global.get $include-seen))) 0
    (if (string=? path (list-get (global.get $include-seen) index)) 1
      (included? path (i32.add index 1)))))
(fn source-failure ((v value) (path string)) value
  (value-case v
    ((failure message) (failure (string-append path (string-append ": " message))))
    (else v)))
(fn expand-file ((path string) (depth s32)) value
  (if (included? path 0) (sequence (list-new value))
    (if (i32.ge_s depth 64) (failure "include nesting limit")
      (match (read-source path)
        ((err message) (failure message))
        ((ok source)
          (begin
            (global.set $include-seen (list-push (global.get $include-seen) path))
            (source-failure (expand-source source path (i32.add depth 1)) path)))))))
(fn expand-include ((parts (list value)) (base string) (depth s32)) value
  (if (i32.ne (list-len parts) 2) (failure "include expects a string path")
    (value-case (list-get parts 1)
      ((text path)
        (match (resolve-path base path)
          ((err message) (failure message))
          ((ok canonical) (expand-file canonical depth))))
      (else (failure "include expects a string path")))))
(fn expand-forms ((forms (list value)) (index s32) (base string) (depth s32) (out (list value))) value
  (if (i32.ge_s index (list-len forms)) (sequence out)
    (let (form (list-get forms index))
      (let (parts (items-of form))
        (if (if (list-len parts) (string=? (symbol-name (list-get parts 0)) "include") 0)
          (let (expanded (expand-include parts base depth))
            (if (failed? expanded) expanded
              (expand-forms forms (i32.add index 1) base depth (copy-list (items-of expanded) 0 out))))
          (expand-forms forms (i32.add index 1) base depth (list-push out form)))))))
(fn expand-source ((source string) (base string) (depth s32)) value
  (let (forms (read-forms source 0 (list-new value)))
    (if (failed? forms) forms (expand-forms (items-of forms) 0 base depth (list-new value)))))
