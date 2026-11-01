; std/result — ergonomic helpers over Wisp's native option and result values.
;
; LIMITATION: Wisp's (some TY v)/(none TY)/(ok OK ERR v)/(err OK ERR v) carry
; STATIC type tags (inherited from Granite) and enforce them. So combinators that
; REBUILD a some/ok (map, and_then) can't be written generically — they'd need the
; concrete payload type. These helpers only *extract* (match, never reconstruct),
; which needs no type and composes fine in the dynamic language.
(module result
  (provide is-some is-none opt-or is-ok is-err res-or res-or-else err-or)

  ; --- option ---
  (define is-some (lambda (o) (match o ((some x) 1) ((none) 0))))
  (define is-none (lambda (o) (match o ((some x) 0) ((none) 1))))
  (define opt-or  (lambda (o default) (match o ((some x) x) ((none) default))))

  ; --- result ---
  (define is-ok  (lambda (r) (match r ((ok v) 1) ((err e) 0))))
  (define is-err (lambda (r) (match r ((ok v) 0) ((err e) 1))))
  (define res-or (lambda (r default) (match r ((ok v) v) ((err e) default))))
  ; run (f e) to produce a fallback from the error
  (define res-or-else (lambda (r f) (match r ((ok v) v) ((err e) (f e)))))
  ; the error value, or `default` if ok
  (define err-or (lambda (r default) (match r ((ok v) default) ((err e) e)))))
