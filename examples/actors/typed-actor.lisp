; typed-actor.lisp — Theater actor using Wisp compound types
;
; This actor uses high-level types instead of hand-crafted CGRF buffers.
; The compiler generates all CGRF encoding/decoding automatically.
;
; Signature: init(state: option<list<u8>>) -> result<tuple<option<list<u8>>>, string>

(import theater:simple/runtime log ((msg string)) s32)

(export "theater:simple/actor.init"
  (fn init ((state (option (list u8)))) (result (tuple (option (list u8))) string)
    (let (_ (log "Typed actor initialized!"))
      (ok (tuple (option (list u8))) string
          (tuple state)))))
