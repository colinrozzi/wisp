; A minimal Theater actor that is not a Wisp interpreter: it has the lifecycle
; init but no theater:simple/wisp.evaluate export. Rebuild with
;   cargo run -- compile wisp/actor/legacy-host/tests/fixtures/not-wisp.wisp \
;     wisp/actor/legacy-host/tests/fixtures/not-wisp
(export "theater:simple/actor.init"
  (fn actor-init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
    (ok (tuple (option (list u8))) string (tuple (none (list u8))))))
