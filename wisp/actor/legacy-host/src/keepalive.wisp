; The daemon's keepalive actor (see `Runtime::new` in lib.rs). Theater exits
; its runtime when the last actor is finalized, so without it, stopping the
; last session (or a lone boot-list service dying) would end the runtime under
; a daemon that is still serving. It has the lifecycle init but no
; theater:simple/wisp.evaluate export, so it is not a session. Rebuild with
;   cargo run -- compile wisp/actor/legacy-host/src/keepalive.wisp \
;     wisp/actor/legacy-host/src/keepalive
(export "theater:simple/actor.init"
  (fn actor-init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
    (ok (tuple (option (list u8))) string (tuple (none (list u8))))))
