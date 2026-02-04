; spawn-repl-actor.lisp — Theater actor that evaluates Wisp expressions by spawning child actors
;
; This implements the "minimal host, maximal WASM" architecture:
; - Compilation happens in WASM (via linked wisp-compiler)
; - Execution happens by spawning expression-actors
; - Results come back via handle-child-exit
;
; Imports:
;   theater:simple/runtime.log — logging
;   wisp:compiler/compiler.compile-source — WASM-to-WASM call to linked compiler
;   wisp:assembler/runtime.wat-to-wasm — host function to assemble WAT to WASM
;   wisp:repl/helpers.wrap-expression — host helper to wrap expression as actor source
;   wisp:repl/helpers.bytes-to-string — host helper to convert list<u8> to string
;   theater:simple/supervisor.spawn-with-wasm — spawn actor with inline WASM bytes
;
; Exports:
;   theater:simple/actor.init — actor initialization
;   theater:simple/message-server-client.handle-request — receives expressions
;   theater:simple/supervisor-handlers.handle-child-exit — receives results

(import theater:simple/runtime log ((msg string)) s32)

; Linked compiler (WASM-to-WASM call via Pack composition)
(import wisp:compiler/compiler compile-source ((src string)) string)

; Host functions
(import wisp:assembler/runtime wat-to-wasm ((wat string)) (result (list u8) string))
(import wisp:repl/helpers wrap-expression ((expr string)) string)
(import wisp:repl/helpers bytes-to-string ((bytes (list u8))) string)

; Theater supervisor (spawn with inline WASM)
(import theater:simple/supervisor spawn-with-wasm
  ((manifest string) (init-bytes (option (list u8))) (wasm-bytes (list u8)))
  (result string string))

; ============================================================
; Actor Exports
; ============================================================

; Initialize the REPL actor
(export "theater:simple/actor.init"
  (fn init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Spawn REPL actor initialized!")
      (ok (tuple (option (list u8))) string
          (tuple state)))))

; Handle fire-and-forget messages (ignored)
(export "theater:simple/message-server-client.handle-send"
  (fn handle-send ((state (option (list u8))) (params (tuple (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Spawn REPL actor received send (ignoring)")
      (ok (tuple (option (list u8))) string
          (tuple state)))))

; Handle request-response messages
; This is where we compile and spawn expression actors
(export "theater:simple/message-server-client.handle-request"
  (fn handle-request ((state (option (list u8))) (params (tuple string (list u8))))
    (result (tuple (option (list u8)) (tuple (option (list u8)))) string)
    (let (request-body (tuple-get params 1))
      (let (expr (bytes-to-string request-body))
        (begin
          (log (string-append "Evaluating: " expr))
          ; Step 1: Wrap expression as actor source
          (let (actor-source (wrap-expression expr))
            (begin
              (log "Wrapped as actor source")
              ; Step 2: Compile to WAT via linked compiler
              (let (wat (compile-source actor-source))
                (begin
                  (log "Compiled to WAT")
                  ; Step 3: Assemble to WASM via host function
                  (match (wat-to-wasm wat)
                    ((ok wasm-bytes)
                      (begin
                        (log "Assembled to WASM")
                        ; Step 4: Spawn expression-actor
                        ; For now, we just return success - full spawn needs manifest
                        ; TODO: Actually call spawn-with-wasm once manifest handling is ready
                        (ok (tuple (option (list u8)) (tuple (option (list u8)))) string
                            (tuple state (tuple (some (list u8) wasm-bytes))))))
                    ((err msg)
                      (begin
                        (log (string-append "Assembly error: " msg))
                        (ok (tuple (option (list u8)) (tuple (option (list u8)))) string
                            (tuple state (tuple (none (list u8)))))))))))))))))

; Handle child actor exit - this is where we receive evaluation results
(export "theater:simple/supervisor-handlers.handle-child-exit"
  (fn handle-child-exit ((state (option (list u8))) (params (tuple string (option (list u8)))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Child actor exited with result")
      ; TODO: Extract result from params and route to waiting client
      (ok (tuple (option (list u8))) string
          (tuple state)))))

; Handle child actor error
(export "theater:simple/supervisor-handlers.handle-child-error"
  (fn handle-child-error ((state (option (list u8))) (params (tuple string (tuple string (option (list u8))))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Child actor error")
      (ok (tuple (option (list u8))) string
          (tuple state)))))

; Handle child actor external stop
(export "theater:simple/supervisor-handlers.handle-child-external-stop"
  (fn handle-child-external-stop ((state (option (list u8))) (params (tuple string)))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Child actor externally stopped")
      (ok (tuple (option (list u8))) string
          (tuple state)))))
