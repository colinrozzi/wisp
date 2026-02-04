; spawn-repl-actor.lisp — Theater actor with WASM-to-WASM compilation and actor spawning
;
; This actor demonstrates the full eval loop with actor spawning:
; 1. Receives expression from user
; 2. Wraps expression as module source (host helper)
; 3. Compiles source to WAT via WASM-to-WASM call to linked compiler!
; 4. Assembles WAT to WASM bytes (wat-to-wasm host function)
; 5. Spawns expression-actor with the WASM bytes and waits for result
;
; Imports:
;   theater:simple/runtime.log — logging
;   wisp:compiler/compiler.compile-source — compile Wisp to WAT (WASM-to-WASM!)
;   wisp:repl/helpers.wrap-expression — wrap expression as eval module
;   wisp:assembler/runtime.wat-to-wasm — assemble WAT string to WASM bytes
;   theater:simple/supervisor.spawn-and-wait — spawn actor and wait for result
;
; Exports:
;   theater:simple/actor.init — actor initialization
;   theater:simple/message-server-client.handle-send — fire-and-forget (ignored)
;   theater:simple/message-server-client.handle-request — compiles and spawns expression actors

(import theater:simple/runtime log ((msg string)) s32)

; This is the WASM-to-WASM call to the linked compiler!
; When we call compile-source, it goes directly to wisp-compiler.wasm
(import wisp:compiler/compiler compile-source ((src string)) string)

; Host helper: wrap expression bytes as module source string
; Takes (tuple request-id body-bytes), returns wrapped source string
(import wisp:repl/helpers wrap-expression
  ((params (tuple string (list u8))))
  string)

; Host function: assemble WAT string to WASM bytes
; Returns option<list<u8>> - Some = success, None = error (logged on host)
(import wisp:assembler/runtime wat-to-wasm
  ((wat string))
  (option (list u8)))

; Host function: spawn an actor with inline WASM and wait for its result
; Takes (tuple tag wasm-bytes) where tag is ignored, returns the actor's result
; (Using tuple because Wisp compiler supports tuple(string, list<u8>) encoding)
; Returns option<list<u8>> - Some = actor result, None = error (logged on host)
(import theater:simple/supervisor spawn-and-wait
  ((params (tuple string (list u8))))
  (option (list u8)))

; Initialize the REPL actor
(export "theater:simple/actor.init"
  (fn init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Spawn REPL actor initialized!")
      ; Demo: call compile-source to prove WASM-to-WASM works
      (log "Testing WASM-to-WASM call to compiler...")
      (let (test-wat (compile-source "(export (fn test () s32 (i32.const 42)))"))
        (begin
          (log "Compiler returned WAT!")
          (log test-wat)
          (ok (tuple (option (list u8))) string
              (tuple state)))))))

; Handle fire-and-forget messages (ignored)
(export "theater:simple/message-server-client.handle-send"
  (fn handle-send ((state (option (list u8))) (params (tuple (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Spawn REPL actor received send (ignoring)")
      (ok (tuple (option (list u8))) string
          (tuple state)))))

; Handle request-response messages — the full eval loop with actor spawning!
(export "theater:simple/message-server-client.handle-request"
  (fn handle-request ((state (option (list u8))) (params (tuple string (list u8))))
    (result (tuple (option (list u8)) (tuple (option (list u8)))) string)
    (begin
      (log "=== Full eval loop with actor spawning ===")

      ; Step 1: Wrap expression as module source (host helper)
      (log "Step 1: Wrapping expression...")
      (let (source (wrap-expression params))
        (begin
          (log "Wrapped source ready")

          ; Step 2: Compile to WAT via WASM-to-WASM call!
          (log "Step 2: Compiling via WASM-to-WASM...")
          (let (wat (compile-source source))
            (begin
              (log "Compilation complete!")

              ; Step 3: Assemble WAT to WASM bytes
              (log "Step 3: Assembling WAT to WASM...")
              (let (wasm-opt (wat-to-wasm wat))
                ; Use match for option pattern matching
                (match wasm-opt
                  ((some wasm-bytes)
                    (begin
                      (log "Assembly complete!")

                      ; Step 4: Spawn expression-actor and wait for result
                      (log "Step 4: Spawning expression-actor...")
                      (let (result-bytes (spawn-and-wait (tuple "spawn" wasm-bytes)))
                        (begin
                          (log "=== Eval loop complete (via actor spawn) ===")
                          (ok (tuple (option (list u8)) (tuple (option (list u8)))) string
                              (tuple state (tuple result-bytes)))))))
                  ((none)
                    (begin
                      (log "Assembly failed!")
                      (err (tuple (option (list u8)) (tuple (option (list u8)))) string
                          "Assembly failed"))))))))))))
