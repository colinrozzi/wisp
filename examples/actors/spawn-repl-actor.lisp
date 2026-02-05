; spawn-repl-actor.lisp — Theater actor with WASM-to-WASM compilation and direct eval
;
; This actor demonstrates the full eval loop:
; 1. Receives expression from user
; 2. Wraps expression as eval module source (host helper)
; 3. Compiles source to WAT via WASM-to-WASM call to linked compiler!
; 4. Assembles WAT to WASM bytes (wat-to-wasm host function)
; 5. Evaluates WASM directly and returns the i32 result (eval-wasm host function)
;
; Imports:
;   theater:simple/runtime.log — logging
;   wisp:compiler/compiler.compile-source — compile Wisp to WAT (WASM-to-WASM!)
;   wisp:repl/helpers.wrap-expression — wrap expression as eval module
;   wisp:assembler/runtime.wat-to-wasm — assemble WAT string to WASM bytes
;   wisp:assembler/runtime.eval-wasm — evaluate WASM and call eval() export
;
; Exports:
;   theater:simple/actor.init — actor initialization
;   theater:simple/message-server-client.handle-send — fire-and-forget (ignored)
;   theater:simple/message-server-client.handle-request — compiles and evaluates expressions

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

; Host function: evaluate WASM bytes by calling the `eval` export
; Returns result<list<u8>, string> - Ok = result bytes (4-byte i32), Err = error message
(import wisp:assembler/runtime eval-wasm
  ((wasm (list u8)))
  (result (list u8) string))

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

; Handle request-response messages — the full eval loop with direct evaluation!
(export "theater:simple/message-server-client.handle-request"
  (fn handle-request ((state (option (list u8))) (params (tuple string (list u8))))
    (result (tuple (option (list u8)) (tuple (option (list u8)))) string)
    (begin
      (log "=== Full eval loop with direct evaluation ===")

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

                      ; Step 4: Evaluate WASM directly via host function
                      (log "Step 4: Evaluating WASM...")
                      (let (eval-result (eval-wasm wasm-bytes))
                        (match eval-result
                          ((ok result-bytes)
                            (begin
                              (log "=== Eval loop complete ===")
                              ; Return success with result bytes as response
                              ; State stays the same, response is the result
                              (ok (tuple (option (list u8)) (tuple (option (list u8)))) string
                                  (tuple state (tuple (some (list u8) result-bytes))))))
                          ((err error-msg)
                            (begin
                              (log "Eval failed!")
                              (log error-msg)
                              (err (tuple (option (list u8)) (tuple (option (list u8)))) string
                                  error-msg)))))))
                  ((none)
                    (begin
                      (log "Assembly failed!")
                      (err (tuple (option (list u8)) (tuple (option (list u8)))) string
                          "Assembly failed"))))))))))))
