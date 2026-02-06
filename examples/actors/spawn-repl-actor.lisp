; spawn-repl-actor.lisp — Theater actor with WASM-to-WASM compilation and direct eval
;
; This actor demonstrates the full eval loop with import support:
; 1. Receives expression (optionally with imports) from user
; 2. Parses imports and wraps expression as eval module source (host helper)
; 3. Compiles source to WAT via WASM-to-WASM call to linked compiler!
; 4. Assembles WAT to WASM bytes (wat-to-wasm host function)
; 5. If imports present, composes with dependency modules (compose-packages)
; 6. Evaluates WASM directly and returns the typed result (eval-wasm host function)
;
; Import syntax:
;   (import interface-name from "path.wasm")
;   (expression...)
;
; Example with imports:
;   (import colin:math/ops from "examples/math.wasm")
;   (add (i32.const 3) (i32.const 4))
;
; Imports:
;   theater:simple/runtime.log - logging
;   wisp:compiler/compiler.compile-source - compile Wisp to WAT (WASM-to-WASM!)
;   wisp:repl/helpers.parse-and-wrap - parse imports, wrap expression as eval module
;   wisp:assembler/runtime.wat-to-wasm - assemble WAT string to WASM bytes
;   wisp:assembler/runtime.eval-wasm - evaluate WASM and call eval() export
;   wisp:compose/packages.compose-packages - compose main WASM with dependencies
;
; Exports:
;   theater:simple/actor.init - actor initialization
;   theater:simple/message-server-client.handle-send - fire-and-forget (ignored)
;   theater:simple/message-server-client.handle-request - compiles and evaluates expressions

(import theater:simple/runtime log ((msg string)) s32)

; This is the WASM-to-WASM call to the linked compiler!
; When we call compile-source, it goes directly to wisp-compiler.wasm
(import wisp:compiler/compiler compile-source ((src string)) string)

; Host helper: parse imports and wrap expression as module source string
; Takes (tuple request-id body-bytes), returns source string
(import wisp:repl/helpers parse-and-wrap
  ((params (tuple string (list u8))))
  (result string string))

; Host function: assemble WAT string to WASM bytes
; Returns option<list<u8>> - Some = success, None = error (logged on host)
(import wisp:assembler/runtime wat-to-wasm
  ((wat string))
  (option (list u8)))

; Host function: evaluate WASM bytes by calling the `eval` export
; Returns result<list<u8>, string> - Ok = result bytes (type-tagged), Err = error message
(import wisp:assembler/runtime eval-wasm
  ((wasm (list u8)))
  (result (list u8) string))

; Host function: compose main WASM with dependency modules
; Takes (main-wasm, original-params) where original-params is the (request-id, body-bytes) tuple
; Returns composed WASM bytes
(import wisp:compose/packages compose-packages
  ((params (tuple (list u8) (tuple string (list u8)))))
  (result (list u8) string))

; Initialize the REPL actor
(export "theater:simple/actor.init"
  (fn init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
    (begin
      (log "Spawn REPL actor initialized!")
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

; Handle request-response messages - the full eval loop with import support!
(export "theater:simple/message-server-client.handle-request"
  (fn handle-request ((state (option (list u8))) (params (tuple string (list u8))))
    (result (tuple (option (list u8)) (tuple (option (list u8)))) string)
    (begin
      (log "=== Full eval loop with import support ===")
      (log "Step 1: Parsing imports and wrapping expression...")
      (let (parse-result (parse-and-wrap params))
        (match parse-result
          ((ok source)
            (begin
              (log "Parsed source ready")
              (log "Step 2: Compiling via WASM-to-WASM...")
              (let (wat (compile-source source))
                (begin
                  (log "Compilation complete!")
                  (log "Step 3: Assembling WAT to WASM...")
                  (let (wasm-opt (wat-to-wasm wat))
                    (match wasm-opt
                      ((some main-wasm)
                        (begin
                          (log "Assembly complete!")
                          (log "Step 4: Composing with dependencies...")
                          (let (compose-result (compose-packages (tuple main-wasm params)))
                            (match compose-result
                              ((ok composed-wasm)
                                (begin
                                  (log "Composition complete!")
                                  (log "Step 5: Evaluating WASM...")
                                  (let (eval-result (eval-wasm composed-wasm))
                                    (match eval-result
                                      ((ok result-bytes)
                                        (begin
                                          (log "=== Eval loop complete ===")
                                          (ok (tuple (option (list u8)) (tuple (option (list u8)))) string
                                              (tuple state (tuple (some (list u8) result-bytes))))))
                                      ((err error-msg)
                                        (begin
                                          (log "Eval failed!")
                                          (log error-msg)
                                          (err (tuple (option (list u8)) (tuple (option (list u8)))) string
                                              error-msg)))))))
                              ((err compose-error)
                                (begin
                                  (log "Composition failed!")
                                  (log compose-error)
                                  (err (tuple (option (list u8)) (tuple (option (list u8)))) string
                                      compose-error)))))))
                      ((none)
                        (begin
                          (log "Assembly failed!")
                          (err (tuple (option (list u8)) (tuple (option (list u8)))) string
                              "Assembly failed")))))))))
          ((err parse-error)
            (begin
              (log "Parse failed!")
              (log parse-error)
              (err (tuple (option (list u8)) (tuple (option (list u8)))) string
                  parse-error))))))))
