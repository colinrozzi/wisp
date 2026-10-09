; A capability-gated console effect, run directly by the interpreter:
;
;   wisp eval examples/hello-effect.wisp
;
; `write-line` is a host import — `wisp eval`'s built-in stdio host prints its
; argument. `Console` is an unforgeable capability: `log` can only run when given
; a borrowed `Console`, and the sole way to get one is `with-cap`, which owns and
; releases it. So the effect is reachable only through the capability, and the
; whole program still goes through the type + linearity checker before it runs.

(capability Console)

(import host write-line ((msg string)) s32)

(fn log ((c (borrow Console)) (msg string)) s32
  (write-line msg))

(export (fn main () s32
  (with-cap (c Console)
    (log (& c) "Hello from Wisp eval!"))))
