; Simple math library for REPL import testing

(export (fn add ((a s32) (b s32)) s32
  (i32.add a b)))

(export (fn multiply ((a s32) (b s32)) s32
  (i32.mul a b)))

(export (fn square ((n s32)) s32
  (i32.mul n n)))
