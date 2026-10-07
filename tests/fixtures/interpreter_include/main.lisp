(include "nested/counter.lisp")
(include "./nested/../shared.lisp")
(export (fn next () s32 (bump)))
(export (fn current () s32 (global.get $count)))
