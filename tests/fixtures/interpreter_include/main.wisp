(include "nested/counter.wisp")
(include "./nested/../shared.wisp")
(export (fn next () s32 (bump)))
(export (fn current () s32 (global.get $count)))
