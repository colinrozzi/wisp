(global $counter s32 mut 0)
(global $step s32 const 2)
(global $items (list s32) mut 0)
(global $ratio f64 mut 2)
(export (fn next () s32
  (begin (global.set $counter (i32.add (global.get $counter) (global.get $step))) (global.get $counter))))
(export (fn current () s32 (global.get $counter)))
(export (fn reset () s32 (begin (global.set $counter 0) (global.get $counter))))
(export (fn lexical () s32 (let ($counter 99) (global.get $counter))))
(export (fn initialize-list () s32
  (begin (global.set $items (list-push (list-new s32) 42)) (list-get (global.get $items) 0))))
(export (fn ratio () s32 (f64.eq (global.get $ratio) 2.0)))
