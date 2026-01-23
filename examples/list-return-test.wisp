; Test returning a list from an exported function
; Using single push due to list-push not copying old data

(fn make-list () (list s32)
  (let (l (list-new s32))
    (list-push l 42)))

(export make-list)
