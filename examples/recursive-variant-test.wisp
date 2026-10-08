; Test recursive variants for S-expressions

; Define a simple tree structure (simpler than full sexpr)
(variant tree
  (leaf s32)
  (node (list tree)))

; Check if tree is a leaf
(fn is-leaf ((t tree)) s32
  (match t
    ((leaf n) (i32.const 1))
    ((node children) (i32.const 0))))

; Get leaf value (0 if node)
(fn get-leaf ((t tree)) s32
  (match t
    ((leaf n) n)
    ((node children) (i32.const 0))))

; Test 1: Create a leaf
(export (fn test-leaf () s32
  (let (t (leaf (i32.const 42)))
    (is-leaf t))))  ; Expected: 1

; Test 2: Get leaf value
(export (fn test-leaf-value () s32
  (let (t (leaf (i32.const 42)))
    (get-leaf t))))  ; Expected: 42

; Test 3: Create an empty node
(export (fn test-empty-node () s32
  (let (children (list-new tree))
    (let (t (node children))
      (is-leaf t)))))  ; Expected: 0
