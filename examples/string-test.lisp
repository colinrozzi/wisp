; Test file for string type

; Test creating a string and getting its length
(export (fn test-string-len () s32
  (let (s "hello")
    (string-len s))))

; Test empty string
(export (fn test-empty-string () s32
  (let (s "")
    (string-len s))))

; Test longer string
(export (fn test-long-string () s32
  (let (s "hello world!")
    (string-len s))))

; Test string with escape sequences
(export (fn test-escape-string () s32
  (let (s "a\nb")  ; 3 characters: a, newline, b
    (string-len s))))

; === string-ref tests ===

; Test getting first character
(export (fn test-string-ref-first () s32
  (let (s "hello")
    (string-ref s (i32.const 0)))))  ; 'h' = 104

; Test getting middle character
(export (fn test-string-ref-middle () s32
  (let (s "hello")
    (string-ref s (i32.const 2)))))  ; 'l' = 108

; Test getting last character
(export (fn test-string-ref-last () s32
  (let (s "hello")
    (string-ref s (i32.const 4)))))  ; 'o' = 111

; === substring tests ===

; Test getting substring from start
(export (fn test-substring-start () s32
  (let (s "hello")
    (let (sub (substring s (i32.const 0) (i32.const 3)))
      (string-len sub)))))  ; "hel" = 3

; Test getting substring from middle
(export (fn test-substring-middle () s32
  (let (s "hello world")
    (let (sub (substring s (i32.const 6) (i32.const 11)))
      (string-len sub)))))  ; "world" = 5

; Test substring first character
(export (fn test-substring-first-char () s32
  (let (s "hello")
    (let (sub (substring s (i32.const 0) (i32.const 1)))
      (string-ref sub (i32.const 0))))))  ; 'h' = 104

; === string-append tests ===

; Test appending two strings - check length
(export (fn test-string-append-len () s32
  (let (s1 "hello")
    (let (s2 " world")
      (let (result (string-append s1 s2))
        (string-len result))))))  ; "hello world" = 11

; Test appending - check first char
(export (fn test-string-append-first-char () s32
  (let (s1 "hello")
    (let (s2 " world")
      (let (result (string-append s1 s2))
        (string-ref result (i32.const 0)))))))  ; 'h' = 104

; Test appending - check char at boundary
(export (fn test-string-append-boundary () s32
  (let (s1 "hello")
    (let (s2 " world")
      (let (result (string-append s1 s2))
        (string-ref result (i32.const 5)))))))  ; ' ' = 32

; Test appending - check last char
(export (fn test-string-append-last-char () s32
  (let (s1 "hello")
    (let (s2 " world")
      (let (result (string-append s1 s2))
        (string-ref result (i32.const 10)))))))  ; 'd' = 100

; === string=? tests ===

; Test equal strings
(export (fn test-string-eq-same () s32
  (let (s1 "hello")
    (let (s2 "hello")
      (string=? s1 s2)))))  ; 1 (true)

; Test different strings same length
(export (fn test-string-eq-different () s32
  (let (s1 "hello")
    (let (s2 "world")
      (string=? s1 s2)))))  ; 0 (false)

; Test different strings different lengths
(export (fn test-string-eq-different-len () s32
  (let (s1 "hello")
    (let (s2 "hi")
      (string=? s1 s2)))))  ; 0 (false)

; Test empty strings equal
(export (fn test-string-eq-empty () s32
  (let (s1 "")
    (let (s2 "")
      (string=? s1 s2)))))  ; 1 (true)

; Test one empty one not
(export (fn test-string-eq-one-empty () s32
  (let (s1 "")
    (let (s2 "a")
      (string=? s1 s2)))))  ; 0 (false)
