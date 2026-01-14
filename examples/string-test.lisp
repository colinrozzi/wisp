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
