; web.wisp — a small HTTP/1.1 server that runs inside a REPL session.
; Serves files under `public/` (relative to the session's filesystem root),
; one request per connection: read the request line, answer, close.
; The session's on-connection/on-data handlers call web-connection/web-data
; (see site/serve.wisp), so (reload web) changes the live server.
(module web
  (provide web-connection web-data web-path content-type)

  (define byte-space (string-ref " " 0))
  (define root "public")

  ; index of the first byte equal to `b` in `s` at or after `i`; -1 if none.
  (define find-byte (lambda (s b i)
    (if (= i (string-len s)) -1
      (if (= (string-ref s i) b) i (find-byte s b (+ i 1))))))

  ; does `s` end with `suffix`?
  (define ends-with (lambda (s suffix)
    (let (n (string-len s))
      (let (k (string-len suffix))
        (if (< n k) 0
          (string=? (substring s (- n k) n) suffix))))))

  ; does `s` contain `pat` at or after `i`?
  (define contains (lambda (s pat i)
    (if (< (string-len s) (+ i (string-len pat))) 0
      (if (string=? (substring s i (+ i (string-len pat))) pat) 1
        (contains s pat (+ i 1))))))

  ; the request target from "GET /path HTTP/1.1...", or "" if malformed.
  (define web-path (lambda (req)
    (let (a (find-byte req byte-space 0))
      (if (< a 0) ""
        (let (b (find-byte req byte-space (+ a 1)))
          (if (< b 0) "" (substring req (+ a 1) b)))))))

  ; strip a query string: "/a?x=1" -> "/a"
  (define strip-query (lambda (p)
    (let (q (find-byte p (string-ref "?" 0) 0))
      (if (< q 0) p (substring p 0 q)))))

  (define content-type (lambda (p)
    (if (ends-with p ".html") "text/html; charset=utf-8"
      (if (ends-with p ".css") "text/css; charset=utf-8"
        (if (ends-with p ".js") "text/javascript; charset=utf-8"
          (if (ends-with p ".svg") "image/svg+xml"
            (if (ends-with p ".txt") "text/plain; charset=utf-8"
              "application/octet-stream")))))))

  ; join a list of strings (string-append takes exactly two)
  (define cat (lambda (xs)
    (if (null? xs) "" (string-append (car xs) (cat (cdr xs))))))

  ; non-negative integer -> decimal text
  (define num->str (lambda (n)
    (if (< n 10) (substring "0123456789" n (+ n 1))
      (string-append (num->str (/ n 10)) (num->str (- n (* 10 (/ n 10))))))))

  (define response (lambda (status ctype body)
    (cat (list "HTTP/1.1 " status "\r\n"
      "Content-Type: " ctype "\r\n"
      "Content-Length: " (num->str (string-len body)) "\r\n"
      "Connection: close\r\n\r\n" body))))

  ; "/" -> public/index.html, "/about" -> public/about.html, "/x.css" -> public/x.css
  (define file-for (lambda (p)
    (if (string=? p "/") (string-append root "/index.html")
      (if (ends-with p "/") (cat (list root p "index.html"))
        (if (< (find-byte p (string-ref "." 0) 0) 0)
          (cat (list root p ".html"))
          (string-append root p))))))

  (define not-found (response "404 Not Found" "text/html; charset=utf-8"
    "<!doctype html><title>Not found</title><p>Nothing here.</p>"))

  ; the full response for a raw request
  (define answer (lambda (req)
    (let (p (strip-query (web-path req)))
      (if (string=? p "") (response "400 Bad Request" "text/plain" "bad request\n")
        (if (contains p ".." 0) not-found
          (let (f (file-for p))
            (match (fs-read f)
              ((ok body) (response "200 OK" (content-type f) (bytes->string body)))
              ((err e) not-found))))))))

  (define web-connection (lambda (cid)
    (begin
      (tcp-activate cid)
      (tcp-set-active cid "active")
      cid)))

  ; e = (conn-id bytes). Answers and closes; returns "METHOD path" for the log.
  (define web-data (lambda (e)
    (let (cid (car e))
      (let (req (bytes->string (car (cdr e))))
        (begin
          (tcp-send cid (answer req))
          (tcp-close cid)
          (web-path req)))))))
