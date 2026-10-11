# theater-repl Ergonomics Findings

**Status:** F2, F3, F5, F8 fixed (interpreter-side). F1, F6 (host-side) and F4, F7
(error-message pass / prelude) still open.
**Date:** 2026-10-11
**Scope:** `theater-repl` v0.1.6 (the live Wisp REPL that drives a Theater runtime)
**Related:** `WISP-REPL-ARCHITECTURE.md`, `THEATER-INTEGRATION.md`

## Objective

Capture concrete usability friction found while dogfooding `theater-repl` end to
end (http client, timers/triggers, a TCP echo server, and a public HTTP server
built on raw TCP). Each finding below has a reproduction, a root cause grounded in
source, and a suggested fix, so the ergonomics pass can pick them up directly.

## Motivation

The verbs all *work* — we drove real HTTP requests, watched timer/connection
events fire, and served `curl` from another machine. But nearly every snag
clustered around one theme: **getting data *out* of the values the runtime hands
you**. Records can't be destructured, event payloads have a surprising shape, and
byte/string conversions are forced in places the underlying interface wouldn't
require. The individual fixes are small; together they'd remove most of the
first-hour pain.

---

## Findings

Severity: 🔴 traps/wedges you · 🟡 blocks a common task · 🔵 papercut

### 🔴 F1. A blocking `tcp-receive` with no data wedges the entire session

**Repro:** connect a client, then `(tcp-receive conn 256)` before any data is
waiting. It blocks forever. Worse, every *subsequent* eval on that session —
even `(+ 1 1)` — then hangs too. Only recovery is `stop <id>`.

**Root cause:** `apply-tcp-receive` (`wisp/interpreter/rpc.wisp:680`) is a
synchronous `raw-invoke` to `theater:simple/tcp receive` with no timeout. The
session actor processes one request at a time, so a blocked host call holds the
actor and starves all later evals.

**Suggested fix:** a `max-wait`/timeout arg on `tcp-receive` (return
`(err "timeout")`), and/or steer users toward push mode (`on-data`) for reads.
At minimum, document the wedge and make it recoverable without `stop`.

### 🟡 F2. Host-returned records have no field accessors — ✅ FIXED

`register-record` (`marshal.wisp`) now calls `add-accessors` when it registers an
arriving type, so `(http-response.status resp)` works on the record the host hands
back. Covered by `test_unmarshal_record_register_on_arrival` (marshal).


**Repro:** `(http-get "https://httpbin.org/ip")` returns an `http-response`
record, but `(http-response.status resp)` / `(status resp)` are unbound, and
`match` rejects it (see F3). The field values are visible when printed but not
reachable in code.

**Root cause:** on arrival, `register-record` (`wisp/interpreter/marshal.wisp:539`)
registers the named type by name + field-name shape (`names-match`,
`marshal.wisp:525`) but never calls `add-accessors` (`types.wisp:104`). Accessors
(`type.field`, published by `add-accessors`) only get created by a *runtime*
`(record …)` eval. And you can't redefine an already-registered type.

**Workaround (ugly):** in a fresh session, `(record http-response (status s32)
(headers s32) (body s32))` **before** the first response arrives — `register-record`
then reuses that id (match is by field *names* only; types are cosmetic because
field-readers are positional, `apply-field`/`types.wisp:175`), so the accessors
line up on arrival.

**Suggested fix:** have `register-record` publish accessors (`add-accessors`) when
it registers an arriving record.

### 🔵 F3. `match` on a record dead-ends with an unhelpful error — ✅ FIXED

`eval-match` now destructures records positionally (a record matches a single
synthetic case named after the record: `(match r ((Rec a b) …))`), and the
fall-through error names records as a matchable shape. Covered by
`test_interpreter_match_destructures_records`.


**Repro:** `(match resp ((http-response s h b) …))` → `error: match expects a
variant`.

**Root cause:** `eval-match` (`wisp/interpreter/matching.wisp`) only handles
option/result/variant; records fall through to `"match expects a variant"`.

**Suggested fix:** either support record destructuring in `match`, or make the
error point at the real path (accessors / F2).

### 🟡 F4. Three incompatible "list-like" shapes, with errors that don't guide

Building the TCP echo server, extracting `(conn-id bytes)` from the `on-data`
event took several iterations because three shapes look alike but need different
accessors, and the errors never say which you have:

| Shape | Accessor | Wrong-accessor error |
| --- | --- | --- |
| cons-list (e.g. the `on-data` event) | `car` / `cdr` | — |
| typed-list (e.g. `(list u8)` bytes) | `list-get` | cons-list → `"expected typed list"` (`collections.wisp:106`) |
| tuple / variant | `match` | → `"match expects a variant"` |

**Suggested fix:** a uniform indexed accessor that works across shapes, or errors
that name the actual shape and the right accessor.

### 🟡 F5. `tcp-send` refuses `list<u8>`, only accepts strings — ✅ FIXED

`str-to-bytes` now passes an existing byte list through unchanged (instead of
dropping it), and `apply-tcp-send` accepts `data` as a string *or* a byte list — so
the bytes an `on-data` event delivered can be relayed with no lossy string
round-trip. Covered by `test_marshal_byte_list_passthrough`.


**Repro:** echo server — `(tcp-send conn bytes)` where `bytes` came from an
`on-data` event → `error: tcp-send expects two strings: connection id and data`.
Must `(bytes->string bytes)` first, which is lossy for non-UTF-8 binary.

**Root cause:** `apply-tcp-send` (`wisp/interpreter/rpc.wisp:675`) guards
`string-arg?` on *both* args, even though the WIT interface is
`send((connection-id string) (data (list u8)))` (`rpc.wisp:169`). It can't relay
the very bytes `on-data` just delivered.

**Suggested fix:** accept a `(list u8)` (typed-list) for `data` and marshal it
directly; keep the string path as sugar.

### 🔵 F6. `tcp-close-listener` won't close by the id `tcp-listen` returned

**Repro:** `(tcp-listen "127.0.0.1:8080")` → `(ok … "8")`; later
`(tcp-close-listener "8")` → `error: Listener not found: 8`. The only way to free
the port was `stop`-ping the whole session.

**Root cause:** the id round-trip between `tcp-listen` and `tcp-close-listener`
(`rpc.wisp:663`) / the host listener registry doesn't line up. Needs
investigation host-side.

**Suggested fix:** make `tcp-close-listener` accept the exact id `tcp-listen`
returns (and round-trip-test it).

### 🔵 F7. Common Lisp builtins are unbound in a bare session

`and`, `or`, `not`, `map`, `number->string`, `int->string` are all unbound in a
fresh session. For something that presents as a Lisp this is a surprise, and it
blocks small tasks (e.g. no `number->string` → can't put `(now)` or a status code
into an HTTP response). Likely a prelude that isn't auto-loaded into sessions.

**Suggested fix:** auto-load a small prelude (boolean ops, `map`/`filter`/`fold`,
number→string) into every session.

### 🔵 F8. `(help "tcp")` documents the wrong event shape — ✅ FIXED

`(help "tcp")` and `(help "triggers")` now describe `on-data`/`on-close` as
2-element lists `(conn-id bytes)` / `(conn-id reason)`, read with `car`/`cdr`,
instead of the misleading `(sequence conn-id bytes)`. Covered by
`test_interpreter_help_tcp_event_shape`.

---

## What worked well (keep it)

- The **observe model** — define `on-tick`/`on-connection`/`on-data`/… by name,
  then drain with `read`/`follow`. Watching the raw request bytes land in the
  server's event buffer is genuinely great.
- **State persists** across `eval` calls; `(help)` / `(help "tcp")` /
  `(help "triggers")` are solid in-session references.
- Results come back **faithful and inspectable** (`(ok <ok-ty> <err-ty> <val>)`),
  which made every finding above easy to see even when it was hard to *use*.
- End to end it genuinely served HTTP on `0.0.0.0:8080` to another machine from a
  handful of lines typed into the REPL.

## Appendix — minimal working TCP HTTP server (post-workarounds)

```lisp
(define hdr "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nConnection: close\r\n\r\n")
(define route (lambda (req)
  (if (string=? (substring req 0 9) "GET /ping") "pong\n"
    (string-append "You sent:\n\n" req))))
(define on-connection (lambda (cid)
  (begin (tcp-activate cid) (tcp-set-active cid "active") cid)))
(define on-data (lambda (e)
  (let (req (bytes->string (car (cdr e))))
    (begin (tcp-send (car e) (string-append hdr (route req)))
           (tcp-close (car e))))))
(tcp-listen "0.0.0.0:8080")
```
