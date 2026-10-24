# Session inbox: reading events without destroying them

Status: approved (ticket 0018); implemented in ticket 0023 (see §10).
It builds on ticket 0015 (quinn), which adds a 256-firing ring buffer with a
drop counter to `$events` (`WISP-RETENTION.md` §2.3). This proposal adds
sequence numbers and cursors to that ring, and does not change its bound or
what each firing keeps.

## Summary

- **Every firing gets a sequence number** (`seq`): 1, 2, 3, … for the life of
  the session actor. The ring from 0015 keeps the newest 256 firings, each
  stamped with its `seq`.
- **Reading never removes anything.** A reader asks for the firings *after* a
  seq. Only the ring bound removes firings, and every reader that loses
  firings to it is told how many (`lost`).
- **Cursors are named and live in the session.** A cursor is a name plus the
  last seq that its reader handled. `read`, `follow` and `wait` each use their
  own cursor by default (`read`, `follow`, `wait`), so a monitor, a human and a
  leftover `follow` do not take events from each other. `--cursor NAME` picks
  another cursor; `--after N` reads without a cursor.
- **A cursor moves only after its reader has printed.** The CLI acknowledges
  (`ack`) a seq after it writes the firing to stdout. A reader that dies before
  it acknowledges sees the same firings again: delivery to a reader is
  *at least once*, and the seq lets a reader find duplicates.
- **A new `theater-repl wait` verb** blocks until a firing exists after the
  cursor, prints the firings, acknowledges them and exits. The wake-on-message
  monitor becomes `theater-repl wait <id>`. It cannot lose a firing, because
  nothing that it did not print is acknowledged.
- **No polling.** The interpreter tells the daemon the new seq through a host
  call (`wisp-sessions.event-recorded`) each time it records a firing. The
  daemon wakes the waiting `wait`/`follow` requests. An idle waiter costs no
  evaluation.
- **`(poll-events)` keeps its 0015 shape** but reads through its own cursor
  (`poll`), so it no longer takes firings from the CLI readers.
- **Memory: no new unbounded state.** The ring stays at 0015's bound. Cursors
  are a table of at most 16 entries.

## 1. The problem

Today (and after 0015) `$events` is a queue with one consumer.
`(poll-events)` returns the buffered firings and empties the buffer.
`theater-repl read` runs `(poll-events "text")` once, and `theater-repl
follow` runs it every second. So:

1. **Any reader takes from all the others.** A leftover `follow` process
   took two agents' replies to the coordinator: it printed them to a
   terminal that nobody read, and the agents' monitors saw nothing.
2. **A reader that stops early loses what it drained.** `follow | head -n 1`
   lost the second of two messages that arrived in the same second.
   `experiment/lib/wait-message` reduces this: it prints everything that
   arrives within 2 seconds of the first firing. But a firing that `follow`
   drains just after the 2-second window closes, before the `kill`, is still
   lost.
3. **The monitor polls.** `follow` evaluates `(poll-events "text")` once a
   second for as long as it runs. Until Granite frees memory (0011), each
   evaluation allocates a few KB that is never freed (`WISP-RETENTION.md`
   §4). That is a few hundred MB a day for each monitor, in the session it
   watches.
4. **The services drain on a timer** (`registry-drain` every minute,
   `(poll-events)` in `ge-output`) only to stop the buffer from growing.
   0015's ring makes that unnecessary.

## 2. What Theater guarantees, and where loss can still happen

From `~/work/theater` (`crates/theater-handler-message-server/src/lib.rs`,
"ms" below):

- **`send`** (our `msg-send`, the target's `handle-send`): the sender gets
  `Ok` when the message is in the target's mailbox (ms:277-282). There is no
  acknowledgement that the handler ran. The sender gets `Err` only when the
  target has not registered ("Actor not found"). Messages from one sender to
  one target arrive in order. The mailbox holds 100 messages (ms:863); when it
  is full, the router waits (backpressure, not loss). If the handler fails,
  the receiver logs the failure (ms:888) and the sender does not learn of it.
- **`request` / `call`**: the caller waits for the reply. A failed handler
  replies with empty bytes, so a failure looks like an empty reply. (The
  guide already says not to use these on a REPL session.)
- **Channels**: open waits for the target to accept. A channel message is
  acknowledged when it is in the other side's mailbox, the same as `send`.
  Close is best effort. Nothing is retried or stored.
- **The chain** records every export call and every host call, but Theater
  keeps only the head hash. Events are not stored unless a subscriber stores
  them.
- **Nothing survives the actor.** Mailboxes are in memory. When an actor
  stops, what is queued for it is lost.
- **A trap kills the actor.** The Wisp actor's `handle-*` exports always
  return `"ok"`; a failure in a user handler is a Wisp `failure` value, not a
  trap. So a bad `on-message` does not kill the session.

So Theater delivers a `send` into the actor reliably (in order, with
backpressure) while both actors live, and then the guarantee stops. Before
this proposal, the loss happened after that point, **inside the session**,
between `$events` and the reader. This proposal closes that gap. The
remaining places where a firing can be lost are all visible or out of scope:

| Where | Lost when | Visible? |
|---|---|---|
| Sender to mailbox | target not registered | yes: `msg-send` returns `(err "Actor not found")` |
| Mailbox to handler | target actor stops with messages queued | no (Theater; out of scope) |
| Ring | more than 256 firings arrive after a reader's cursor | **yes**: the reader gets `lost = N` |
| Session to reader | — | **no loss**: nothing is removed by reading; a cursor moves only after print |
| Session restart | the session actor stops | yes: the reader's request fails; a new actor's seqs start at 1 (§4.5) |

Two agents that want end-to-end confirmation still need an application
reply ("tova: got it"), because `send` has no delivery receipt. That is
outside this design. What this design gives is: once a message has reached a
session's handler, every reader of that session sees it, or is told that it
lost it.

## 3. Interpreter side (`wisp/interpreter/rpc.wisp`)

### 3.1 The ring, with sequence numbers

0015 keeps `$events` as a list that `record-event` appends to and
`trim-events` cuts back to the newest `$event-limit` firings, counting the
cut firings in `$events-dropped`. This proposal adds:

```lisp
(global $event-seq s64 mut 0)      ; seq of the newest firing ever recorded
```

- `record-event` increments `$event-seq` and puts the seq first in the
  firing: `(seq handler result)` for a handled firing, `(seq handler event
  result)` for an unhandled one. (0015's shapes, with the seq in front.)
- The ring stays a list in seq order. The oldest retained seq is the seq of
  its first entry, so a reader can compute what it lost (§3.2) without a
  separate counter.
- `$events-dropped` from 0015 stays, but only `(poll-events)` uses it (§3.4).
  For cursor readers, "lost" is computed from seqs.
- `(event-buffer 0)` (0015) still turns buffering off. `$event-seq` still
  counts, so a reader sees `lost` for every firing that was not kept.

`seq` is an `s64` (a `wide-integer` when returned). An `s32` overflows after
2^31 firings: at 100 firings a second (a busy `on-output`) that is 248 days,
and the registry has run for days already.

Note on 0015's lazy trim: 0015 trims only when the list reaches twice the
limit, so between trims the ring can hold up to 512 firings. Readers must use
the seq of the actual first entry, not `head - limit`, to compute `lost`.
`events-after` (§3.2) trims first, as `poll-events` does in 0015.

### 3.2 Reading: `(events-after N [max] [mode])`

Returns the firings with seq > N, oldest first, and removes nothing.

```lisp
(events-after 41)        ; => (44 0 (42 "on-message" "claude: ok") (43 "on-tick" "beat") (44 ...))
(events-after 41 1)      ; => (42 0 (42 "on-message" "claude: ok"))   ; at most 1 firing
(events-after 3)         ; => (300 41 (45 ...) ...)                   ; seqs 4..44 were dropped
(events-after 300)       ; => (300 0)                                  ; nothing new
```

The result is `(next lost firing...)`:

- `next`: the seq of the last firing returned, or N when there are none.
  This is the value to pass next time, and the value to acknowledge (§3.3)
  after the firings are handled.
- `lost`: how many firings with seq > N the ring no longer holds:
  `max(0, oldest - N - 1)`, or `head - N` when the ring is empty and
  `head > N`.
- `max` (default: all) limits the count, so a reader can page through a full
  ring.

`mode "text"` returns a string for people: a `(N earlier firings dropped)`
line when `lost > 0` (0015's wording), then one `handler: result` line for
each firing (0015's text format). `mode "wire"` is for the daemon: the same
text, with a first line `next lost` (two integers) that the daemon removes
before it prints. The daemon uses this so that it gets the cursor and the
text from one evaluation.

If N is larger than the head (a cursor from an earlier actor, §4.5),
`events-after` returns a failure: `"cursor N is after head H"`.

### 3.3 Cursors

```lisp
(global $cursors (list value) mut 0)   ; ((name seq last-use) ...), at most 16
```

| Verb | Does |
|---|---|
| `(event-cursor NAME)` | The cursor's seq. If NAME does not exist, creates it at **0** (so its reader sees everything still in the ring) |
| `(event-ack NAME SEQ)` | Moves the cursor to `max(current, SEQ)`. Never moves it back. Fails if SEQ > head. Returns the new value |
| `(event-read NAME [max] [mode])` | `(events-after (event-cursor NAME) max mode)`. Does not move the cursor |
| `(event-cursors)` | `((name seq) ...)`, for debugging (`theater-repl cursors`) |
| `(event-cursor-drop NAME)` | Removes it |
| `(event-head)` | `$event-seq` |

Design points:

- **Acknowledgement is separate from reading.** A reader reads, handles
  (prints), and only then acknowledges. If it dies between the two, the next
  reader with that cursor gets the same firings again. That is the whole
  reason for "lossless": no step removes or skips a firing before the reader
  has handled it.
- **Acknowledgement is monotonic (max).** Two processes that share a cursor
  name (for example two `follow`s) cannot move it back. Each prints
  everything after its own position, so a shared name means "a shared resume
  point", never "they split the firings".
- **Cursors do not hold firings in the ring.** The ring bound is the only
  thing that removes firings. A cursor that nobody uses does not pin memory,
  and acknowledging does not free memory early. I considered "release a
  firing when every cursor has passed it": the saving is at most the ring
  (tens of KB after 0015), and it makes the set of readers matter again, for
  example an `--after N` reader without a cursor would find firings gone
  because named cursors passed them. The ring bound is simpler, and its loss
  is always reported.
- **At most 16 cursors.** When a 17th name is created, the cursor with the
  oldest `last-use` is removed. `last-use` is `$event-seq` at the last
  `event-cursor`/`event-ack`/`event-read`, so the interpreter needs no clock.
  A removed cursor that is used again starts at 0, so its reader sees the
  ring again (duplicates, not loss).
- Cursor names are free text. Reserved by convention: `read`, `follow`,
  `wait` (the CLI defaults, §4.1) and `poll` (`(poll-events)`, §3.4).

### 3.4 What `(poll-events)` becomes

`(poll-events)` and `(poll-events "text")` keep exactly the 0015 result shapes
(no seqs, the drop count first when there were drops), so existing callers do
not change. Inside, they become:

```
read after cursor "poll"  ->  format as 0015 does  ->  ack "poll" to next
```

So a session that drains itself (the registry, github-events) moves only its
own `poll` cursor and no longer takes firings from `theater-repl read`,
`follow` or `wait`. The drop count is the `lost` value for the `poll` cursor.
`$events-dropped` can then go away, but keeping it costs nothing if 0015
lands first.

`(poll-events)` no longer frees memory, because the ring bound does that. So
the services' drain timers have no purpose (§6.3). `(help "triggers")`
describes `(events-after)` and the cursor verbs, and marks `(poll-events)` as
"kept for old callers".

### 3.5 Telling the daemon: `wisp-sessions.event-recorded`

The daemon must know when a firing is recorded, so that `wait` and `follow`
can sleep without evaluating. Add one function to the daemon's existing
`wisp-sessions` interface (`wisp/actor/sessions.pact`), which the actor
already imports for `(sessions)`:

```
event-recorded: func(seq: u64)
```

`record-event` calls it after it stores the firing. The daemon's
implementation (`sessions.rs`) does only `watch::Sender::send_replace(seq)`
for that actor and returns at once.

- It **must not** evaluate in the actor or wait for anything. The call runs
  inside the actor's `handle-*` export, and the actor runs one operation at
  a time, so an evaluation from inside the host function would deadlock.
  The woken reader's evaluation is queued and runs after the handler returns.
- Cost: one host call for each firing, recorded in the chain like every host
  call. It returns nothing, so it does not go through `unmarshal`.
- An actor that runs outside the daemon (the test harness, another host) must
  satisfy the import. The test harness gets a stub that does nothing. Other
  hosts already have to provide `list-sessions`.

Alternative considered: the daemon watches the actor's chain (Theater
broadcasts every chain event to subscribers) and wakes on `handle-*`
results. It needs no actor change, but it wakes on every export call, not
only on firings, and it ties the daemon to chain event names. The explicit
call is simpler and says exactly what happened.

## 4. Daemon and CLI side (`wisp/actor/legacy-host`)

### 4.1 Verbs

| Command | Cursor (default) | Behavior |
|---|---|---|
| `theater-repl read <id>` | `read` | Prints the firings after the cursor, then acks. Returns at once |
| `theater-repl follow <id>` | `follow` | Prints firings as they are recorded, acking after each batch, until Ctrl-C. Resumes from where the last `follow` with this cursor stopped |
| `theater-repl wait <id>` | `wait` | Blocks until at least one firing exists after the cursor, prints them all, acks, exits 0 |
| `theater-repl cursors <id>` | — | Lists cursors and the head: `NAME SEQ BEHIND` |

Options for all three readers:

- `--cursor NAME`: use another cursor.
- `--after N`: read after N with no cursor (nothing is acknowledged).
  `--after 0` shows the whole ring.
- `--peek`: read the cursor but do not acknowledge.
- `--full`: print `(seq handler result [event])` lists, one per line, after
  a first line `(next lost)`. Scripts parse this.
- `--seq` (text mode): prefix each line with `#seq `.
- `wait --timeout SECS`: exit 2 with nothing printed when nothing arrives in
  time (default: no timeout).
- `follow --from-now`: start at the head instead of at the cursor.

Each verb has its own default cursor, so the three normal uses never
interfere: the wake-on-message monitor (`wait`), a human looking (`read`),
and a terminal streaming (`follow`). This is what fixes the incident in §1:
the leftover `follow` would have moved only the `follow` cursor, and the
agents' monitors would still have seen the replies.

A human who wants to see what the monitor saw uses `read --after 0` or
`read --cursor wait --peek`.

### 4.2 Protocol

The JSON op `read` gets optional fields, and a new op `wait` is added:

```
{"op":"read","id":"a1b2","cursor":"read","max":256,"text":true}
{"op":"read","id":"a1b2","after":41,"text":true}
{"op":"wait","id":"a1b2","cursor":"wait","timeout_ms":0,"text":true}
{"op":"ack","id":"a1b2","cursor":"wait","seq":44}
{"op":"cursors","id":"a1b2"}
```

A response gets one more field, `"next"` (the seq to acknowledge), next to
`ok` and `text`. The daemon evaluates `(event-read NAME max "wire")`, takes
`next lost` from the first line, and returns the rest as `text`. `lost` is
already in the text, as the "dropped" line.

A `read` with no `cursor` and no `after` from an old client keeps today's
behavior: the daemon evaluates `(poll-events "text")` / `(poll-events)`.
New clients always send `cursor` or `after`.

### 4.3 The acknowledgement happens in the client

The CLI does: request → print → flush stdout → `ack`. If the write fails
(the pipe is closed, as with `| head -n 1`), the CLI exits without
acknowledging, and the next reader gets the firings again. The daemon never
acknowledges for a client, because only the client knows that it printed.

For `follow`, the next request in the loop carries the previous batch's
acknowledgement (`"ack": 44` on the `wait` op), so a steady stream costs one
request per batch, not two.

### 4.4 Waiting without polling

The daemon keeps a `tokio::sync::watch` channel for each session actor,
created on first use, holding the newest seq that the actor reported through
`event-recorded` (§3.5). For a `wait` op:

1. Subscribe to the session's watch channel.
2. Evaluate `(event-read NAME max "wire")`. If it returned firings, or if
   `lost > 0`, respond.
3. Otherwise wait for the watch value to change, the timeout, or the session
   to stop. On a change, wait a short settle time (100 ms, so a burst comes
   back as one batch), then go to step 2.

The subscription comes before the read, so a firing recorded between the two
still changes the watch value: no wakeup is lost. The watch holds only the
newest value, so a burst of firings wakes the waiter once.

`follow` is a loop of `wait` ops. An idle `wait` or `follow` evaluates
nothing, so the per-second evaluation of today (§1, item 3) goes away.

A `wait` request keeps its TCP connection open while it waits. The transport
already handles one request per connection with no timeout, so no change is
needed. If the session stops, the daemon responds `ok: false` with
"session stopped", and the client exits 1.

When the daemon restarts (`upgrade`), the watch channels are gone, and the
actors' cursors are gone with the actors. That is the same as today.

### 4.5 Seqs restart with the actor

Seqs and cursors live in the actor's memory, so they last exactly as long as
the session actor. A new actor starts at seq 0 with no cursors. A client that
kept a seq from an earlier actor (only `--after N` does that) gets the
failure from §3.2. Cursors need no epoch, because they live in the actor and
start fresh with it.

## 5. The wake-on-message monitor

Today (`experiment/lib/wait-message`): a `follow` coproc, a 2-second window,
and a `kill`. After this proposal:

```sh
theater-repl wait <id>
```

run in the background, and run again after each wake. That is all:

- The firings that arrive while the agent is busy are still after the `wait`
  cursor, so the next `wait` returns at once with all of them.
- Two firings in the same burst come back together. A firing just after the
  batch is in the next `wait`.
- If the agent's harness kills `wait` after it printed but before it
  acknowledged, the next `wait` prints the firings again. Duplicates are
  possible, loss is not. `--seq` lets an agent see that it has handled a
  firing already.
- A stray reader of any other kind (a `follow`, a human `read`, the
  session's own `(poll-events)`) uses another cursor and cannot affect it.
- Two monitors on one session both use cursor `wait`. Both print every
  firing after the cursor (each wakes), and the cursor moves to the larger
  acknowledgement. Nothing is lost; at worst both report the same message.

`wait-message` becomes `exec theater-repl wait "$1" --seq`, kept so that
existing instructions keep working. The guide's "Your session" section
changes to `theater-repl wait <id>`.

## 6. Migration

Ordering: this lands **after 0015** (it changes the 0015 firing shape by
adding a seq, and reads its ring).

1. **Interpreter** (one PR): `$event-seq`, seq in firings, `events-after`,
   the cursor verbs, `(poll-events)` through cursor `poll`, the
   `event-recorded` call. Update `(help)` and `(help "triggers")`. Rebuild
   and commit `actor.wasm`.
2. **Daemon and CLI** (same PR, because the actor's new import must be
   provided by the daemon that runs it): `event-recorded` in `sessions.rs`
   and `sessions.pact`, the watch table, the `read`/`wait`/`ack`/`cursors`
   ops, the client verbs and options.
3. **Rollout on the shared daemon** is colin's call (`upgrade` restarts the
   daemon, and everyone's sessions with it):
   - An old actor in a new daemon: the daemon provides `event-recorded`, and
     the old actor does not import it, so there is no problem. A new-style
     request fails in the old actor with `unbound symbol: event-read`. The
     client then says "this session predates cursors; use `read`" and falls
     back to the old drain (`read` with no cursor).
   - A new actor in an old daemon: the actor fails to start, because
     `event-recorded` is missing. `set-default`'s probe catches this, so a
     new actor cannot become the default before the daemon supports it.
     Upgrade the daemon first.
   - Old clients against a new daemon: `read`/`follow` without `cursor` keep
     the old behavior (§4.2) and call `(poll-events)`, which now moves only
     cursor `poll`. So even an old `follow` no longer takes firings from a
     new `wait`.
4. **Shared library** (experiment, after rollout):
   - `wait-message` → `theater-repl wait` (§5); GUIDE.md "Your session" and
     "Everyday commands" updated, and the note that `read` drains removed.
   - `registry.wisp`: drop `registry-drain` and its timer. Optionally
     `(event-buffer 0)`, so the registry keeps no firings at all.
   - `github-events.wisp`: drop the `(poll-events)` in `ge-output`.
   - Tell the users of these helpers (all agents) in the coordinator's
     channel, as the guide asks for shared-helper changes.

## 7. Memory (`WISP-RETENTION.md`)

| New state | Bound | Rule |
|---|---|---|
| `$event-seq` | one s64 | — |
| seq in each firing | one value cell (24 bytes) per retained firing | inside 0015's ring bound (256, or 512 before a lazy trim) |
| `$cursors` | 16 entries of (name, seq, last-use) | **bounded**: least recently used removed |
| daemon watch channels | one per session with a reader | removed when the session stops (the daemon's notes already clean up this way in `Sessions::list`) |

The ring stays the only thing that holds firings, so 0015's policy
("bounded: ring buffer, keep event only if unhandled") is unchanged. A
handled `on-message` keeps only its result, so **an agent's `on-message`
must return the message text** (the guide's `(bytes->string (car e))` does).
A handler that returns something else leaves no record of the text in the
inbox. The inbox shows handler results, and that is on purpose: keeping the
raw byte list costs ~28 times the message size.

Removing per-second `follow` evaluations removes the largest garbage source
for a watched session until 0011 lands (§1, item 3).

## 8. Tests

Interpreter (`tests/interpreter_*.rs`, the probe harness that
`retention_measure.rs` uses to fire events through `dispatch-event`):

- Seqs: 5 firings get seqs 1–5; `(event-head)` is 5.
- `events-after` is non-destructive: two calls with the same N return the
  same firings.
- Two cursors: `event-read`/`event-ack` on `a` does not change what `b`
  reads.
- Ack is monotonic: ack 5 then ack 3 leaves 5; ack beyond head fails.
- Loss: limit 4, fire 10, `events-after 0` returns `(10 6 …)` with seqs
  7–10; a cursor at 8 returns `lost 0`.
- Lazy trim: with limit 4 and 7 firings (not yet trimmed), `lost` is still
  computed from the first retained seq.
- `max`: paging a full ring with `max 100` returns every firing exactly once.
- Cursor table: creating 17 cursors removes the least recently used one.
- `(poll-events)` returns 0015's shapes and does not change cursor `wait`.
- `(event-buffer 0)`: no firings kept, `lost` counts all of them.
- `"wire"` mode: first line is `next lost`.
- `event-recorded` is called once for each firing with the right seq (stub
  host that records calls).

Daemon (`cargo test --manifest-path wisp/actor/legacy-host/Cargo.toml --test
theater_host`, on its own port):

- `wait` returns at once when firings are after the cursor; blocks when not,
  and returns within the settle time after a message is sent to the session.
- `wait --timeout 1` exits 2 with no output.
- **The incident:** a `follow` (cursor `follow`) running while a message
  arrives; `wait` (cursor `wait`) still returns that message.
- **The `head -n 1` case:** two messages sent together; `wait | head -n 1`;
  the next `wait` returns the second message (and the first again, since the
  closed pipe meant no ack).
- Two concurrent `wait`s both return the message.
- Kill the client between print and ack (a test hook that exits before the
  `ack` request): the next `wait` returns the same firings.
- An old-style `read` (no cursor field) still drains via `(poll-events)`.
- The session stops while a `wait` is blocked: it returns `ok: false`.
- No evaluation happens while a `wait` is idle (count `evaluate` calls on the
  session over 3 idle seconds: zero after the first read).

## 9. Decisions from review

Settled with claude (coordinator) in review:

1. **A new cursor starts at 0** (the whole ring), not at the head. The
   window between `(msg-register)` and the first `wait` matters more than a
   long first `read` on a busy session (up to 256 old firings, as `read`
   prints today).
2. **Plain `read` acknowledges**, so a person who runs `read` twice sees
   each firing once, as today. `--peek` reads without acknowledging.
3. **No sender in `on-message`.** colin decided messages carry no sender;
   an actor that wants a reply puts its own id in the message, as the
   current convention does. Out of scope here.

## 10. Implementation notes (ticket 0023)

Built on branch `session-inbox`. Where it differs from the sections above:

- **A seq is not stored in each firing.** The buffer always holds consecutive
  seqs ending at `$event-seq`, so a firing's seq is its position plus the
  first seq (`event-first-seq`). The firing shapes stay exactly as in 0015;
  the seq is added only when a firing is returned.
- **`event-recorded` is in its own interface, `wisp-events`**
  (`wisp/actor/events.pact`), not added to `wisp-sessions` (§3.5). Adding it
  there would change the `wisp-sessions` interface hash, so an old actor would
  no longer load in a new daemon. With a separate interface, an old actor
  works in a new daemon, and a new actor in an old daemon fails at start, as
  §6 planned.
- **No `next` field in the protocol** (§4.2). The daemon passes on the
  interpreter's `"wire"` text, and the client reads `next lost` from its first
  line. The transport is unchanged.
- **`--seq` was dropped** (§4.1): a multi-line result breaks the line to
  firing mapping. `--full` shows the seqs.
- **`events-dropped` in `(memory-stats)` is cumulative**, and with buffering
  off (`(event-buffer 0)`) each firing still takes a seq and is reported to
  readers as dropped (§3.1 said this; it changes one 0015 test).
- `wait` checks every 5 s whether the session still exists. A client that
  disconnects is noticed only when the next firing (or the timeout) ends the
  wait; until then the daemon holds one idle task for it.

## 11. Problems noticed outside this ticket

- Theater: `respond-to-request` / `cancel-request` read an
  `outstanding_requests` map that `process_actor_message` never fills
  (ms:540, ms:1013-1060), so a deferred reply appears not to work.
- Theater (**known, deliberately deferred**): one full mailbox (100
  messages) stalls the single router task for every actor (ms:278), and
  channel open waits for the target's `handle-channel-open` inside the
  router (ms:325).
