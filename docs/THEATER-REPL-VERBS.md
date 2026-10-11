# theater-repl Host Verbs

A reference of the Theater host verbs exposed to the Wisp REPL by
`wisp/interpreter/rpc.wisp`. Each verb evaluates its string/value arguments, calls
the matching Theater `rpc` host import, and hands the result back as an ordinary,
inspectable Wisp value. Results generally come back as a `(ok ...)` / `(err ...)`
value you can pattern-match on.

This is generated strictly from what `rpc.wisp` actually dispatches
(`host-builtin?` + `apply-host-builtin`); it invents nothing.

## self / rpc

Identity, logging, and cross-actor calls. (`exports`, `implements`, and `call`
error when targeted at `(self)` — a self-directed blocking RPC would deadlock the
session; use `(describe (self))` for your own exports.)

| Verb | Description |
|------|-------------|
| `(self)` | This actor's own id. |
| `(log msg)` | Write a log line to Theater. |
| `(describe actor-id)` | Describe an actor (safe on self; served from metadata). |
| `(exports actor-id)` | List the actor's exported interfaces/functions. |
| `(implements actor-id interface)` | Whether the actor implements `interface`. |
| `(call actor-id function arg ...)` | Call `function` on another actor with the args as params. |

## store

Content-addressed persistence.

| Verb | Description |
|------|-------------|
| `(store-new)` | Create a new store; returns its id. |
| `(store-put store-id content)` | Store content; returns its content ref. |
| `(store-get store-id content-ref)` | Fetch content by ref. |
| `(store-get-by-label store-id label)` | Fetch the content ref bound to a label. |
| `(store-label store-id label content-ref)` | Bind a label to a content ref. |
| `(store-list-labels store-id)` | List all labels in the store. |
| `(store-exists store-id content-ref)` | Whether a content ref exists. |
| `(store-size store-id)` | Total size of the store in bytes. |
| `(store-put-at store-id label content)` | Store content and bind it to a label in one call. |

## runtime

The actor-management control surface.

| Verb | Description |
|------|-------------|
| `(list-actors)` | List all actors (id, name, parent-id). |
| `(actor-status id)` | An actor's status string. |
| `(actor-state id)` | An actor's serialized state bytes (errors on self). |
| `(actor-manifest id)` | An actor's manifest. |
| `(stop-actor id)` | Gracefully stop an actor. |
| `(kill-actor id)` | Forcibly kill an actor. |
| `(subscribe-spawns)` | Subscribe to spawn events (delivered to `on-spawn`). |
| `(unsubscribe-spawns)` | Stop receiving spawn events. |
| `(shutdown-runtime)` | Shut down the Theater runtime. |

## messaging

Inter-actor messaging (`theater:simple/message-server-host`).

| Verb | Description |
|------|-------------|
| `(msg-register)` | Register this actor as a message server (delivers to `on-message`). |
| `(msg-send actor-id message)` | Fire-and-forget send to another actor. |
| `(msg-request actor-id message)` | Send a request and return the response bytes. |
| `(msg-list-requests)` | List outstanding request ids awaiting a response. |
| `(msg-respond request-id response)` | Respond to an outstanding request. |
| `(msg-cancel request-id)` | Cancel an outstanding request. |
| `(msg-open actor-id initial-message)` | Open a channel; returns the channel id. |
| `(msg-send-channel channel-id message)` | Send a message on an open channel. |
| `(msg-close-channel channel-id)` | Close a channel. |

## files

Filesystem access (`theater:simple/filesystem`).

| Verb | Description |
|------|-------------|
| `(fs-read path)` | Read a file's bytes. |
| `(fs-write path content)` | Write content to a file. |
| `(fs-append path content)` | Append content to a file. |
| `(fs-exists path)` | Whether a path exists. |
| `(fs-list path)` | List a directory's entries (name, is-dir). |
| `(fs-meta path)` | File metadata (size, is-dir, read-only). |
| `(fs-delete path)` | Delete a file. |
| `(fs-mkdir path)` | Create a directory. |
| `(fs-rmdir path)` | Remove a directory. |

## http

Outbound HTTP (`theater:simple/http-client`).

| Verb | Description |
|------|-------------|
| `(http-get url)` | Perform a GET request. |
| `(http-req method url)` | Perform a request with an explicit method. |

## assembler

| Verb | Description |
|------|-------------|
| `(wat-to-wasm wat-text)` | Compile WAT text to a wasm byte vector (`wisp:assembler/runtime`). |

## timer

| Verb | Description |
|------|-------------|
| `(now)` | Read the clock (u64). |
| `(set-interval name interval-ms)` | Start a repeating tick (delivered to `on-tick`). |
| `(clear-interval name)` | Stop a named interval. |

## terminal

Stdio and tty control.

| Verb | Description |
|------|-------------|
| `(term-write text)` | Write to stdout; returns bytes written. |
| `(term-write-err text)` | Write to stderr; returns bytes written. |
| `(term-raw bool)` | Enable or disable raw mode. |
| `(term-size)` | Terminal size as a (cols, rows) tuple. |
| `(term-input)` | Enable terminal input. |

## tcp

Raw sockets (`theater:simple/tcp`); works both outbound and inbound.

| Verb | Description |
|------|-------------|
| `(tcp-connect address)` | Connect to `address`; returns a connection id. |
| `(tcp-listen address)` | Listen on `address`; returns a listener id (fires `on-connection`). |
| `(tcp-accept listener-id)` | Accept a connection on a listener. |
| `(tcp-activate connection-id)` | Activate a pending connection (required before reading). |
| `(tcp-set-active connection-id mode)` | Set a connection's active mode (e.g. `"active"`). |
| `(tcp-transfer connection-id target-actor)` | Transfer a connection to another actor. |
| `(tcp-transfer-async connection-id target-actor)` | Transfer a connection asynchronously. |
| `(tcp-peer connection-id)` | The connection's peer address. |
| `(tcp-is-tls connection-id)` | Whether the connection is TLS. |
| `(tcp-send connection-id data)` | Send data (string or byte list); returns bytes sent. |
| `(tcp-receive connection-id max-bytes)` | Receive up to `max-bytes` bytes. |
| `(tcp-close connection-id)` | Close a connection. |
| `(tcp-close-listener listener-id)` | Close a listener. |
| `(tcp-tls-server connection-id)` | Upgrade a connection to a TLS server. |
| `(tcp-tls-client connection-id server-name)` | Upgrade a connection to a TLS client. |

## podman

Container management (`theater:simple/podman`).

| Verb | Description |
|------|-------------|
| `(podman-run image name [env [cmd [mounts]]])` | Run a container; returns its id. `env`=`((KEY VAL) ...)`, `cmd`=`(arg ...)`, `mounts`=`((SRC TGT [read-only]) ...)`. |
| `(podman-stop name)` | Stop a container. |
| `(podman-rm name force)` | Remove a container (`force` is a boolean). |
| `(podman-list)` | List containers (id, name, image, status, exit-code). |

## lifecycle

Attach a relationship to another actor (the "subject"), self-service.

| Verb | Description |
|------|-------------|
| `(monitor id)` | Watch an actor's chain; events delivered to `on-actor-event`. |
| `(unmonitor id)` | Stop monitoring an actor. |
| `(link id)` | Link to an actor (fate-sharing, no callback). |
| `(unlink id)` | Remove a link. |

## session / events

| Verb | Description |
|------|-------------|
| `(poll-events)` | Drain the buffer of inbound trigger firings (`(handler-name event result)` each) since the last poll. |
| `(help [topic])` | Print the live verb catalog, or deeper docs for `"tcp"` / `"triggers"`. |

## Inbound triggers

Theater delivers inbound events by calling a handler you *define by name* in the live
session; defining it is all it takes, and firings buffer until `(poll-events)`.

| Handler | Fires after |
|---------|-------------|
| `on-tick` | `(set-interval name ms)` |
| `on-spawn` | `(subscribe-spawns)` |
| `on-message` | `(msg-register)` |
| `on-connection` | `(tcp-listen ...)` |
| `on-data` | TCP active-mode read |
| `on-close` | TCP connection closed |
| `on-actor-event` | `(monitor id)` |
