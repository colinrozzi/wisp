//! What each actor is doing, kept by the daemon from a global chain
//! subscription, for `theater-repl inspect`. It is answered from here, not by
//! the session, so a session that is busy or stuck in a long evaluation still
//! reports, and the guest call it is stuck in shows as `busy`.
//!
//! Only counts, names and hashes are kept: message payloads and file contents
//! never leave the drain task, and an error is its first line, cut short.
//!
//! Chain delivery is strict: the actor waits for each subscriber to take each
//! event (theater `docs/messaging-and-lifecycle-behavior.md` §3.1). So the
//! drain task looks at the event type first and decodes only the few it reads
//! (guest calls, messages sent, timers, TCP and `.wisp` file reads), keeps what
//! it needs under a short lock, and never waits on anything else.
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::sync::{Arc, Mutex};
use std::time::Instant;
use theater::TheaterId;
use theater::chain::ChainEvent;
use theater::events::wasm::WasmEventData;
use theater::events::{ChainEventPayload, decode_chain_event_payload};
use theater::pack_bridge::Value;
use tokio::sync::mpsc;

/// `.wisp` files remembered per actor.
pub const FILES: usize = 256;
/// Open handles of one kind remembered per actor.
pub const HANDLES: usize = 1024;
/// Host calls kept per actor for `chain-tail`, and the most bytes of their
/// events kept in all; a call whose event is larger than [`CALL_BYTES`] keeps
/// only its size.
pub const CALLS: usize = 64;
pub const TAIL_BYTES: usize = 1 << 20;
pub const CALL_BYTES: usize = 64 << 10;
/// Characters of an error kept.
const ERROR_CHARS: usize = 160;

/// A `.wisp` file the actor read, as it was when it was read.
#[derive(Clone, Debug)]
pub struct File {
    pub sha256: String,
    pub bytes: usize,
    pub at: Instant,
}

/// Messages in and out over the last minute, in one-second slots.
#[derive(Clone, Debug, Default)]
struct Window {
    slots: Vec<(u64, u64, u64)>,
}

impl Window {
    fn add(&mut self, second: u64, inbound: u64, outbound: u64) {
        if self.slots.is_empty() {
            self.slots = vec![(u64::MAX, 0, 0); 60];
        }
        let slot = &mut self.slots[(second % 60) as usize];
        if slot.0 != second {
            *slot = (second, 0, 0);
        }
        slot.1 += inbound;
        slot.2 += outbound;
    }

    /// (in, out) over the 60 seconds up to and including `second`.
    fn minute(&self, second: u64) -> (u64, u64) {
        self.slots
            .iter()
            .filter(|(s, _, _)| *s != u64::MAX && *s <= second && second - s < 60)
            .fold((0, 0), |(i, o), (_, a, b)| (i + a, o + b))
    }
}

/// One actor's health.
#[derive(Clone, Debug, Default)]
pub struct Actor {
    /// Chain events seen.
    pub events: u64,
    /// Guest calls (evaluations and handler calls) begun.
    pub calls: u64,
    /// The guest call running now, and when it began.
    pub busy: Option<(String, Instant)>,
    pub msgs_in: u64,
    pub msgs_out: u64,
    window: Window,
    /// Failed guest calls: an evaluation or handler that answered `error: …`,
    /// or a trap.
    pub errors: u64,
    pub last_error: Option<(String, Instant)>,
    /// Open handles by kind ("timer", "listener", "connection"), by id.
    pub handles: BTreeMap<&'static str, BTreeSet<String>>,
    /// `.wisp` files read, by path.
    pub files: BTreeMap<String, File>,
    /// The newest host calls, oldest first, still encoded: they are decoded
    /// only when read ([`HostCall::decode`]).
    pub tail: VecDeque<HostCall>,
    tail_bytes: usize,
}

/// One host call as its chain event holds it.
#[derive(Clone, Debug)]
pub struct HostCall {
    /// `INTERFACE/FUNCTION`.
    pub event_type: String,
    /// The event's payload, or None when it was larger than [`CALL_BYTES`].
    pub data: Option<Vec<u8>>,
    pub bytes: usize,
    pub at: Instant,
}

impl HostCall {
    /// The call's input and output.
    pub fn decode(&self) -> Option<(Value, Value)> {
        match decode_chain_event_payload(self.data.as_deref()?) {
            Some(ChainEventPayload::HostFunction(call)) => Some((call.input, call.output)),
            _ => None,
        }
    }
}

impl Actor {
    /// Messages (in, out) over the last minute, as of `now`.
    pub fn per_minute(&self, epoch: Instant, now: Instant) -> (u64, u64) {
        self.window.minute(now.duration_since(epoch).as_secs())
    }

    fn open(&mut self, kind: &'static str, id: String) {
        let ids = self.handles.entry(kind).or_default();
        if !id.is_empty() && ids.len() < HANDLES {
            ids.insert(id);
        }
    }

    fn close(&mut self, kind: &'static str, id: &str) {
        if let Some(ids) = self.handles.get_mut(kind) {
            ids.remove(id);
        }
    }

    fn called(&mut self, call: HostCall) {
        self.tail_bytes += call.data.as_ref().map_or(0, Vec::len);
        self.tail.push_back(call);
        while self.tail.len() > CALLS || self.tail_bytes > TAIL_BYTES {
            let Some(old) = self.tail.pop_front() else {
                break;
            };
            self.tail_bytes -= old.data.as_ref().map_or(0, Vec::len);
        }
    }

    fn failed(&mut self, error: &str, at: Instant) {
        self.errors += 1;
        let line = error.lines().next().unwrap_or("");
        self.last_error = Some((line.chars().take(ERROR_CHARS).collect(), at));
    }
}

/// Every actor's health. Cloning shares the table.
#[derive(Clone)]
pub struct Health {
    epoch: Instant,
    table: Arc<Mutex<HashMap<TheaterId, Actor>>>,
}

impl Default for Health {
    fn default() -> Self {
        Self {
            epoch: Instant::now(),
            table: Arc::default(),
        }
    }
}

impl Health {
    fn table(&self) -> std::sync::MutexGuard<'_, HashMap<TheaterId, Actor>> {
        self.table.lock().unwrap_or_else(|p| p.into_inner())
    }

    /// A copy of `id`'s health, if any event of it has been seen.
    pub fn get(&self, id: &TheaterId) -> Option<Actor> {
        self.table().get(id).cloned()
    }

    /// Messages (in, out) over the last minute.
    pub fn per_minute(&self, actor: &Actor) -> (u64, u64) {
        actor.per_minute(self.epoch, Instant::now())
    }

    /// The channel to pass to `add_global_subscription`; a thread of its own
    /// drains it into the table. Not a tokio task: an actor sends a guest
    /// call's event and then runs the call on the same worker without
    /// yielding, and a task that send wakes waits in that worker's LIFO slot,
    /// which other workers don't steal from, until the call ends. Then a long
    /// evaluation's start and end arrived together and it never showed as
    /// busy (1 run in 5 on two cores; tests/daemon.rs
    /// inspect_reports_from_the_daemon_even_while_the_session_is_busy).
    pub fn subscription(&self) -> mpsc::Sender<(TheaterId, ChainEvent)> {
        let (tx, mut rx) = mpsc::channel::<(TheaterId, ChainEvent)>(1024);
        let health = self.clone();
        std::thread::Builder::new()
            .name("health".into())
            .spawn(move || {
                while let Some((id, event)) = rx.blocking_recv() {
                    health.observe(id, &event, Instant::now());
                }
            })
            .expect("spawn the health thread");
        tx
    }

    /// Take one chain event of `id`.
    pub fn observe(&self, id: TheaterId, event: &ChainEvent, now: Instant) {
        if event.event_type == "terminated" {
            self.table().remove(&id);
            return;
        }
        let seen = Seen::of(event);
        // Host calls are `INTERFACE/FUNCTION`; guest calls and lifecycle
        // events have no slash.
        let call = event.event_type.contains('/').then(|| HostCall {
            event_type: event.event_type.clone(),
            data: (event.data.len() <= CALL_BYTES).then(|| event.data.clone()),
            bytes: event.data.len(),
            at: now,
        });
        let second = now.duration_since(self.epoch).as_secs();
        let mut table = self.table();
        let actor = table.entry(id).or_default();
        actor.events += 1;
        if let Some(call) = call {
            actor.called(call);
        }
        match seen {
            Seen::Nothing => {}
            Seen::Call(function) => {
                actor.calls += 1;
                if is_inbound_message(&function) {
                    actor.msgs_in += 1;
                    actor.window.add(second, 1, 0);
                }
                actor.busy = Some((function, now));
            }
            Seen::Connection(function, id, opened) => {
                actor.calls += 1;
                if opened {
                    actor.open("connection", id);
                } else {
                    actor.close("connection", &id);
                }
                actor.busy = Some((function, now));
            }
            Seen::Opened(kind, handle) => actor.open(kind, handle),
            Seen::Closed(kind, handle) => actor.close(kind, &handle),
            Seen::Done(error) => {
                actor.busy = None;
                if let Some(error) = error {
                    actor.failed(&error, now);
                }
            }
            Seen::Sent => {
                actor.msgs_out += 1;
                actor.window.add(second, 0, 1);
            }
            Seen::File(path, file) => {
                if actor.files.len() < FILES || actor.files.contains_key(&path) {
                    actor.files.insert(path, File { at: now, ..file });
                }
            }
        }
    }
}

/// What one event says, decoded outside the lock.
enum Seen {
    Nothing,
    /// A guest call began.
    Call(String),
    /// A guest call began that tells of a TCP connection: accepted (true) or
    /// closed by the peer.
    Connection(String, String, bool),
    /// A guest call ended, with its error if it failed.
    Done(Option<String>),
    Opened(&'static str, String),
    Closed(&'static str, String),
    Sent,
    File(String, File),
}

/// Guest exports a message arrives through.
fn is_inbound_message(function: &str) -> bool {
    ["handle-send", "handle-request", "handle-channel-message"]
        .iter()
        .any(|name| function.ends_with(name))
}

impl Seen {
    fn of(event: &ChainEvent) -> Seen {
        let ty = event.event_type.as_str();
        let host = match ty {
            "wasm" => return Self::wasm(event),
            "theater:simple/message-server-host/send"
            | "theater:simple/message-server-host/request"
            | "theater:simple/message-server-host/send-on-channel" => return Seen::Sent,
            "theater:simple/timer/set-interval"
            | "theater:simple/timer/clear-interval"
            | "theater:simple/tcp/listen"
            | "theater:simple/tcp/close-listener"
            | "theater:simple/tcp/connect"
            | "theater:simple/tcp/close"
            | "theater:simple/tcp/transfer"
            | "theater:simple/tcp/transfer-async"
            | "theater:simple/filesystem/read-file" => {
                match decode_chain_event_payload(&event.data) {
                    Some(ChainEventPayload::HostFunction(call)) => call,
                    _ => return Seen::Nothing,
                }
            }
            _ => return Seen::Nothing,
        };
        let input = first_string(&host.input).unwrap_or_default();
        match ty {
            "theater:simple/timer/set-interval" if is_ok(&host.output) => {
                Seen::Opened("timer", input)
            }
            "theater:simple/timer/clear-interval" => Seen::Closed("timer", input),
            "theater:simple/tcp/listen" => match ok_string(&host.output) {
                Some(id) => Seen::Opened("listener", id),
                None => Seen::Nothing,
            },
            "theater:simple/tcp/close-listener" => Seen::Closed("listener", input),
            "theater:simple/tcp/connect" => match ok_string(&host.output) {
                Some(id) => Seen::Opened("connection", id),
                None => Seen::Nothing,
            },
            "theater:simple/tcp/close"
            | "theater:simple/tcp/transfer"
            | "theater:simple/tcp/transfer-async" => Seen::Closed("connection", input),
            "theater:simple/filesystem/read-file" if input.ends_with(".wisp") => {
                match ok_bytes(&host.output) {
                    Some(bytes) => Seen::File(
                        input,
                        File {
                            sha256: sha256_hex(&bytes),
                            bytes: bytes.len(),
                            at: Instant::now(),
                        },
                    ),
                    None => Seen::Nothing,
                }
            }
            _ => Seen::Nothing,
        }
    }

    fn wasm(event: &ChainEvent) -> Seen {
        match decode_chain_event_payload(&event.data) {
            Some(ChainEventPayload::Wasm(data)) => match data {
                WasmEventData::WasmCall {
                    function_name,
                    params,
                } => {
                    // A connection a listener accepted, or one closed by the peer.
                    let opened = function_name.ends_with("tcp-client.handle-connection")
                        || function_name.ends_with("tcp-client.handle-connection-transfer");
                    if (opened || function_name.ends_with("tcp-client.on-close"))
                        && let Some(id) = first_string(&params)
                    {
                        return Seen::Connection(function_name, id, opened);
                    }
                    Seen::Call(function_name)
                }
                WasmEventData::WasmResult { response, .. } => {
                    Seen::Done(first_string(&response).filter(|s| s.starts_with("error:")))
                }
                WasmEventData::WasmError { message, .. } => Seen::Done(Some(message)),
                WasmEventData::WasmComponentCreationError { error } => Seen::Done(Some(error)),
            },
            _ => Seen::Nothing,
        }
    }
}

/// The first string in `v`, depth first (a host call's input is a tuple of
/// its parameters).
fn first_string(v: &Value) -> Option<String> {
    match v {
        Value::String(s) => Some(s.clone()),
        Value::Tuple(items) | Value::List { items, .. } => items.first().and_then(first_string),
        Value::Option { value, .. } => value.as_deref().and_then(first_string),
        Value::Result { value: Ok(v), .. } => first_string(v),
        _ => None,
    }
}

fn is_ok(v: &Value) -> bool {
    match v {
        Value::Result { value, .. } => value.is_ok(),
        Value::Tuple(items) if items.len() == 1 => is_ok(&items[0]),
        _ => true,
    }
}

fn ok_string(v: &Value) -> Option<String> {
    is_ok(v).then(|| first_string(v)).flatten()
}

/// The bytes of an ok `list<u8>` result.
fn ok_bytes(v: &Value) -> Option<Vec<u8>> {
    match v {
        Value::Result { value: Ok(v), .. } => ok_bytes(v),
        Value::Result { value: Err(_), .. } => None,
        Value::Tuple(items) if items.len() == 1 => ok_bytes(&items[0]),
        Value::List { items, .. } => items
            .iter()
            .map(|item| match item {
                Value::U8(b) => Some(*b),
                _ => None,
            })
            .collect(),
        _ => None,
    }
}

/// The sha256 of `data`, in lowercase hex, as `sha256sum` prints it.
pub fn sha256_hex(data: &[u8]) -> String {
    Sha256::digest(data)
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect()
}
