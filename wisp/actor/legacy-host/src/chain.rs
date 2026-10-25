//! The `wisp-chain` interface: the daemon's chain recorder. Theater hashes each
//! event an actor handles into a chain, sends it to the chain's subscribers and
//! keeps only the head hash (theater `crates/theater/src/chain/mod.rs`). The
//! recorder subscribes to the actors it is asked to watch and keeps the newest
//! [`RING`] events of each, without their payloads, so a session can read an
//! actor's chain (`chain-watch`, `chain-head`, `chain-tail`, `chain-verify`).
//!
//! Chain delivery is strict: the actor waits for each subscriber to take each
//! event (theater `docs/messaging-and-lifecycle-behavior.md` §3.1). So one task
//! drains every subscription at once, keeps what it needs under a short lock,
//! and never waits on the runtime. Host functions only read the table, apart
//! from `watch`, which asks the runtime whether the actor is running.
//!
//! Recording stops at the actor's `terminated` event. A stopped actor's ring is
//! kept, so its last events can still be read, for the newest [`STOPPED`]
//! stopped actors.
use anyhow::Result;
use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex};
use theater::TheaterId;
use theater::chain::ChainEvent;
use theater::handler::{Handler, HandlerContext};
use theater::messages::TheaterCommand;
use theater::pack_bridge::{
    HostImports, InterfaceImpl, Value, ValueType, pact_result_host_fn, parse_pact,
};
use tokio::sync::mpsc::{self, UnboundedSender};
use tokio::sync::oneshot;

/// Events kept per actor.
pub const RING: usize = 256;
/// Stopped actors whose rings are kept.
pub const STOPPED: usize = 16;
/// Running actors that can be watched at once.
pub const WATCHED: usize = 64;

/// One event, without its payload.
#[derive(Clone, Debug)]
pub struct Entry {
    pub hash: Vec<u8>,
    pub parent: Option<Vec<u8>>,
    pub event_type: String,
    pub data_bytes: usize,
}

/// One watched actor.
#[derive(Debug, Default)]
pub struct Recording {
    /// The newest [`RING`] events, oldest first.
    pub ring: VecDeque<Entry>,
    /// Events seen since the watch began.
    pub count: u64,
    /// Events whose parent was not the hash of the event seen before them.
    pub breaks: u64,
    /// The first event seen had no parent: recording began at the root.
    pub from_root: bool,
    /// False once the actor's `terminated` event has arrived.
    pub recording: bool,
}

impl Recording {
    fn push(&mut self, event: &ChainEvent) {
        match self.ring.back() {
            Some(last) => {
                if event.parent_hash.as_deref() != Some(&last.hash[..]) {
                    self.breaks += 1;
                }
            }
            None if self.count == 0 => self.from_root = event.parent_hash.is_none(),
            None => {}
        }
        if self.ring.len() == RING {
            self.ring.pop_front();
        }
        self.ring.push_back(Entry {
            hash: event.hash.clone(),
            parent: event.parent_hash.clone(),
            event_type: event.event_type.clone(),
            data_bytes: event.data.len(),
        });
        self.count += 1;
    }
}

#[derive(Default)]
struct Table {
    actors: HashMap<TheaterId, Recording>,
    /// Stopped actors, oldest stop first.
    stopped: VecDeque<TheaterId>,
}

impl Table {
    fn stop(&mut self, id: TheaterId) {
        if let Some(recording) = self.actors.get_mut(&id)
            && recording.recording
        {
            recording.recording = false;
            self.stopped.push_back(id);
            while self.stopped.len() > STOPPED {
                if let Some(old) = self.stopped.pop_front() {
                    self.actors.remove(&old);
                }
            }
        }
    }

    fn forget(&mut self, id: &TheaterId) {
        self.actors.remove(id);
        self.stopped.retain(|other| other != id);
    }
}

/// The recorder. Cloning shares the table and the subscription channel.
#[derive(Clone)]
pub struct ChainRecorder {
    commands: UnboundedSender<TheaterCommand>,
    table: Arc<Mutex<Table>>,
    /// Every watched chain sends here; the drain task receives.
    events: mpsc::Sender<(TheaterId, ChainEvent)>,
}

impl ChainRecorder {
    /// A recorder and its drain task. Needs a tokio runtime.
    pub fn new(commands: UnboundedSender<TheaterCommand>) -> Self {
        let (events, mut rx) = mpsc::channel::<(TheaterId, ChainEvent)>(1024);
        let table: Arc<Mutex<Table>> = Arc::default();
        let drained = table.clone();
        tokio::spawn(async move {
            while let Some((id, event)) = rx.recv().await {
                let mut table = drained.lock().unwrap_or_else(|p| p.into_inner());
                let Some(recording) = table.actors.get_mut(&id) else {
                    continue; // unwatched; the unsubscribe is on its way
                };
                if !recording.recording {
                    continue;
                }
                recording.push(&event);
                if event.event_type == "terminated" {
                    table.stop(id);
                }
            }
        });
        Self {
            commands,
            table,
            events,
        }
    }

    fn table(&self) -> std::sync::MutexGuard<'_, Table> {
        self.table.lock().unwrap_or_else(|p| p.into_inner())
    }

    async fn running(&self, id: TheaterId) -> Result<bool> {
        let (response_tx, response_rx) = oneshot::channel();
        self.commands.send(TheaterCommand::GetActorHandle {
            actor_id: id,
            response_tx,
        })?;
        Ok(response_rx.await?.is_some())
    }

    /// Start recording `id`. Watching an actor already being recorded does
    /// nothing; watching a stopped recording again starts a fresh one.
    pub async fn watch(&self, id: TheaterId) -> Result<(), String> {
        {
            let mut table = self.table();
            if table.actors.get(&id).is_some_and(|r| r.recording) {
                return Ok(());
            }
            let running = table.actors.values().filter(|r| r.recording).count();
            if running >= WATCHED {
                return Err(format!("already watching {WATCHED} actors"));
            }
            table.forget(&id);
            table.actors.insert(
                id,
                Recording {
                    recording: true,
                    ..Recording::default()
                },
            );
        }
        let error = |e: anyhow::Error| format!("{e:#}");
        // SubscribeToActor has no reply, and ignores an actor that isn't running.
        // The runtime handles commands in order, so if the actor is still there
        // when the next command is answered, the subscription took, and its
        // `terminated` event will arrive.
        self.commands
            .send(TheaterCommand::SubscribeToActor {
                actor_id: id,
                event_tx: self.events.clone(),
            })
            .map_err(|e| error(e.into()))?;
        if self.running(id).await.map_err(error)? {
            Ok(())
        } else {
            self.table().forget(&id);
            Err(format!("actor {id} is not running"))
        }
    }

    /// Stop recording `id` and drop its ring.
    pub fn unwatch(&self, id: TheaterId) -> Result<(), String> {
        let was = {
            let mut table = self.table();
            table.stopped.retain(|other| *other != id);
            table.actors.remove(&id)
        };
        match was {
            None => Err(format!("not watching {id}")),
            Some(recording) => {
                if recording.recording {
                    let _ = self.commands.send(TheaterCommand::UnsubscribeFromActor {
                        actor_id: id,
                        event_tx: self.events.clone(),
                    });
                }
                Ok(())
            }
        }
    }

    /// Run `f` on `id`'s recording.
    pub fn with<T>(&self, id: TheaterId, f: impl FnOnce(&Recording) -> T) -> Result<T, String> {
        self.table()
            .actors
            .get(&id)
            .map(f)
            .ok_or_else(|| format!("not watching {id} (chain-watch it first)"))
    }
}

pub fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn count(n: impl TryInto<i32>) -> Value {
    Value::S32(n.try_into().unwrap_or(i32::MAX))
}

fn record(name: &str, fields: Vec<(&str, Value)>) -> Value {
    Value::Record {
        type_name: name.to_string(),
        fields: fields
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect(),
    }
}

fn entry_value(entry: &Entry) -> Value {
    record(
        "chain-event",
        vec![
            ("hash", Value::String(hex(&entry.hash))),
            (
                "parent",
                Value::String(entry.parent.as_deref().map(hex).unwrap_or_default()),
            ),
            ("event-type", Value::String(entry.event_type.clone())),
            ("data-bytes", count(entry.data_bytes)),
        ],
    )
}

fn head_value(r: &Recording) -> Value {
    record(
        "chain-head",
        vec![
            (
                "hash",
                Value::String(r.ring.back().map(|e| hex(&e.hash)).unwrap_or_default()),
            ),
            ("count", count(r.count)),
            ("recording", Value::Bool(r.recording)),
        ],
    )
}

fn check_value(r: &Recording) -> Value {
    let kept = r.ring.len() as u64;
    record(
        "chain-check",
        vec![
            ("ok", Value::Bool(r.breaks == 0)),
            ("count", count(r.count)),
            ("kept", count(kept)),
            ("dropped", count(r.count - kept)),
            ("breaks", count(r.breaks)),
            ("from-root", Value::Bool(r.from_root)),
        ],
    )
}

fn tail_value(r: &Recording, n: usize) -> Value {
    let skip = r.ring.len().saturating_sub(n);
    Value::List {
        elem_type: ValueType::Record("chain-event".to_string()),
        items: r.ring.iter().skip(skip).map(entry_value).collect(),
    }
}

/// A single-string call arrives bare or as a one-element tuple.
fn actor_arg(input: &Value) -> Result<TheaterId, Value> {
    let text = match input {
        Value::String(s) => s,
        Value::Tuple(items) if !items.is_empty() => match &items[0] {
            Value::String(s) => s,
            _ => return Err(Value::String("expected an actor id".into())),
        },
        _ => return Err(Value::String("expected an actor id".into())),
    };
    TheaterId::parse(text).map_err(|_| Value::String(format!("not an actor id: {text}")))
}

fn n_arg(input: &Value) -> Result<usize, Value> {
    let n = match input {
        Value::Tuple(items) if items.len() == 2 => match &items[1] {
            Value::S32(n) => i64::from(*n),
            Value::S64(n) => *n,
            Value::U32(n) => i64::from(*n),
            Value::U64(n) => i64::try_from(*n).unwrap_or(i64::MAX),
            _ => -1,
        },
        _ => -1,
    };
    usize::try_from(n).map_err(|_| Value::String("expected a count n >= 0".into()))
}

impl Handler for ChainRecorder {
    fn create_instance(
        &self,
        _: Option<&theater::config::actor_manifest::HandlerConfig>,
    ) -> Box<dyn Handler> {
        Box::new(self.clone())
    }
    fn name(&self) -> &str {
        "wisp-chain"
    }
    fn imports(&self) -> Option<Vec<String>> {
        Some(vec!["wisp-chain".into()])
    }
    fn exports(&self) -> Option<Vec<String>> {
        None
    }
    fn supports_composite(&self) -> bool {
        true
    }
    fn interfaces(&self) -> Vec<InterfaceImpl> {
        vec![InterfaceImpl::from_pact(
            &parse_pact(include_str!("../../chain.pact")).expect("chain interface"),
        )]
    }
    fn register_host_functions(
        &mut self,
        imports: &mut HostImports,
        ctx: &mut HandlerContext,
    ) -> Result<()> {
        if ctx.is_satisfied("wisp-chain") {
            return Ok(());
        }
        let unit = || Value::Tuple(vec![]);
        let recorder = self.clone();
        imports.define(
            "wisp-chain",
            "watch",
            pact_result_host_fn(move |input| {
                let recorder = recorder.clone();
                async move {
                    let id = actor_arg(&input)?;
                    recorder.watch(id).await.map_err(Value::String)?;
                    Ok(unit())
                }
            }),
        );
        let recorder = self.clone();
        imports.define(
            "wisp-chain",
            "unwatch",
            pact_result_host_fn(move |input| {
                let result = actor_arg(&input)
                    .and_then(|id| recorder.unwatch(id).map_err(Value::String))
                    .map(|_| unit());
                async move { result }
            }),
        );
        let recorder = self.clone();
        imports.define(
            "wisp-chain",
            "head",
            pact_result_host_fn(move |input| {
                let result = actor_arg(&input)
                    .and_then(|id| recorder.with(id, head_value).map_err(Value::String));
                async move { result }
            }),
        );
        let recorder = self.clone();
        imports.define(
            "wisp-chain",
            "tail",
            pact_result_host_fn(move |input| {
                let result = actor_arg(&input).and_then(|id| {
                    let n = n_arg(&input)?;
                    recorder
                        .with(id, |r| tail_value(r, n))
                        .map_err(Value::String)
                });
                async move { result }
            }),
        );
        let recorder = self.clone();
        imports.define(
            "wisp-chain",
            "verify",
            pact_result_host_fn(move |input| {
                let result = actor_arg(&input)
                    .and_then(|id| recorder.with(id, check_value).map_err(Value::String));
                async move { result }
            }),
        );
        ctx.mark_satisfied("wisp-chain");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn event(hash: u8, parent: Option<u8>) -> ChainEvent {
        ChainEvent {
            hash: vec![hash],
            parent_hash: parent.map(|p| vec![p]),
            event_type: "wasm".into(),
            data: vec![0; hash as usize],
        }
    }

    #[test]
    fn a_linked_chain_has_no_breaks() {
        let mut r = Recording::default();
        r.push(&event(1, None));
        r.push(&event(2, Some(1)));
        r.push(&event(3, Some(2)));
        assert_eq!((r.count, r.breaks, r.from_root), (3, 0, true));
        assert_eq!(r.ring.back().unwrap().data_bytes, 3);
    }

    #[test]
    fn a_missing_link_is_a_break_and_a_late_start_is_not() {
        let mut r = Recording::default();
        r.push(&event(5, Some(4)));
        r.push(&event(7, Some(6)));
        r.push(&event(8, Some(7)));
        assert_eq!((r.count, r.breaks, r.from_root), (3, 1, false));
    }

    #[test]
    fn the_ring_keeps_the_newest_and_counts_all() {
        let mut r = Recording::default();
        r.push(&event(0, None));
        for i in 1..=(RING as u16 + 9) {
            let (h, p) = ((i % 256) as u8, ((i - 1) % 256) as u8);
            r.push(&event(h, Some(p)));
        }
        assert_eq!(r.count, RING as u64 + 10);
        assert_eq!(r.ring.len(), RING);
        assert_eq!(r.breaks, 0);
        match check_value(&r) {
            Value::Record { fields, .. } => {
                assert_eq!(fields[3], ("dropped".to_string(), Value::S32(10)));
            }
            other => panic!("{other:?}"),
        }
        match tail_value(&r, 2) {
            Value::List { items, .. } => assert_eq!(items.len(), 2),
            other => panic!("{other:?}"),
        }
    }

    #[test]
    fn hex_is_lowercase_two_digits() {
        assert_eq!(hex(&[0, 15, 255]), "000fff");
    }
}
