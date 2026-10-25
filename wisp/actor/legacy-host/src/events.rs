//! The `wisp-events` interface: an interpreter actor tells the daemon the seq of
//! each trigger firing it records (`event-recorded`), so `theater-repl wait` and
//! `follow` sleep until there is something to read instead of polling the
//! session (see wisp's docs/proposals/SESSION-INBOX.md).
//!
//! The host function only updates a watch channel. It must never call back into
//! the actor: the actor is in the middle of an export when it calls, and runs one
//! operation at a time, so a call back would deadlock. The woken reader's
//! evaluation queues behind the export instead.
use anyhow::Result;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use theater::TheaterId;
use theater::handler::{Handler, HandlerContext};
use theater::pack_bridge::{HostImports, InterfaceImpl, Value, parse_pact, plain_host_fn};
use tokio::sync::watch;

/// The newest recorded seq of each session that has fired, as a watch channel,
/// and how many readers are parked on each of its cursors. Cloning shares both.
#[derive(Clone, Default)]
pub struct EventWatch {
    seqs: Arc<Mutex<HashMap<TheaterId, Arc<watch::Sender<u64>>>>>,
    parked: Parked,
}

impl EventWatch {
    fn sender(&self, id: TheaterId) -> Arc<watch::Sender<u64>> {
        self.seqs
            .lock()
            .unwrap_or_else(|poison| poison.into_inner())
            .entry(id)
            .or_insert_with(|| Arc::new(watch::channel(0).0))
            .clone()
    }

    /// A receiver for `id`'s newest recorded seq. Subscribe *before* reading the
    /// session: a firing recorded between the read and the wait then still
    /// changes the value, so no wake-up is lost.
    pub fn subscribe(&self, id: TheaterId) -> watch::Receiver<u64> {
        self.sender(id).subscribe()
    }

    /// Drop the channel of a session that has stopped.
    pub fn forget(&self, id: &TheaterId) {
        self.seqs
            .lock()
            .unwrap_or_else(|poison| poison.into_inner())
            .remove(id);
    }

    /// Count a `wait` parked on `id`'s cursor `cursor` (it has read and found
    /// nothing) until the guard drops. `theater-repl cursors` shows the count,
    /// so a test or a person can see that a monitor is in place.
    pub fn park(&self, id: TheaterId, cursor: &str) -> ParkGuard {
        let key = (id, cursor.to_string());
        *self.parked.lock().entry(key.clone()).or_default() += 1;
        ParkGuard {
            parked: self.parked.clone(),
            key,
        }
    }

    /// `id`'s cursors that have parked readers, with how many.
    pub fn parked(&self, id: TheaterId) -> Vec<(String, usize)> {
        let mut parked: Vec<_> = self
            .parked
            .lock()
            .iter()
            .filter(|((session, _), _)| *session == id)
            .map(|((_, cursor), count)| (cursor.clone(), *count))
            .collect();
        parked.sort();
        parked
    }
}

type ParkKey = (TheaterId, String);

#[derive(Clone, Default)]
struct Parked(Arc<Mutex<HashMap<ParkKey, usize>>>);

impl Parked {
    fn lock(&self) -> std::sync::MutexGuard<'_, HashMap<ParkKey, usize>> {
        self.0.lock().unwrap_or_else(|poison| poison.into_inner())
    }
}

/// One parked reader; dropping it (the wait returned, or its client went away)
/// uncounts it.
pub struct ParkGuard {
    parked: Parked,
    key: ParkKey,
}

impl Drop for ParkGuard {
    fn drop(&mut self) {
        let mut parked = self.parked.lock();
        if let Some(count) = parked.get_mut(&self.key) {
            *count -= 1;
            if *count == 0 {
                parked.remove(&self.key);
            }
        }
    }
}

/// `event-recorded(seq: u64)` arrives as the bare u64, or as a one-element tuple.
fn seq_of(input: &Value) -> Option<u64> {
    match input {
        Value::U64(seq) => Some(*seq),
        Value::S64(seq) => u64::try_from(*seq).ok(),
        Value::Tuple(items) if items.len() == 1 => seq_of(&items[0]),
        _ => None,
    }
}

impl Handler for EventWatch {
    fn create_instance(
        &self,
        _: Option<&theater::config::actor_manifest::HandlerConfig>,
    ) -> Box<dyn Handler> {
        Box::new(self.clone())
    }
    fn name(&self) -> &str {
        "wisp-events"
    }
    fn imports(&self) -> Option<Vec<String>> {
        Some(vec!["wisp-events".into()])
    }
    fn exports(&self) -> Option<Vec<String>> {
        None
    }
    fn supports_composite(&self) -> bool {
        true
    }
    fn interfaces(&self) -> Vec<InterfaceImpl> {
        vec![InterfaceImpl::from_pact(
            &parse_pact(include_str!("../../events.pact")).expect("events interface"),
        )]
    }
    fn register_host_functions(
        &mut self,
        imports: &mut HostImports,
        ctx: &mut HandlerContext,
    ) -> Result<()> {
        if ctx.is_satisfied("wisp-events") {
            return Ok(());
        }
        let id = ctx
            .actor_id
            .ok_or_else(|| anyhow::anyhow!("actor_id not set in HandlerContext"))?;
        let sender = self.sender(id);
        imports.define(
            "wisp-events",
            "event-recorded",
            plain_host_fn(move |input: Value| {
                if let Some(seq) = seq_of(&input) {
                    sender.send_replace(seq);
                }
                async { Value::Tuple(vec![]) }
            }),
        );
        ctx.mark_satisfied("wisp-events");
        Ok(())
    }
}
