//! Who spawned each actor, and when, for `inspect`. The runtime keeps no
//! lineage (`GetActors` always answers `None` for the parent), but it tells
//! runtime-wide spawn subscribers each birth as `(id, name, parent)` (theater
//! `TheaterCommand::SubscribeToSpawns`). The daemon subscribes before it
//! spawns anything, so every actor it can list was born under this watch.
//!
//! The runtime offers each birth with `try_send` and drops it when the
//! channel is full, so the channel is wide and a task drains it at once.
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Instant;
use theater::TheaterId;
use theater::messages::TheaterCommand;
use tokio::sync::mpsc;

/// One birth: the actor that spawned it (`None`: the daemon, through the
/// runtime directly) and when.
#[derive(Clone, Copy, Debug)]
pub struct Born {
    pub parent: Option<TheaterId>,
    pub at: Instant,
}

/// Births seen so far. Cloning shares them.
#[derive(Clone, Default)]
pub struct Lineage(Arc<Mutex<HashMap<TheaterId, Born>>>);

impl Lineage {
    fn map(&self) -> std::sync::MutexGuard<'_, HashMap<TheaterId, Born>> {
        self.0.lock().unwrap_or_else(|p| p.into_inner())
    }

    /// Subscribe to the runtime's births and keep them.
    pub fn watch(commands: &mpsc::UnboundedSender<TheaterCommand>) -> anyhow::Result<Self> {
        let lineage = Self::default();
        let (event_tx, mut births) = mpsc::channel(4096);
        commands.send(TheaterCommand::SubscribeToSpawns { event_tx })?;
        let kept = lineage.clone();
        tokio::spawn(async move {
            while let Some((id, _name, parent)) = births.recv().await {
                kept.note(id, parent, Instant::now());
            }
        });
        Ok(lineage)
    }

    pub fn note(&self, id: TheaterId, parent: Option<TheaterId>, at: Instant) {
        self.map().insert(id, Born { parent, at });
    }

    pub fn get(&self, id: &TheaterId) -> Option<Born> {
        self.map().get(id).copied()
    }

    /// Forget every actor not in `live`.
    pub fn prune(&self, live: &[TheaterId]) {
        self.map().retain(|id, _| live.contains(id));
    }
}
