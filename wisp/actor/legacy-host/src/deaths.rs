//! Death notices for the daemon: one `(actor, cause)` per actor that stops,
//! however it stops. The runtime records exactly one `terminated` event on a
//! dying actor's chain (theater `docs/messaging-and-lifecycle-behavior.md` §2.1)
//! and sends it only to that chain's subscribers (§2.2). The daemon subscribes
//! to every chain with `add_global_subscription` before it spawns anything, so
//! no actor's death is missed (§3.2).
//!
//! Chain delivery is strict: the runtime's command loop waits for this
//! subscriber to take each event, including the terminal one (§3.1). So the
//! drain task only looks at the event type, decodes the few terminal events,
//! and forwards them unbounded; it never waits on anything else.
//!
//! An actor can stay in the runtime's list after its terminal event: a
//! force-killed (`kill-actor`) REPL session never finishes its teardown, so
//! the runtime never finalizes it (theater `crates/theater/src/actor/runtime.rs`
//! `ActorControl::Terminate` awaits the handlers' shutdown with no deadline,
//! unlike the graceful path; seen 2026-10-25 at theater `cbb48982`). The
//! [`Dead`] set lets the daemon leave such actors out of its directory.
use std::collections::HashSet;
use std::sync::{Arc, Mutex};
use theater::TheaterId;
use theater::chain::ChainEvent;
use theater::events::lifecycle::{ActorLifecycleEvent, TerminationCause};
use theater::events::{ChainEventPayload, decode_chain_event_payload};
use tokio::sync::mpsc;

/// An actor that stopped, and why.
pub type Death = (TheaterId, TerminationCause);

/// Actors whose terminal event has arrived but that the runtime may still
/// list. Cloning shares the set.
#[derive(Clone, Default)]
pub struct Dead(Arc<Mutex<HashSet<TheaterId>>>);

impl Dead {
    fn set(&self) -> std::sync::MutexGuard<'_, HashSet<TheaterId>> {
        self.0.lock().unwrap_or_else(|p| p.into_inner())
    }

    pub fn contains(&self, id: &TheaterId) -> bool {
        self.set().contains(id)
    }

    /// Forget the dead the runtime no longer lists (`listed` is everything it
    /// does), so the set stays as small as the zombies.
    pub fn prune(&self, listed: &[TheaterId]) {
        self.set().retain(|id| listed.contains(id));
    }
}

/// The channel to pass to `add_global_subscription`, and the deaths drained
/// from it; each death is also added to `dead`. Needs a tokio runtime.
pub fn subscription(
    dead: Dead,
) -> (
    mpsc::Sender<(TheaterId, ChainEvent)>,
    mpsc::UnboundedReceiver<Death>,
) {
    let (events, mut rx) = mpsc::channel::<(TheaterId, ChainEvent)>(1024);
    let (deaths, out) = mpsc::unbounded_channel();
    tokio::spawn(async move {
        while let Some((id, event)) = rx.recv().await {
            if event.event_type != "terminated" {
                continue;
            }
            let cause = match decode_chain_event_payload(&event.data) {
                Some(ChainEventPayload::Lifecycle(ActorLifecycleEvent::Terminated { cause })) => {
                    cause
                }
                _ => TerminationCause::Failed {
                    error: "terminated (cause not decodable)".into(),
                },
            };
            dead.set().insert(id);
            // Nobody listening (the daemon is going down): keep draining, so
            // the runtime never waits on this subscriber.
            let _ = deaths.send((id, cause));
        }
    });
    (events, out)
}

/// A cause as the daemon prints it: `failed: <trap text>`, `stopped`, ….
pub fn describe(cause: &TerminationCause) -> String {
    match cause {
        TerminationCause::Completed { .. } => "completed".into(),
        TerminationCause::Failed { error } => format!("failed: {error}"),
        TerminationCause::Stopped => "stopped".into(),
        TerminationCause::Killed => "killed".into(),
        TerminationCause::PeerKilled { peer } => format!("peer-killed: {peer}"),
    }
}
