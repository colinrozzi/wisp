//! The daemon's live session: a shared-interpreter `ReplSession` whose host is a
//! `TheaterHost`.
//!
//! The convergence, realized: the daemon no longer runs the Wisp-interpreter actor
//! to evaluate — it holds a `wisp::compiler::ReplSession` (the same session type the
//! local REPL uses) and feeds it a `TheaterHost`, so forms reach the live runtime.
//! The session accumulates `(fn …)`/`(define …)` across inputs and pre-declares the
//! Theater host interface so calls like `(list-actors)` type-check and dispatch.
//!
//! eval is synchronous and blocks on the runtime, so `feed` must run on a blocking
//! thread (`tokio::task::spawn_blocking`) — see `TheaterHost`.

use crate::TheaterHost;
use theater::messages::TheaterCommand;
use tokio::sync::mpsc::UnboundedSender;
use wisp::compiler::{Outcome, ReplSession};

/// The Theater host interface the session exposes to evaluated code, as `(import …)`
/// declarations. Grows alongside `TheaterHost`; for now, the live-actor roster.
const THEATER_INTERFACE: &str = "(import host list-actors () (list string))\n";

/// A REPL session bound to a live Theater runtime.
pub struct EvalSession {
    commands: UnboundedSender<TheaterCommand>,
    session: ReplSession,
}

impl EvalSession {
    pub fn new(commands: UnboundedSender<TheaterCommand>) -> Self {
        Self {
            commands,
            session: ReplSession::with_preamble(THEATER_INTERFACE),
        }
    }

    /// Feed one source form (a `(fn …)`, a `(define …)`, or an expression), reaching
    /// the live runtime through a fresh `TheaterHost`. Synchronous — the host
    /// `blocking_recv`s the runtime's reply, so call this from a blocking thread.
    pub fn feed(&mut self, input: &str) -> anyhow::Result<Outcome> {
        let mut host = TheaterHost::new(self.commands.clone());
        self.session.feed(input, &mut host)
    }
}
