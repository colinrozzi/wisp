//! The convergence seam: the shared Rust interpreter's `Host`, implemented against
//! a live Theater runtime.
//!
//! An imported-function call under `granite::compiler::eval` — e.g. `(list-actors)` —
//! dispatches to `Host::call`, which marshals it to a `TheaterCommand` on the
//! runtime's channel and waits for the reply. So one interpreter serves both the
//! local REPL (a trivial stdio host) and the live Theater session (this host): the
//! language logic is shared; only the host differs.
//!
//! eval is synchronous and the runtime is async, so `Host::call` must run on a
//! blocking thread (`tokio::task::spawn_blocking`). It sends the command — a
//! non-blocking channel send — and `blocking_recv`s the oneshot reply. Calling it
//! from a normal async task would panic (`blocking_recv` inside the reactor).
//!
//! Only the runtime-control surface a REPL needs (list / spawn / stop / eval-on-
//! actor) is reachable host-side through the command channel. The per-actor handler
//! interfaces (store, tcp, timer, …) are served to guest *actors*, not to the host,
//! so they are deliberately not here.

use anyhow::{Result, anyhow, bail};
use granite::compiler::{Host, Value};
use theater::messages::TheaterCommand;
use tokio::sync::{mpsc, oneshot};

/// A `granite::compiler::Host` backed by a live Theater runtime's command channel.
pub struct TheaterHost {
    commands: mpsc::UnboundedSender<TheaterCommand>,
}

impl TheaterHost {
    pub fn new(commands: mpsc::UnboundedSender<TheaterCommand>) -> Self {
        Self { commands }
    }
}

impl Host for TheaterHost {
    fn call(&mut self, _module: &str, name: &str, _args: &[Value]) -> Result<Value> {
        match name {
            // The ids of every live actor. The simplest round-trip that proves the
            // bridge: eval -> TheaterCommand -> runtime -> reply -> Value.
            "list-actors" => {
                let (tx, rx) = oneshot::channel();
                self.commands
                    .send(TheaterCommand::GetActors { response_tx: tx })
                    .map_err(|_| anyhow!("theater host: the runtime is gone"))?;
                let rows = rx
                    .blocking_recv()
                    .map_err(|_| anyhow!("theater host: no reply from the runtime"))?
                    .map_err(|e| anyhow!("theater host: list-actors failed: {e}"))?;
                Ok(Value::List(
                    rows.into_iter()
                        .map(|(id, _name, _parent)| Value::Str(id.to_string()))
                        .collect(),
                ))
            }
            other => bail!("theater host: unsupported import '{other}'"),
        }
    }
}
