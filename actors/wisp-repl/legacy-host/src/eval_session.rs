//! A host-side REPL session on the shared interpreter.
//!
//! The convergence step past brick 1: instead of spawning the Wisp-interpreter
//! actor and calling its `evaluate` export, the daemon holds this session and
//! evaluates forms with Rust `eval` directly — the same interpreter the local REPL
//! runs on. Its capability to reach the live runtime is supplied by a `TheaterHost`:
//! the session pre-registers the Theater host interface as imports, so a form like
//! `(list-actors)` type-checks and, under eval, dispatches to the runtime.
//!
//! eval is synchronous and blocks on the runtime, so `evaluate` must be called from
//! a blocking context (`tokio::task::spawn_blocking`) — see `TheaterHost`.

use crate::TheaterHost;
use theater::messages::TheaterCommand;
use tokio::sync::mpsc::UnboundedSender;
use wisp::compiler::{Import, Type, Value, eval_repl_expr_with_host};

/// A REPL session that evaluates against a live Theater runtime.
pub struct EvalSession {
    commands: UnboundedSender<TheaterCommand>,
    imports: Vec<Import>,
}

impl EvalSession {
    pub fn new(commands: UnboundedSender<TheaterCommand>) -> Self {
        Self {
            commands,
            imports: theater_interface(),
        }
    }

    /// Evaluate one expression through the shared interpreter, reaching the live
    /// runtime via a fresh `TheaterHost`. Returns the value and its inferred type.
    /// Synchronous — the host `blocking_recv`s the runtime's reply, so call this
    /// from a blocking thread, never directly on a reactor task.
    pub fn evaluate(&mut self, expr: &str) -> anyhow::Result<(Value, Type)> {
        let mut host = TheaterHost::new(self.commands.clone());
        eval_repl_expr_with_host(expr, &Default::default(), &[], &self.imports, &mut host)
    }
}

/// The Theater host interface the session exposes to evaluated code. Grows
/// alongside `TheaterHost`; for now, the live-actor roster.
fn theater_interface() -> Vec<Import> {
    vec![Import {
        module: "host".to_string(),
        name: "list-actors".to_string(),
        params: vec![],
        return_type: Type::List(Box::new(Type::Str)),
    }]
}
