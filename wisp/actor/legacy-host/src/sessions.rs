//! The session directory, shared with the actors: a session is any interpreter
//! actor in the runtime (see `crate::interpreters`), so the directory is the
//! runtime itself. The `wisp-sessions` interface lets any actor read it (the
//! `(sessions)` REPL verb), and the daemon reads it for `list`.
//!
//! What the runtime does not know, when an actor started and which manifest the
//! daemon loaded it from, the daemon notes here for the actors it spawns. Notes
//! are for display only; finding a session never reads them.
use crate::{Interpreter, interpreters, short_id};
use anyhow::Result;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Instant;
use theater::TheaterId;
use theater::handler::{Handler, HandlerContext};
use theater::messages::TheaterCommand;
use theater::pack_bridge::{
    HostError, HostImports, InterfaceImpl, Value, ValueType, host_fn, parse_pact,
};
use tokio::sync::mpsc::UnboundedSender;

/// What the daemon knows about an actor it spawned.
#[derive(Clone)]
pub struct Note {
    /// Where its actor came from (an `ActorSource` origin).
    pub source: String,
    pub since: Instant,
}

/// The runtime's command channel plus the daemon's notes. Cloning shares both.
#[derive(Clone)]
pub struct Sessions {
    commands: UnboundedSender<TheaterCommand>,
    notes: Arc<Mutex<HashMap<TheaterId, Note>>>,
}

impl Sessions {
    pub fn new(commands: UnboundedSender<TheaterCommand>) -> Self {
        Self {
            commands,
            notes: Arc::default(),
        }
    }

    fn notes(&self) -> std::sync::MutexGuard<'_, HashMap<TheaterId, Note>> {
        self.notes
            .lock()
            .unwrap_or_else(|poison| poison.into_inner())
    }

    pub fn note(&self, id: TheaterId, note: Note) {
        self.notes().insert(id, note);
    }

    /// The note for `id`, if the daemon spawned it.
    pub fn noted(&self, id: &TheaterId) -> Option<Note> {
        self.notes().get(id).cloned()
    }

    /// Every session, in id order. Notes for actors that have stopped, however
    /// they stopped, are dropped on the way.
    pub async fn list(&self) -> Result<Vec<Interpreter>> {
        let found = interpreters(&self.commands).await?;
        self.notes()
            .retain(|id, _| found.iter().any(|interpreter| interpreter.id == *id));
        Ok(found)
    }

    /// The directory as `list<session-info>`, in id order. `age-secs` is 0 for a
    /// session the daemon did not spawn (the runtime does not record start times).
    async fn to_value(&self) -> Result<Value> {
        let found = self.list().await?;
        let items = found
            .iter()
            .map(|interpreter| Value::Record {
                type_name: "session-info".to_string(),
                fields: vec![
                    (
                        "id".to_string(),
                        Value::String(short_id(&found, &interpreter.id)),
                    ),
                    ("name".to_string(), Value::String(interpreter.name.clone())),
                    (
                        "actor".to_string(),
                        Value::String(interpreter.id.to_string()),
                    ),
                    (
                        "age-secs".to_string(),
                        Value::U64(
                            self.noted(&interpreter.id)
                                .map_or(0, |note| note.since.elapsed().as_secs()),
                        ),
                    ),
                ],
            })
            .collect();
        Ok(Value::List {
            elem_type: ValueType::Record("session-info".to_string()),
            items,
        })
    }
}

impl Handler for Sessions {
    fn create_instance(
        &self,
        _: Option<&theater::config::actor_manifest::HandlerConfig>,
    ) -> Box<dyn Handler> {
        Box::new(self.clone())
    }
    fn name(&self) -> &str {
        "wisp-sessions"
    }
    fn imports(&self) -> Option<Vec<String>> {
        Some(vec!["wisp-sessions".into()])
    }
    fn exports(&self) -> Option<Vec<String>> {
        None
    }
    fn supports_composite(&self) -> bool {
        true
    }
    fn interfaces(&self) -> Vec<InterfaceImpl> {
        vec![InterfaceImpl::from_pact(
            &parse_pact(include_str!("../../sessions.pact")).expect("sessions interface"),
        )]
    }
    fn register_host_functions(
        &mut self,
        imports: &mut HostImports,
        ctx: &mut HandlerContext,
    ) -> Result<()> {
        if ctx.is_satisfied("wisp-sessions") {
            return Ok(());
        }
        let sessions = self.clone();
        imports.define(
            "wisp-sessions",
            "list-sessions",
            host_fn(move |_input| {
                let sessions = sessions.clone();
                async move {
                    sessions
                        .to_value()
                        .await
                        .map_err(|error| HostError(format!("{error:#}")))
                }
            }),
        );
        ctx.mark_satisfied("wisp-sessions");
        Ok(())
    }
}
