//! The session directory, shared with the actors: a session is any interpreter
//! actor in the runtime (see `crate::interpreters`), so the directory is the
//! runtime itself. The `wisp-sessions` interface lets any actor read it (the
//! `(sessions)` REPL verb), and the daemon reads it for `list`.
//!
//! What the runtime does not know, when an actor started and which manifest the
//! daemon loaded it from, the daemon notes here for the actors it spawns. Notes
//! are for display only; finding a session never reads them.
//!
//! Listing is a grant: `[[handler]] type = "wisp-sessions"` with `list = false`
//! gives an actor an empty directory, and a spawned child can't turn it back on
//! (`check_child_config`). Actors the host starts list everything, as before.
use crate::{Interpreter, deaths::Dead, interpreters, short_id};
use anyhow::Result;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};
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
    /// The start file evaluated in it (a boot-list service's), and the sha256
    /// of the text that was evaluated.
    pub start: Option<(String, String)>,
}

/// The runtime's command channel plus the daemon's notes. Cloning shares both.
#[derive(Clone)]
pub struct Sessions {
    commands: UnboundedSender<TheaterCommand>,
    dead: Dead,
    notes: Arc<Mutex<HashMap<TheaterId, Note>>>,
    /// Whether this actor may read the directory.
    list: bool,
    /// The daemon's handler types, set once every handler is registered.
    types: Arc<OnceLock<Vec<String>>>,
}

/// A `[[handler]] type = "wisp-sessions"` block: only `list`.
#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct SessionsGrant {
    list: bool,
}

impl Sessions {
    pub fn new(commands: UnboundedSender<TheaterCommand>, dead: Dead) -> Self {
        Self {
            commands,
            dead,
            notes: Arc::default(),
            list: true,
            types: Arc::default(),
        }
    }

    /// Record the daemon's handler types (every clone sees them). The first call wins.
    pub fn set_types(&self, mut names: Vec<String>) {
        names.sort();
        names.dedup();
        let _ = self.types.set(names);
    }

    /// An actor's instance: a block's `list`, never more than this instance's.
    /// A malformed block (only a caller skipping `check_child_config` could pass
    /// one) lists nothing.
    fn instance(&self, config: Option<&theater::config::actor_manifest::HandlerConfig>) -> Self {
        let mut instance = self.clone();
        if let Some(config) = config {
            instance.list = self.list
                && config
                    .parse::<SessionsGrant>()
                    .is_ok_and(|grant| grant.list);
        }
        instance
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
        let found = interpreters(&self.commands, &self.dead).await?;
        self.notes()
            .retain(|id, _| found.iter().any(|interpreter| interpreter.id == *id));
        Ok(found)
    }

    /// The directory as `list<session-info>`, in id order. `age-secs` is 0 for a
    /// session the daemon did not spawn (the runtime does not record start times).
    async fn to_value(&self) -> Result<Value> {
        let found = if self.list {
            self.list().await?
        } else {
            Vec::new()
        };
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
        config: Option<&theater::config::actor_manifest::HandlerConfig>,
    ) -> Box<dyn Handler> {
        Box::new(self.instance(config))
    }
    fn check_child_config(
        &self,
        config: &theater::config::actor_manifest::HandlerConfig,
    ) -> Result<(), String> {
        let grant: SessionsGrant = config.parse().map_err(|error| format!("{error}"))?;
        if grant.list && !self.list {
            return Err("list = true is not allowed: the spawner can't list sessions".into());
        }
        Ok(())
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
        let types = self.types.clone();
        imports.define(
            "wisp-sessions",
            "handler-types",
            host_fn(move |_input| {
                let types = types.clone();
                async move {
                    Ok(Value::List {
                        elem_type: ValueType::String,
                        items: types
                            .get()
                            .into_iter()
                            .flatten()
                            .cloned()
                            .map(Value::String)
                            .collect(),
                    })
                }
            }),
        );
        ctx.mark_satisfied("wisp-sessions");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use theater::config::actor_manifest::HandlerConfig;
    use theater::handler::HandlerRegistry;

    fn block(fields: &[(&str, toml::Value)]) -> HandlerConfig {
        let table: toml::Table = fields
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        HandlerConfig::new("wisp-sessions", table)
    }

    /// A spawn, the way Theater does it: refused, or the child's instance.
    fn spawn(spawner: &Sessions, config: &HandlerConfig) -> Result<Sessions, String> {
        let mut registry = HandlerRegistry::new();
        registry.register(spawner.clone());
        registry.narrow_with_configs(std::slice::from_ref(config), None)?;
        Ok(spawner.instance(Some(config)))
    }

    #[tokio::test]
    async fn a_child_lists_sessions_only_if_granted_and_never_more_than_its_spawner() {
        let (commands, _rx) = tokio::sync::mpsc::unbounded_channel();
        let host = Sessions::new(commands, Dead::default());
        assert!(host.list);
        // The supervisor may list; a service is given list = false.
        let supervisor = spawn(&host, &block(&[("list", true.into())])).unwrap();
        let service = spawn(&supervisor, &block(&[("list", false.into())])).unwrap();
        assert!(supervisor.list && !service.list);
        // Its directory is empty without asking the runtime (the channel is gone).
        match service.to_value().await.unwrap() {
            Value::List { items, .. } => assert!(items.is_empty()),
            other => panic!("not a list: {other:?}"),
        }
        // A grandchild can't turn it back on, and only `list` may be named.
        let refused = spawn(&service, &block(&[("list", true.into())]))
            .err()
            .unwrap();
        assert_eq!(
            refused,
            "[[handler]] 'wisp-sessions': list = true is not allowed: the spawner can't list sessions"
        );
        assert!(spawn(&service, &block(&[("list", false.into())])).is_ok());
        let unknown = spawn(
            &host,
            &block(&[("list", true.into()), ("all", true.into())]),
        );
        assert!(unknown.err().unwrap().contains("unknown field `all`"));
        // Even without the check, a block never gives more than its spawner had.
        assert!(
            !service
                .instance(Some(&block(&[("list", true.into())])))
                .list
        );
        assert!(!host.instance(Some(&block(&[("lst", true.into())]))).list);
    }
}
