//! The daemon's session registry, shared with the actors it holds: the daemon
//! adds and removes sessions; the `wisp-sessions` interface lets any actor in the
//! runtime read the directory (the `(sessions)` REPL verb).
use crate::Session;
use anyhow::Result;
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::time::Instant;
use theater::handler::{Handler, HandlerContext};
use theater::pack_bridge::{HostImports, InterfaceImpl, Value, ValueType, host_fn, parse_pact};

/// A live session held by the daemon.
pub struct Entry {
    pub session: Arc<Session>,
    pub name: String,
    pub actor: String,
    /// Where its actor came from (an `ActorSource` origin).
    pub source: String,
    pub since: Instant,
}

/// Sessions by short id. Cloning shares the same registry.
#[derive(Clone, Default)]
pub struct Sessions(Arc<Mutex<BTreeMap<String, Entry>>>);

impl Sessions {
    /// Run `f` with the registry locked. Never hold the lock across an await.
    pub fn with<R>(&self, f: impl FnOnce(&mut BTreeMap<String, Entry>) -> R) -> R {
        f(&mut self.0.lock().unwrap_or_else(|poison| poison.into_inner()))
    }

    pub fn get(&self, id: &str) -> Option<Arc<Session>> {
        self.with(|map| map.get(id).map(|entry| entry.session.clone()))
    }

    /// The directory as `list<session-info>`, in id order.
    fn to_value(&self) -> Value {
        let items = self.with(|map| {
            map.iter()
                .map(|(id, entry)| Value::Record {
                    type_name: "session-info".to_string(),
                    fields: vec![
                        ("id".to_string(), Value::String(id.clone())),
                        ("name".to_string(), Value::String(entry.name.clone())),
                        ("actor".to_string(), Value::String(entry.actor.clone())),
                        (
                            "age-secs".to_string(),
                            Value::U64(entry.since.elapsed().as_secs()),
                        ),
                    ],
                })
                .collect()
        });
        Value::List {
            elem_type: ValueType::Record("session-info".to_string()),
            items,
        }
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
                async move { Ok(sessions.to_value()) }
            }),
        );
        ctx.mark_satisfied("wisp-sessions");
        Ok(())
    }
}
