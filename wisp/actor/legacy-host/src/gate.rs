//! A handler an actor either has or hasn't: `[[handler]] type = T` with one
//! boolean field (`chain`, `monitor`, `enabled`). It narrows like every grant
//! (0156): a spawned child may turn it off, never on under a spawner that has it
//! off, and a block that names anything else is refused. A block without the
//! field (`[[handler]] type = "lifecycle"`, as manifests have it) is the
//! spawner's value, like no block, so a spawner closing a gate writes the field.
use std::future::Future;
use std::pin::Pin;
use theater::actor::handle::ActorHandle;
use theater::config::actor_manifest::HandlerConfig;
use theater::config::permissions::HandlerPermission;
use theater::handler::{Handler, HandlerContext, HandlerEventReceiver, SharedActorInstance};
use theater::pack_bridge::{HostImports, InterfaceImpl, TypeHash, Value, pact_result_host_fn};
use theater::shutdown::ShutdownReceiver;

/// The child's value of `field` in its block, checked against the spawner's.
pub fn child(field: &str, spawner: bool, config: &HandlerConfig) -> Result<bool, String> {
    let table: toml::Table = config.parse().map_err(|error| format!("{error}"))?;
    if let Some(other) = table.keys().find(|key| *key != field) {
        return Err(format!(
            "{other} is not a setting here; the only one is {field}"
        ));
    }
    let on = match table.get(field) {
        Some(toml::Value::Boolean(on)) => *on,
        Some(_) => return Err(format!("{field} must be true or false")),
        None => spawner,
    };
    if on && !spawner {
        return Err(format!(
            "{field} = true is not allowed: the spawner has it off"
        ));
    }
    Ok(on)
}

/// An actor's value: its block's, never more than its spawner's. No block keeps
/// the spawner's; a block `child` would refuse turns it off.
pub fn instance(field: &str, spawner: bool, config: Option<&HandlerConfig>) -> bool {
    match config {
        None => spawner,
        Some(config) => spawner && child(field, spawner, config).unwrap_or(false),
    }
}

/// A Theater handler behind a gate: everything is the inner handler's, except
/// that an actor whose gate is off gets `interface`'s `verbs` as refusals. They
/// still satisfy the import, so the interpreter, which imports every interface,
/// instantiates either way.
pub struct Gated {
    inner: Box<dyn Handler>,
    field: &'static str,
    interface: &'static str,
    verbs: &'static [&'static str],
    on: bool,
}

impl Gated {
    pub fn new(
        inner: impl Handler,
        field: &'static str,
        interface: &'static str,
        verbs: &'static [&'static str],
        on: bool,
    ) -> Self {
        Self {
            inner: Box::new(inner),
            field,
            interface,
            verbs,
            on,
        }
    }
}

impl Handler for Gated {
    /// The inner handler takes no config here: the block is the gate's.
    fn create_instance(&self, config: Option<&HandlerConfig>) -> Box<dyn Handler> {
        Box::new(Self {
            inner: self.inner.create_instance(None),
            field: self.field,
            interface: self.interface,
            verbs: self.verbs,
            on: instance(self.field, self.on, config),
        })
    }
    fn set_permissions(&mut self, permissions: Option<&HandlerPermission>) {
        self.inner.set_permissions(permissions);
    }
    fn check_child_config(&self, config: &HandlerConfig) -> Result<(), String> {
        child(self.field, self.on, config).map(|_| ())
    }
    fn check_child_path(&self, key: &str, path: &std::path::Path) -> Result<(), String> {
        self.inner.check_child_path(key, path)
    }
    fn init(&mut self, actor_handle: ActorHandle, actor_instance: SharedActorInstance) {
        self.inner.init(actor_handle, actor_instance);
    }
    fn run(
        &mut self,
        shutdown_receiver: ShutdownReceiver,
        event_rx: HandlerEventReceiver,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send>> {
        self.inner.run(shutdown_receiver, event_rx)
    }
    fn setup(
        &mut self,
        actor_handle: ActorHandle,
        actor_instance: SharedActorInstance,
        shutdown_receiver: ShutdownReceiver,
        event_rx: HandlerEventReceiver,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send>> {
        self.inner
            .setup(actor_handle, actor_instance, shutdown_receiver, event_rx)
    }
    fn register_host_functions(
        &mut self,
        imports: &mut HostImports,
        ctx: &mut HandlerContext,
    ) -> anyhow::Result<()> {
        if self.on {
            return self.inner.register_host_functions(imports, ctx);
        }
        if ctx.is_satisfied(self.interface) {
            return Ok(());
        }
        for verb in self.verbs {
            let refused = format!("{} is not granted to this actor", self.inner.name());
            imports.define(
                self.interface,
                verb,
                pact_result_host_fn(move |_input| {
                    let refused = refused.clone();
                    async move { Err::<Value, _>(Value::String(refused)) }
                }),
            );
        }
        ctx.mark_satisfied(self.interface);
        Ok(())
    }
    fn name(&self) -> &str {
        self.inner.name()
    }
    fn imports(&self) -> Option<Vec<String>> {
        self.inner.imports()
    }
    fn exports(&self) -> Option<Vec<String>> {
        self.inner.exports()
    }
    fn interface_hashes(&self) -> Vec<(String, TypeHash)> {
        self.inner.interface_hashes()
    }
    fn interfaces(&self) -> Vec<InterfaceImpl> {
        self.inner.interfaces()
    }
    fn supports_composite(&self) -> bool {
        self.inner.supports_composite()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block(fields: &[(&str, toml::Value)]) -> HandlerConfig {
        let table: toml::Table = fields
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        HandlerConfig::new("t", table)
    }

    #[test]
    fn a_gate_only_closes() {
        let on = block(&[("chain", true.into())]);
        let off = block(&[("chain", false.into())]);
        assert_eq!(child("chain", true, &on), Ok(true));
        assert_eq!(child("chain", true, &off), Ok(false));
        assert_eq!(child("chain", false, &off), Ok(false));
        assert_eq!(
            child("chain", false, &on),
            Err("chain = true is not allowed: the spawner has it off".into())
        );
        assert_eq!(child("chain", true, &block(&[])), Ok(true));
        assert_eq!(child("chain", false, &block(&[])), Ok(false));
        assert!(
            child("chain", true, &block(&[("chain", "yes".into())]))
                .unwrap_err()
                .contains("true or false")
        );
        assert!(
            child(
                "chain",
                true,
                &block(&[("chain", true.into()), ("all", true.into())])
            )
            .unwrap_err()
            .contains("all is not a setting here")
        );
        assert!(instance("chain", true, None) && !instance("chain", false, None));
        assert!(!instance("chain", false, Some(&on)));
        assert!(!instance(
            "chain",
            true,
            Some(&block(&[("chan", true.into())]))
        ));
    }

    /// Each gate as a spawn sees it: Theater's registry narrowing a child's block.
    #[tokio::test]
    async fn every_gate_narrows_at_spawn() {
        use theater::handler::HandlerRegistry;
        let (commands, _rx) = tokio::sync::mpsc::unbounded_channel();
        /// A registry holding one gated handler, on or off; its name; its field.
        type Gate<'a> = (Box<dyn Fn(bool) -> HandlerRegistry + 'a>, &'a str, &'a str);
        let gates: Vec<Gate> = vec![
            (
                Box::new(|on| {
                    let mut registry = HandlerRegistry::new();
                    let lifecycle = Gated::new(
                        theater_handler_lifecycle::LifecycleHandler::new(commands.clone()),
                        "monitor",
                        "theater:simple/lifecycle",
                        &["monitor"],
                        on,
                    );
                    registry.register(lifecycle);
                    registry
                }),
                "lifecycle",
                "monitor",
            ),
            (
                Box::new(|on| {
                    let mut registry = HandlerRegistry::new();
                    registry.register(Gated::new(
                        theater_handler_podman::PodmanHandler::new(
                            theater_handler_podman::PodmanHandlerConfig {},
                        ),
                        "enabled",
                        "theater:simple/podman",
                        &["list"],
                        on,
                    ));
                    registry
                }),
                "podman",
                "enabled",
            ),
            (
                Box::new(|on| {
                    let mut registry = HandlerRegistry::new();
                    let chain = crate::chain::ChainRecorder::new(commands.clone());
                    registry.register(*chain.instance_for_test(on));
                    registry
                }),
                "wisp-chain",
                "chain",
            ),
        ];
        for (registry, name, field) in &gates {
            let narrow = |on: bool, value: bool| {
                registry(on)
                    .narrow_with_configs(
                        &[HandlerConfig::new(
                            name,
                            toml::Table::from_iter([(field.to_string(), value.into())]),
                        )],
                        None,
                    )
                    .map(|_| ())
            };
            assert!(narrow(true, false).is_ok(), "{name}: off under on");
            assert!(narrow(true, true).is_ok(), "{name}: on under on");
            assert!(narrow(false, false).is_ok(), "{name}: off under off");
            assert_eq!(
                narrow(false, true).err().unwrap(),
                format!(
                    "[[handler]] '{name}': {field} = true is not allowed: the spawner has it off"
                ),
            );
            let widened = registry(true).narrow_with_configs(
                &[HandlerConfig::new(
                    name,
                    toml::Table::from_iter([("all".to_string(), true.into())]),
                )],
                None,
            );
            assert!(
                widened.err().unwrap().contains("all is not a setting here"),
                "{name}"
            );
        }
    }
}
