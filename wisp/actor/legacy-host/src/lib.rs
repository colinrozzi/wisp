pub mod chain;
pub mod eval_session;
pub mod events;
pub mod sessions;
pub mod source;
pub mod theater_host;
pub mod transport;

pub use eval_session::EvalSession;
pub use theater_host::TheaterHost;

/// The actor's public evaluation export (theater:simple/wisp.evaluate).
pub const EVALUATE: &str = "theater:simple/wisp.evaluate";

use anyhow::{Context, Result};
use std::sync::Arc;
use theater::actor::handle::ActorHandle;
use theater::handler::HandlerRegistry;
use theater::messages::{TheaterCommand, default_init_state};
use theater::pack_bridge::Value;
use theater::theater_runtime::TheaterRuntime;
use theater::utils::ResourceCache;
use theater::{ManifestConfig, TheaterId};
use tokio::sync::{mpsc, oneshot};

pub const MANIFEST: &str = include_str!("../../manifest.toml");

/// How the REPL actor's filesystem capability is granted. The default is the
/// historical behavior: an ephemeral sandbox in the `serve` cwd. A configured
/// `root` makes the sandbox a real, persistent directory, so `require` can load
/// libraries from disk across sessions. This is the human granting the actor its
/// filesystem capability — the role an actor manifest plays in real Theater.
pub struct FsAccess {
    pub root: Option<std::path::PathBuf>,
    pub read: bool,
    pub write: bool,
}

impl Default for FsAccess {
    fn default() -> Self {
        Self {
            root: None,
            read: true,
            write: true,
        }
    }
}

pub struct Runtime {
    pub commands: mpsc::UnboundedSender<TheaterCommand>,
    /// The session directory (the runtime's interpreter actors, plus the daemon's
    /// notes); readable by every actor via `wisp-sessions`.
    pub sessions: sessions::Sessions,
    /// Each session's newest trigger-firing seq, for `wait` and `follow`.
    pub events: events::EventWatch,
    task: tokio::task::JoinHandle<Result<()>>,
}

impl Runtime {
    pub async fn new(bundle: source::SourceBundle, fs: FsAccess) -> Result<Self> {
        let (commands, rx) = mpsc::unbounded_channel();
        let mut handlers = HandlerRegistry::new();
        handlers.register(bundle);
        let sessions = sessions::Sessions::new(commands.clone());
        handlers.register(sessions.clone());
        let events = events::EventWatch::default();
        handlers.register(events.clone());
        handlers.register(chain::ChainRecorder::new(commands.clone()));
        handlers.register(theater_handler_rpc::RpcHandler::new(commands.clone()));
        // Per-interface handlers the local runtime provides. Each is one line;
        // real Theater deployments enable handlers via the actor manifest.
        handlers.register(theater_handler_self::SelfHandler::new(
            theater_handler_self::SelfHostConfig {},
            commands.clone(),
            None,
        ));
        handlers.register(theater_handler_store::StoreHandler::new(
            theater_handler_store::StoreHandlerConfig::default(),
            None,
        ));
        // Grant the REPL full runtime control so it can manage other actors.
        handlers.register(theater_handler_runtime::RuntimeHandler::new(
            theater_handler_runtime::RuntimeHostConfig {},
            Some(theater::config::permissions::RuntimePermissions {
                inspect: true,
                mutate: true,
            }),
        ));
        // Inter-actor messaging (shares one background router across actors).
        handlers.register(theater_handler_message_server::MessageServerHandler::new(
            Some(theater::config::permissions::MessageServerPermissions),
            theater_handler_message_server::MessageRouter::new(),
        ));
        // Periodic ticks + wall clock.
        handlers.register(theater_handler_timer::TimerHandler::new(
            theater_handler_timer::TimerHandlerConfig::default(),
        ));
        // WAT -> wasm assembly (wisp:assembler/runtime).
        handlers.register(theater_handler_assembler::AssemblerHandler::new());
        // Filesystem access. With a configured root the sandbox is that real
        // directory (persistent across sessions); with none it falls back to the
        // historical ephemeral sandbox in the `serve` cwd. `new_dir` is only true in
        // the ephemeral case — a configured root is used as-is, not replaced.
        let fs_ephemeral = fs.root.is_none();
        let fs_path = fs.root.clone().or_else(|| std::env::current_dir().ok());
        handlers.register(theater_handler_filesystem::FileSystemHandler::new(
            theater::config::actor_manifest::FileSystemHandlerConfig {
                path: fs_path,
                new_dir: Some(fs_ephemeral),
                allowed_commands: None,
            },
            Some(theater::config::permissions::FileSystemPermissions {
                read: fs.read,
                write: fs.write,
                execute: false,
                allowed_commands: None,
                new_dir: Some(fs_ephemeral),
                allowed_paths: Some(vec!["/".to_string()]),
            }),
        ));
        // Terminal stdio + tty control.
        handlers.register(theater_handler_terminal::TerminalHandler::new(
            theater_handler_terminal::TerminalHandlerConfig::default(),
        ));
        // Outbound HTTP. allowed_hosts is an exact-match allowlist (no wildcard);
        // seed a few well-known hosts so the REPL can make real requests.
        handlers.register(theater_handler_http_client::HttpClientHandler::new(
            theater::config::actor_manifest::HttpClientHandlerConfig {
                allowed_hosts: vec![
                    "example.com".to_string(),
                    "api.github.com".to_string(),
                    "httpbin.org".to_string(),
                    "raw.githubusercontent.com".to_string(),
                ],
            },
        ));
        // Raw TCP + UDP sockets and Podman container control.
        handlers.register(theater_handler_tcp::TcpHandler::new(
            theater_handler_tcp::TcpHandlerConfig::default(),
        ));
        handlers.register(theater_handler_udp::UdpHandler::new(
            theater_handler_udp::UdpHandlerConfig::default(),
        ));
        // Native OS processes (pipes + PTY). Default config = any program,
        // unlimited, inherit env — the permissive dev-REPL stance the other
        // handlers take; isolation comes from WHAT you spawn, not this handler.
        handlers.register(theater_handler_process::ProcessHandler::new(
            theater_handler_process::ProcessHandlerConfig::default(),
        ));
        handlers.register(theater_handler_podman::PodmanHandler::new(
            theater_handler_podman::PodmanHandlerConfig {},
        ));
        // Lifecycle relationships: link (fate-sharing) + monitor (watch another
        // actor's chain, delivered to handle-actor-event). Self-service — the
        // runtime forces subscriber = caller.
        handlers.register(theater_handler_lifecycle::LifecycleHandler::new(
            commands.clone(),
        ));
        let mut runtime = TheaterRuntime::new(
            commands.clone(),
            rx,
            handlers,
            Arc::new(ResourceCache::new()),
            theater_native::TokioSpawn,
        )
        .await?;
        let task = tokio::spawn(async move { runtime.run().await });
        Ok(Self {
            commands,
            sessions,
            events,
            task,
        })
    }

    pub async fn spawn(&self, wasm: Vec<u8>) -> Result<Session> {
        self.spawn_with_manifest(wasm, ManifestConfig::from_toml_str(MANIFEST)?)
            .await
    }

    pub async fn spawn_with_manifest(
        &self,
        wasm: Vec<u8>,
        manifest: ManifestConfig,
    ) -> Result<Session> {
        let name = manifest.name.clone();
        self.spawn_named(wasm, manifest, name).await
    }

    /// Spawn an actor under the runtime name `name` (the manifest keeps its own).
    /// The runtime holds the name, so `find` reaches the actor by it.
    pub async fn spawn_named(
        &self,
        wasm: Vec<u8>,
        manifest: ManifestConfig,
        name: String,
    ) -> Result<Session> {
        let (response_tx, response_rx) = oneshot::channel();
        self.commands.send(TheaterCommand::SpawnActor {
            wasm_bytes: wasm,
            name: Some(name),
            manifest: Some(manifest),
            init_state: default_init_state(),
            response_tx,
            subscription_tx: None,
            parent_id: None,
        })?;
        let id = response_rx.await??;
        self.attach(id).await
    }

    /// Every interpreter actor in the runtime; see [`interpreters`].
    pub async fn interpreters(&self) -> Result<Vec<Interpreter>> {
        interpreters(&self.commands).await
    }

    /// The interpreter actor `target` names: a full actor id, an id prefix, or a
    /// runtime actor name (`new NAME` gives that name; `spawn-actor` gives the
    /// manifest's). A name or prefix that matches several actors is an error.
    pub async fn find(&self, target: &str) -> Result<Session> {
        let interpreters = self.interpreters().await?;
        let id = resolve(&interpreters, target)?.id;
        self.attach(id).await
    }

    /// A `Session` for a running actor, by id.
    pub async fn attach(&self, id: TheaterId) -> Result<Session> {
        let (response_tx, response_rx) = oneshot::channel();
        self.commands.send(TheaterCommand::GetActorHandle {
            actor_id: id,
            response_tx,
        })?;
        let handle = response_rx
            .await?
            .with_context(|| format!("actor {id} has stopped"))?;
        Ok(Session {
            id,
            handle,
            commands: self.commands.clone(),
        })
    }

    pub async fn shutdown(self) -> Result<()> {
        // Theater may already have exited after its last actor stopped.
        let _ = self.commands.send(TheaterCommand::ShutdownRuntime);
        tokio::time::timeout(std::time::Duration::from_secs(15), self.task).await???;
        Ok(())
    }
}

/// An interpreter actor as the runtime describes it.
#[derive(Clone, Debug)]
pub struct Interpreter {
    pub id: TheaterId,
    /// The runtime's name for the actor.
    pub name: String,
}

/// Every interpreter actor in the runtime (any actor that exports
/// `theater:simple/wisp.evaluate`), in id order. The runtime is the directory:
/// this asks it each time and keeps nothing.
pub async fn interpreters(
    commands: &mpsc::UnboundedSender<TheaterCommand>,
) -> Result<Vec<Interpreter>> {
    let (response_tx, response_rx) = oneshot::channel();
    commands.send(TheaterCommand::GetActors { response_tx })?;
    let mut found = Vec::new();
    for (id, name, _) in response_rx.await?? {
        // Served from the runtime's metadata cache: no call into the actor, so
        // this is safe from inside an actor's own host call.
        let (response_tx, response_rx) = oneshot::channel();
        commands.send(TheaterCommand::GetActorMetadata {
            actor_id: id,
            response_tx,
        })?;
        // None: the actor stopped between the two questions.
        if let Some(metadata) = response_rx.await?
            && exports_evaluate(&metadata.arena)
        {
            found.push(Interpreter { id, name });
        }
    }
    found.sort_by_key(|interpreter| interpreter.id.to_string());
    Ok(found)
}

/// Does this package metadata export `theater:simple/wisp.evaluate`?
fn exports_evaluate(arena: &theater::pack_bridge::Arena) -> bool {
    let (interface, function) = EVALUATE.rsplit_once('.').expect("EVALUATE is iface.fn");
    arena
        .children
        .iter()
        .filter(|section| section.name == "exports")
        .flat_map(|section| &section.children)
        .filter(|exported| exported.name == interface)
        .any(|exported| exported.functions.iter().any(|f| f.name == function))
}

/// Pick the interpreter `target` names. A full id wins; then an exact name; then
/// an id prefix. Several matches at the chosen level is an error that lists them.
pub fn resolve<'a>(interpreters: &'a [Interpreter], target: &str) -> Result<&'a Interpreter> {
    if target.is_empty() {
        anyhow::bail!("no session given");
    }
    if let Some(exact) = interpreters.iter().find(|i| i.id.to_string() == target) {
        return Ok(exact);
    }
    let by_name: Vec<_> = interpreters.iter().filter(|i| i.name == target).collect();
    let shared_name = by_name.len() > 1;
    let matches = if by_name.is_empty() {
        interpreters
            .iter()
            .filter(|i| i.id.to_string().starts_with(target))
            .collect()
    } else {
        by_name
    };
    match matches.as_slice() {
        [] => anyhow::bail!("no such session: {target}"),
        [one] => Ok(one),
        several => {
            let listed: Vec<String> = several
                .iter()
                .map(|i| format!("{} ({})", i.id, i.name))
                .collect();
            // spawn-actor names an actor after its manifest, so every spawn of
            // one manifest shares a name.
            let hint = if shared_name {
                " (spawn-actor names an actor after its manifest's `name`; give \
                 each manifest its own name to address it by name)"
            } else {
                ""
            };
            anyhow::bail!(
                "{target} matches {} sessions, use an id: {}{hint}",
                several.len(),
                listed.join(", ")
            )
        }
    }
}

/// The shortest id prefix (at least 8 characters) that names `id` alone among
/// `interpreters`: what `list` shows and `new` prints.
pub fn short_id(interpreters: &[Interpreter], id: &TheaterId) -> String {
    let full = id.to_string();
    let others: Vec<String> = interpreters
        .iter()
        .filter(|i| i.id != *id)
        .map(|i| i.id.to_string())
        .collect();
    let len = (8..full.len())
        .find(|&len| !others.iter().any(|other| other.starts_with(&full[..len])))
        .unwrap_or(full.len());
    full[..len].to_string()
}

pub struct Session {
    pub id: TheaterId,
    pub handle: ActorHandle,
    commands: mpsc::UnboundedSender<TheaterCommand>,
}

impl Session {
    pub async fn evaluate(&self, source: &str) -> Result<String> {
        if source.len() > 65536 {
            return Ok("error: input exceeds 65536 bytes".into());
        }
        match self
            .handle
            .call_function(EVALUATE.into(), Value::String(source.into()))
            .await?
        {
            Value::String(output) => Ok(output),
            other => anyhow::bail!("unexpected evaluator result: {other:?}"),
        }
    }
    pub async fn shutdown(self) -> Result<()> {
        self.stop().await
    }

    /// Stop the session's actor by reference. The daemon holds sessions in a shared
    /// registry (`Arc<Session>`), so it cannot consume `self` to shut one down.
    pub async fn stop(&self) -> Result<()> {
        // Ask the runtime to remove the actor as well as stop its task loops.
        let (response_tx, response_rx) = oneshot::channel();
        self.commands.send(TheaterCommand::StopActor {
            actor_id: self.id,
            response_tx,
        })?;
        tokio::time::timeout(std::time::Duration::from_secs(15), response_rx).await???;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn interp(id: &str, name: &str) -> Interpreter {
        Interpreter {
            id: TheaterId::parse(id).unwrap(),
            name: name.into(),
        }
    }

    fn found(all: &[Interpreter], target: &str) -> String {
        match resolve(all, target) {
            Ok(i) => i.name.clone(),
            Err(e) => format!("error: {e}"),
        }
    }

    #[test]
    fn resolve_by_id_prefix_and_name() {
        let all = [
            interp("0512410b-a31c-465f-8629-ac2e52315a72", "iris"),
            interp("058b0eef-09b0-4014-ac88-d9e76f6c7f27", "claude"),
            interp("058b1111-09b0-4014-ac88-d9e76f6c7f27", "claude"),
        ];
        assert_eq!(found(&all, "0512410b-a31c-465f-8629-ac2e52315a72"), "iris");
        assert_eq!(found(&all, "0512410b"), "iris");
        assert_eq!(found(&all, "051"), "iris");
        assert_eq!(found(&all, "iris"), "iris");
        assert!(found(&all, "058b").contains("matches 2 sessions"));
        assert!(found(&all, "claude").contains("matches 2 sessions"));
        assert_eq!(found(&all, "058b0eef"), "claude");
        assert_eq!(found(&all, "nobody"), "error: no such session: nobody");
        assert_eq!(found(&all, ""), "error: no session given");
    }

    #[test]
    fn a_name_beats_an_id_prefix() {
        let all = [
            interp("abcd0000-a31c-465f-8629-ac2e52315a72", "x"),
            interp("12345678-09b0-4014-ac88-d9e76f6c7f27", "abcd"),
        ];
        assert_eq!(found(&all, "abcd"), "abcd");
    }

    #[test]
    fn short_id_grows_until_unique() {
        let all = [
            interp("058b0eef-09b0-4014-ac88-d9e76f6c7f27", "a"),
            interp("058b0eef-1111-4014-ac88-d9e76f6c7f27", "b"),
            interp("0512410b-a31c-465f-8629-ac2e52315a72", "c"),
        ];
        assert_eq!(short_id(&all, &all[0].id), "058b0eef-0");
        assert_eq!(short_id(&all, &all[2].id), "0512410b");
    }
}
