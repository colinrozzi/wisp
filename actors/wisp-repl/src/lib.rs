pub mod adapter;
pub mod source;
pub mod transport;

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

pub const MANIFEST: &str = include_str!("../manifest.toml");

pub struct Runtime {
    pub commands: mpsc::UnboundedSender<TheaterCommand>,
    task: tokio::task::JoinHandle<Result<()>>,
}

impl Runtime {
    pub async fn new(bundle: source::SourceBundle) -> Result<Self> {
        let (commands, rx) = mpsc::unbounded_channel();
        let mut handlers = HandlerRegistry::new();
        handlers.register(bundle);
        handlers.register(theater_handler_rpc::RpcHandler::new(commands.clone()));
        let mut runtime = TheaterRuntime::new(
            commands.clone(),
            rx,
            handlers,
            Arc::new(ResourceCache::new()),
            theater_native::TokioSpawn,
        )
        .await?;
        let task = tokio::spawn(async move { runtime.run().await });
        Ok(Self { commands, task })
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
        let (response_tx, response_rx) = oneshot::channel();
        self.commands.send(TheaterCommand::SpawnActor {
            wasm_bytes: wasm,
            name: Some(manifest.name.clone()),
            manifest: Some(manifest),
            init_state: default_init_state(),
            response_tx,
            subscription_tx: None,
            parent_id: None,
        })?;
        let id = response_rx.await??;
        let (response_tx, response_rx) = oneshot::channel();
        self.commands.send(TheaterCommand::GetActorHandle {
            actor_id: id,
            response_tx,
        })?;
        let handle = response_rx.await?.context("spawned actor has no handle")?;
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

pub struct Session {
    pub id: TheaterId,
    pub handle: ActorHandle,
    commands: mpsc::UnboundedSender<TheaterCommand>,
}

impl Session {
    pub async fn evaluate(&self, source: &str) -> Result<String> {
        if source.len() > 4096 {
            return Ok("error: input exceeds 4096 bytes".into());
        }
        match self
            .handle
            .call_function(adapter::EVALUATE.into(), Value::String(source.into()))
            .await?
        {
            Value::String(output) => Ok(output),
            other => anyhow::bail!("unexpected evaluator result: {other:?}"),
        }
    }
    pub async fn shutdown(self) -> Result<()> {
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
