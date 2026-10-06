use std::{collections::BTreeMap, path::Path, sync::Arc};
use theater::pack_bridge::Value;
use theater::{
    TheaterId,
    chain::ChainEvent,
    handler::HandlerRegistry,
    messages::{TheaterCommand, default_init_state},
    theater_runtime::TheaterRuntime,
    utils::ResourceCache,
};
use tokio::sync::{mpsc, oneshot};
use wisp_interpreter_actor::{adapter, source::SourceBundle};

async fn drive(
    wasm: Vec<u8>,
    bundle: SourceBundle,
    recorded: Option<Vec<ChainEvent>>,
) -> anyhow::Result<(Vec<String>, Vec<ChainEvent>)> {
    let (commands, rx) = mpsc::unbounded_channel();
    let mut registry = HandlerRegistry::new();
    registry.register(bundle);
    if let Some(events) = recorded {
        registry.set_replay_chain(events);
    }
    let mut runtime = TheaterRuntime::new(
        commands.clone(),
        rx,
        registry,
        Arc::new(ResourceCache::new()),
        theater_native::TokioSpawn,
    )
    .await?;
    let (events_tx, mut events_rx) = mpsc::channel::<(TheaterId, ChainEvent)>(256);
    runtime.add_global_subscription(events_tx);
    let task = tokio::spawn(async move { runtime.run().await });
    let (response_tx, response_rx) = oneshot::channel();
    commands.send(TheaterCommand::SpawnActor {
        wasm_bytes: wasm,
        name: Some("replay-wisp".into()),
        manifest: None,
        init_state: default_init_state(),
        response_tx,
        subscription_tx: None,
        parent_id: None,
    })?;
    let id = response_rx.await??;
    let (response_tx, response_rx) = oneshot::channel();
    commands.send(TheaterCommand::GetActorHandle {
        actor_id: id,
        response_tx,
    })?;
    let handle = response_rx.await?.unwrap();
    let mut results = Vec::new();
    for source in [
        "(define add-two (lambda (x) (+ x 2)))",
        "(add-two 40)",
        "(include \"library.lisp\") (increment 41)",
        "(/ 1 0)",
        "(increment 41)",
    ] {
        let Value::String(result) = handle
            .call_function(adapter::EVALUATE.into(), Value::String(source.into()))
            .await?
        else {
            panic!("expected printed value")
        };
        results.push(result);
    }
    let mut events = Vec::new();
    while let Ok((_, event)) = events_rx.try_recv() {
        events.push(event);
    }
    commands.send(TheaterCommand::ShutdownRuntime)?;
    tokio::time::timeout(std::time::Duration::from_secs(15), task).await???;
    Ok((results, events))
}

#[tokio::test]
async fn test_actor_replays_recorded_source_responses() -> anyhow::Result<()> {
    let output = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/replay-test");
    let wasm = adapter::build(&output)?;
    let bundle = SourceBundle::new(BTreeMap::from([(
        "library.lisp".into(),
        "(fn increment ((x s32)) s32 (i32.add x 1))".into(),
    )]))?;
    let (results, events) = drive(wasm.clone(), bundle, None).await?;
    assert_eq!(results[2], "42");
    assert!(
        events
            .iter()
            .any(|event| event.event_type.contains("wisp-source")),
        "source imports must be recorded"
    );
    // No source files in the replay host: only recorded responses can succeed.
    let (replayed, replay_events) =
        drive(wasm, SourceBundle::default(), Some(events.clone())).await?;
    assert_eq!(results, replayed);
    assert!(!events.is_empty());
    assert_eq!(
        events.iter().map(|e| &e.hash).collect::<Vec<_>>(),
        replay_events.iter().map(|e| &e.hash).collect::<Vec<_>>()
    );
    Ok(())
}
