//! Record/replay: a session's chain, replayed with no source files in the host,
//! gives the same results and the same event hashes. Only the recorded
//! `wisp-source` responses can make the `include` succeed the second time.

use std::{collections::BTreeMap, path::Path};
use theater::chain::ChainEvent;
use tokio::sync::mpsc;
use wisp_interpreter_actor::{Runtime, source::SourceBundle};

async fn drive(
    wasm: Vec<u8>,
    bundle: SourceBundle,
    recorded: Option<Vec<ChainEvent>>,
) -> anyhow::Result<(Vec<String>, Vec<ChainEvent>)> {
    let (events_tx, mut events_rx) = mpsc::channel(4096);
    let runtime =
        Runtime::with_chain(bundle, Default::default(), recorded, Some(events_tx)).await?;
    let session = runtime.spawn(wasm).await?;
    let mut results = Vec::new();
    for source in [
        "(define add-two (lambda (x) (+ x 2)))",
        "(add-two 40)",
        "(include \"library.wisp\") (increment 41)",
        "(/ 1 0)",
        "(increment 41)",
    ] {
        results.push(session.evaluate(source).await?);
    }
    let mut events = Vec::new();
    while let Ok((_, event)) = events_rx.try_recv() {
        events.push(event);
    }
    runtime.shutdown().await?;
    Ok((results, events))
}

#[tokio::test]
async fn test_actor_replays_recorded_source_responses() -> anyhow::Result<()> {
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let bundle = SourceBundle::new(BTreeMap::from([(
        "library.wisp".into(),
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
