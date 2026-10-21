//! `(sessions)`: a session reads the daemon's session directory through the
//! `wisp-sessions` interface. The registry the daemon fills (`Runtime::sessions`)
//! is the one the guest sees, so adding and removing entries shows up live.

use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;
use std::time::Instant;
use wisp_interpreter_actor::{Runtime, sessions::Entry, source::SourceBundle};

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn sessions_verb_lists_the_daemon_registry() -> anyhow::Result<()> {
    let runtime = Runtime::new(SourceBundle::new(BTreeMap::new())?, Default::default()).await?;
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let alice = Arc::new(runtime.spawn(wasm).await?);
    let actor = alice.id.to_string();

    // Not registered yet: the directory is empty.
    assert_eq!(alice.evaluate("(list-len (sessions))").await?, "0");

    runtime.sessions.with(|map| {
        map.insert(
            "a1".into(),
            Entry {
                session: alice.clone(),
                name: "alice".into(),
                actor: actor.clone(),
                source: "test".into(),
                since: Instant::now(),
            },
        )
    });
    assert_eq!(
        alice
            .evaluate("(match (list-get (sessions) 0) ((session-info id name actor age) (list id name actor)))")
            .await?,
        format!("(\"a1\" \"alice\" \"{actor}\")")
    );
    assert!(alice.evaluate("(sessions 1)").await?.starts_with("error:"));
    assert!(alice.evaluate("(help)").await?.contains("(sessions)"));

    runtime.sessions.with(|map| map.remove("a1"));
    assert_eq!(alice.evaluate("(list-len (sessions))").await?, "0");

    alice.stop().await?;
    runtime.shutdown().await?;
    Ok(())
}
