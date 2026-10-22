//! `(sessions)`: a session reads the session directory through the
//! `wisp-sessions` interface. The directory is the runtime's interpreter actors,
//! so an actor shows up when it is spawned (by the daemon or anyone) and is gone
//! when it stops. The daemon's notes only add an age.

use std::collections::BTreeMap;
use std::path::Path;
use std::time::Instant;
use theater::ManifestConfig;
use wisp_interpreter_actor::{MANIFEST, Runtime, sessions::Note, source::SourceBundle};

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn sessions_verb_lists_the_runtime_interpreters() -> anyhow::Result<()> {
    let runtime = Runtime::new(SourceBundle::new(BTreeMap::new())?, Default::default()).await?;
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let manifest = ManifestConfig::from_toml_str(MANIFEST)?;
    let alice = runtime
        .spawn_named(wasm.clone(), manifest.clone(), "alice".into())
        .await?;
    let actor = alice.id.to_string();

    // alice sees herself: the runtime has one interpreter.
    assert_eq!(alice.evaluate("(list-len (sessions))").await?, "1");
    assert_eq!(
        alice
            .evaluate("(match (list-get (sessions) 0) ((session-info id name actor age) (list id name actor age)))")
            .await?,
        format!("(\"{}\" \"alice\" \"{actor}\" 0u64)", &actor[..8])
    );

    // A second interpreter shows up without anyone registering it.
    let bob = runtime.spawn_named(wasm, manifest, "bob".into()).await?;
    assert_eq!(alice.evaluate("(list-len (sessions))").await?, "2");

    // A daemon note adds an age; it does not decide who is listed.
    runtime.sessions.note(
        alice.id,
        Note {
            source: "test".into(),
            since: Instant::now() - std::time::Duration::from_secs(5),
        },
    );
    let ages = alice
        .evaluate("(match (list-get (sessions) 0) ((session-info id name actor age) age))")
        .await?;
    let ages_bob = alice
        .evaluate("(match (list-get (sessions) 1) ((session-info id name actor age) age))")
        .await?;
    let (alice_age, bob_age) = if alice.id.to_string() < bob.id.to_string() {
        (ages, ages_bob)
    } else {
        (ages_bob, ages)
    };
    assert!(
        alice_age.starts_with('5') || alice_age.starts_with('6'),
        "{alice_age}"
    );
    assert_eq!(bob_age, "0u64");

    assert!(alice.evaluate("(sessions 1)").await?.starts_with("error:"));
    assert!(alice.evaluate("(help)").await?.contains("(sessions)"));

    bob.stop().await?;
    assert_eq!(alice.evaluate("(list-len (sessions))").await?, "1");

    alice.stop().await?;
    runtime.shutdown().await?;
    Ok(())
}
