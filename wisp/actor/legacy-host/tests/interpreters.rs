//! The runtime is the session directory: `Runtime::interpreters` finds every actor
//! that exports `theater:simple/wisp.evaluate`, however it was spawned, and
//! `Runtime::find` reaches one by full id, id prefix, or runtime name.

use std::collections::BTreeMap;
use std::path::Path;
use theater::ManifestConfig;
use wisp_interpreter_actor::{MANIFEST, Runtime, source::SourceBundle};

fn repo_file(path: &str) -> Vec<u8> {
    std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join(path)).unwrap()
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn interpreters_are_found_and_reached_through_the_runtime() -> anyhow::Result<()> {
    let runtime = Runtime::new(SourceBundle::new(BTreeMap::new())?, Default::default()).await?;
    let wasm = repo_file("../actor.wasm");
    let manifest = ManifestConfig::from_toml_str(MANIFEST)?;

    let alice = runtime
        .spawn_named(wasm.clone(), manifest.clone(), "alice".into())
        .await?;
    // Spawned the way `spawn-actor` does: the runtime name is the manifest's.
    let other = runtime.spawn_with_manifest(wasm, manifest).await?;
    // An actor that is not an interpreter: never listed, never reachable.
    let hello = runtime
        .spawn_with_manifest(
            repo_file("tests/fixtures/not-wisp.wasm"),
            ManifestConfig::from_toml_str(
                "name = \"hello\"\nversion = \"0.1.0\"\npackage = \"not-wisp.wasm\"\n",
            )?,
        )
        .await?;

    let found = runtime.interpreters().await?;
    let mut names: Vec<_> = found.iter().map(|i| i.name.as_str()).collect();
    names.sort();
    assert_eq!(names, ["alice", "wisp-interpreter"]);
    assert!(found.iter().all(|i| i.id != hello.id));

    // Definitions persist across separately found handles: the state is the actor's.
    runtime
        .find("alice")
        .await?
        .evaluate("(define x 41)")
        .await?;
    let full = alice.id.to_string();
    assert_eq!(runtime.find(&full).await?.evaluate("(+ x 1)").await?, "42");
    assert_eq!(runtime.find(&full[..8]).await?.evaluate("x").await?, "41");
    assert_eq!(runtime.find("wisp-interpreter").await?.id, other.id);

    let missing = runtime.find("hello").await.err().unwrap().to_string();
    assert_eq!(missing, "no such session: hello");

    // A stopped actor leaves the directory with nothing else to clean up.
    runtime.find("alice").await?.stop().await?;
    assert!(runtime.find("alice").await.is_err());
    assert_eq!(runtime.interpreters().await?.len(), 1);

    other.stop().await?;
    hello.stop().await?;
    runtime.shutdown().await
}
