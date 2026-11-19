//! Handlers behind a gate (0156): podman is off unless config.toml has [podman],
//! and its verbs then refuse while still satisfying the interpreter's import; the
//! lifecycle and chain verbs stay on for actors the host starts.

use std::collections::BTreeMap;
use std::path::Path;
use theater::ManifestConfig;
use wisp_interpreter_actor::{MANIFEST, Runtime, source::SourceBundle};

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn podman_is_off_by_default_and_the_others_stay_on() -> anyhow::Result<()> {
    let runtime = Runtime::new(SourceBundle::new(BTreeMap::new())?, Default::default()).await?;
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let manifest = ManifestConfig::from_toml_str(MANIFEST)?;
    let session = runtime.spawn_named(wasm, manifest, "gates".into()).await?;
    let listed = session.evaluate("(podman-list)").await?;
    assert!(
        listed.contains("podman is not granted to this actor"),
        "{listed}"
    );
    // lifecycle answers the call itself (a bad id), not the gate.
    let monitored = session.evaluate("(monitor \"not-an-actor\")").await?;
    assert!(!monitored.contains("not granted"), "{monitored}");
    let head = session.evaluate("(chain-head \"not-an-actor\")").await?;
    assert!(!head.contains("not granted"), "{head}");
    runtime.shutdown().await
}
