//! No actor may rpc a session (theater #251, `callers = []`): only the host,
//! the daemon serving the CLI, runs a session's evaluate. Before, `(call ...)`
//! from one session could run code inside another with its grant.

use std::collections::BTreeMap;
use std::path::Path;
use theater::ManifestConfig;
use wisp_interpreter_actor::{MANIFEST, Runtime, source::SourceBundle};

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_session_cannot_call_into_another_session() -> anyhow::Result<()> {
    let runtime = Runtime::new(SourceBundle::new(BTreeMap::new())?, Default::default()).await?;
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let manifest = ManifestConfig::from_toml_str(MANIFEST)?;
    let alice = runtime
        .spawn_named(wasm.clone(), manifest.clone(), "alice".into())
        .await?;
    let bob = runtime.spawn_named(wasm, manifest, "bob".into()).await?;

    let out = alice
        .evaluate(&format!(
            r#"(call "{}" "theater:simple/wisp.evaluate" "(define pwned 1)")"#,
            bob.id
        ))
        .await?;
    assert!(
        out.contains("rpc refused") && out.contains("no actor (only the host)"),
        "{out}"
    );
    // Nothing ran in bob; the host still reaches him.
    assert!(bob.evaluate("pwned").await?.contains("error"));
    assert_eq!(bob.evaluate("(+ 1 2)").await?, "3");
    Ok(())
}
