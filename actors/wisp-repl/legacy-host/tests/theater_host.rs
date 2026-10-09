//! The convergence seam, end to end: a Wisp form evaluated by the *shared Rust
//! interpreter* reaches a *live Theater runtime* through `TheaterHost`.
//!
//! `(list-actors)` is an imported call; under eval it dispatches to the host, which
//! marshals it to a `GetActors` command on the runtime's channel and blocks for the
//! reply. Same interpreter the local REPL uses — only the host differs. Proves both
//! the round-trip and that it reflects real runtime state (empty, then one actor).

use std::collections::BTreeMap;
use std::path::Path;
use theater::messages::TheaterCommand;
use tokio::sync::mpsc::UnboundedSender;
use wisp::compiler::{Outcome, Value};
use wisp_interpreter_actor::{EvalSession, Runtime, TheaterHost, source::SourceBundle};

/// The value of an expression outcome (panics on a definition/binding).
fn evaluated(outcome: Outcome) -> Value {
    match outcome {
        Outcome::Evaluated { value, .. } => value,
        other => panic!("expected an evaluated expression, got {other:?}"),
    }
}

/// A program whose entry calls the imported `list-actors` and returns the ids.
const LIST_ACTORS: &str = r#"
(import host list-actors () (list string))
(export (fn test-func () (list string) (list-actors)))
"#;

/// Evaluate `(list-actors)` with the shared interpreter + a `TheaterHost`, on a
/// blocking thread (eval is sync; the host `blocking_recv`s the runtime's reply).
async fn live_actor_ids(commands: UnboundedSender<TheaterCommand>) -> anyhow::Result<Vec<String>> {
    let value = tokio::task::spawn_blocking(move || {
        let mut host = TheaterHost::new(commands);
        wisp::compiler::eval_source_with_host(LIST_ACTORS, &mut host)
    })
    .await??;
    match value {
        Value::List(items) => Ok(items
            .into_iter()
            .map(|v| match v {
                Value::Str(s) => s,
                other => panic!("expected a string id, got {other:?}"),
            })
            .collect()),
        other => panic!("expected a list, got {other:?}"),
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn eval_reaches_live_runtime_through_theater_host() -> anyhow::Result<()> {
    let runtime = Runtime::new(SourceBundle::new(BTreeMap::new())?).await?;

    // Nothing spawned yet: the interpreter's (list-actors) sees an empty runtime.
    assert!(live_actor_ids(runtime.commands.clone()).await?.is_empty());

    // Spawn the interpreter actor; now the same form sees exactly it.
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let session = runtime.spawn(wasm).await?;
    let ids = live_actor_ids(runtime.commands.clone()).await?;
    assert_eq!(ids, vec![session.id.to_string()]);

    runtime.shutdown().await?;
    Ok(())
}

// A host-side REPL session runs entirely on the shared Rust interpreter — not the
// Wisp-interpreter actor — accumulating definitions across inputs and reaching the
// live runtime through its Theater interface. Proves the unified session: a function
// definition, a binding that uses it, and a Theater-reaching call, all in one
// session on the same interpreter the local REPL uses.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn eval_session_runs_on_shared_interpreter() -> anyhow::Result<()> {
    let runtime = Runtime::new(SourceBundle::new(BTreeMap::new())?).await?;
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let session_actor = runtime.spawn(wasm).await?;
    let expected_id = session_actor.id.to_string();
    let commands = runtime.commands.clone();

    let (pure, bound, live) = tokio::task::spawn_blocking(move || -> anyhow::Result<_> {
        let mut session = EvalSession::new(commands);
        let pure = evaluated(session.feed("(i32.add 40 2)")?);
        session.feed("(fn dbl ((x s32)) s32 (i32.mul x 2))")?; // definition persists
        let bound = evaluated(session.feed("(dbl 21)")?); // ...and is callable
        let live = evaluated(session.feed("(list-actors)")?); // ...alongside a Theater call
        Ok((pure, bound, live))
    })
    .await??;

    assert_eq!(pure, Value::Int(42));
    assert_eq!(bound, Value::Int(42));
    assert_eq!(live, Value::List(vec![Value::Str(expected_id)]));

    runtime.shutdown().await?;
    Ok(())
}
