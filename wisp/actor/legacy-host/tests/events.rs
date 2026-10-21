use std::path::Path;
use theater::pack_bridge::Value;
use wisp_interpreter_actor::{Runtime, source::SourceBundle};

/// (poll-events) drains firings as (handler event result) lists; (poll-events
/// "text") drains them as "handler: result" lines, a string result raw.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_poll_events_full_and_text() -> anyhow::Result<()> {
    // The actor built by wisp/actor/build.sh, the one the binary embeds.
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let runtime = Runtime::new(SourceBundle::default(), Default::default()).await?;
    let session = runtime.spawn(wasm).await?;
    let tick = |name: &str| {
        session.handle.call_function(
            "theater:simple/timer.handle-tick".into(),
            Value::String(name.into()),
        )
    };
    session
        .evaluate("(define on-tick (lambda (name) (string-append name \"\\nfired\")))")
        .await?;
    tick("a").await?;
    assert_eq!(
        session.evaluate("(poll-events)").await?,
        r#"(("on-tick" "a" "a\nfired"))"#
    );
    assert_eq!(session.evaluate("(poll-events \"text\")").await?, r#""""#);

    session
        .evaluate("(define on-tick (lambda (name) (list name 1)))")
        .await?;
    tick("b").await?;
    tick("c").await?;
    assert_eq!(
        session.evaluate("(poll-events \"text\")").await?,
        r#""on-tick: (\"b\" 1)\non-tick: (\"c\" 1)""#
    );
    assert_eq!(session.evaluate("(poll-events)").await?, "()");
    assert!(
        session
            .evaluate("(poll-events \"lines\")")
            .await?
            .starts_with("error:")
    );
    runtime.shutdown().await?;
    Ok(())
}
