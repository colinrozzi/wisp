use std::path::Path;
use theater::pack_bridge::Value;
use wisp_interpreter_actor::{Runtime, source::SourceBundle};

/// (poll-events) drains firings as (handler result) lists; (poll-events
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
        r#"(("on-tick" "a\nfired"))"#
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

/// A handled firing keeps (handler result); an unhandled one keeps the event too.
/// The buffer keeps the newest 256 firings and reports how many it dropped.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_event_ring_drops_oldest_and_counts() -> anyhow::Result<()> {
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let runtime = Runtime::new(SourceBundle::default(), Default::default()).await?;
    let session = runtime.spawn(wasm).await?;
    let tick = |name: String| {
        session.handle.call_function(
            "theater:simple/timer.handle-tick".into(),
            Value::String(name),
        )
    };
    // No handler yet: the event is kept.
    tick("a".into()).await?;
    assert_eq!(
        session.evaluate("(poll-events)").await?,
        r#"(("on-tick" "a" "no handler defined"))"#
    );
    session
        .evaluate("(define on-tick (lambda (name) name))")
        .await?;
    for i in 0..300 {
        tick(format!("t{i}")).await?;
    }
    let text = session.evaluate("(poll-events \"text\")").await?;
    assert!(
        text.starts_with(r#""(44 earlier firings dropped)\non-tick: t44\non-tick: t45"#),
        "{text}"
    );
    assert!(text.ends_with(r#"on-tick: t299""#), "{text}");
    assert_eq!(text.matches("on-tick:").count(), 256);
    assert_eq!(session.evaluate("(poll-events)").await?, "()");

    // The list form puts the count first.
    assert_eq!(session.evaluate("(event-buffer 2)").await?, "256");
    for i in 0..5 {
        tick(format!("u{i}")).await?;
    }
    assert_eq!(
        session.evaluate("(poll-events)").await?,
        r#"(3 ("on-tick" "u3") ("on-tick" "u4"))"#
    );
    // A failing handler keeps the event.
    session
        .evaluate("(define on-tick (lambda (name) (error name)))")
        .await?;
    tick("bad".into()).await?;
    assert_eq!(
        session.evaluate("(poll-events)").await?,
        r#"(("on-tick" "bad" error: bad))"#
    );
    // 0 turns buffering off; handlers still run.
    assert_eq!(session.evaluate("(event-buffer 0)").await?, "2");
    session
        .evaluate("(define on-tick (lambda (name) name))")
        .await?;
    tick("off".into()).await?;
    assert_eq!(session.evaluate("(poll-events \"text\")").await?, r#""""#);
    runtime.shutdown().await?;
    Ok(())
}

/// (handles) follows the open verbs, the close verbs, and the close/exit events.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_handles_track_open_and_close() -> anyhow::Result<()> {
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let runtime = Runtime::new(SourceBundle::default(), Default::default()).await?;
    let session = runtime.spawn(wasm).await?;
    assert_eq!(session.evaluate("(handles)").await?, "()");
    session.evaluate("(set-interval \"slow\" 600000)").await?;
    assert_eq!(
        session.evaluate("(handles)").await?,
        r#"(("timer" "slow"))"#
    );
    // An inbound connection, then its close event.
    session
        .handle
        .call_function(
            "theater:simple/tcp-client.handle-connection".into(),
            Value::String("c1".into()),
        )
        .await?;
    assert_eq!(
        session.evaluate("(handles)").await?,
        r#"(("timer" "slow") ("connection" "c1"))"#
    );
    session.evaluate("(clear-interval \"slow\")").await?;
    assert_eq!(
        session.evaluate("(handles)").await?,
        r#"(("connection" "c1"))"#
    );
    assert!(
        session
            .evaluate("(memory-stats)")
            .await?
            .contains(r#"("handles" 1)"#)
    );
    session
        .handle
        .call_function(
            "theater:simple/tcp-client.on-close".into(),
            Value::Tuple(vec![
                Value::String("c1".into()),
                Value::String("eof".into()),
            ]),
        )
        .await?;
    assert_eq!(session.evaluate("(handles)").await?, "()");
    runtime.shutdown().await?;
    Ok(())
}
