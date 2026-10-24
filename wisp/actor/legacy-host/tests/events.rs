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
    // 0 turns buffering off; handlers still run. The firing still takes a seq,
    // so a reader is told it was dropped rather than never hearing of it.
    assert_eq!(session.evaluate("(event-buffer 0)").await?, "2");
    session
        .evaluate("(define on-tick (lambda (name) name))")
        .await?;
    tick("off".into()).await?;
    assert_eq!(
        session.evaluate("(poll-events \"text\")").await?,
        r#""(1 earlier firings dropped)""#
    );
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

/// A session with an `on-tick` that returns the tick's name, and a way to fire it.
async fn ticking() -> anyhow::Result<(Runtime, wisp_interpreter_actor::Session)> {
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let runtime = Runtime::new(SourceBundle::default(), Default::default()).await?;
    let session = runtime.spawn(wasm).await?;
    session
        .evaluate("(define on-tick (lambda (name) name))")
        .await?;
    Ok((runtime, session))
}

async fn fire(session: &wisp_interpreter_actor::Session, name: &str) -> anyhow::Result<()> {
    session
        .handle
        .call_function(
            "theater:simple/timer.handle-tick".into(),
            Value::String(name.into()),
        )
        .await?;
    Ok(())
}

/// Firings are numbered 1, 2, 3, ...; reading after a seq removes nothing.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_events_after_numbers_firings_and_removes_nothing() -> anyhow::Result<()> {
    let (runtime, session) = ticking().await?;
    assert_eq!(session.evaluate("(event-head)").await?, "0");
    assert_eq!(session.evaluate("(events-after 0)").await?, "(0 0)");
    for name in ["a", "b", "c", "d", "e"] {
        fire(&session, name).await?;
    }
    assert_eq!(session.evaluate("(event-head)").await?, "5");
    let tail = r#"(5 0 (4 "on-tick" "d") (5 "on-tick" "e"))"#;
    assert_eq!(session.evaluate("(events-after 3)").await?, tail);
    assert_eq!(session.evaluate("(events-after 3)").await?, tail);
    assert_eq!(
        session.evaluate("(events-after 0 2)").await?,
        r#"(2 0 (1 "on-tick" "a") (2 "on-tick" "b"))"#
    );
    assert_eq!(
        session.evaluate("(events-after 3 \"text\")").await?,
        r#""on-tick: d\non-tick: e""#
    );
    assert_eq!(
        session.evaluate("(events-after 3 \"wire\")").await?,
        r#""5 0\non-tick: d\non-tick: e""#
    );
    assert_eq!(
        session.evaluate("(events-after 5 \"wire\")").await?,
        r#""5 0\n""#
    );
    assert!(
        session
            .evaluate("(events-after 6)")
            .await?
            .contains("cursor 6 is after head 5")
    );
    // An unhandled firing keeps its event, after the seq.
    session.evaluate("(define on-tick 1)").await?;
    fire(&session, "f").await?;
    assert!(
        session
            .evaluate("(events-after 5)")
            .await?
            .starts_with(r#"(6 0 (6 "on-tick" "f" "#)
    );
    runtime.shutdown().await?;
    Ok(())
}

/// Each cursor is its own reader: acknowledging one does not move another.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_cursors_are_independent_and_never_move_back() -> anyhow::Result<()> {
    let (runtime, session) = ticking().await?;
    for name in ["a", "b", "c"] {
        fire(&session, name).await?;
    }
    // A new cursor starts at 0: it sees everything still buffered.
    assert_eq!(session.evaluate("(event-cursor \"monitor\")").await?, "0");
    assert_eq!(session.evaluate("(event-ack \"monitor\" 2)").await?, "2");
    assert_eq!(
        session.evaluate("(event-read \"monitor\")").await?,
        r#"(3 0 (3 "on-tick" "c"))"#
    );
    assert_eq!(
        session.evaluate("(event-read \"human\" \"text\")").await?,
        r#""on-tick: a\non-tick: b\non-tick: c""#
    );
    // Reading does not acknowledge; acknowledging never moves back.
    assert_eq!(session.evaluate("(event-cursor \"monitor\")").await?, "2");
    assert_eq!(session.evaluate("(event-ack \"monitor\" 1)").await?, "2");
    assert!(
        session
            .evaluate("(event-ack \"monitor\" 9)")
            .await?
            .contains("seq 9 is after head 3")
    );
    assert_eq!(
        session.evaluate("(event-cursors)").await?,
        r#"(("human" 0) ("monitor" 2))"#
    );
    session.evaluate("(event-cursor-drop \"human\")").await?;
    assert_eq!(
        session.evaluate("(event-cursors)").await?,
        r#"(("monitor" 2))"#
    );
    // poll-events reads through its own cursor: it takes nothing from "monitor".
    assert_eq!(
        session.evaluate("(poll-events \"text\")").await?,
        r#""on-tick: a\non-tick: b\non-tick: c""#
    );
    assert_eq!(session.evaluate("(poll-events)").await?, "()");
    assert_eq!(
        session.evaluate("(event-read \"monitor\")").await?,
        r#"(3 0 (3 "on-tick" "c"))"#
    );
    runtime.shutdown().await?;
    Ok(())
}

/// Firings the limit dropped are counted for each reader, also before the lazy
/// trim has run, and a reader that lost them moves past them.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_lost_firings_are_reported_per_reader() -> anyhow::Result<()> {
    let (runtime, session) = ticking().await?;
    session.evaluate("(event-buffer 4)").await?;
    for i in 1..=7 {
        fire(&session, &format!("t{i}")).await?;
    }
    // 7 firings is under the lazy trim's 2x, so they are all still in the list.
    assert_eq!(
        session.evaluate("(events-after 0)").await?,
        r#"(7 3 (4 "on-tick" "t4") (5 "on-tick" "t5") (6 "on-tick" "t6") (7 "on-tick" "t7"))"#
    );
    assert_eq!(
        session.evaluate("(events-after 5 \"text\")").await?,
        r#""on-tick: t6\non-tick: t7""#
    );
    assert_eq!(
        session.evaluate("(events-after 1 \"text\")").await?,
        r#""(2 earlier firings dropped)\non-tick: t4\non-tick: t5\non-tick: t6\non-tick: t7""#
    );
    // Nothing buffered but firings lost: next is the head.
    session.evaluate("(event-buffer 0)").await?;
    fire(&session, "t8").await?;
    assert_eq!(session.evaluate("(events-after 7)").await?, "(8 1)");
    runtime.shutdown().await?;
    Ok(())
}

/// Past 16 cursors, a new one replaces the least recently used.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_cursor_table_is_bounded() -> anyhow::Result<()> {
    let (runtime, session) = ticking().await?;
    for i in 0..16 {
        session
            .evaluate(&format!("(event-cursor \"c{i}\")"))
            .await?;
    }
    // Touch c0 so c1 is the least recently used.
    session.evaluate("(event-cursor \"c0\")").await?;
    session.evaluate("(event-cursor \"c16\")").await?;
    let cursors = session.evaluate("(event-cursors)").await?;
    assert_eq!(cursors.matches("(\"c").count(), 16, "{cursors}");
    assert!(
        cursors.contains("\"c0\"") && cursors.contains("\"c16\""),
        "{cursors}"
    );
    assert!(!cursors.contains("\"c1\""), "{cursors}");
    runtime.shutdown().await?;
    Ok(())
}

/// Each firing reports its seq to the daemon's watch, without an evaluation.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_firings_report_their_seq_to_the_daemon() -> anyhow::Result<()> {
    let (runtime, session) = ticking().await?;
    let mut recorded = runtime.events.subscribe(session.id);
    assert_eq!(*recorded.borrow_and_update(), 0);
    fire(&session, "a").await?;
    fire(&session, "b").await?;
    recorded.changed().await?;
    assert_eq!(*recorded.borrow(), 2);
    runtime.shutdown().await?;
    Ok(())
}
