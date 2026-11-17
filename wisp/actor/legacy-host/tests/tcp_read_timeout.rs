// (tcp-set-read-timeout ms) bounds a pull receive: against a peer that
// accepts and then never sends (a half-open connection, as wedged field's
// peer for five hours on 2026-11-17), (tcp-receive conn n) returns an error
// naming the timeout instead of waiting forever, and the session lives on.
// 0 turns it off again. Driven through a real actor, like invalid_utf8.rs.

use std::path::Path;
use std::time::{Duration, Instant};
use wisp_interpreter_actor::{Grants, Runtime, Session, source::SourceBundle};

async fn session() -> anyhow::Result<Session> {
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let runtime = Runtime::new(SourceBundle::default(), Grants::default()).await?;
    runtime.spawn(wasm).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_pull_receive_gives_up_after_the_read_timeout() -> anyhow::Result<()> {
    // a peer that accepts and then says nothing
    let listener = std::net::TcpListener::bind("127.0.0.1:0")?;
    let port = listener.local_addr()?.port();
    let silent = std::thread::spawn(move || {
        let (conn, _) = listener.accept().expect("accept");
        std::thread::sleep(Duration::from_secs(10));
        drop(conn);
    });

    let session = session().await?;
    let set = session.evaluate("(tcp-set-read-timeout 300)").await?;
    assert!(set.starts_with("(ok"), "set-read-timeout: {set}");
    let conn = session
        .evaluate(&format!(
            r#"(define c (match (tcp-connect "127.0.0.1:{port}") ((ok v) v) ((err e) e)))"#
        ))
        .await?;
    assert!(!conn.contains("error"), "connect: {conn}");

    let started = Instant::now();
    let got = session.evaluate("(tcp-receive c 10)").await?;
    let took = started.elapsed();
    assert!(
        got.contains("timeout"),
        "expected a timeout error, got {got}"
    );
    assert!(took < Duration::from_secs(5), "the receive took {took:?}");

    assert_eq!(session.evaluate("(+ 40 2)").await?, "42");
    let off = session.evaluate("(tcp-set-read-timeout 0)").await?;
    assert!(off.starts_with("(ok"), "turning it off: {off}");
    let bad = session.evaluate("(tcp-set-read-timeout)").await?;
    assert!(bad.contains("expects"), "no argument: {bad}");

    session.evaluate("(tcp-close c)").await?;
    drop(silent);
    Ok(())
}
