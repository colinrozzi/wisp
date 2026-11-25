// (tcp-set-write-timeout ms) bounds a send: against a peer that accepts and
// then never reads (a sleeping browser tab holds a zero window, and the
// kernel never times that out; live on server3 sat 9 minutes in one send on
// 2026-11-24), (tcp-send conn data) returns an error naming the timeout
// once the buffers are full, instead of waiting forever, and the session
// lives on. 0 turns it off again. Driven through a real actor, like
// tcp_read_timeout.rs.

use std::path::Path;
use std::time::{Duration, Instant};
use wisp_interpreter_actor::{Grants, Runtime, Session, source::SourceBundle};

async fn session() -> anyhow::Result<Session> {
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let runtime = Runtime::new(SourceBundle::default(), Grants::default()).await?;
    runtime.spawn(wasm).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_send_gives_up_after_the_write_timeout() -> anyhow::Result<()> {
    // a peer that accepts and then never reads
    let listener = std::net::TcpListener::bind("127.0.0.1:0")?;
    let port = listener.local_addr()?.port();
    let deaf = std::thread::spawn(move || {
        let (conn, _) = listener.accept().expect("accept");
        std::thread::sleep(Duration::from_secs(20));
        drop(conn);
    });

    let session = session().await?;
    let set = session.evaluate("(tcp-set-write-timeout 300)").await?;
    assert!(set.starts_with("(ok"), "set-write-timeout: {set}");
    let conn = session
        .evaluate(&format!(
            r#"(define c (match (tcp-connect "127.0.0.1:{port}") ((ok v) v) ((err e) e)))"#
        ))
        .await?;
    assert!(!conn.contains("error"), "connect: {conn}");
    // 16 MiB, far more than both sockets' buffers
    session
        .evaluate(r#"(define big "0123456789abcdef")"#)
        .await?;
    for _ in 0..20 {
        session
            .evaluate("(define big (string-append big big))")
            .await?;
    }

    let started = Instant::now();
    let got = session.evaluate("(tcp-send c big)").await?;
    let took = started.elapsed();
    assert!(
        got.contains("timeout"),
        "expected a timeout error, got {got}"
    );
    assert!(took < Duration::from_secs(10), "the send took {took:?}");

    assert_eq!(session.evaluate("(+ 40 2)").await?, "42");
    let off = session.evaluate("(tcp-set-write-timeout 0)").await?;
    assert!(off.starts_with("(ok"), "turning it off: {off}");
    let bad = session.evaluate("(tcp-set-write-timeout)").await?;
    assert!(bad.contains("expects"), "no argument: {bad}");
    drop(deaf);
    Ok(())
}
