// A string that isn't UTF-8 must not cross to the host: Theater fails the call
// and the session dies (ticket 0048). A session takes bytes from strangers, so
// both ways it can happen are driven through a real actor here: an eval's result
// and a TCP handler's, which reaches the host when the firing is read.

use std::path::Path;
use std::time::{Duration, Instant};
use tokio::io::AsyncWriteExt;
use tokio::net::TcpStream;
use wisp_interpreter_actor::{Grants, Runtime, Session, source::SourceBundle};

async fn session() -> anyhow::Result<Session> {
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let runtime = Runtime::new(SourceBundle::default(), Grants::default()).await?;
    runtime.spawn(wasm).await
}

async fn assert_alive(session: &Session) -> anyhow::Result<()> {
    assert_eq!(session.evaluate("(+ 40 2)").await?, "42");
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_eval_returning_invalid_utf8_keeps_the_session() -> anyhow::Result<()> {
    let session = session().await?;
    assert_eq!(
        session
            .evaluate("(bytes->string (list 255 0 200 13 10))")
            .await?,
        r#""\xff\0\xc8\r\n""#
    );
    assert_alive(&session).await?;
    // State from before it is still there.
    session.evaluate("(define kept 7)").await?;
    session.evaluate(r#""\xff""#).await?;
    assert_eq!(session.evaluate("kept").await?, "7");
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_handler_returning_invalid_utf8_keeps_the_session() -> anyhow::Result<()> {
    let session = session().await?;
    let port = std::net::TcpListener::bind("127.0.0.1:0")?
        .local_addr()?
        .port();
    for form in [
        r#"(define on-connection (lambda (cid) (begin (tcp-activate cid) (tcp-set-active cid "active") "open")))"#,
        "(define on-data (lambda (e) (bytes->string (car (cdr e)))))",
        &format!(r#"(tcp-listen "127.0.0.1:{port}")"#),
    ] {
        let out = session.evaluate(form).await?;
        assert!(!out.starts_with("error"), "{form} -> {out}");
    }

    let mut stream = TcpStream::connect(("127.0.0.1", port)).await?;
    stream.write_all(&[b'h', b'i', 0xff, 0xc8]).await?;
    stream.flush().await?;

    // The text form, which `theater-repl read` and `follow` print: raw, with the
    // bad bytes marked.
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        let text = session.evaluate(r#"(events-after 0 16 "text")"#).await?;
        if text.contains("on-data: ") {
            assert!(text.contains(r"on-data: hi\xff\xc8"), "{text}");
            break;
        }
        assert!(Instant::now() < deadline, "no on-data firing: {text}");
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    assert_alive(&session).await?;

    // The other forms a reader asks for: `wait` and `read`'s wire text, --full's
    // lists (quoted, so the escape reads back as the byte), and the old poll.
    let wire = session.evaluate(r#"(events-after 0 16 "wire")"#).await?;
    assert!(wire.contains(r"on-data: hi\xff\xc8"), "{wire}");
    let full = session.evaluate("(events-after 0 16)").await?;
    assert!(full.contains(r#""hi\xff\xc8""#), "{full}");
    let polled = session.evaluate("(poll-events)").await?;
    assert!(polled.contains(r#""hi\xff\xc8""#), "{polled}");
    assert_alive(&session).await?;

    // And the session still takes events.
    stream.write_all(b"plain").await?;
    stream.flush().await?;
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        let text = session.evaluate(r#"(events-after 0 16 "text")"#).await?;
        if text.contains("on-data: plain") {
            break;
        }
        assert!(Instant::now() < deadline, "no second firing: {text}");
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    Ok(())
}

// Every host call taking a string, with the invalid string in each position that
// reaches the host as a string (ticket 0049). The host can't decode such a call and
// writes no result; the caller gets an (err ...) it can match, and the session lives.
const REJECTED: &[&str] = &[
    "(describe bad)",
    "(exports bad)",
    r#"(implements bad "i")"#,
    r#"(implements "a" bad)"#,
    r#"(call bad "f")"#,
    r#"(call "a" bad)"#,
    r#"(call "a" "f" bad)"#,
    "(store-get bad bad)",
    "(store-get-by-label bad bad)",
    "(store-list-labels bad)",
    "(store-exists bad bad)",
    "(store-size bad)",
    r#"(store-put bad "x")"#,
    "(store-label bad bad bad)",
    r#"(store-put-at bad bad "x")"#,
    "(actor-status bad)",
    "(actor-state bad)",
    "(actor-manifest bad)",
    "(stop-actor bad)",
    "(kill-actor bad)",
    "(spawn-actor bad)",
    "(chain-watch bad)",
    "(chain-unwatch bad)",
    "(chain-head bad)",
    "(chain-tail bad 1)",
    "(chain-verify bad)",
    "(key-new bad)",
    "(key-public bad)",
    r#"(sign bad "x")"#,
    r#"(verify bad "00" "x")"#,
    r#"(verify "00" bad "x")"#,
    r#"(msg-send bad "x")"#,
    r#"(msg-request bad "x")"#,
    r#"(msg-respond bad "x")"#,
    "(msg-cancel bad)",
    r#"(msg-open bad "x")"#,
    r#"(msg-send-channel bad "x")"#,
    "(msg-close-channel bad)",
    "(wat-to-wasm bad)",
    "(set-interval bad 100000)",
    "(clear-interval bad)",
    "(fs-read bad)",
    "(fs-read-text bad)",
    "(fs-exists bad)",
    "(fs-list bad)",
    "(fs-meta bad)",
    r#"(fs-write bad "x")"#,
    r#"(fs-append bad "x")"#,
    "(fs-delete bad)",
    "(fs-mkdir bad)",
    "(fs-rmdir bad)",
    "(fs-sync bad)",
    "(fs-sync-dir bad)",
    r#"(fs-rename bad "b")"#,
    r#"(fs-rename "a" bad)"#,
    r#"(fs-write-durable bad "x")"#,
    "(http-get bad)",
    r#"(http-req bad "http://127.0.0.1:9/")"#,
    r#"(http-req "GET" bad)"#,
    r#"(http-req "GET" "http://127.0.0.1:9/" (list (list bad "v")) "")"#,
    "(tcp-connect bad)",
    "(tcp-listen bad)",
    "(tcp-accept bad)",
    "(tcp-activate bad)",
    r#"(tcp-set-active bad "active")"#,
    r#"(tcp-set-active "c" bad)"#,
    r#"(tcp-transfer bad "a")"#,
    r#"(tcp-transfer "c" bad)"#,
    r#"(tcp-transfer-async bad "a")"#,
    "(tcp-peer bad)",
    "(tcp-is-tls bad)",
    r#"(tcp-send bad "x")"#,
    "(tcp-receive bad 1)",
    "(tcp-close bad)",
    "(tcp-close-listener bad)",
    "(tcp-tls-server bad)",
    r#"(tcp-tls-client bad "h")"#,
    r#"(tcp-tls-client "c" bad)"#,
    "(udp-bind bad)",
    "(udp-close bad)",
    r#"(udp-connect bad "a")"#,
    r#"(udp-connect "s" bad)"#,
    "(udp-disconnect bad)",
    "(udp-local-addr bad)",
    "(udp-connected-peer bad)",
    r#"(udp-send-to bad "p" "x")"#,
    r#"(udp-send-to "s" bad "x")"#,
    r#"(udp-send bad "x")"#,
    "(udp-recv-from bad 1)",
    r#"(udp-set-active bad "active")"#,
    "(udp-set-recv-timeout bad 1)",
    "(process-spawn bad)",
    r#"(process-spawn "true" (list bad))"#,
    r#"(process-spawn "true" (list) bad)"#,
    r#"(process-spawn "true" (list) "/" (list (list bad "v")))"#,
    "(process-run bad)",
    r#"(process-run "true" (list bad))"#,
    "(process-spawn-pty bad 80 24)",
    r#"(process-write-stdin bad "x")"#,
    "(process-close-stdin bad)",
    "(process-read bad 1)",
    r#"(process-set-active bad "active")"#,
    "(process-resize bad 80 24)",
    "(process-signal bad 15)",
    "(process-kill bad)",
    "(process-status bad)",
    "(process-wait bad)",
    "(process-os-pid bad)",
    "(process-close bad)",
    r#"(process-transfer bad "a")"#,
    r#"(process-transfer "p" bad)"#,
    r#"(podman-run bad "n")"#,
    r#"(podman-run "i" bad)"#,
    "(podman-stop bad)",
    "(podman-rm bad false)",
    "(monitor bad)",
    "(unmonitor bad)",
    "(link bad)",
    "(unlink bad)",
];

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_invalid_utf8_host_argument_is_an_error_the_caller_can_match() -> anyhow::Result<()> {
    let session = session().await?;
    session
        .evaluate("(define bad (bytes->string (list 255 200)))")
        .await?;
    let mut wrong = Vec::new();
    for form in REJECTED {
        let matched = format!(r#"(match {form} ((err e) "rejected") ((ok v) "accepted"))"#);
        // A dead session makes this call fail, which ends the test right here.
        let out = session.evaluate(&matched).await?;
        if out != r#""rejected""# {
            wrong.push(format!("{form} -> {out}"));
        }
        assert_alive(&session).await?;
    }
    assert!(wrong.is_empty(), "{}", wrong.join("\n"));
    Ok(())
}

// Calls that have no (err ...) to give: loading reports a failure, log keeps the
// message with the bad bytes marked, and the terminal takes bytes, so they arrive.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_invalid_utf8_in_load_and_log_keeps_the_session() -> anyhow::Result<()> {
    let session = session().await?;
    session
        .evaluate("(define bad (bytes->string (list 255 200)))")
        .await?;
    assert_eq!(session.evaluate("(log bad)").await?, "()");
    assert_alive(&session).await?;
    for form in ["(term-write bad)", "(term-write-err bad)"] {
        let out = session.evaluate(form).await?;
        assert!(out.starts_with("(ok"), "{form} -> {out}");
        assert_alive(&session).await?;
    }
    let out = session.evaluate("(require m bad)").await?;
    assert!(out.starts_with("error"), "{out}");
    assert_alive(&session).await?;
    let out = session.evaluate(r#"(require m "\xff")"#).await?;
    assert!(out.starts_with("error"), "{out}");
    assert_alive(&session).await?;
    let out = session.evaluate(r#"(include "\xff")"#).await?;
    assert!(out.starts_with("error"), "{out}");
    assert_alive(&session).await?;
    Ok(())
}
