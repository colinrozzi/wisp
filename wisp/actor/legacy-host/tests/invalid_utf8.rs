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
