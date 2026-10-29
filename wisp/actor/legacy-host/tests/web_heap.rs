// A session serving HTTP keeps its memory flat (ticket 0030). The server is
// colin's site (experiment/site/web.wisp, copied to tests/fixtures/site/): each
// request is three firings (on-connection, on-data, on-close) and the host calls
// tcp-activate, tcp-set-active, fs-read, tcp-send and tcp-close. Before reference
// counting each request retained about 127 KB; with it, nothing but the event
// buffer (bounded: 256 firings) stays.
//
// WISP_WEB_REQUESTS=<n> serves n measured requests instead of 1,000;
// WISP_ACTOR_WASM=<path> measures another build of the actor. The numbers print
// with --nocapture.

use std::path::{Path, PathBuf};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;
use wisp_interpreter_actor::{FsAccess, Grants, Runtime, Session, source::SourceBundle};

fn actor_path() -> PathBuf {
    std::env::var_os("WISP_ACTOR_WASM")
        .map(PathBuf::from)
        .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))
}

/// One count from `(memory-stats)`, which prints as `(("name" n) ...)`.
fn stat(stats: &str, name: &str) -> Option<i64> {
    let rest = stats.split(&format!("(\"{name}\" ")).nth(1)?;
    rest[..rest.find(')')?].parse().ok()
}

/// heap-bytes, and heap-pages (absent before ticket 0030's actor).
async fn memory(session: &Session) -> anyhow::Result<(i64, Option<i64>)> {
    let stats = session.evaluate("(memory-stats)").await?;
    let bytes = stat(&stats, "heap-bytes").ok_or_else(|| anyhow::anyhow!("{stats}"))?;
    Ok((bytes, stat(&stats, "heap-pages")))
}

async fn get(port: u16) -> anyhow::Result<String> {
    let mut stream = TcpStream::connect(("127.0.0.1", port)).await?;
    stream
        .write_all(b"GET / HTTP/1.1\r\nHost: test\r\n\r\n")
        .await?;
    let mut response = Vec::new();
    stream.read_to_end(&mut response).await?; // the server closes after answering
    Ok(String::from_utf8(response)?)
}

async fn serve(session: &Session, port: u16, n: usize) -> anyhow::Result<()> {
    for _ in 0..n {
        let response = get(port).await?;
        assert!(
            response.starts_with("HTTP/1.1 200 OK") && response.contains("web-heap test"),
            "unexpected response: {response}"
        );
    }
    // The last on-close may still be in the mailbox; a later evaluation waits
    // behind it, so this settles the session before it is measured.
    session.evaluate("0").await?;
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_web_server_requests_keep_memory_flat() -> anyhow::Result<()> {
    let root = std::env::temp_dir().join(format!("wisp-web-heap-{}", std::process::id()));
    let site = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/site");
    std::fs::create_dir_all(root.join("public"))?;
    std::fs::copy(site.join("web.wisp"), root.join("web.wisp"))?;
    std::fs::copy(
        site.join("public/index.html"),
        root.join("public/index.html"),
    )?;
    let port = std::net::TcpListener::bind("127.0.0.1:0")?
        .local_addr()?
        .port();

    let wasm = std::fs::read(actor_path())?;
    let fs = FsAccess {
        root: Some(root.clone()),
        ..Default::default()
    };
    let grants = Grants {
        fs,
        ..Default::default()
    };
    let runtime = Runtime::new(SourceBundle::default(), grants).await?;
    let session = runtime.spawn(wasm).await?;
    // site/serve.wisp, on a free port.
    for form in [
        r#"(require web "web.wisp")"#,
        "(define on-connection (lambda (cid) (web-connection cid)))",
        "(define on-close (lambda (e) (car e)))",
        "(define on-data (lambda (e) (web-data e)))",
        &format!(r#"(tcp-listen "127.0.0.1:{port}")"#),
    ] {
        let out = session.evaluate(form).await?;
        assert!(!out.starts_with("error"), "{form} -> {out}");
    }

    // Warm up: past the event buffer's first trim (256 kept, trimmed at 512
    // firings; 3 per request), and any first-use growth.
    serve(&session, port, 200).await?;
    let (bytes_before, pages_before) = memory(&session).await?;
    let n: usize = std::env::var("WISP_WEB_REQUESTS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(1000);
    serve(&session, port, n).await?;
    let (bytes_after, pages_after) = memory(&session).await?;
    let per_request = (bytes_after - bytes_before) as f64 / n as f64;
    println!(
        "{n} requests: heap-bytes {bytes_before} -> {bytes_after} ({per_request:.1} B/request), \
         heap-pages {pages_before:?} -> {pages_after:?}"
    );
    // (memory-stats) trims the buffer to exactly 256 firings, so the two
    // measurements hold the same number of firings; firings differ in size only
    // by a few bytes (connection ids). Without RC this was ~127 KB per request.
    assert!(
        bytes_after - bytes_before < 4096,
        "{n} requests retained {} B ({per_request:.1} B/request)",
        bytes_after - bytes_before
    );
    assert_eq!(pages_after, pages_before, "memory grew over {n} requests");

    runtime.shutdown().await?;
    let _ = std::fs::remove_dir_all(&root);
    Ok(())
}
