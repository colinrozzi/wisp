//! Outbound HTTP through a real daemon: config `[http] allowed_hosts` decides
//! which hosts the REPL may reach, and `http-req` sends headers and a body.
//! The server is a one-request-at-a-time loopback listener, not the internet.

use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpListener;
use std::path::Path;
use std::process::{Child, Command, Output};
use std::sync::mpsc;
use std::time::{Duration, Instant};

const BIN: &str = env!("CARGO_BIN_EXE_theater-repl");

/// A daemon on a free port with `--config config`, killed when dropped.
struct Daemon {
    child: Child,
    port: u16,
}

impl Daemon {
    fn start(work: &Path, config: &Path) -> Self {
        let port = free_port();
        let child = Command::new(BIN)
            .args(["-p", &port.to_string(), "--config"])
            .arg(config)
            .arg("serve")
            .current_dir(work)
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn()
            .unwrap();
        let daemon = Self { child, port };
        let deadline = Instant::now() + Duration::from_secs(60);
        while !daemon.cli(&["status"]).status.success() {
            assert!(Instant::now() < deadline, "daemon did not come up");
            std::thread::sleep(Duration::from_millis(200));
        }
        daemon
    }

    fn cli(&self, args: &[&str]) -> Output {
        Command::new(BIN)
            .args(["-p", &self.port.to_string()])
            .args(args)
            .output()
            .unwrap()
    }

    fn ok(&self, args: &[&str]) -> String {
        let out = self.cli(args);
        assert!(out.status.success(), "{args:?}: {}", text(&out.stderr));
        text(&out.stdout)
    }
}

impl Drop for Daemon {
    fn drop(&mut self) {
        let _ = self.cli(&["down"]);
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).trim().to_string()
}

fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

/// Serve requests on `listener` forever, answering each `200 hi`, and send the
/// request head (lowercased) and body of each to `seen`.
fn serve(listener: TcpListener, seen: mpsc::Sender<(String, String)>) {
    for stream in listener.incoming() {
        let Ok(mut stream) = stream else { continue };
        let mut reader = BufReader::new(stream.try_clone().unwrap());
        let mut head = String::new();
        loop {
            let mut line = String::new();
            if reader.read_line(&mut line).unwrap_or(0) == 0 || line == "\r\n" {
                break;
            }
            head.push_str(&line.to_lowercase());
        }
        let len = head
            .lines()
            .find_map(|l| l.strip_prefix("content-length:"))
            .map_or(0, |n| n.trim().parse().unwrap());
        let mut body = vec![0; len];
        reader.read_exact(&mut body).unwrap();
        let _ = stream.write_all(
            b"HTTP/1.1 200 OK\r\nContent-Length: 2\r\nX-Reply: yes\r\nConnection: close\r\n\r\nhi",
        );
        let _ = seen.send((head, String::from_utf8(body).unwrap()));
    }
}

#[test]
fn http_allowlist_comes_from_config_and_http_req_sends_headers() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/http-test")
        .join(std::process::id().to_string());
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    // 127.0.0.1 is granted; localhost (the same server) is not.
    let config = root.join("config.toml");
    std::fs::write(&config, "[http]\nallowed_hosts = [\"127.0.0.1\"]\n").unwrap();

    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    let (tx, seen) = mpsc::channel();
    std::thread::spawn(move || serve(listener, tx));

    let daemon = Daemon::start(&root, &config);
    let id = daemon.ok(&["new", "http"]);

    // A host not in the list is refused before any request is made.
    let refused = daemon.ok(&[
        "eval",
        &id,
        &format!("(http-get \"http://localhost:{port}/\")"),
    ]);
    assert!(
        refused.contains("host 'localhost' not in allowed_hosts"),
        "{refused}"
    );
    // So is one of the built-in defaults: the config replaces the list.
    let refused = daemon.ok(&["eval", &id, "(http-get \"http://example.com/\")"]);
    assert!(refused.contains("not in allowed_hosts"), "{refused}");
    assert!(seen.try_recv().is_err());

    // A configured host is allowed: plain http-get.
    let got = daemon.ok(&[
        "eval",
        &id,
        &format!("(http-get \"http://127.0.0.1:{port}/plain\")"),
    ]);
    assert!(got.contains("(http-response 200 "), "{got}");
    let (head, body) = seen.recv_timeout(Duration::from_secs(10)).unwrap();
    assert!(head.starts_with("get /plain "), "{head}");
    assert_eq!(body, "");

    // http-req with headers and a string body: both reach the server, and the
    // response's headers and body come back.
    let form = format!(
        "(http-req \"POST\" \"http://127.0.0.1:{port}/post\" \
           (list (list \"User-Agent\" \"wisp-test\") (list \"X-Token\" \"abc 123\")) \"hello\")"
    );
    let got = daemon.ok(&["eval", &id, &form]);
    assert!(got.contains("(http-response 200 "), "{got}");
    assert!(got.contains("(http-header \"x-reply\" \"yes\")"), "{got}");
    assert!(got.contains("#<u8 104> #<u8 105>"), "{got}");
    let (head, body) = seen.recv_timeout(Duration::from_secs(10)).unwrap();
    assert!(head.starts_with("post /post "), "{head}");
    assert!(head.contains("user-agent: wisp-test\r\n"), "{head}");
    assert!(head.contains("x-token: abc 123\r\n"), "{head}");
    assert_eq!(body, "hello");

    // A bytes body and an empty header list.
    let form = format!(
        "(http-req \"PUT\" \"http://127.0.0.1:{port}/put\" (list) (string->bytes \"raw\"))"
    );
    let got = daemon.ok(&["eval", &id, &form]);
    assert!(got.contains("(http-response 200 "), "{got}");
    let (head, body) = seen.recv_timeout(Duration::from_secs(10)).unwrap();
    assert!(head.starts_with("put /put "), "{head}");
    assert_eq!(body, "raw");

    // Bad arguments are reported, not sent.
    let out = daemon.cli(&[
        "eval",
        &id,
        &format!("(http-req \"GET\" \"http://127.0.0.1:{port}/\" (list \"x\"))"),
    ]);
    assert!(!out.status.success());
    let bad = text(&out.stderr);
    assert!(bad.contains("(name value) string pairs"), "{bad}");
    assert!(seen.try_recv().is_err());
}
