//! The daemon over its socket, driven through the real `theater-repl` binary:
//! per-session actors (`new --manifest`) and a changeable default (`set-default`).

use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output};
use std::time::{Duration, Instant};

const BIN: &str = env!("CARGO_BIN_EXE_theater-repl");

/// A daemon on a free port, brought down (or killed) when dropped.
struct Daemon {
    child: Child,
    port: u16,
}

impl Daemon {
    fn start(work: &Path) -> Self {
        Self::start_with(work, &[])
    }

    /// A daemon with these arguments before `serve` (such as `--config`).
    /// Its HOME is `work`, so a machine's own ~/.config/theater-repl/config.toml
    /// (services, a filesystem root) doesn't change what the test sees.
    fn start_with(work: &Path, args: &[&str]) -> Self {
        let port = std::net::TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
        let child = Command::new(BIN)
            .args(["-p", &port.to_string()])
            .args(args)
            .arg("serve")
            .current_dir(work)
            .env("HOME", work)
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
        self.cli_in(Path::new("."), args)
    }

    fn cli_in(&self, dir: &Path, args: &[&str]) -> Output {
        Command::new(BIN)
            .args(["-p", &self.port.to_string()])
            .args(args)
            .current_dir(dir)
            .output()
            .unwrap()
    }

    /// Run a command that must succeed; its stdout, trimmed.
    fn ok(&self, args: &[&str]) -> String {
        let out = self.cli(args);
        assert!(out.status.success(), "{args:?}: {}", text(&out.stderr));
        text(&out.stdout)
    }

    /// Run a command that must fail; its stderr.
    fn fails(&self, args: &[&str]) -> String {
        let out = self.cli(args);
        assert!(!out.status.success(), "{args:?} should fail");
        text(&out.stderr)
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

/// A directory holding the Wisp actor's manifest and `wasm` as its package.
fn actor_dir(root: &Path, name: &str, wasm: &[u8]) -> PathBuf {
    let dir = root.join(name);
    std::fs::create_dir_all(&dir).unwrap();
    let actor = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    std::fs::copy(actor.join("manifest.toml"), dir.join("manifest.toml")).unwrap();
    std::fs::write(dir.join("actor.wasm"), wasm).unwrap();
    dir
}

#[test]
fn sessions_start_from_a_manifest_and_the_default_can_change() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/daemon-test")
        .join(std::process::id().to_string());
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm")).unwrap();
    let good = actor_dir(&root, "good", &wasm);
    let broken = actor_dir(&root, "broken", b"not wasm");
    let unbuilt = root.join("unbuilt");
    std::fs::create_dir_all(&unbuilt).unwrap();
    std::fs::copy(good.join("manifest.toml"), unbuilt.join("manifest.toml")).unwrap();
    let good_manifest = std::fs::canonicalize(good.join("manifest.toml")).unwrap();
    let good_manifest = good_manifest.to_str().unwrap();

    let daemon = Daemon::start(&root);
    assert!(daemon.ok(&["status"]).contains("default actor: built-in"));

    // Without --manifest, a session runs the daemon's default actor.
    let plain = daemon.ok(&["new", "plain"]);
    assert!(daemon.ok(&["status", &plain]).contains("source: built-in"));

    // With --manifest, that one session runs the named actor. A relative path is
    // resolved against the client's directory, not the daemon's.
    let out = daemon.cli_in(&good, &["new", "custom", "--manifest", "manifest.toml"]);
    assert!(out.status.success(), "{}", text(&out.stderr));
    let custom = text(&out.stdout);
    assert_eq!(daemon.ok(&["eval", &custom, "(+ 40 2)"]), "42");
    assert!(daemon.ok(&["status", &custom]).contains(good_manifest));
    let list = daemon.ok(&["list"]);
    let row = list.lines().find(|l| l.contains("custom")).unwrap();
    // ACTOR stays the 4th word (lib/sessions.wisp relies on it); SOURCE follows.
    let words: Vec<&str> = row.split_whitespace().collect();
    assert_eq!(words[0], custom);
    assert!(words[3].starts_with(&custom));
    assert_eq!(words[4], good_manifest);

    // Bad manifests fail that `new` only.
    assert!(
        daemon
            .fails(&["new", "--manifest", "/no/such/manifest.toml"])
            .contains("no such manifest")
    );
    let unbuilt_manifest = unbuilt.join("manifest.toml");
    assert!(
        daemon
            .fails(&["new", "--manifest", unbuilt_manifest.to_str().unwrap()])
            .contains("reading package")
    );
    assert_eq!(daemon.ok(&["eval", &plain, "(+ 1 1)"]), "2");

    // A broken build is refused as the default; the old default stays.
    let broken_manifest = broken.join("manifest.toml");
    assert!(
        daemon
            .fails(&["set-default", broken_manifest.to_str().unwrap()])
            .contains("default unchanged")
    );
    assert!(daemon.ok(&["status"]).contains("default actor: built-in"));

    // A good one becomes the default for later sessions; earlier ones keep theirs.
    assert!(
        daemon
            .ok(&["set-default", good_manifest])
            .contains(good_manifest)
    );
    let later = daemon.ok(&["new", "later"]);
    assert!(daemon.ok(&["status", &later]).contains(good_manifest));
    assert!(daemon.ok(&["status", &plain]).contains("source: built-in"));

    // --reset goes back to the actor the daemon started with.
    daemon.ok(&["set-default", "--reset"]);
    assert!(daemon.ok(&["status"]).contains("default actor: built-in"));
    daemon.fails(&["set-default"]);
}

/// An interpreter the daemon did not make (here, `spawn-actor` inside a session)
/// is a session like any other: `list` shows it, and `eval`/`read`/`status`/`stop`
/// reach it by full id, id prefix, or name. The runtime is the directory.
#[test]
fn every_interpreter_in_the_runtime_is_a_session() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/daemon-test")
        .join(format!("{}-any", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    let actor = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    // spawn-actor does not resolve a relative `package`, so name it absolutely.
    let package = std::fs::canonicalize(actor.join("actor.wasm")).unwrap();
    let manifest = std::fs::read_to_string(actor.join("manifest.toml"))
        .unwrap()
        .replace(
            "package = \"./actor.wasm\"",
            &format!("package = \"{}\"", package.display()),
        )
        .replace("name = \"wisp-interpreter\"", "name = \"spawned\"");
    let manifest_path = root.join("spawned.toml");
    std::fs::write(&manifest_path, manifest).unwrap();

    let daemon = Daemon::start(&root);
    let host = daemon.ok(&["new", "host"]);
    let spawned = daemon.ok(&[
        "eval",
        &host,
        &format!("(spawn-actor \"{}\")", manifest_path.display()),
    ]);
    assert!(!spawned.contains("error"), "{spawned}");

    // `list` finds it through the runtime; the daemon knows no age or source for it.
    let list = daemon.ok(&["list"]);
    let row = list.lines().find(|l| l.contains(" spawned ")).expect(&list);
    let words: Vec<&str> = row.split_whitespace().collect();
    assert_eq!(&words[2..3], ["-"]);
    let (short, full) = (words[0].to_string(), words[3].to_string());
    assert!(
        full.starts_with(&short) && spawned.contains(&full),
        "{spawned}"
    );
    assert!(list.lines().any(|l| l.starts_with(&host)));

    // Definitions live in the actor, whichever way it is named.
    daemon.ok(&["eval", "spawned", "(define y 5)"]);
    assert_eq!(daemon.ok(&["eval", &full, "(+ y 1)"]), "6");
    assert_eq!(daemon.ok(&["eval", &short, "y"]), "5");
    // Its trigger firings are buffered in the actor itself, so `read` (which
    // evaluates (poll-events) in it) drains them like any session's.
    assert_eq!(daemon.ok(&["read", "spawned"]), "");
    daemon.ok(&[
        "eval",
        "spawned",
        "(define on-tick (lambda (n) (string-append \"fired: \" n)))",
    ]);
    daemon.ok(&["eval", "spawned", "(set-interval \"beat\" 100)"]);
    let deadline = Instant::now() + Duration::from_secs(20);
    loop {
        let events = daemon.ok(&["read", &short]);
        if events.contains("fired: ") {
            break;
        }
        assert!(Instant::now() < deadline, "no tick read: {events}");
        std::thread::sleep(Duration::from_millis(200));
    }
    let status = daemon.ok(&["status", "spawned"]);
    assert!(
        status.contains(&full) && status.contains("source: -"),
        "{status}"
    );
    assert!(daemon.ok(&["status"]).contains("2 session(s)"));

    // Names are addresses now: `new` refuses a taken one, and a shared one is an error.
    assert!(daemon.fails(&["new", "spawned"]).contains("already exists"));
    let unnamed = daemon.ok(&["new"]);
    let again = daemon.ok(&["new"]);
    assert!(
        daemon
            .fails(&["eval", "wisp-interpreter", "1"])
            .contains("spawn-actor names an actor after its manifest")
    );
    assert_eq!(daemon.ok(&["eval", &again, "1"]), "1");
    assert!(
        daemon
            .fails(&["eval", "nobody", "1"])
            .contains("no such session")
    );

    // `stop` ends it in the runtime, so every view of it is gone.
    daemon.ok(&["stop", "spawned"]);
    assert!(!daemon.ok(&["list"]).contains(&full));
    assert!(
        daemon
            .fails(&["eval", &full, "y"])
            .contains("no such session")
    );
    daemon.ok(&["stop", &unnamed]);
    assert_eq!(daemon.ok(&["eval", "wisp-interpreter", "2"]), "2");
}

#[test]
fn inspect_reports_from_the_daemon_even_while_the_session_is_busy() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/daemon-test")
        .join(format!("{}-inspect", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    let module = "(module m (provide x) (define x 1))";
    std::fs::write(root.join("m.wisp"), module).unwrap();
    let config = root.join("config.toml");
    std::fs::write(
        &config,
        format!("[filesystem]\nroot = {:?}\n", root.display().to_string()),
    )
    .unwrap();

    let daemon = Daemon::start_with(&root, &["--config", config.to_str().unwrap()]);
    daemon.ok(&["new", "a"]);
    daemon.ok(&["new", "b"]);
    let list = daemon.ok(&["list"]);
    let b = list
        .lines()
        .find(|l| l.split_whitespace().nth(1) == Some("b"))
        .and_then(|l| l.split_whitespace().nth(3))
        .expect(&list)
        .to_string();
    daemon.ok(&[
        "eval",
        "b",
        "(begin (msg-register) (define on-message (lambda (e) (car (list)))) \
         (set-interval \"beat\" 100000) (require m \"m.wisp\"))",
    ]);
    for _ in 0..2 {
        daemon.ok(&["eval", "a", &format!("(msg-send \"{b}\" \"hi\")")]);
    }
    daemon.fails(&["eval", "b", "(car (list))"]);

    // Two failed handler calls and the failed evaluation; the file's hash is
    // that of the text read.
    let sha = wisp_interpreter_actor::health::sha256_hex(module.as_bytes());
    let deadline = Instant::now() + Duration::from_secs(20);
    let shown = loop {
        let shown = daemon.ok(&["inspect", "b"]);
        if shown.contains("errors:   3,") {
            break shown;
        }
        assert!(Instant::now() < deadline, "{shown}");
        std::thread::sleep(Duration::from_millis(200));
    };
    for line in [
        "busy:     no",
        "messages: in 2/min (2 total), out 0/min (0 total)",
        "error: car/cdr expects a nonempty list",
        "timers:   1 (beat)",
        "steps:    most ",
        &format!("m.wisp  sha256 {sha}  {} bytes", module.len()),
    ] {
        assert!(shown.contains(line), "{line:?} not in\n{shown}");
    }
    assert!(daemon.ok(&["inspect", "a"]).contains("out 2/min (2 total)"));
    let all = daemon.ok(&["inspect"]);
    assert!(all.lines().count() == 4 && all.contains(" b "), "{all}");

    // A long evaluation: the daemon still answers, and names the call.
    daemon.ok(&[
        "eval",
        "b",
        "(begin (step-limit 20000000) (define f (lambda (n) (if (= n 0) 0 (f (- n 1))))))",
    ]);
    let mut long = Command::new(BIN)
        .args(["-p", &daemon.port.to_string(), "eval", "b", "(f 3000000)"])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    loop {
        let shown = daemon.ok(&["inspect", "b"]);
        if shown.contains("in theater:simple/wisp.evaluate") {
            assert!(shown.contains("heap:     busy"), "{shown}");
            break;
        }
        assert!(Instant::now() < deadline, "never busy: {shown}");
        std::thread::sleep(Duration::from_millis(100));
    }
    long.wait().unwrap();
    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn chain_tail_shows_host_calls_without_their_bytes() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/daemon-test")
        .join(format!("{}-tail", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(root.join("m.wisp"), "(module m (provide x) (define x 1))").unwrap();
    let config = root.join("config.toml");
    std::fs::write(
        &config,
        format!("[filesystem]\nroot = {:?}\n", root.display().to_string()),
    )
    .unwrap();

    let daemon = Daemon::start_with(&root, &["--config", config.to_str().unwrap()]);
    daemon.ok(&["new", "b"]);
    daemon.ok(&[
        "eval",
        "b",
        "(begin (set-interval \"beat\" 100000) (fs-read-text \"m.wisp\") \
         (fs-read-text \"nope.txt\") (msg-send (self) \"secret\"))",
    ]);
    let all = daemon.ok(&["chain-tail", "b"]);
    for line in [
        "theater:simple/timer/set-interval(\"beat\", 100000) -> ok(\"beat\")",
        "theater:simple/filesystem/read-file(\"m.wisp\") -> ok(<35 bytes>)",
        "<6 bytes>",
    ] {
        assert!(all.contains(line), "{line:?} not in\n{all}");
    }
    assert!(
        !all.contains("secret") && !all.contains("module m"),
        "{all}"
    );
    let fs = daemon.ok(&["chain-tail", "b", "--kind", "fs", "-n", "1"]);
    assert_eq!(fs.lines().count(), 1, "{fs}");
    assert!(fs.contains("read-file(\"nope.txt\") -> err("), "{fs}");
    assert!(
        daemon
            .fails(&["chain-tail", "b", "--kind", "bogus"])
            .contains("one of all")
    );
    let _ = std::fs::remove_dir_all(&root);
}

/// One JSON request line to `port`, and the answer's `ok` and `text`.
fn ask(port: u16, request: &str) -> (bool, String) {
    use std::io::{BufRead, Write};
    let mut stream = std::net::TcpStream::connect(("127.0.0.1", port)).unwrap();
    stream.write_all(format!("{request}\n").as_bytes()).unwrap();
    let mut line = String::new();
    std::io::BufReader::new(stream)
        .read_line(&mut line)
        .unwrap();
    let answer: serde_json::Value = serde_json::from_str(&line).expect(&line);
    (
        answer["ok"].as_bool().unwrap_or(false),
        answer["text"].as_str().unwrap_or("").to_string(),
    )
}

#[test]
fn the_inspect_port_answers_only_what_reports() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/daemon-test")
        .join(format!("{}-inspect-port", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    let port = std::net::TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port();
    let config = root.join("config.toml");
    std::fs::write(
        &config,
        format!(
            "[filesystem]\nroot = {:?}\n[inspect]\nport = {port}\n",
            root.display().to_string()
        ),
    )
    .unwrap();

    let daemon = Daemon::start_with(&root, &["--config", config.to_str().unwrap()]);
    daemon.ok(&["new", "b"]);
    let deadline = Instant::now() + Duration::from_secs(20);
    while std::net::TcpStream::connect(("127.0.0.1", port)).is_err() {
        assert!(Instant::now() < deadline, "no inspect port");
        std::thread::sleep(Duration::from_millis(100));
    }

    let (ok, text) = ask(port, r#"{"op":"inspect"}"#);
    assert!(ok && text.contains(" b "), "{text}");
    let (ok, text) = ask(port, r#"{"op":"inspect","id":"b"}"#);
    assert!(ok && text.contains("name:     b"), "{text}");
    assert!(ask(port, r#"{"op":"chain-tail","id":"b","n":5}"#).0);
    assert!(ask(port, r#"{"op":"list"}"#).0);
    for refused in [
        r#"{"op":"eval","id":"b","form":"(define x 1)"}"#,
        r#"{"op":"new","name":"c"}"#,
        r#"{"op":"stop","id":"b"}"#,
        r#"{"op":"shutdown"}"#,
        r#"{}"#,
    ] {
        let (ok, text) = ask(port, refused);
        assert!(!ok && text.contains("only answers"), "{refused}: {text}");
    }
    // Nothing above happened: b is still there, x undefined, the daemon up.
    assert!(daemon.ok(&["list"]).contains(" b "));
    assert!(!daemon.ok(&["list"]).contains(" c "));
    assert!(daemon.fails(&["eval", "b", "x"]).contains("x"));
    let _ = std::fs::remove_dir_all(&root);
}
