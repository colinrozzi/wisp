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
        let port = std::net::TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
        let child = Command::new(BIN)
            .args(["-p", &port.to_string(), "serve"])
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
