//! The boot list (`[[service]]` in the config), driven through the real
//! `theater-repl` binary: `serve` starts each service, a killed one comes back
//! under its name, a crash-looping one gives up at its limit, and one stopped
//! on purpose stays stopped until `start`.

use std::path::Path;
use std::process::{Child, Command, Output};
use std::time::{Duration, Instant};

const BIN: &str = env!("CARGO_BIN_EXE_theater-repl");

/// A daemon on a free port with a boot list, brought down when dropped.
struct Daemon {
    child: Child,
    port: u16,
}

impl Daemon {
    /// Serve with a filesystem root holding `files`, and `services` (TOML
    /// `[[service]]` tables) as the boot list.
    fn start(test: &str, files: &[(&str, &str)], services: &str) -> Self {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("target/services-test")
            .join(format!("{}-{test}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        let fs = root.join("fs");
        std::fs::create_dir_all(&fs).unwrap();
        for (name, text) in files {
            let path = fs.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        let config = root.join("config.toml");
        std::fs::write(
            &config,
            format!(
                "[filesystem]\nroot = {:?}\n\n{services}",
                fs.display().to_string()
            ),
        )
        .unwrap();
        let port = std::net::TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
        let child = Command::new(BIN)
            .args(["-p", &port.to_string(), "--config"])
            .arg(&config)
            .arg("serve")
            .current_dir(&root)
            .stdout(std::process::Stdio::null())
            .stderr(std::fs::File::create(root.join("daemon.log")).unwrap())
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

    /// The `services` row for `name`, split into words: NAME STATE RESTARTS ID
    /// LAST-DEATH... ("given up" is two words).
    fn service(&self, name: &str) -> Vec<String> {
        let table = self.ok(&["services"]);
        let row = table
            .lines()
            .find(|line| line.split_whitespace().next() == Some(name))
            .unwrap_or_else(|| panic!("no service {name} in:\n{table}"));
        row.replace("given up", "given-up")
            .split_whitespace()
            .map(String::from)
            .collect()
    }

    /// Wait until service `name` satisfies `done`; its row then.
    fn until(&self, name: &str, what: &str, done: impl Fn(&[String]) -> bool) -> Vec<String> {
        let deadline = Instant::now() + Duration::from_secs(90);
        loop {
            let row = self.service(name);
            if done(&row) {
                return row;
            }
            assert!(Instant::now() < deadline, "{name} never {what}: {row:?}");
            std::thread::sleep(Duration::from_millis(250));
        }
    }

    /// The ACTOR of the session named `name` in `list`, if there is one.
    fn actor(&self, name: &str) -> Option<String> {
        self.ok(&["list"])
            .lines()
            .map(|line| line.split_whitespace().collect::<Vec<_>>())
            .find(|words| words.get(1) == Some(&name))
            .map(|words| words[3].to_string())
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

const GOOD: &str = "(define booted 42)\n(define started (list 1 2))\n";

#[test]
fn boot_starts_services_and_a_killed_one_comes_back() {
    let daemon = Daemon::start(
        "kill",
        &[("svc/start.wisp", GOOD)],
        "[[service]]\nname = \"svc\"\nstart = \"svc/start.wisp\"\n",
    );
    let first = daemon.until("svc", "ran", |row| row[1] == "running");
    assert_eq!(first[2], "0");
    assert_eq!(daemon.ok(&["eval", "svc", "booted"]), "42");
    assert!(daemon.ok(&["status"]).contains("services:"));
    let old = daemon.actor("svc").unwrap();

    // Killed from another session: a fresh session under the same name runs
    // the start file again.
    daemon.ok(&["new", "killer"]);
    daemon.ok(&["eval", "killer", &format!("(kill-actor \"{old}\")")]);
    let row = daemon.until("svc", "restarted", |row| {
        row[1] == "running" && row[2] == "1"
    });
    assert_eq!(row[4], "killed", "{row:?}");
    let new = daemon.actor("svc").unwrap();
    assert_ne!(new, old);
    assert_eq!(daemon.ok(&["eval", "svc", "booted"]), "42");
    // The killed session is not listed beside it.
    let list = daemon.ok(&["list"]);
    assert!(!list.contains(&old), "{list}");
}

#[test]
fn a_crash_looping_service_gives_up_at_its_limit() {
    let daemon = Daemon::start(
        "loop",
        &[
            ("bad.wisp", "(define x 1)\n(no-such-verb)\n"),
            ("ok.wisp", GOOD),
        ],
        "[[service]]\nname = \"bad\"\nstart = \"bad.wisp\"\nmax_restarts = 1\n\n\
         [[service]]\nname = \"fine\"\nstart = \"ok.wisp\"\n",
    );
    let row = daemon.until("bad", "gave up", |row| row[1] == "given-up");
    assert_eq!(row[2], "1", "one restart, then the limit: {row:?}");
    let death = row[4..].join(" ");
    assert!(
        death.contains("start file failed") && death.contains("no-such-verb"),
        "{death}"
    );
    assert!(daemon.ok(&["status"]).contains("given up"));
    assert_eq!(daemon.actor("bad"), None);
    // The other service is not affected.
    assert_eq!(daemon.service("fine")[1], "running");
    std::thread::sleep(Duration::from_secs(2));
    assert_eq!(daemon.service("bad")[1], "given-up");
}

#[test]
fn a_stopped_service_stays_stopped_until_started() {
    let daemon = Daemon::start(
        "stop",
        &[("ok.wisp", GOOD)],
        "[[service]]\nname = \"svc\"\nstart = \"ok.wisp\"\n",
    );
    daemon.until("svc", "ran", |row| row[1] == "running");
    daemon.ok(&["stop", "svc"]);
    std::thread::sleep(Duration::from_secs(3));
    let row = daemon.service("svc");
    assert_eq!((row[1].as_str(), row[2].as_str()), ("stopped", "0"));
    assert_eq!(daemon.actor("svc"), None);

    daemon.ok(&["start", "svc"]);
    daemon.until("svc", "started again", |row| row[1] == "running");
    assert_eq!(daemon.ok(&["eval", "svc", "booted"]), "42");
    assert!(!daemon.cli(&["start", "svc"]).status.success());
    assert!(!daemon.cli(&["start", "nobody"]).status.success());
}

/// A service with `manifest = PATH` starts from that manifest instead of the
/// daemon's default actor (0156: the root supervisor needs a store base_path
/// that its children's lie inside). Here the manifest narrows tcp to no
/// listening, which the default doesn't: the same start file listens in one
/// service and is refused in the other, and status names the manifest.
#[test]
fn a_service_can_start_from_its_own_manifest() {
    let actor = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let package = std::fs::canonicalize(actor.join("actor.wasm")).unwrap();
    let manifest = std::fs::read_to_string(actor.join("manifest.toml"))
        .unwrap()
        .replace(
            "package = \"./actor.wasm\"",
            &format!("package = \"{}\"", package.display()),
        )
        + "\n[[handler]]\ntype = \"tcp\"\nallowed_listen = []\nallowed_connect = []\n";
    let dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/services-test")
        .join(format!("{}-manifest-file", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("narrow.toml");
    std::fs::write(&path, manifest).unwrap();

    let daemon = Daemon::start(
        "manifest",
        &[("svc/start.wisp", GOOD)],
        &format!(
            "[[service]]\nname = \"plain\"\nstart = \"svc/start.wisp\"\n\n\
             [[service]]\nname = \"narrow\"\nstart = \"svc/start.wisp\"\nmanifest = {:?}\n",
            path.display().to_string()
        ),
    );
    daemon.until("plain", "ran", |row| row[1] == "running");
    daemon.until("narrow", "ran", |row| row[1] == "running");
    // both ran the start file
    assert_eq!(daemon.ok(&["eval", "narrow", "booted"]), "42");
    assert!(
        daemon
            .ok(&["status", "narrow"])
            .contains(&path.display().to_string())
    );
    assert!(
        !daemon
            .ok(&["status", "plain"])
            .contains(&path.display().to_string())
    );
    // the default listens; the manifest's tcp block refuses it
    let listen = |name: &str| {
        let port = std::net::TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
        daemon.ok(&["eval", name, &format!("(tcp-listen \"127.0.0.1:{port}\")")])
    };
    assert!(listen("plain").starts_with("(ok"), "{}", listen("plain"));
    assert!(
        listen("narrow").contains("not allowed"),
        "{}",
        listen("narrow")
    );
}
