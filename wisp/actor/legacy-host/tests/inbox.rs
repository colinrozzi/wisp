//! The session inbox through the real `theater-repl` binary: `read`, `follow`
//! and `wait` read after their own cursors, so no reader takes messages from
//! another, and nothing a reader failed to print is acknowledged (wisp
//! docs/proposals/SESSION-INBOX.md).

use std::io::Read;
use std::process::{Child, Command, Output, Stdio};
use std::time::{Duration, Instant};

const BIN: &str = env!("CARGO_BIN_EXE_theater-repl");

/// A daemon on a free port, brought down (or killed) when dropped.
struct Daemon {
    child: Child,
    port: u16,
}

impl Daemon {
    fn start() -> Self {
        let port = std::net::TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
        let child = Command::new(BIN)
            .args(["-p", &port.to_string(), "serve"])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
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

    fn command(&self, args: &[&str]) -> Command {
        let mut command = Command::new(BIN);
        command.args(["-p", &self.port.to_string()]).args(args);
        command
    }

    fn cli(&self, args: &[&str]) -> Output {
        self.command(args).output().unwrap()
    }

    fn ok(&self, args: &[&str]) -> String {
        let out = self.cli(args);
        assert!(out.status.success(), "{args:?}: {}", text(&out.stderr));
        text(&out.stdout)
    }

    /// Start a reader in the background, its stdout piped.
    fn spawn(&self, args: &[&str]) -> Child {
        self.command(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap()
    }

    /// The full actor id of session `name` (the 4th column of `list`).
    fn actor(&self, name: &str) -> String {
        self.ok(&["list"])
            .lines()
            .find(|line| line.split_whitespace().nth(1) == Some(name))
            .and_then(|line| line.split_whitespace().nth(3))
            .unwrap()
            .to_string()
    }

    /// Cursor `cursor` of session `id` as `cursors` shows it: (seq, behind,
    /// waiting), or None while the cursor does not exist.
    fn cursor(&self, id: &str, cursor: &str) -> Option<(u64, u64, usize)> {
        let listing = self.ok(&["cursors", id]);
        let line = listing
            .lines()
            .find(|line| line.split_whitespace().next() == Some(cursor))?;
        let fields: Vec<&str> = line.split_whitespace().collect();
        Some((
            fields[1].parse().ok()?,
            fields[2].parse().ok()?,
            fields[3].parse().ok()?,
        ))
    }

    /// Wait until `n` readers are parked on cursor `cursor` of session `id`.
    fn parked(&self, id: &str, cursor: &str, n: usize) {
        until(&format!("{n} parked on {cursor}"), || {
            self.cursor(id, cursor)
                .is_some_and(|(_, _, waiting)| waiting == n)
        });
    }

    /// Send `body` to session `to` from session `from`.
    fn message(&self, from: &str, to: &str, body: &str) {
        let form = format!("(msg-send \"{}\" \"{body}\")", self.actor(to));
        let out = self.ok(&["eval", from, &form]);
        assert!(out.starts_with("(ok"), "msg-send: {out}");
    }
}

impl Drop for Daemon {
    fn drop(&mut self) {
        let _ = self.cli(&["down"]);
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Poll `done` until it holds; fail after 30s. Waits on a condition the
/// daemon reports, never on a guess at how long a reader takes to start.
fn until(what: &str, mut done: impl FnMut() -> bool) {
    let deadline = Instant::now() + Duration::from_secs(30);
    while !done() {
        assert!(Instant::now() < deadline, "timed out waiting for {what}");
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).trim().to_string()
}

/// Wait up to `seconds` for `child` to exit.
fn finish(mut child: Child, seconds: u64) -> (Option<i32>, String) {
    let deadline = Instant::now() + Duration::from_secs(seconds);
    loop {
        if let Some(status) = child.try_wait().unwrap() {
            let mut out = String::new();
            if let Some(mut stdout) = child.stdout.take() {
                let _ = stdout.read_to_string(&mut out);
            }
            return (status.code(), out.trim().to_string());
        }
        assert!(
            Instant::now() < deadline,
            "reader did not exit in {seconds}s"
        );
        std::thread::sleep(Duration::from_millis(100));
    }
}

fn still_running(child: &mut Child) -> bool {
    child.try_wait().unwrap().is_none()
}

#[test]
fn readers_keep_their_own_place_and_lose_nothing() {
    let daemon = Daemon::start();
    daemon.ok(&["new", "alice"]);
    daemon.ok(&["new", "bob"]);
    daemon.ok(&["eval", "alice", "(msg-register)"]);
    daemon.ok(&[
        "eval",
        "alice",
        "(define on-message (lambda (e) (bytes->string (car e))))",
    ]);

    // wait returns what is already there, and acknowledges it.
    daemon.message("bob", "alice", "one");
    assert_eq!(daemon.ok(&["wait", "alice"]), "on-message: one");

    // With nothing new, wait blocks until a message arrives.
    let mut waiter = daemon.spawn(&["wait", "alice"]);
    daemon.parked("alice", "wait", 1);
    assert!(
        still_running(&mut waiter),
        "wait returned with nothing to read"
    );
    daemon.message("bob", "alice", "two");
    assert_eq!(finish(waiter, 15), (Some(0), "on-message: two".into()));

    // --timeout: exit 2, nothing printed.
    let out = daemon.cli(&["wait", "alice", "--timeout", "1"]);
    assert_eq!(out.status.code(), Some(2));
    assert_eq!(text(&out.stdout), "");

    // The incident: a stray follow is running when a message arrives. The
    // monitor's wait still gets it, and so does the follow.
    // Parked means --from-now has moved its cursor to the head.
    let mut follower = daemon.spawn(&["follow", "alice", "--from-now"]);
    daemon.parked("alice", "follow", 1);
    daemon.message("bob", "alice", "three");
    assert_eq!(daemon.ok(&["wait", "alice"]), "on-message: three");
    // A follow acknowledges a batch with its next round, after printing it.
    until("follow to acknowledge three", || {
        matches!(daemon.cursor("alice", "follow"), Some((_, 0, 1)))
    });
    let _ = follower.kill();
    let (_, followed) = finish(follower, 5);
    assert!(
        followed.contains("on-message: three"),
        "follow saw: {followed}"
    );

    // Two monitors on one session both get the message.
    let first = daemon.spawn(&["wait", "alice"]);
    let second = daemon.spawn(&["wait", "alice"]);
    daemon.parked("alice", "wait", 2);
    daemon.message("bob", "alice", "four");
    assert_eq!(finish(first, 15).1, "on-message: four");
    assert_eq!(finish(second, 15).1, "on-message: four");

    // A reader whose output goes nowhere (the `| head -n 1` case) does not
    // acknowledge: the next wait gets the message again.
    let mut blind = daemon.spawn(&["wait", "alice"]);
    drop(blind.stdout.take());
    daemon.parked("alice", "wait", 1);
    daemon.message("bob", "alice", "five");
    let (code, _) = finish(blind, 15);
    assert_ne!(code, Some(0), "a wait that could not print must fail");
    assert_eq!(daemon.ok(&["wait", "alice"]), "on-message: five");

    // read has its own cursor: it starts from everything still buffered.
    let all = daemon.ok(&["read", "alice"]);
    for body in ["one", "two", "three", "four", "five"] {
        assert!(all.contains(&format!("on-message: {body}")), "read: {all}");
    }
    assert_eq!(daemon.ok(&["read", "alice"]), "");
    // --after reads without a cursor; --full shows seqs.
    assert!(
        daemon
            .ok(&["read", "alice", "--after", "4", "--full"])
            .starts_with("(5 0 (5 ")
    );

    let cursors = daemon.ok(&["cursors", "alice"]);
    for name in ["wait", "read", "follow"] {
        assert!(cursors.contains(name), "cursors: {cursors}");
    }

    // A wait on a session that stops returns an error.
    let waiter = daemon.spawn(&["wait", "alice"]);
    daemon.parked("alice", "wait", 1);
    daemon.ok(&["stop", "alice"]);
    let (code, _) = finish(waiter, 20);
    assert_eq!(code, Some(1));
}
