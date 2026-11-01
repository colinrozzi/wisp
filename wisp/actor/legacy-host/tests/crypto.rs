//! The crypto verbs through a real daemon: sha256 and ed25519 against published
//! vectors, and the key directory `[keys]` configures.

use std::net::TcpListener;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output};
use std::time::{Duration, Instant};

const BIN: &str = env!("CARGO_BIN_EXE_theater-repl");

// RFC 8032 section 7.1, test 1 (over the empty message).
const SEED: &str = "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60";
const PUBLIC: &str = "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a";
const SIGNATURE: &str = "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b";

/// A daemon on a free port with `--config config`, killed when dropped.
struct Daemon {
    child: Child,
    port: u16,
}

impl Daemon {
    fn start(work: &Path, config: &Path) -> Self {
        let port = TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
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

    /// A new session, and a function evaluating a form in it.
    fn session(&self) -> impl Fn(&str) -> String + '_ {
        let new = self.cli(&["new", "crypto"]);
        assert!(new.status.success(), "{}", text(&new.stderr));
        let id = text(&new.stdout);
        move |form| {
            let out = self.cli(&["eval", &id, form]);
            assert!(out.status.success(), "{form}: {}", text(&out.stderr));
            text(&out.stdout)
        }
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

fn workdir(name: &str) -> PathBuf {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/crypto-test")
        .join(format!("{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    root
}

fn mode(path: &Path) -> u32 {
    std::fs::metadata(path).unwrap().permissions().mode() & 0o777
}

/// Flip the lowest bit of the hex digit at `at`.
fn flipped(hex: &str, at: usize) -> String {
    let digit = u8::from_str_radix(&hex[at..=at], 16).unwrap() ^ 1;
    format!("{}{digit:x}{}", &hex[..at], &hex[at + 1..])
}

#[test]
fn keys_sign_and_verify_with_a_configured_directory() {
    let root = workdir("keys");
    let dir = root.join("state/keys");
    let config = root.join("config.toml");
    std::fs::write(&config, format!("[keys]\ndir = {:?}\n", dir)).unwrap();
    let daemon = Daemon::start(&root, &config);
    let eval = daemon.session();

    // The directory appears with the first key.
    assert_eq!(
        eval("(key-list)"),
        "(ok (list string) string #<list string ()>)"
    );

    // key-new: a 0600 file in a 0700 directory, holding a 32-byte hex seed.
    let new = eval("(match (key-new \"author.1\") ((ok v) v) ((err e) e))");
    assert_eq!(new.len(), 64, "{new}");
    assert!(new.bytes().all(|b| b.is_ascii_hexdigit()), "{new}");
    let file = dir.join("author.1.key");
    assert_eq!(mode(&dir), 0o700);
    assert_eq!(mode(&file), 0o600);
    assert_eq!(std::fs::read_to_string(&file).unwrap().len(), 64);
    assert_eq!(
        std::fs::read_dir(&dir).unwrap().count(),
        1,
        "temporary file left"
    );

    // It refuses a duplicate, and the key survives the attempt.
    let seed = std::fs::read_to_string(&file).unwrap();
    assert!(eval("(key-new \"author.1\")").contains("key 'author.1' exists"));
    assert_eq!(std::fs::read_to_string(&file).unwrap(), seed);

    // Bad names, which make no files anywhere.
    for name in ["../x", "a/b", "", &"n".repeat(65)] {
        let got = eval(&format!("(key-new \"{name}\")"));
        assert!(got.contains("bad key name"), "{name}: {got}");
    }
    assert!(eval("(key-public \"../x\")").contains("bad key name"));
    assert!(eval("(sign \"a/b\" \"m\")").contains("bad key name"));
    assert!(!root.join("state/x.key").exists());
    assert_eq!(std::fs::read_dir(&dir).unwrap().count(), 1);

    // A new key's signature checks out, and no output carries the seed.
    let signed = eval("(match (sign \"author.1\" \"event\") ((ok v) v) ((err e) e))");
    assert_eq!(signed.len(), 128, "{signed}");
    assert_eq!(
        eval(&format!("(verify \"{new}\" \"{signed}\" \"event\")")),
        "true"
    );
    assert_eq!(
        eval(&format!(
            "(verify \"{new}\" \"{signed}\" (string->bytes \"event\"))"
        )),
        "true"
    );
    assert_eq!(
        eval(&format!("(verify \"{new}\" \"{signed}\" \"other\")")),
        "false"
    );
    for output in [&new, &signed] {
        assert!(!output.contains(seed.as_str()));
    }

    // RFC 8032 test 1, from a key file put there by hand.
    std::fs::write(dir.join("rfc.key"), format!("{SEED}\n")).unwrap();
    let got = eval("(key-public \"rfc\")");
    assert!(got.contains(&format!("\"{PUBLIC}\"")), "{got}");
    let got = eval("(sign \"rfc\" \"\")");
    assert!(got.contains(&format!("\"{SIGNATURE}\"")), "{got}");
    let got = eval("(sign \"rfc\" (string->bytes \"\"))");
    assert!(got.contains(&format!("\"{SIGNATURE}\"")), "{got}");
    assert_eq!(eval("(key-list)").matches('"').count(), 4);
    let list = eval("(key-list)");
    assert!(list.find("\"author.1\"") < list.find("\"rfc\""), "{list}");

    // Not a key: a missing one, a damaged one, a file of another kind.
    assert!(eval("(key-public \"gone\")").contains("no key named 'gone'"));
    assert!(eval("(sign \"gone\" \"m\")").contains("no key named 'gone'"));
    std::fs::write(dir.join("bad.key"), "not hex").unwrap();
    assert!(eval("(sign \"bad\" \"m\")").contains("not a 32-byte hex seed"));
    std::fs::remove_file(dir.join("bad.key")).unwrap();
    std::fs::write(dir.join("notes.txt"), "").unwrap();
    assert!(!eval("(key-list)").contains("notes"));
}

#[test]
fn hashing_and_verifying_need_no_keys() {
    let root = workdir("no-keys");
    let config = root.join("config.toml");
    std::fs::write(&config, "").unwrap();
    let daemon = Daemon::start(&root, &config);
    let eval = daemon.session();

    // FIPS 180-2.
    let empty = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
    let abc = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad";
    assert_eq!(eval("(sha256 \"\")"), empty);
    assert_eq!(eval("(sha256 \"abc\")"), abc);
    assert_eq!(eval("(sha256 (string->bytes \"\"))"), empty);
    assert_eq!(eval("(sha256 (string->bytes \"abc\"))"), abc);

    // Every byte value crosses unchanged, not as UTF-8 text.
    let got = eval("(sha256 (string->bytes \"\\xFF\"))");
    let wrapped = eval("(sha256 \"\\xFF\")");
    assert_eq!(got, wrapped);
    assert_eq!(
        got,
        "a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89"
    );

    // RFC 8032 test 1: accepted, then each way of being wrong.
    let verify = |public: &str, signature: &str| {
        eval(&format!("(verify \"{public}\" \"{signature}\" \"\")"))
    };
    assert_eq!(verify(PUBLIC, SIGNATURE), "true");
    assert_eq!(verify(PUBLIC, &flipped(SIGNATURE, 0)), "false");
    assert_eq!(verify(PUBLIC, &flipped(SIGNATURE, 127)), "false");
    assert_eq!(
        eval(&format!("(verify \"{PUBLIC}\" \"{SIGNATURE}\" \"x\")")),
        "false"
    );
    // Wrong key: RFC 8032 test 2's public key.
    let other = "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";
    assert_eq!(verify(other, SIGNATURE), "false");
    // Bad hex: not digits, odd length, too short, too long, empty.
    let not_digits = format!("zz{}", &SIGNATURE[2..]);
    for signature in [&not_digits, &SIGNATURE[1..], &SIGNATURE[..126], ""] {
        assert_eq!(verify(PUBLIC, signature), "false", "{signature}");
    }
    assert_eq!(verify(&format!("{PUBLIC}00"), SIGNATURE), "false");
    assert_eq!(verify(&format!("zz{}", &PUBLIC[2..]), SIGNATURE), "false");
    assert_eq!(verify("", SIGNATURE), "false");
    // It is a boolean `if` takes.
    assert_eq!(
        eval(&format!(
            "(if (verify \"{PUBLIC}\" \"{SIGNATURE}\" \"\") 1 2)"
        )),
        "1"
    );

    // No [keys] section.
    for form in [
        "(key-new \"a\")",
        "(key-public \"a\")",
        "(key-list)",
        "(sign \"a\" \"m\")",
    ] {
        let got = eval(form);
        assert!(got.starts_with("(err "), "{form}: {got}");
        assert!(got.contains("no key directory configured"), "{form}: {got}");
    }

    // Arguments of the wrong kind fail before reaching the host.
    let out = daemon.cli(&["eval", "crypto", "(sha256 1)"]);
    assert!(text(&out.stderr).contains("sha256 expects"), "{out:?}");

    let help = eval("(help)");
    for verb in [
        "crypto:",
        "(sha256",
        "(key-new",
        "(key-public",
        "(key-list",
        "(sign",
        "(verify",
    ] {
        assert!(help.contains(verb), "{verb} not in (help)");
    }
}
