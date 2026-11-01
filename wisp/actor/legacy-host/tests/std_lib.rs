//! The standard library (`wisp/std/`), loaded through the real `theater-repl`
//! binary from a daemon whose filesystem root has nothing to do with it: the
//! modules must come from the source bundle, and a file on disk must shadow them.

use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::sync::Mutex;
use std::time::{Duration, Instant};

const BIN: &str = env!("CARGO_BIN_EXE_theater-repl");

/// A daemon on a free port, filesystem rooted at a fresh directory holding
/// `files`, brought down when dropped.
struct Daemon {
    child: Child,
    port: u16,
}

/// A port no other test in this process has been given. The kernel can hand the
/// same free port to two tests running at once, and the second daemon then fails
/// to bind while its test talks to the first.
fn free_port() -> u16 {
    static TAKEN: Mutex<Vec<u16>> = Mutex::new(Vec::new());
    let mut taken = TAKEN.lock().unwrap();
    loop {
        let port = std::net::TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
        if !taken.contains(&port) {
            taken.push(port);
            return port;
        }
    }
}

impl Daemon {
    fn start(test: &str, files: &[(&str, &str)]) -> Self {
        Self::start_with(test, files, "", None)
    }

    /// `fs_config` adds to the `[filesystem]` table; `bundle` is `--bundle`'s JSON.
    fn start_with(
        test: &str,
        files: &[(&str, &str)],
        fs_config: &str,
        bundle: Option<&str>,
    ) -> Self {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("target/std-lib-test")
            .join(format!("{}-{test}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        let fs = root.join("unrelated-root");
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
                "[filesystem]\nroot = {:?}\n{fs_config}\n",
                fs.display().to_string()
            ),
        )
        .unwrap();
        let mut bundle_args = Vec::new();
        if let Some(json) = bundle {
            let path = root.join("bundle.json");
            std::fs::write(&path, json).unwrap();
            bundle_args = vec!["--bundle".to_string(), path.display().to_string()];
        }
        let port = free_port();
        let child = Command::new(BIN)
            .args(["-p", &port.to_string(), "--config"])
            .arg(&config)
            .args(&bundle_args)
            .arg("serve")
            .current_dir(&root)
            .stdout(Stdio::null())
            .stderr(std::fs::File::create(root.join("daemon.log")).unwrap())
            .spawn()
            .unwrap();
        let daemon = Self { child, port };
        let deadline = Instant::now() + Duration::from_secs(60);
        while !daemon.cli(&["status"]).0 {
            assert!(Instant::now() < deadline, "daemon did not come up");
            std::thread::sleep(Duration::from_millis(200));
        }
        assert!(daemon.cli(&["new", "t"]).0);
        daemon
    }

    fn cli(&self, args: &[&str]) -> (bool, String) {
        let out = Command::new(BIN)
            .args(["-p", &self.port.to_string()])
            .args(args)
            .output()
            .unwrap();
        let mut text = String::from_utf8_lossy(&out.stdout).into_owned();
        text.push_str(&String::from_utf8_lossy(&out.stderr));
        (out.status.success(), text.trim().to_string())
    }

    /// Evaluate `form`; what it printed, an `error:` included.
    fn eval(&self, form: &str) -> String {
        self.cli(&["eval", "t", form]).1
    }

    /// Evaluate each `(form, expected)`.
    fn check(&self, cases: &[(&str, &str)]) {
        for (form, expected) in cases {
            assert_eq!(self.eval(form), *expected, "{form}");
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

#[test]
fn list_functions_from_the_bundle() {
    let daemon = Daemon::start("list", &[]);
    daemon.check(&[
        (r#"(require list "std/list.wisp")"#, "()"),
        ("(map (lambda (x) (+ x 1)) (list 1 2 3))", "(2 3 4)"),
        ("(map (lambda (x) x) (list))", "()"),
        ("(filter (lambda (x) (> x 1)) (list 1 2 3))", "(2 3)"),
        ("(fold + 0 (list 1 2 3))", "6"),
        ("(fold (lambda (acc x) (- acc x)) 10 (list 1 2))", "7"),
        ("(reverse (list 1 2 3))", "(3 2 1)"),
        ("(reverse (list))", "()"),
        ("(append (list 1) (list 2 3))", "(1 2 3)"),
        ("(append (list) (list))", "()"),
        ("(nth (list 4 5) 1)", "5"),
        ("(nth (list 4 5) 2)", "error: list index out of bounds"),
        ("(member 2 (list 1 2) =)", "1"),
        ("(member 3 (list 1 2) =)", "0"),
        (r#"(member "a" (list "b" "a") string=?)"#, "1"),
        ("(define lt (lambda (a b) (< a b)))", "#<closure>"),
        ("(sort lt (list 3 1 2))", "(1 2 3)"),
        ("(sort lt (list))", "()"),
        ("(sort lt (list 1))", "(1)"),
        // Stable: pairs compare by their first element only.
        (
            r#"(map (lambda (p) (car (cdr p))) (sort (lambda (a b) (< (car a) (car b))) (list (list 2 "x") (list 1 "y") (list 2 "z") (list 1 "w"))))"#,
            r#"("y" "w" "x" "z")"#,
        ),
        // Typed lists go in too; what comes out is untyped.
        (
            "(map (lambda (x) (+ x 1)) (list-push (list-new s32) 1))",
            "(2)",
        ),
    ]);
}

#[test]
fn list_functions_on_long_lists() {
    let daemon = Daemon::start("long", &[]);
    daemon.check(&[
        (r#"(require list "std/list.wisp")"#, "()"),
        (
            "(define build (lambda (i n acc) (if (= i n) acc (build (+ i 1) n (list-push acc i)))))",
            "#<closure>",
        ),
    ]);
    daemon.eval("(define xs (build 0 10000 (list)))");
    daemon.check(&[
        ("(length xs)", "10000"),
        ("(nth (map (lambda (x) (+ x 1)) xs) 9999)", "10000"),
        ("(length (filter (lambda (x) (< x 2500)) xs))", "2500"),
        ("(fold + 0 xs)", "49995000"),
        ("(nth (reverse xs) 0)", "9999"),
        ("(nth (reverse xs) 9999)", "0"),
        ("(length (append xs xs))", "20000"),
        ("(nth (append xs xs) 19999)", "9999"),
        ("(nth xs 9999)", "9999"),
        ("(member 9999 xs =)", "1"),
        ("(member -1 xs =)", "0"),
    ]);
}

// sort reaches the step budget well below 10000 (see its comment), so it runs at
// a size that fits. The ceiling is pinned too: if the budget or the evaluator
// changes, its comment needs updating.
#[test]
fn sort_up_to_the_step_budget() {
    let daemon = Daemon::start("sort", &[]);
    daemon.check(&[
        (r#"(require list "std/list.wisp")"#, "()"),
        (
            "(define mix (lambda (i n acc) (if (= i n) acc (mix (+ i 1) n (list-push acc (i32.rem_s (* i 7919) 10007))))))",
            "#<closure>",
        ),
        ("(define lt (lambda (a b) (< a b)))", "#<closure>"),
        (
            "(define ordered (lambda (xs i) (if (>= (+ i 1) (length xs)) 1 (if (< (nth xs (+ i 1)) (nth xs i)) 0 (ordered xs (+ i 1))))))",
            "#<closure>",
        ),
    ]);
    daemon.eval("(define small (mix 0 1500 (list)))");
    daemon.eval("(define large (mix 0 2000 (list)))");
    daemon.eval("(define sorted (sort lt small))");
    daemon.check(&[
        ("(length sorted)", "1500"),
        ("(ordered sorted 0)", "1"),
        ("(ordered small 0)", "0"),
        ("(fold + 0 sorted)", &daemon.eval("(fold + 0 small)")),
        ("(sort lt large)", "error: evaluation step limit"),
    ]);
    // A session that raises its budget can sort 10000. The limit applies from the
    // next evaluation.
    daemon.eval("(define huge (mix 0 10000 (list)))");
    daemon.eval("(step-limit 10000000)");
    daemon.eval("(define sorted-huge (sort lt huge))");
    daemon.check(&[
        ("(length sorted-huge)", "10000"),
        ("(ordered sorted-huge 0)", "1"),
    ]);
}

#[test]
fn str_functions_from_the_bundle() {
    let daemon = Daemon::start("str", &[]);
    daemon.check(&[
        (r#"(require str "std/str.wisp")"#, "()"),
        (r#"(split "a,,b" ",")"#, r#"("a" "" "b")"#),
        (r#"(split "" ",")"#, r#"("")"#),
        (r#"(split ",a," ",")"#, r#"("" "a" "")"#),
        (r#"(split "a::b:c" "::")"#, r#"("a" "b:c")"#),
        (r#"(split "abc" "")"#, "error: split: empty separator"),
        (r#"(join (list "a" "b" "c") "-")"#, "a-b-c"),
        (r#"(join (list "a") "-")"#, "a"),
        (r#"(string-len (join (list) "-"))"#, "0"),
        ("(number->string 0)", "0"),
        ("(number->string 120)", "120"),
        ("(number->string -5)", "-5"),
        ("(number->string 2147483647)", "2147483647"),
        ("(number->string -2147483648)", "-2147483648"),
        ("(u64->string 0u64)", "0"),
        ("(u64->string 120u64)", "120"),
        ("(u64->string (- 0u64 1u64))", "18446744073709551615"),
        (r#"(string->number "123")"#, "123"),
        ("(mod 7 3)", "1"),
        (r#"(index-of "abc" (string-ref "c" 0))"#, "2"),
        (r#"(index-of "abc" (string-ref "x" 0))"#, "-1"),
        (r#"(index-of-from "abca" (string-ref "a" 0) 1)"#, "3"),
        (r#"(index-of-sub "abc" "bc")"#, "1"),
        (r#"(index-of-sub "abc" "x")"#, "-1"),
        (r#"(index-of-sub "abc" "")"#, "0"),
        (r#"(field-between "a b" 0 byte-space)"#, r#"("a" 1)"#),
        (r#"(starts-with "abc" "ab")"#, "1"),
        (r#"(starts-with "a" "ab")"#, "0"),
        (r#"(drop "abc" 1)"#, "bc"),
        (r#"(after "a::b" "::")"#, "b"),
        (r#"(string-len (after "ab" "::"))"#, "0"),
        ("(= byte-nl 10)", "1"),
        ("(= byte-cr 13)", "1"),
    ]);
    // 10000 fields both ways.
    daemon.check(&[(
        r#"(define many (lambda (i acc) (if (= i 10000) acc (many (+ i 1) (list-push acc "abcd")))))"#,
        "#<closure>",
    )]);
    daemon.eval("(define fields (many 0 (list)))");
    daemon.eval(r#"(define joined (join fields ","))"#);
    daemon.check(&[
        ("(string-len joined)", "49999"),
        (r#"(length (split joined ","))"#, "10000"),
    ]);
}

#[test]
fn result_helpers_from_the_bundle() {
    let daemon = Daemon::start("result", &[]);
    daemon.check(&[
        (r#"(require result "std/result.wisp")"#, "()"),
        ("(opt-or (some s32 1) 2)", "1"),
        ("(opt-or (none s32) 2)", "2"),
        ("(is-ok (ok s32 string 1))", "1"),
        ("(res-or (err s32 s32 1) 2)", "2"),
    ]);
}

#[test]
fn a_file_on_disk_shadows_the_bundle() {
    let shadow = "(module list (provide map) (define map (lambda (f xs) 42)))";
    let daemon = Daemon::start("shadow", &[("std/list.wisp", shadow)]);
    daemon.check(&[
        (r#"(require list "std/list.wisp")"#, "()"),
        ("(map 1 2)", "42"),
        (r#"(require str "std/str.wisp")"#, "()"),
        ("(number->string 5)", "5"),
    ]);
}

#[test]
fn the_bundle_serves_std_when_the_filesystem_is_denied() {
    let daemon = Daemon::start_with("denied", &[], "read = false", None);
    daemon.check(&[
        (r#"(require list "std/list.wisp")"#, "()"),
        ("(reverse (list 1 2))", "(2 1)"),
    ]);
}

#[test]
fn a_path_in_neither_place_reports_the_filesystem_error() {
    let daemon = Daemon::start("missing", &[]);
    daemon.check(&[(
        r#"(require nope "std/nope.wisp")"#,
        "error: cannot load module nope: cannot read source: std/nope.wisp",
    )]);
}

#[test]
fn a_bundle_keeps_std_and_can_replace_it() {
    let json = r#"{"lib/extra.wisp": "(module extra (provide x) (define x 7))",
        "std/str.wisp": "(module str (provide y) (define y 8))"}"#;
    let daemon = Daemon::start_with("bundle", &[], "", Some(json));
    daemon.check(&[
        (r#"(require extra "lib/extra.wisp")"#, "()"),
        ("x", "7"),
        (r#"(require str "std/str.wisp")"#, "()"),
        ("y", "8"),
        (r#"(require list "std/list.wisp")"#, "()"),
        ("(reverse (list 1 2))", "(2 1)"),
    ]);
}
