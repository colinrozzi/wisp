// Stamp a version into the binary so `theater-repl version` can report it.
// Preference: an explicit THEATER_REPL_VERSION (CI sets it to the release tag),
// then `git describe`, then the crate version.
use std::process::Command;

fn main() {
    let version = std::env::var("THEATER_REPL_VERSION")
        .ok()
        .filter(|s| !s.is_empty())
        .or_else(git_describe)
        .unwrap_or_else(|| {
            format!(
                "v{}",
                std::env::var("CARGO_PKG_VERSION").unwrap_or_else(|_| "0.0.0".into())
            )
        });
    println!("cargo:rustc-env=THEATER_REPL_VERSION={version}");
    println!("cargo:rerun-if-env-changed=THEATER_REPL_VERSION");
    // Re-stamp when the checked-out commit/tag moves.
    for path in ["../../../.git/HEAD", "../../../.git/packed-refs"] {
        println!("cargo:rerun-if-changed={path}");
    }
}

fn git_describe() -> Option<String> {
    let output = Command::new("git")
        .args(["describe", "--tags", "--always", "--dirty"])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if version.is_empty() {
        None
    } else {
        Some(version)
    }
}
