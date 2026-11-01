// Compile the Wisp actor, which is generated and not in git, and stamp a version
// into the binary so `theater-repl version` can report it.
// Version preference: an explicit THEATER_REPL_VERSION (CI sets it to the release
// tag), then `git describe`, then the crate version.
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    compile_actor();

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
    // Ask git where they are: in a worktree `.git` is a file, and a path that doesn't
    // exist would rerun this script, and so recompile the actor, on every build.
    for name in ["HEAD", "packed-refs"] {
        if let Some(path) = git_path(name).filter(|p| Path::new(p).exists()) {
            println!("cargo:rerun-if-changed={path}");
        }
    }
}

/// Compile `../actor.wisp` to `$OUT_DIR/actor.wasm` (which main.rs embeds) and copy
/// it to `../actor.wasm` (which `--actor-dir`, the manifest and the tests read).
///
/// Granite is a build-dependency, so this runs in-process: a nested `cargo` would
/// wait forever on the shared build lock its parent holds (lib/shims/cargo).
fn compile_actor() {
    // Everything actor.wisp can include. Granite's own source needs no line:
    // changing a build-dependency reruns this script.
    for path in ["../actor.wisp", "../../interpreter", "../actor.wasm"] {
        println!("cargo:rerun-if-changed={path}");
    }
    println!("cargo:rerun-if-env-changed=GRANITE_RC");
    println!("cargo:rerun-if-env-changed=GRANITE_DEBUG_HEAP");

    // The interpreter relies on reference counting (see wisp/actor/build.sh).
    if std::env::var_os("GRANITE_RC").is_some_and(|v| v == "0") {
        panic!("the Wisp interpreter needs reference counting; unset GRANITE_RC=0");
    }

    let manifest_dir = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    let source = manifest_dir.join("../actor.wisp");
    let out_stem = out_dir.join("actor");
    // Compiling actor.wisp recurses deeply: use the 1 GiB stack `granite` itself uses.
    let compiled = std::thread::Builder::new()
        .stack_size(1 << 30)
        .spawn(move || granite::compiler::compile(&source, &out_stem, Default::default()))
        .unwrap()
        .join()
        .unwrap();
    if let Err(error) = compiled {
        panic!("compiling wisp/actor/actor.wisp failed: {error:#}");
    }

    publish(
        &out_dir.join("actor.wasm"),
        &manifest_dir.join("../actor.wasm"),
    );
}

/// Copy by rename, so a reader in this worktree never sees half a file. Leave an
/// identical file alone, so its mtime doesn't move.
fn publish(from: &Path, to: &Path) {
    let bytes = std::fs::read(from).unwrap();
    if std::fs::read(to).is_ok_and(|current| current == bytes) {
        return;
    }
    let temporary = to.with_extension(format!("wasm.{}", std::process::id()));
    std::fs::write(&temporary, &bytes).unwrap();
    std::fs::rename(&temporary, to).unwrap();
}

fn git_path(name: &str) -> Option<String> {
    let output = Command::new("git")
        .args(["rev-parse", "--path-format=absolute", "--git-path", name])
        .output()
        .ok()?;
    output
        .status
        .success()
        .then(|| String::from_utf8_lossy(&output.stdout).trim().to_string())
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
