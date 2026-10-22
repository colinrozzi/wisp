use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use serde_json::{Value, json};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;
use theater::messages::TheaterCommand;
use tokio::io::{AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::sync::Mutex;
use wisp_interpreter_actor::{MANIFEST, Runtime, Session, source::SourceBundle, transport};

/// The actor built into the binary (produced by actors/wisp-repl/build.sh). A
/// released theater-repl is self-contained: it carries its own actor + bundle.
const EMBEDDED_WASM: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/../actor.wasm"));
/// The immutable source bundle served to the guest for relative `(include …)`.
const EMBEDDED_SOURCES: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../sources.json"));

const VERSION: &str = env!("THEATER_REPL_VERSION");

#[derive(Parser)]
#[command(
    name = "theater-repl",
    version = VERSION,
    about = "A live Wisp REPL that drives and observes a Theater runtime"
)]
struct Args {
    /// Load the actor, manifest, and source bundle from this directory instead of
    /// the copies built into the binary (for hacking on the REPL itself).
    #[arg(long, global = true)]
    actor_dir: Option<PathBuf>,
    /// JSON object mapping bundle paths to source strings; overrides the built-in bundle.
    #[arg(long, global = true)]
    bundle: Option<PathBuf>,
    /// Path to a theater-repl config file (TOML). Without one, the default config
    /// path (~/.config/theater-repl/config.toml) is used if it exists; otherwise the
    /// REPL's filesystem capability is an ephemeral sandbox. Only `serve`/`repl` use it.
    #[arg(long, global = true)]
    config: Option<PathBuf>,
    /// Which daemon to talk to (loopback port). Resolves: this flag, then
    /// $THEATER_REPL_PORT, then 7777.
    #[arg(
        short = 'p',
        long,
        global = true,
        env = "THEATER_REPL_PORT",
        default_value_t = 7777
    )]
    port: u16,
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Run the daemon: a long-running Theater runtime that holds sessions (default).
    Serve,
    /// Create a new session on the daemon and print its id.
    New {
        name: Option<String>,
        /// Start this session from the actor this manifest names, instead of the
        /// daemon's default. Other sessions are not affected.
        #[arg(long)]
        manifest: Option<PathBuf>,
    },
    /// Change the actor that later `new` sessions start from. Running sessions keep
    /// theirs. The daemon tries the actor first and keeps the old default if it fails.
    #[command(group = clap::ArgGroup::new("target").required(true))]
    SetDefault {
        /// Manifest of the new default actor.
        #[arg(group = "target")]
        manifest: Option<PathBuf>,
        /// Go back to the actor the daemon started with.
        #[arg(long, group = "target")]
        reset: bool,
    },
    /// List the daemon's sessions.
    List,
    /// Evaluate a form in a session. FORM may be omitted or "-" to read stdin.
    Eval { id: String, form: Option<String> },
    /// Drain a session's buffered events (inbound triggers: on-tick, on-message, …).
    Read { id: String },
    /// Stream a session's events live until Ctrl-C.
    Follow { id: String },
    /// Show a session (with id) or the daemon (without).
    Status { id: Option<String> },
    /// End a session and reclaim its heap.
    Stop { id: String },
    /// Stop the daemon itself: end all sessions and exit (frees the port).
    Down,
    /// Interactive stdin/stdout prompt against a private session (for a human).
    Repl,
    /// Print the installed version.
    Version,
    /// Download the latest release and replace this binary in place.
    Upgrade,
}

#[tokio::main]
async fn main() -> Result<()> {
    // Env-filtered logs (RUST_LOG=...), so the daemon's Theater/handler activity
    // is visible. Silent unless RUST_LOG is set.
    let _ = tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .try_init();
    let args = Args::parse();
    let port = args.port;
    match args.command.unwrap_or(Command::Serve) {
        Command::Serve => {
            serve(
                load(&args.actor_dir, &args.bundle)?,
                load_config(&args.config)?,
                port,
            )
            .await
        }
        Command::Repl => {
            repl(
                load(&args.actor_dir, &args.bundle)?,
                load_config(&args.config)?,
            )
            .await
        }
        Command::Version => {
            println!("{VERSION}");
            Ok(())
        }
        Command::Upgrade => upgrade(),
        client => run_client(client, port).await,
    }
}

/// Re-run the installer, targeting this binary's own location so the running
/// `theater-repl` is replaced in place with the latest published release.
fn upgrade() -> Result<()> {
    let exe = std::env::current_exe()?;
    let dir = exe.parent().context("binary has no parent directory")?;
    eprintln!("current version: {VERSION}");
    eprintln!("upgrading {} ...", exe.display());
    let status = std::process::Command::new("sh")
        .arg("-c")
        .arg("curl -fsSL https://raw.githubusercontent.com/colinrozzi/wisp/main/install.sh | sh")
        .env("THEATER_REPL_BIN", dir)
        .status()
        .context("running the installer (is curl installed?)")?;
    anyhow::ensure!(status.success(), "upgrade failed");
    Ok(())
}

// ---- actor artifacts ------------------------------------------------------

/// An actor a session can start from: a manifest and the wasm package it names.
/// `origin` says where it came from, for `list`/`status`.
#[derive(Clone)]
struct ActorSource {
    manifest: theater::ManifestConfig,
    wasm: Arc<Vec<u8>>,
    origin: String,
}

impl ActorSource {
    fn built_in() -> Result<Self> {
        Ok(Self {
            manifest: theater::ManifestConfig::from_toml_str(MANIFEST)?,
            wasm: Arc::new(EMBEDDED_WASM.to_vec()),
            origin: format!("built-in {VERSION}"),
        })
    }

    /// Read a manifest and the package it names, resolved against the manifest's
    /// directory (so `package = "./actor.wasm"` means the file beside it).
    fn from_manifest(path: &Path) -> Result<Self> {
        let text = std::fs::read_to_string(path)
            .with_context(|| format!("reading manifest {}", path.display()))?;
        let manifest = theater::ManifestConfig::from_toml_str(&text)
            .with_context(|| format!("parsing manifest {}", path.display()))?;
        let package = path
            .parent()
            .unwrap_or(Path::new("."))
            .join(&manifest.package);
        let wasm = std::fs::read(&package)
            .with_context(|| format!("reading package {} (is it built?)", package.display()))?;
        Ok(Self {
            manifest,
            wasm: Arc::new(wasm),
            origin: path.display().to_string(),
        })
    }
}

struct Loaded {
    actor: ActorSource,
    bundle_json: String,
}

/// Where the actor comes from: the built-in copies, or `--actor-dir` on disk.
fn load(actor_dir: &Option<PathBuf>, bundle: &Option<PathBuf>) -> Result<Loaded> {
    let (actor, mut bundle_json) = match actor_dir {
        Some(dir) => (
            ActorSource::from_manifest(&dir.join("manifest.toml"))?,
            std::fs::read_to_string(dir.join("sources.json"))?,
        ),
        None => (ActorSource::built_in()?, EMBEDDED_SOURCES.to_string()),
    };
    if let Some(path) = bundle {
        bundle_json = std::fs::read_to_string(path)
            .with_context(|| format!("reading bundle {}", path.display()))?;
    }
    Ok(Loaded { actor, bundle_json })
}

// ---- config ---------------------------------------------------------------

/// theater-repl config (TOML). For now its one job is granting the REPL actor's
/// filesystem capability — a root directory it may read/write. This is the human's
/// slice of the actor manifest; other capability knobs can grow here later.
#[derive(Debug, Default, serde::Deserialize)]
#[serde(default, deny_unknown_fields)]
struct ReplConfig {
    filesystem: FilesystemConfig,
}

#[derive(Debug, serde::Deserialize)]
#[serde(default, deny_unknown_fields)]
struct FilesystemConfig {
    /// The directory the REPL's filesystem capability is rooted at. A leading `~/`
    /// expands to $HOME. Absent → an ephemeral sandbox (the historical behavior).
    root: Option<PathBuf>,
    read: bool,
    write: bool,
}

impl Default for FilesystemConfig {
    fn default() -> Self {
        Self {
            root: None,
            read: true,
            write: true,
        }
    }
}

fn default_config_path() -> Option<PathBuf> {
    std::env::var_os("HOME").map(|h| PathBuf::from(h).join(".config/theater-repl/config.toml"))
}

fn expand_tilde(path: PathBuf) -> PathBuf {
    match (path.strip_prefix("~"), std::env::var_os("HOME")) {
        (Ok(rest), Some(home)) => PathBuf::from(home).join(rest),
        _ => path,
    }
}

/// Resolve config: an explicit `--config`, else the default path if it exists, else
/// the default (ephemeral filesystem). An explicit path that is missing is an error;
/// a missing default path is not.
fn load_config(explicit: &Option<PathBuf>) -> Result<ReplConfig> {
    let path = match explicit {
        Some(p) => Some(p.clone()),
        None => default_config_path().filter(|p| p.exists()),
    };
    let Some(path) = path else {
        return Ok(ReplConfig::default());
    };
    let text = std::fs::read_to_string(&path)
        .with_context(|| format!("reading config {}", path.display()))?;
    let mut config: ReplConfig =
        toml::from_str(&text).with_context(|| format!("parsing config {}", path.display()))?;
    config.filesystem.root = config.filesystem.root.map(expand_tilde);
    Ok(config)
}

fn fs_access(config: &ReplConfig) -> Result<wisp_interpreter_actor::FsAccess> {
    if let Some(root) = &config.filesystem.root {
        // Create the configured library root up front so first use is smooth.
        std::fs::create_dir_all(root)
            .with_context(|| format!("creating filesystem root {}", root.display()))?;
        eprintln!("filesystem root: {}", root.display());
    }
    Ok(wisp_interpreter_actor::FsAccess {
        root: config.filesystem.root.clone(),
        read: config.filesystem.read,
        write: config.filesystem.write,
    })
}

async fn new_runtime(loaded: &Loaded, config: &ReplConfig) -> Result<Runtime> {
    let sources: BTreeMap<String, String> = serde_json::from_str(&loaded.bundle_json)?;
    Runtime::new(SourceBundle::new(sources)?, fs_access(config)?).await
}

// ---- the daemon -----------------------------------------------------------

/// A live session held by the daemon.
struct Entry {
    session: Arc<Session>,
    name: String,
    actor: String,
    /// Where its actor came from (an `ActorSource` origin).
    source: String,
    since: Instant,
}
type Registry = BTreeMap<String, Entry>;

/// Everything a connection can touch. `default` is what `new` starts sessions
/// from; `startup` is the actor the daemon was launched with, for `set-default --reset`.
struct Daemon {
    registry: Mutex<Registry>,
    runtime: Runtime,
    default: Mutex<ActorSource>,
    startup: ActorSource,
    shutdown: tokio::sync::Notify,
}

async fn serve(loaded: Loaded, config: ReplConfig, port: u16) -> Result<()> {
    let runtime = new_runtime(&loaded, &config).await?;
    let address = std::net::SocketAddr::from(([127, 0, 0, 1], port));
    let listener = tokio::net::TcpListener::bind(address)
        .await
        .with_context(|| format!("binding {address} (is a daemon already on this port?)"))?;
    let daemon = Arc::new(Daemon {
        registry: Mutex::new(BTreeMap::new()),
        runtime,
        default: Mutex::new(loaded.actor.clone()),
        startup: loaded.actor,
        shutdown: tokio::sync::Notify::new(),
    });
    let hint = if port == 7777 {
        String::new()
    } else {
        format!(" -p {port}")
    };
    eprintln!("theater-repl daemon on {address}");
    eprintln!("create a session:  theater-repl new{hint}");

    // Triggered by the `shutdown` op (the `theater-repl down` client command), so
    // the daemon can be brought down cleanly over the socket — no pid-hunting.
    let mut connections = tokio::task::JoinSet::new();
    let stop = tokio::signal::ctrl_c();
    tokio::pin!(stop);
    loop {
        tokio::select! {
            _ = &mut stop => break,
            _ = daemon.shutdown.notified() => {
                eprintln!("theater-repl daemon going down");
                break;
            }
            accepted = listener.accept() => {
                let (stream, _) = accepted?;
                let daemon = daemon.clone();
                connections.spawn(async move {
                    if let Err(error) = handle(stream, &daemon).await {
                        eprintln!("connection error: {error}");
                    }
                });
            }
            Some(_) = connections.join_next(), if !connections.is_empty() => {}
        }
    }
    let _ = daemon
        .runtime
        .commands
        .send(TheaterCommand::ShutdownRuntime);
    Ok(())
}

async fn handle(stream: tokio::net::TcpStream, daemon: &Daemon) -> Result<()> {
    let mut reader = BufReader::new(stream);
    let Some(line) = transport::read_line(&mut reader).await? else {
        return Ok(());
    };
    let request: Value = serde_json::from_str(&line).unwrap_or_else(|_| json!({}));
    let (ok, text) = dispatch(&request, daemon).await;
    let mut stream = reader.into_inner();
    let response = json!({ "ok": ok, "text": text }).to_string();
    stream.write_all(response.as_bytes()).await?;
    stream.write_all(b"\n").await?;
    Ok(())
}

fn field<'a>(request: &'a Value, key: &str) -> &'a str {
    request.get(key).and_then(Value::as_str).unwrap_or("")
}

async fn lookup(registry: &Mutex<Registry>, id: &str) -> Option<Arc<Session>> {
    registry
        .lock()
        .await
        .get(id)
        .map(|entry| entry.session.clone())
}

/// The actor a request names with `manifest`, or the daemon's current default.
async fn requested_actor(request: &Value, daemon: &Daemon) -> Result<ActorSource> {
    match request.get("manifest").and_then(Value::as_str) {
        Some(path) => ActorSource::from_manifest(Path::new(path)),
        None => Ok(daemon.default.lock().await.clone()),
    }
}

/// Start an actor, check that it answers `evaluate`, then stop it. `set-default`
/// runs this so a broken build can't become everyone's next session.
async fn probe(runtime: &Runtime, actor: &ActorSource) -> Result<()> {
    let session = runtime
        .spawn_with_manifest((*actor.wasm).clone(), actor.manifest.clone())
        .await?;
    let answer = session.evaluate("(+ 1 2)").await;
    let _ = session.stop().await;
    match answer? {
        output if output == "3" => Ok(()),
        output => anyhow::bail!("(+ 1 2) gave {output:?}, expected 3"),
    }
}

async fn dispatch(request: &Value, daemon: &Daemon) -> (bool, String) {
    let registry = &daemon.registry;
    match field(request, "op") {
        // `theater-repl down`: reply first, then wake the serve loop to exit.
        "shutdown" => {
            daemon.shutdown.notify_one();
            (true, "daemon going down".to_string())
        }
        "new" => {
            let name = request
                .get("name")
                .and_then(Value::as_str)
                .map(String::from);
            let actor = match requested_actor(request, daemon).await {
                Ok(actor) => actor,
                Err(error) => return (false, format!("error: could not load actor: {error:#}")),
            };
            match daemon
                .runtime
                .spawn_with_manifest((*actor.wasm).clone(), actor.manifest.clone())
                .await
            {
                Ok(session) => {
                    let actor_id = session.id.to_string();
                    let short: String = actor_id.chars().take(8).collect();
                    let mut map = registry.lock().await;
                    let id = if map.contains_key(&short) {
                        actor_id.clone()
                    } else {
                        short
                    };
                    let name = name.unwrap_or_else(|| id.clone());
                    map.insert(
                        id.clone(),
                        Entry {
                            session: Arc::new(session),
                            name,
                            actor: actor_id,
                            source: actor.origin,
                            since: Instant::now(),
                        },
                    );
                    (true, id)
                }
                Err(error) => (false, format!("error: could not create session: {error}")),
            }
        }
        "set-default" => {
            let actor = if request.get("reset").and_then(Value::as_bool) == Some(true) {
                daemon.startup.clone()
            } else if request.get("manifest").and_then(Value::as_str).is_some() {
                match requested_actor(request, daemon).await {
                    Ok(actor) => actor,
                    Err(error) => {
                        return (false, format!("error: could not load actor: {error:#}"));
                    }
                }
            } else {
                return (false, "error: set-default needs a manifest or reset".into());
            };
            if let Err(error) = probe(&daemon.runtime, &actor).await {
                return (
                    false,
                    format!("error: actor failed its check, default unchanged: {error:#}"),
                );
            }
            let origin = actor.origin.clone();
            *daemon.default.lock().await = actor;
            (true, format!("new sessions now start from {origin}"))
        }
        "eval" => run(registry, field(request, "id"), field(request, "form")).await,
        "read" => run(registry, field(request, "id"), "(poll-events)").await,
        "list" => {
            let map = registry.lock().await;
            if map.is_empty() {
                return (
                    true,
                    "no sessions — create one with: theater-repl new".into(),
                );
            }
            // SOURCE stays last: lib/sessions.wisp reads ACTOR as the 4th word.
            let mut out = format!(
                "{:<9} {:<16} {:>5}  {:<36}  {}",
                "ID", "NAME", "AGE", "ACTOR", "SOURCE"
            );
            for (id, entry) in map.iter() {
                out.push_str(&format!(
                    "\n{:<9} {:<16} {:>4}s  {:<36}  {}",
                    id,
                    entry.name,
                    entry.since.elapsed().as_secs(),
                    entry.actor,
                    entry.source
                ));
            }
            (true, out)
        }
        "status" => {
            let map = registry.lock().await;
            match request.get("id").and_then(Value::as_str) {
                Some(id) => match map.get(id) {
                    Some(entry) => (
                        true,
                        format!(
                            "session {id}\n  name:   {}\n  actor:  {}\n  source: {}\n  age:    {}s",
                            entry.name,
                            entry.actor,
                            entry.source,
                            entry.since.elapsed().as_secs()
                        ),
                    ),
                    None => (false, format!("no such session: {id}")),
                },
                None => (
                    true,
                    format!(
                        "daemon up — {} session(s)\n  default actor: {}",
                        map.len(),
                        daemon.default.lock().await.origin
                    ),
                ),
            }
        }
        "stop" => {
            let id = field(request, "id");
            let entry = registry.lock().await.remove(id);
            match entry {
                Some(entry) => {
                    let _ = entry.session.stop().await;
                    (true, format!("stopped {id}"))
                }
                None => (false, format!("no such session: {id}")),
            }
        }
        other => (false, format!("unknown op: {other}")),
    }
}

/// Evaluate `form` in session `id`; `ok` is false when the result is a diagnostic.
async fn run(registry: &Mutex<Registry>, id: &str, form: &str) -> (bool, String) {
    match lookup(registry, id).await {
        Some(session) => match session.evaluate(form).await {
            Ok(output) => (!output.starts_with("error:"), output),
            Err(error) => (false, format!("error: {error}")),
        },
        None => (false, format!("no such session: {id}")),
    }
}

// ---- the client -----------------------------------------------------------

async fn run_client(command: Command, port: u16) -> Result<()> {
    match command {
        Command::New { name, manifest } => {
            let manifest = manifest.map(|path| absolute(&path)).transpose()?;
            send(
                port,
                json!({ "op": "new", "name": name, "manifest": manifest }),
            )
            .await
        }
        Command::SetDefault { manifest, reset } => {
            let manifest = manifest.map(|path| absolute(&path)).transpose()?;
            send(
                port,
                json!({ "op": "set-default", "manifest": manifest, "reset": reset }),
            )
            .await
        }
        Command::List => send(port, json!({ "op": "list" })).await,
        Command::Eval { id, form } => {
            let form = read_form(form).await?;
            send(port, json!({ "op": "eval", "id": id, "form": form })).await
        }
        Command::Read { id } => send(port, json!({ "op": "read", "id": id })).await,
        Command::Status { id } => send(port, json!({ "op": "status", "id": id })).await,
        Command::Stop { id } => send(port, json!({ "op": "stop", "id": id })).await,
        Command::Down => send(port, json!({ "op": "shutdown" })).await,
        Command::Follow { id } => follow(port, id).await,
        Command::Serve | Command::Repl | Command::Version | Command::Upgrade => {
            unreachable!("handled in main")
        }
    }
}

/// The daemon has its own working directory, so send it an absolute path. A
/// missing file fails here, before anything reaches the daemon.
fn absolute(path: &Path) -> Result<String> {
    let path = std::fs::canonicalize(path)
        .with_context(|| format!("no such manifest: {}", path.display()))?;
    Ok(path.display().to_string())
}

/// Send one request and print the response (stdout on ok, stderr + exit 1 otherwise).
async fn send(port: u16, request: Value) -> Result<()> {
    let (ok, text) = transport::request(port, request).await?;
    if ok {
        if !text.is_empty() {
            println!("{text}");
        }
        Ok(())
    } else {
        eprintln!("{text}");
        std::process::exit(1);
    }
}

/// A form from the CLI arg, or from stdin when omitted or "-".
async fn read_form(form: Option<String>) -> Result<String> {
    match form {
        Some(form) if form != "-" => Ok(form),
        _ => {
            let mut buffer = String::new();
            tokio::io::stdin().read_to_string(&mut buffer).await?;
            Ok(buffer)
        }
    }
}

async fn follow(port: u16, id: String) -> Result<()> {
    let stop = tokio::signal::ctrl_c();
    tokio::pin!(stop);
    loop {
        tokio::select! {
            _ = &mut stop => return Ok(()),
            _ = tokio::time::sleep(std::time::Duration::from_millis(1000)) => {
                let (ok, text) = transport::request(port, json!({ "op": "read", "id": id })).await?;
                if !ok {
                    eprintln!("{text}");
                    std::process::exit(1);
                }
                let text = text.trim();
                if !text.is_empty() && text != "()" {
                    println!("{text}");
                }
            }
        }
    }
}

// ---- the human REPL -------------------------------------------------------

/// Interactive stdin/stdout prompt against one private session.
async fn repl(loaded: Loaded, config: ReplConfig) -> Result<()> {
    let runtime = new_runtime(&loaded, &config).await?;
    let session = runtime
        .spawn_with_manifest((*loaded.actor.wasm).clone(), loaded.actor.manifest)
        .await?;
    eprintln!("Wisp Theater actor {} — :quit to exit", session.id);
    let result = async {
        let mut input = BufReader::new(tokio::io::stdin());
        let mut output = tokio::io::stdout();
        loop {
            output.write_all(b"wisp> ").await?;
            output.flush().await?;
            let Some(line) = transport::read_line(&mut input).await? else {
                break;
            };
            if line.trim() == ":quit" {
                break;
            }
            let value = session.evaluate(&line).await?;
            output.write_all(format!("{value}\n").as_bytes()).await?;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await;
    session.shutdown().await?;
    runtime.shutdown().await?;
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn config_defaults_to_ephemeral_filesystem() {
        let c = ReplConfig::default();
        assert!(c.filesystem.root.is_none());
        assert!(c.filesystem.read && c.filesystem.write);
    }

    #[test]
    fn config_parses_filesystem_root_and_perms() {
        let c: ReplConfig =
            toml::from_str("[filesystem]\nroot = \"/srv/wisp-libs\"\nread = true\nwrite = false\n")
                .unwrap();
        assert_eq!(
            c.filesystem.root.unwrap().to_str().unwrap(),
            "/srv/wisp-libs"
        );
        assert!(c.filesystem.read);
        assert!(!c.filesystem.write);
    }

    #[test]
    fn config_empty_filesystem_table_keeps_defaults() {
        let c: ReplConfig = toml::from_str("[filesystem]\n").unwrap();
        assert!(c.filesystem.root.is_none());
        assert!(c.filesystem.read && c.filesystem.write);
    }

    #[test]
    fn expand_tilde_rewrites_leading_home() {
        // SAFETY: single-threaded test setting an env var for the expansion check.
        unsafe { std::env::set_var("HOME", "/home/tester") };
        assert_eq!(
            expand_tilde(PathBuf::from("~/wisp/lib")),
            PathBuf::from("/home/tester/wisp/lib")
        );
        assert_eq!(
            expand_tilde(PathBuf::from("/abs/path")),
            PathBuf::from("/abs/path")
        );
    }
}
