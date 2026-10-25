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
use wisp_interpreter_actor::{
    MANIFEST, Runtime, resolve, sessions::Note, short_id, source::SourceBundle, transport,
};

mod boot;

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
    /// Spawn an interpreter actor (a session) named NAME and print its id.
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
    /// List every interpreter actor in the runtime.
    List,
    /// Evaluate a form in a session. FORM may be omitted or "-" to read stdin.
    /// A string result prints raw (its text, real newlines); other values print
    /// as Wisp. For scripts, parse --literal output.
    Eval {
        id: String,
        form: Option<String>,
        /// Print every result as a Wisp literal, strings quoted and escaped.
        #[arg(short, long)]
        literal: bool,
    },
    /// Print a session's trigger firings (on-tick, on-message, …) since this
    /// reader's cursor, then move the cursor past them. Reading removes nothing:
    /// other readers (follow, wait, the session's own poll-events) still see
    /// them. Prints "handler: result" per firing, a string result raw. For
    /// scripts, parse --full output.
    Read {
        id: String,
        #[command(flatten)]
        from: ReadFrom,
    },
    /// Stream a session's firings until Ctrl-C (same format as read), resuming
    /// from where the last follow with this cursor stopped.
    Follow {
        id: String,
        #[command(flatten)]
        from: ReadFrom,
        /// Start at the newest firing: skip what is already buffered.
        #[arg(long)]
        from_now: bool,
    },
    /// Wait until the session has a firing after this cursor, print every one
    /// there is, move the cursor past them, and exit. The wake-on-message
    /// monitor: run it in the background and start it again after each wake.
    /// Nothing it did not print is acknowledged, so nothing is lost.
    Wait {
        id: String,
        #[command(flatten)]
        from: ReadFrom,
        /// Give up after this many seconds: exit 2 with nothing printed.
        #[arg(long)]
        timeout: Option<u64>,
    },
    /// List a session's event cursors: NAME, the seq it has acknowledged, how
    /// many firings it is behind, and how many `wait`s (or follows) are parked on
    /// it now. A running wake-on-message monitor shows as WAITING 1 on `wait`.
    Cursors { id: String },
    /// Show a session (with id) or the daemon (without).
    Status { id: Option<String> },
    /// Stop a session's actor and reclaim its heap. A boot-list service stopped
    /// this way stays stopped until `start`.
    Stop { id: String },
    /// Show each boot-list service: state, restarts, session, last death.
    Services,
    /// Start a boot-list service that is stopped, exited, or given up, with a
    /// fresh restart limit.
    Start { name: String },
    /// Stop the daemon itself: end all sessions and exit (frees the port).
    Down,
    /// Interactive stdin/stdout prompt against a private session (for a human).
    Repl,
    /// Print the installed version.
    Version,
    /// Download the latest release and replace this binary in place.
    Upgrade,
}

/// Where a reader (read, follow, wait) starts, and how it prints.
#[derive(clap::Args)]
struct ReadFrom {
    /// Read after this cursor. Each verb has its own by default (read, follow,
    /// wait), so they never take firings from each other.
    #[arg(long, conflicts_with = "after")]
    cursor: Option<String>,
    /// Read the firings after seq N, without a cursor (nothing is acknowledged).
    /// --after 0 shows everything still buffered.
    #[arg(long)]
    after: Option<u64>,
    /// Read without acknowledging: the cursor does not move.
    #[arg(long)]
    peek: bool,
    /// Print the Wisp list (next lost (seq handler result) ...), with
    /// (seq handler event result) for an unhandled firing.
    #[arg(long)]
    full: bool,
}

impl ReadFrom {
    /// The cursor to read and acknowledge, unless --after names a seq instead.
    fn cursor(&self, default: &str) -> Option<String> {
        match self.after {
            Some(_) => None,
            None => Some(self.cursor.clone().unwrap_or_else(|| default.to_string())),
        }
    }
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

/// theater-repl config (TOML). It grants the REPL actor's filesystem capability —
/// a root directory it may read/write, the human's slice of the actor manifest —
/// and lists the services `serve` starts and keeps running (`[[service]]`, see
/// `boot`).
#[derive(Debug, Default, serde::Deserialize)]
#[serde(default, deny_unknown_fields)]
struct ReplConfig {
    filesystem: FilesystemConfig,
    #[serde(rename = "service")]
    services: Vec<boot::ServiceConfig>,
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
    boot::validate(&config.services, config.filesystem.root.as_deref())
        .with_context(|| format!("in config {}", path.display()))?;
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

/// Everything a connection can touch. The session registry is `runtime.sessions`
/// (shared with the actors, for `(sessions)`). `default` is what `new` starts
/// sessions from; `startup` is the actor the daemon was launched with, for
/// `set-default --reset`. `services` is the boot list and its supervisor's state.
struct Daemon {
    runtime: Runtime,
    default: Mutex<ActorSource>,
    startup: ActorSource,
    services: boot::Services,
    shutdown: tokio::sync::Notify,
}

async fn serve(loaded: Loaded, config: ReplConfig, port: u16) -> Result<()> {
    let runtime = new_runtime(&loaded, &config).await?;
    let address = std::net::SocketAddr::from(([127, 0, 0, 1], port));
    let listener = tokio::net::TcpListener::bind(address)
        .await
        .with_context(|| format!("binding {address} (is a daemon already on this port?)"))?;
    let daemon = Arc::new(Daemon {
        runtime,
        default: Mutex::new(loaded.actor.clone()),
        startup: loaded.actor,
        services: boot::Services::new(config.services, config.filesystem.root),
        shutdown: tokio::sync::Notify::new(),
    });
    boot::boot(&daemon);
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
    daemon.services.close();
    let _ = daemon
        .runtime
        .commands
        .send(TheaterCommand::ShutdownRuntime);
    Ok(())
}

async fn handle(stream: tokio::net::TcpStream, daemon: &Arc<Daemon>) -> Result<()> {
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

async fn dispatch(request: &Value, daemon: &Arc<Daemon>) -> (bool, String) {
    match field(request, "op") {
        // `theater-repl down`: reply first, then wake the serve loop to exit.
        "shutdown" => {
            daemon.shutdown.notify_one();
            (true, "daemon going down".to_string())
        }
        "new" => match new_session(request, daemon).await {
            Ok(id) => (true, id),
            Err(error) => (false, format!("error: {error:#}")),
        },
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
        "eval" => run(daemon, field(request, "id"), field(request, "form")).await,
        // A read names a cursor or a seq. Clients that predate cursors name
        // neither and get the old poll-events read (and those that predate "text"
        // get the full list of firings).
        "read" => match reader_form(request) {
            Some(form) => run(daemon, field(request, "id"), &form).await,
            None => {
                let form = if request.get("text").and_then(Value::as_bool) == Some(true) {
                    "(poll-events \"text\")"
                } else {
                    "(poll-events)"
                };
                run(daemon, field(request, "id"), form).await
            }
        },
        "ack" => match ack_form(request) {
            Some(form) => run(daemon, field(request, "id"), &form).await,
            None => (false, "error: ack needs a cursor and a seq".into()),
        },
        "wait" => wait(daemon, request).await,
        "cursors" => cursors(daemon, field(request, "id")).await,
        "list" => {
            let interpreters = match daemon.runtime.sessions.list().await {
                Ok(interpreters) => interpreters,
                Err(error) => return (false, format!("error: {error:#}")),
            };
            if interpreters.is_empty() {
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
            for interpreter in &interpreters {
                let (age, source) = shown(daemon.runtime.sessions.noted(&interpreter.id).as_ref());
                out.push_str(&format!(
                    "\n{:<9} {:<16} {:>5}  {:<36}  {}",
                    short_id(&interpreters, &interpreter.id),
                    interpreter.name,
                    age,
                    interpreter.id,
                    source
                ));
            }
            (true, out)
        }
        "status" => match request.get("id").and_then(Value::as_str) {
            Some(target) => {
                let interpreters = match daemon.runtime.interpreters().await {
                    Ok(interpreters) => interpreters,
                    Err(error) => return (false, format!("error: {error:#}")),
                };
                match resolve(&interpreters, target) {
                    Ok(interpreter) => {
                        let (age, source) =
                            shown(daemon.runtime.sessions.noted(&interpreter.id).as_ref());
                        (
                            true,
                            format!(
                                "session {}\n  name:   {}\n  actor:  {}\n  source: {source}\n  age:    {age}",
                                short_id(&interpreters, &interpreter.id),
                                interpreter.name,
                                interpreter.id,
                            ),
                        )
                    }
                    Err(error) => (false, error.to_string()),
                }
            }
            None => match daemon.runtime.interpreters().await {
                Ok(interpreters) => {
                    let mut out = format!(
                        "daemon up — {} session(s)\n  default actor: {}",
                        interpreters.len(),
                        daemon.default.lock().await.origin
                    );
                    if !daemon.services.is_empty() {
                        out.push_str("\nservices:");
                        for line in daemon.services.render(&interpreters).lines() {
                            out.push_str(&format!("\n  {line}"));
                        }
                    }
                    (true, out)
                }
                Err(error) => (false, format!("error: {error:#}")),
            },
        },
        "services" => match daemon.runtime.interpreters().await {
            Ok(interpreters) => (true, daemon.services.render(&interpreters)),
            Err(error) => (false, format!("error: {error:#}")),
        },
        "start" => match boot::start(daemon, field(request, "name")) {
            Ok(text) => (true, text),
            Err(error) => (false, format!("error: {error:#}")),
        },
        "stop" => {
            let target = field(request, "id");
            match daemon.runtime.find(target).await {
                Ok(session) => {
                    // A service stopped on purpose is not restarted.
                    daemon.services.stopping(session.id);
                    match session.stop().await {
                        Ok(()) => (true, format!("stopped {target}")),
                        Err(error) => (false, format!("error: could not stop {target}: {error:#}")),
                    }
                }
                Err(_) if daemon.services.stop_named(target) => {
                    (true, format!("stopped service {target}"))
                }
                Err(error) => (false, error.to_string()),
            }
        }
        other => (false, format!("unknown op: {other}")),
    }
}

/// `new`: spawn an interpreter actor under the requested name, and print its id.
/// The runtime holds the name, so it is the address from now on; a name another
/// interpreter already has is refused rather than made ambiguous.
async fn new_session(request: &Value, daemon: &Daemon) -> Result<String> {
    let actor = requested_actor(request, daemon)
        .await
        .context("could not load actor")?;
    let name = match request.get("name").and_then(Value::as_str) {
        Some(name) => {
            if let Some(taken) = daemon
                .runtime
                .interpreters()
                .await?
                .into_iter()
                .find(|interpreter| interpreter.name == name)
            {
                anyhow::bail!("a session named {name} already exists: {}", taken.id);
            }
            name.to_string()
        }
        None => actor.manifest.name.clone(),
    };
    let session = daemon
        .runtime
        .spawn_named((*actor.wasm).clone(), actor.manifest.clone(), name)
        .await
        .context("could not create session")?;
    daemon.runtime.sessions.note(
        session.id,
        Note {
            source: actor.origin,
            since: Instant::now(),
        },
    );
    let interpreters = daemon.runtime.interpreters().await?;
    Ok(short_id(&interpreters, &session.id))
}

/// A note's age and source as `list`/`status` show them; "-" when there is none.
fn shown(note: Option<&Note>) -> (String, String) {
    match note {
        Some(note) => (
            format!("{}s", note.since.elapsed().as_secs()),
            note.source.clone(),
        ),
        None => ("-".into(), "-".into()),
    }
}

/// Evaluate `form` in the session `target` names; `ok` is false when the result is
/// a diagnostic.
async fn run(daemon: &Daemon, target: &str, form: &str) -> (bool, String) {
    match daemon.runtime.find(target).await {
        Ok(session) => match session.evaluate(form).await {
            Ok(output) => (!output.starts_with("error:"), output),
            Err(error) => (false, format!("error: {error}")),
        },
        Err(error) => (false, error.to_string()),
    }
}

// ---- event readers (daemon side) -----------------------------------------
//
// The session numbers its trigger firings and never removes one when it is read
// (wisp docs/proposals/SESSION-INBOX.md). A reader asks for the firings after a
// cursor or a seq, and the client acknowledges a cursor only after it printed.

/// A Wisp string literal for `s`.
fn wisp_string(s: &str) -> String {
    format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
}

/// The form that reads what a `read`/`wait` request names: a cursor, or the seq
/// in `after`. "text" requests get the daemon's "wire" text (a `next lost` line
/// first). None when the request names neither (an old client).
fn reader_form(request: &Value) -> Option<String> {
    let max = request
        .get("max")
        .and_then(Value::as_u64)
        .map(|max| format!(" {max}"))
        .unwrap_or_default();
    let mode = if request.get("text").and_then(Value::as_bool) == Some(true) {
        " \"wire\""
    } else {
        ""
    };
    if let Some(cursor) = request.get("cursor").and_then(Value::as_str) {
        Some(format!("(event-read {}{max}{mode})", wisp_string(cursor)))
    } else {
        let after = request.get("after").and_then(Value::as_u64)?;
        Some(format!("(events-after {after}{max}{mode})"))
    }
}

/// `(event-ack CURSOR SEQ)` for an ack request; `"seq": "head"` acknowledges
/// everything recorded so far.
fn ack_form(request: &Value) -> Option<String> {
    let cursor = request.get("cursor").and_then(Value::as_str)?;
    let seq = match request.get("seq")? {
        Value::String(head) if head == "head" => "(event-head)".to_string(),
        seq => seq.as_u64()?.to_string(),
    };
    Some(format!("(event-ack {} {seq})", wisp_string(cursor)))
}

/// A seq as the session prints it: a plain integer, or with an `s64` suffix
/// once it no longer fits an s32.
fn parse_seq(printed: &str) -> Option<u64> {
    printed.trim_end_matches("s64").parse().ok()
}

/// The `(next, lost)` a reader result starts with, and whether it carries
/// anything to show (firings, or a report of lost ones). `text` results are the
/// "wire" string; the others the `(next lost firing...)` list.
fn reader_head(printed: &str, text: bool) -> Option<(u64, u64, bool)> {
    if text {
        let wire = unquote(printed)?;
        let (head, body) = wire.split_once('\n').unwrap_or((&wire, ""));
        let (next, lost) = head.split_once(' ')?;
        let (next, lost) = (parse_seq(next)?, parse_seq(lost)?);
        Some((next, lost, lost > 0 || !body.is_empty()))
    } else {
        let mut parts = printed.strip_prefix('(')?.splitn(3, [' ', ')']);
        let next = parse_seq(parts.next()?)?;
        let lost = parse_seq(parts.next()?)?;
        let rest = parts.next().unwrap_or("").trim();
        Some((next, lost, lost > 0 || !rest.is_empty()))
    }
}

/// Block until the session has firings after the request's cursor (or seq),
/// then answer with them, like `read`. Waiting costs no evaluation: the
/// session reports each new seq through `wisp-events.event-recorded`.
async fn wait(daemon: &Daemon, request: &Value) -> (bool, String) {
    let target = field(request, "id");
    let Some(mut form) = reader_form(request) else {
        return (false, "error: wait needs a cursor or a seq".into());
    };
    let cursor = request.get("cursor").and_then(Value::as_str);
    let mut parked = None;
    let text = request.get("text").and_then(Value::as_bool) == Some(true);
    let session = match daemon.runtime.find(target).await {
        Ok(session) => session,
        Err(error) => return (false, error.to_string()),
    };
    // The previous batch's acknowledgement rides on the next wait (follow).
    if let Some(ack) = request.get("ack").and_then(|_| {
        let mut ack = request.clone();
        ack["seq"] = request["ack"].clone();
        ack_form(&ack)
    }) {
        let (ok, output) = run(daemon, target, &ack).await;
        if !ok {
            return (false, output);
        }
    }
    let deadline = request
        .get("timeout_ms")
        .and_then(Value::as_u64)
        .filter(|ms| *ms > 0)
        .map(|ms| tokio::time::Instant::now() + std::time::Duration::from_millis(ms));
    // Subscribe before the first read, so nothing recorded after it is missed.
    let mut recorded = daemon.runtime.events.subscribe(session.id);
    loop {
        let (ok, output) = match session.evaluate(&form).await {
            Ok(output) => (!output.starts_with("error:"), output),
            Err(error) => (false, format!("error: {error}")),
        };
        if !ok {
            return (false, output);
        }
        match reader_head(&output, text) {
            Some((_, _, true)) => return (true, output),
            // Nothing yet. Read after the seq the cursor was at from now on, not
            // after the cursor: another reader of the same cursor (a second
            // monitor) may acknowledge the next firing before this one re-reads.
            Some((next, _, false)) if parked.is_none() => {
                if let Some(cursor) = cursor {
                    parked = Some(daemon.runtime.events.park(session.id, cursor));
                    let mut after = request.clone();
                    after.as_object_mut().map(|fields| fields.remove("cursor"));
                    after["after"] = json!(next);
                    form = reader_form(&after).expect("a seq names a read");
                }
            }
            Some(_) => {}
            None => return (false, format!("error: unexpected reader output: {output}")),
        }
        // Sleep until a firing is recorded, the deadline passes, or the session
        // stops (checked every few seconds; a stopped session records nothing).
        loop {
            let timeout = async {
                match deadline {
                    Some(deadline) => tokio::time::sleep_until(deadline).await,
                    None => std::future::pending().await,
                }
            };
            tokio::select! {
                changed = recorded.changed() => {
                    if changed.is_err() {
                        recorded = daemon.runtime.events.subscribe(session.id);
                    }
                    // A burst of firings comes back as one batch.
                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                    break;
                }
                _ = timeout => return (true, output),
                _ = tokio::time::sleep(std::time::Duration::from_secs(5)) => {
                    if daemon.runtime.dead.contains(&session.id)
                        || daemon.runtime.attach(session.id).await.is_err()
                    {
                        daemon.runtime.events.forget(&session.id);
                        return (false, format!("error: session {target} stopped"));
                    }
                }
            }
        }
    }
}

/// `NAME SEQ BEHIND WAITING` for each of a session's cursors.
async fn cursors(daemon: &Daemon, target: &str) -> (bool, String) {
    let (ok, head) = run(daemon, target, "(event-head)").await;
    if !ok {
        return (false, head);
    }
    let (ok, list) = run(daemon, target, "(event-cursors)").await;
    if !ok {
        return (false, list);
    }
    let head = parse_seq(&head).unwrap_or(0);
    let parked = match daemon.runtime.find(target).await {
        Ok(session) => daemon.runtime.events.parked(session.id),
        Err(_) => Vec::new(),
    };
    let waiting = |name: &str| {
        parked
            .iter()
            .find(|(cursor, _)| cursor == name)
            .map_or(0, |(_, count)| *count)
    };
    let mut out = format!(
        "{:<16} {:>8} {:>8} {:>8}",
        "NAME", "SEQ", "BEHIND", "WAITING"
    );
    // (("name" seq) ...): names are written as Wisp strings.
    for entry in list.split("(\"").skip(1) {
        let Some((name, rest)) = entry.split_once("\" ") else {
            continue;
        };
        let seq = parse_seq(rest.split(')').next().unwrap_or("")).unwrap_or(0);
        let name = name.replace("\\\"", "\"").replace("\\\\", "\\");
        out.push_str(&format!(
            "\n{:<16} {:>8} {:>8} {:>8}",
            name,
            seq,
            head.saturating_sub(seq),
            waiting(&name)
        ));
    }
    if out.lines().count() == 1 {
        out.push_str(&format!("\n(no cursors; head is {head})"));
    }
    (true, out)
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
        Command::Eval { id, form, literal } => {
            let form = read_form(form).await?;
            let request = json!({ "op": "eval", "id": id, "form": form });
            if literal {
                send(port, request).await
            } else {
                send_raw(port, request).await
            }
        }
        Command::Read { id, from } => read(port, id, from).await,
        Command::Status { id } => send(port, json!({ "op": "status", "id": id })).await,
        Command::Stop { id } => send(port, json!({ "op": "stop", "id": id })).await,
        Command::Services => send(port, json!({ "op": "services" })).await,
        Command::Start { name } => send(port, json!({ "op": "start", "name": name })).await,
        Command::Down => send(port, json!({ "op": "shutdown" })).await,
        Command::Follow { id, from, from_now } => follow(port, id, from, from_now).await,
        Command::Wait { id, from, timeout } => wait_once(port, id, from, timeout).await,
        Command::Cursors { id } => send(port, json!({ "op": "cursors", "id": id })).await,
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

/// Like `send`, but a string result prints raw: its text, ending in exactly one
/// newline. An empty string prints nothing; other values print as `send` does.
async fn send_raw(port: u16, request: Value) -> Result<()> {
    let (ok, text) = transport::request(port, request).await?;
    if ok {
        print!("{}", raw_output(&text));
        Ok(())
    } else {
        eprintln!("{text}");
        std::process::exit(1);
    }
}

/// What `send_raw` prints for a printed value.
fn raw_output(printed: &str) -> String {
    let text = unquote(printed).unwrap_or_else(|| printed.to_string());
    if text.is_empty() || text.ends_with('\n') {
        text
    } else {
        text + "\n"
    }
}

/// The contents of a Wisp string literal as the printer writes it (escapes
/// \" \\ \n \t \r), or None when `printed` is not a string literal.
fn unquote(printed: &str) -> Option<String> {
    let body = printed.strip_prefix('"')?.strip_suffix('"')?;
    let mut out = String::with_capacity(body.len());
    let mut chars = body.chars();
    while let Some(c) = chars.next() {
        if c == '"' {
            return None;
        }
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next()? {
            'n' => out.push('\n'),
            't' => out.push('\t'),
            'r' => out.push('\r'),
            other => out.push(other),
        }
    }
    Some(out)
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

// ---- event readers (client side) -----------------------------------------

/// The read/wait request for `from`, starting at `cursor` or at `--after`.
fn reader_request(
    op: &str,
    id: &str,
    from: &ReadFrom,
    cursor: Option<&str>,
    after: Option<u64>,
) -> Value {
    let mut request = json!({ "op": op, "id": id, "text": !from.full });
    match cursor {
        Some(cursor) => request["cursor"] = json!(cursor),
        None => request["after"] = json!(after.unwrap_or(0)),
    }
    request
}

/// A session whose actor predates cursors answers a cursor read with an
/// unbound-symbol error.
fn predates_cursors(output: &str) -> bool {
    output.contains("unbound symbol: event-") || output.contains("unbound symbol: events-")
}

/// Print a reader result: the text after the `next lost` line, or with --full
/// the whole list. Fails if stdout is closed (e.g. `| head`): the caller then
/// must not acknowledge, so the firings are read again next time.
fn emit(output: &str, full: bool) -> std::io::Result<()> {
    use std::io::Write;
    let text = if full {
        format!("{output}\n")
    } else {
        let wire = unquote(output).unwrap_or_default();
        let body = wire.split_once('\n').map(|(_, body)| body).unwrap_or("");
        if body.is_empty() || body.ends_with('\n') {
            body.to_string()
        } else {
            format!("{body}\n")
        }
    };
    let mut stdout = std::io::stdout().lock();
    stdout.write_all(text.as_bytes())?;
    stdout.flush()
}

/// Request, check, and parse one reader result: (output, next, has firings).
async fn reader_round(port: u16, request: Value, full: bool) -> Result<(String, u64, bool)> {
    let (ok, output) = transport::request(port, request).await?;
    if !ok {
        anyhow::bail!("{output}");
    }
    let (next, _, has) = reader_head(&output, !full)
        .with_context(|| format!("unexpected reader output: {output}"))?;
    Ok((output, next, has))
}

async fn ack(port: u16, id: &str, cursor: &str, seq: Value) -> Result<()> {
    let (ok, output) = transport::request(
        port,
        json!({ "op": "ack", "id": id, "cursor": cursor, "seq": seq }),
    )
    .await?;
    if !ok {
        anyhow::bail!("{output}");
    }
    Ok(())
}

/// Print the result, then acknowledge it (unless --peek or --after). A failed
/// print exits 1 without acknowledging.
async fn print_and_ack(
    port: u16,
    id: &str,
    from: &ReadFrom,
    cursor: Option<&str>,
    output: &str,
    next: u64,
) -> Result<()> {
    if emit(output, from.full).is_err() {
        std::process::exit(1);
    }
    match cursor {
        Some(cursor) if !from.peek => ack(port, id, cursor, json!(next)).await,
        _ => Ok(()),
    }
}

fn fail(error: anyhow::Error) -> ! {
    eprintln!("{error:#}");
    std::process::exit(1);
}

async fn read(port: u16, id: String, from: ReadFrom) -> Result<()> {
    let cursor = from.cursor("read");
    let request = reader_request("read", &id, &from, cursor.as_deref(), from.after);
    match reader_round(port, request, from.full).await {
        Ok((output, next, _)) => {
            print_and_ack(port, &id, &from, cursor.as_deref(), &output, next).await
        }
        Err(error) if predates_cursors(&error.to_string()) => {
            eprintln!("theater-repl: session {id} predates event cursors; this read drains it");
            let request = json!({ "op": "read", "id": id, "text": !from.full });
            if from.full {
                send(port, request).await
            } else {
                send_raw(port, request).await
            }
        }
        Err(error) => fail(error),
    }
}

async fn wait_once(port: u16, id: String, from: ReadFrom, timeout: Option<u64>) -> Result<()> {
    let cursor = from.cursor("wait");
    let mut request = reader_request("wait", &id, &from, cursor.as_deref(), from.after);
    if let Some(seconds) = timeout {
        request["timeout_ms"] = json!(seconds.saturating_mul(1000));
    }
    match reader_round(port, request, from.full).await {
        Ok((_, _, false)) => std::process::exit(2),
        Ok((output, next, true)) => {
            print_and_ack(port, &id, &from, cursor.as_deref(), &output, next).await
        }
        Err(error) if predates_cursors(&error.to_string()) => fail(anyhow::anyhow!(
            "session {id} predates event cursors; use experiment/lib/wait-message for it"
        )),
        Err(error) => fail(error),
    }
}

/// A loop of waits. Each wait carries the previous batch's acknowledgement, so
/// a steady stream costs one request per batch; Ctrl-C acknowledges the last.
async fn follow(port: u16, id: String, from: ReadFrom, from_now: bool) -> Result<()> {
    let cursor = from.cursor("follow");
    let mut after = from.after;
    if from_now {
        match &cursor {
            Some(cursor) => ack(port, &id, cursor, json!("head"))
                .await
                .unwrap_or_else(|e| fail(e)),
            None => {
                let (ok, head) = transport::request(
                    port,
                    json!({ "op": "eval", "id": id, "form": "(event-head)" }),
                )
                .await?;
                after = Some(if ok { parse_seq(&head).unwrap_or(0) } else { 0 });
            }
        }
    }
    let stop = tokio::signal::ctrl_c();
    tokio::pin!(stop);
    let mut pending: Option<u64> = None;
    loop {
        let mut request = reader_request("wait", &id, &from, cursor.as_deref(), after);
        if let (Some(seq), false) = (pending, from.peek) {
            request["ack"] = json!(seq);
        }
        let round = tokio::select! {
            _ = &mut stop => {
                if let (Some(cursor), Some(seq), false) = (&cursor, pending, from.peek) {
                    let _ = ack(port, &id, cursor, json!(seq)).await;
                }
                return Ok(());
            }
            round = reader_round(port, request, from.full) => round,
        };
        match round {
            Ok((output, next, _)) => {
                if emit(&output, from.full).is_err() {
                    std::process::exit(1);
                }
                pending = cursor.as_ref().map(|_| next);
                if cursor.is_none() {
                    after = Some(next);
                }
            }
            Err(error) if predates_cursors(&error.to_string()) => {
                eprintln!(
                    "theater-repl: session {id} predates event cursors; this follow drains it"
                );
                return legacy_follow(port, id, from.full).await;
            }
            Err(error) => fail(error),
        }
    }
}

/// The pre-cursor follow: drain with poll-events once a second. Only for a
/// session whose actor predates cursors.
async fn legacy_follow(port: u16, id: String, full: bool) -> Result<()> {
    let stop = tokio::signal::ctrl_c();
    tokio::pin!(stop);
    loop {
        tokio::select! {
            _ = &mut stop => return Ok(()),
            _ = tokio::time::sleep(std::time::Duration::from_millis(1000)) => {
                let request = json!({ "op": "read", "id": id, "text": !full });
                let (ok, text) = transport::request(port, request).await?;
                if !ok {
                    eprintln!("{text}");
                    std::process::exit(1);
                }
                if !full {
                    print!("{}", raw_output(&text));
                    continue;
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
    fn raw_output_prints_string_contents() {
        assert_eq!(raw_output(r#""a\nb""#), "a\nb\n");
        assert_eq!(raw_output(r#""say \"hi\"\t\\ok""#), "say \"hi\"\t\\ok\n");
        assert_eq!(raw_output(r#""done\n""#), "done\n");
        assert_eq!(raw_output(r#""""#), "");
    }

    #[test]
    fn raw_output_keeps_other_values() {
        assert_eq!(raw_output("42"), "42\n");
        assert_eq!(raw_output(r#"("x" 1)"#), "(\"x\" 1)\n");
        assert_eq!(raw_output("#<closure>"), "#<closure>\n");
        // Not one literal: quotes at both ends, but an unescaped one inside.
        assert_eq!(raw_output(r#""a" "b""#), "\"a\" \"b\"\n");
    }

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
