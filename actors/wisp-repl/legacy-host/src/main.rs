use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use serde_json::{Value, json};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;
use theater::messages::TheaterCommand;
use tokio::io::{AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::sync::Mutex;
use wisp::compiler::Outcome;
use wisp_interpreter_actor::{
    EvalSession, MANIFEST, Runtime, Session, source::SourceBundle, transport,
};

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
    New { name: Option<String> },
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
        Command::Serve => serve(load(&args.actor_dir, &args.bundle)?, port).await,
        Command::Repl => repl(load(&args.actor_dir, &args.bundle)?).await,
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

struct Loaded {
    manifest: theater::ManifestConfig,
    wasm: Vec<u8>,
    bundle_json: String,
}

/// Where the actor comes from: the built-in copies, or `--actor-dir` on disk.
fn load(actor_dir: &Option<PathBuf>, bundle: &Option<PathBuf>) -> Result<Loaded> {
    let (manifest_str, wasm, mut bundle_json) = match actor_dir {
        Some(dir) => {
            let manifest_str = std::fs::read_to_string(dir.join("manifest.toml"))?;
            let package = theater::ManifestConfig::from_toml_str(&manifest_str)?.package;
            let wasm = std::fs::read(dir.join(&package))
                .context("actor is not built; run actors/wisp-repl/build.sh first")?;
            let bundle = std::fs::read_to_string(dir.join("sources.json"))?;
            (manifest_str, wasm, bundle)
        }
        None => (
            MANIFEST.to_string(),
            EMBEDDED_WASM.to_vec(),
            EMBEDDED_SOURCES.to_string(),
        ),
    };
    if let Some(path) = bundle {
        bundle_json = std::fs::read_to_string(path)
            .with_context(|| format!("reading bundle {}", path.display()))?;
    }
    let manifest = theater::ManifestConfig::from_toml_str(&manifest_str)?;
    Ok(Loaded {
        manifest,
        wasm,
        bundle_json,
    })
}

async fn new_runtime(loaded: &Loaded) -> Result<Runtime> {
    let sources: BTreeMap<String, String> = serde_json::from_str(&loaded.bundle_json)?;
    Runtime::new(SourceBundle::new(sources)?).await
}

// ---- the daemon -----------------------------------------------------------

/// A live session held by the daemon.
struct Entry {
    session: Arc<Session>,
    name: String,
    actor: String,
    since: Instant,
}
type Registry = BTreeMap<String, Entry>;

async fn serve(loaded: Loaded, port: u16) -> Result<()> {
    let runtime = Arc::new(new_runtime(&loaded).await?);
    let address = std::net::SocketAddr::from(([127, 0, 0, 1], port));
    let listener = tokio::net::TcpListener::bind(address)
        .await
        .with_context(|| format!("binding {address} (is a daemon already on this port?)"))?;
    let registry: Arc<Mutex<Registry>> = Arc::new(Mutex::new(BTreeMap::new()));
    let wasm = Arc::new(loaded.wasm);
    let manifest = Arc::new(loaded.manifest);
    let hint = if port == 7777 {
        String::new()
    } else {
        format!(" -p {port}")
    };
    eprintln!("theater-repl daemon on {address}");
    eprintln!("create a session:  theater-repl new{hint}");

    let mut connections = tokio::task::JoinSet::new();
    let stop = tokio::signal::ctrl_c();
    tokio::pin!(stop);
    loop {
        tokio::select! {
            _ = &mut stop => break,
            accepted = listener.accept() => {
                let (stream, _) = accepted?;
                let (registry, runtime, wasm, manifest) =
                    (registry.clone(), runtime.clone(), wasm.clone(), manifest.clone());
                connections.spawn(async move {
                    if let Err(error) = handle(stream, registry, runtime, wasm, manifest).await {
                        eprintln!("connection error: {error}");
                    }
                });
            }
            Some(_) = connections.join_next(), if !connections.is_empty() => {}
        }
    }
    let _ = runtime.commands.send(TheaterCommand::ShutdownRuntime);
    Ok(())
}

async fn handle(
    stream: tokio::net::TcpStream,
    registry: Arc<Mutex<Registry>>,
    runtime: Arc<Runtime>,
    wasm: Arc<Vec<u8>>,
    manifest: Arc<theater::ManifestConfig>,
) -> Result<()> {
    let mut reader = BufReader::new(stream);
    let Some(line) = transport::read_line(&mut reader).await? else {
        return Ok(());
    };
    let request: Value = serde_json::from_str(&line).unwrap_or_else(|_| json!({}));
    let (ok, text) = dispatch(&request, &registry, &runtime, &wasm, &manifest).await;
    let mut stream = reader.into_inner();
    let response = json!({ "ok": ok, "text": text }).to_string();
    stream.write_all(response.as_bytes()).await?;
    stream.write_all(b"\n").await?;
    Ok(())
}

fn field<'a>(request: &'a Value, key: &str) -> &'a str {
    request.get(key).and_then(Value::as_str).unwrap_or("")
}

async fn lookup(registry: &Arc<Mutex<Registry>>, id: &str) -> Option<Arc<Session>> {
    registry
        .lock()
        .await
        .get(id)
        .map(|entry| entry.session.clone())
}

async fn dispatch(
    request: &Value,
    registry: &Arc<Mutex<Registry>>,
    runtime: &Arc<Runtime>,
    wasm: &Arc<Vec<u8>>,
    manifest: &Arc<theater::ManifestConfig>,
) -> (bool, String) {
    match field(request, "op") {
        "new" => {
            let name = request
                .get("name")
                .and_then(Value::as_str)
                .map(String::from);
            match runtime
                .spawn_with_manifest((**wasm).clone(), (**manifest).clone())
                .await
            {
                Ok(session) => {
                    let actor = session.id.to_string();
                    let short: String = actor.chars().take(8).collect();
                    let mut map = registry.lock().await;
                    let id = if map.contains_key(&short) {
                        actor.clone()
                    } else {
                        short
                    };
                    let name = name.unwrap_or_else(|| id.clone());
                    map.insert(
                        id.clone(),
                        Entry {
                            session: Arc::new(session),
                            name,
                            actor,
                            since: Instant::now(),
                        },
                    );
                    (true, id)
                }
                Err(error) => (false, format!("error: could not create session: {error}")),
            }
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
            let mut out = format!("{:<9} {:<16} {:>5}  {}", "ID", "NAME", "AGE", "ACTOR");
            for (id, entry) in map.iter() {
                out.push_str(&format!(
                    "\n{:<9} {:<16} {:>4}s  {}",
                    id,
                    entry.name,
                    entry.since.elapsed().as_secs(),
                    entry.actor
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
                            "session {id}\n  name:  {}\n  actor: {}\n  age:   {}s",
                            entry.name,
                            entry.actor,
                            entry.since.elapsed().as_secs()
                        ),
                    ),
                    None => (false, format!("no such session: {id}")),
                },
                None => (true, format!("daemon up — {} session(s)", map.len())),
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
async fn run(registry: &Arc<Mutex<Registry>>, id: &str, form: &str) -> (bool, String) {
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
        Command::New { name } => send(port, json!({ "op": "new", "name": name })).await,
        Command::List => send(port, json!({ "op": "list" })).await,
        Command::Eval { id, form } => {
            let form = read_form(form).await?;
            send(port, json!({ "op": "eval", "id": id, "form": form })).await
        }
        Command::Read { id } => send(port, json!({ "op": "read", "id": id })).await,
        Command::Status { id } => send(port, json!({ "op": "status", "id": id })).await,
        Command::Stop { id } => send(port, json!({ "op": "stop", "id": id })).await,
        Command::Follow { id } => follow(port, id).await,
        Command::Serve | Command::Repl | Command::Version | Command::Upgrade => {
            unreachable!("handled in main")
        }
    }
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

/// Interactive stdin/stdout prompt on a shared-interpreter session bound to a live
/// Theater runtime. The read-eval-print loop runs on a blocking thread because
/// `EvalSession::feed` is synchronous (its `TheaterHost` blocks on the runtime), so
/// it can't run on a reactor task; the runtime itself keeps running on other threads.
async fn repl(loaded: Loaded) -> Result<()> {
    let runtime = new_runtime(&loaded).await?;
    eprintln!("Wisp REPL on a live Theater runtime — :quit to exit");
    let commands = runtime.commands.clone();
    let result = tokio::task::spawn_blocking(move || -> Result<()> {
        use std::io::{BufRead, Write};
        let mut session = EvalSession::new(commands);
        let stdin = std::io::stdin();
        let mut stdout = std::io::stdout();
        let mut line = String::new();
        loop {
            write!(stdout, "wisp> ")?;
            stdout.flush()?;
            line.clear();
            if stdin.lock().read_line(&mut line)? == 0 {
                break; // EOF
            }
            let input = line.trim();
            if input.is_empty() {
                continue;
            }
            if input == ":quit" || input == ":q" {
                break;
            }
            match session.feed(input) {
                Ok(Outcome::Defined(name)) => writeln!(stdout, "defined {name}")?,
                Ok(Outcome::Bound { name, value, .. }) => writeln!(stdout, "{name} = {value}")?,
                Ok(Outcome::Evaluated { value, .. }) => writeln!(stdout, "{value}")?,
                Err(e) => eprintln!("error: {e:#}"),
            }
        }
        Ok(())
    })
    .await?;
    runtime.shutdown().await?;
    result
}
