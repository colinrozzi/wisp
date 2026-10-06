use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use tokio::io::{AsyncWriteExt, BufReader};
use wisp_interpreter_actor::{Runtime, source::SourceBundle, transport};

#[derive(Parser)]
struct Args {
    /// Directory containing the built actor, manifest, and immutable source bundle
    /// (the wisp-repl actor dir — this host crate's parent).
    #[arg(long, default_value = concat!(env!("CARGO_MANIFEST_DIR"), "/.."))]
    actor_dir: PathBuf,
    /// JSON object mapping bundle paths to source strings; loaded once at startup.
    #[arg(long)]
    bundle: Option<PathBuf>,
    #[command(subcommand)]
    command: Command,
}
#[derive(Subcommand)]
enum Command {
    Repl,
    /// Local TCP endpoint: newline-delimited JSON strings, one session per connection.
    Serve {
        #[arg(long, default_value = "127.0.0.1:7777")]
        listen: std::net::SocketAddr,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let manifest = theater::ManifestConfig::from_toml_str(&std::fs::read_to_string(
        args.actor_dir.join("manifest.toml"),
    )?)?;
    let wasm = std::fs::read(args.actor_dir.join(&manifest.package))
        .context("actor is not built; run actors/wisp-repl/build.sh first")?;
    let bundle_path = args
        .bundle
        .unwrap_or_else(|| args.actor_dir.join("sources.json"));
    let bundle = SourceBundle::new(serde_json::from_slice(
        &std::fs::read(&bundle_path)
            .with_context(|| format!("reading bundle {}", bundle_path.display()))?,
    )?)?;
    let runtime = Runtime::new(bundle).await?;
    if let Command::Serve { listen } = args.command {
        let result = serve(&runtime, wasm, manifest, listen).await;
        runtime.shutdown().await?;
        return result;
    }
    let session = runtime.spawn_with_manifest(wasm, manifest).await?;
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
    runtime.shutdown().await?;
    result
}

async fn serve(
    runtime: &Runtime,
    wasm: Vec<u8>,
    manifest: theater::ManifestConfig,
    address: std::net::SocketAddr,
) -> Result<()> {
    anyhow::ensure!(
        address.ip().is_loopback(),
        "the development REPL only listens on loopback"
    );
    let listener = tokio::net::TcpListener::bind(address).await?;
    // Current Theater exits when its last actor stops. Keep a service actor
    // alive while the listener exists, including between client connections.
    let listener_actor = runtime
        .spawn_with_manifest(wasm.clone(), manifest.clone())
        .await?;
    eprintln!(
        "Wisp Theater REPL listening on {} (JSON strings, one per line)",
        listener.local_addr()?
    );
    // A live sibling actor in the same runtime — a real target for rpc verbs:
    //   (describe "<id>") / (exports "<id>") / (implements "<id>" "<iface>")
    eprintln!("rpc target actor id: {}", listener_actor.id);
    let mut connections = tokio::task::JoinSet::new();
    let stop = tokio::signal::ctrl_c();
    tokio::pin!(stop);
    let result = loop {
        tokio::select! {
            _ = &mut stop => break Ok(()),
            connection = listener.accept(), if connections.len() < 16 => {
                let (stream, _) = match connection { Ok(value) => value, Err(error) => break Err(error.into()) };
                let session = match runtime.spawn_with_manifest(wasm.clone(), manifest.clone()).await { Ok(session) => session, Err(error) => break Err(error) };
                connections.spawn(transport::connection(stream, session));
            }
            Some(result) = connections.join_next(), if !connections.is_empty() => {
                match result {
                    Ok(Ok(())) => {}
                    Ok(Err(error)) => eprintln!("connection closed: {error}"),
                    Err(error) => eprintln!("connection task failed: {error}"),
                }
            }
        }
    };
    connections.abort_all();
    while connections.join_next().await.is_some() {}
    result
}
