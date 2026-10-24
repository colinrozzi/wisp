// Host calls in a loop keep the actor's memory flat (docs/proposals/MEMORY.md §7
// stage 2): the import wrappers and raw-invoke free their argument and result
// buffers. `(memory-stats)` heap-bytes is the allocator's live bytes.
//
// WISP_ACTOR_WASM=<path> measures another build of the actor (e.g. an older one);
// the numbers print with --nocapture.

use std::path::{Path, PathBuf};
use wisp_interpreter_actor::{Runtime, Session, source::SourceBundle};

fn actor_path() -> PathBuf {
    std::env::var_os("WISP_ACTOR_WASM")
        .map(PathBuf::from)
        .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))
}

async fn heap_bytes(session: &Session) -> anyhow::Result<i64> {
    let stats = session.evaluate("(memory-stats)").await?;
    let rest = stats
        .split("(\"heap-bytes\" ")
        .nth(1)
        .ok_or_else(|| anyhow::anyhow!("no heap-bytes in {stats}"))?;
    Ok(rest[..rest.find(')').unwrap()].parse()?)
}

/// Live bytes one evaluation of `(spin n)` leaves behind.
async fn retained(session: &Session, n: u32) -> anyhow::Result<i64> {
    let before = heap_bytes(session).await?;
    assert_eq!(session.evaluate(&format!("(spin {n})")).await?, "0");
    Ok(heap_bytes(session).await? - before)
}

/// The bytes a host call retains, measured as the difference between a short and a
/// long loop so the evaluation's own garbage (the same for both) cancels out.
async fn per_call(session: &Session, verb: &str) -> anyhow::Result<i64> {
    session
        .evaluate(&format!(
            "(define spin (lambda (n) (if (= n 0) 0 (begin ({verb}) (spin (- n 1))))))"
        ))
        .await?;
    retained(session, 100).await?; // warm up: first-use tables, grown buffers
    let short = retained(session, 100).await?;
    let long = retained(session, 1100).await?;
    let per_call = (long - short) / 1000;
    println!("({verb}): {short} B for 100 calls, {long} B for 1100, {per_call} B/call");
    Ok(per_call)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_host_calls_in_a_loop_keep_memory_flat() -> anyhow::Result<()> {
    let wasm = std::fs::read(actor_path())?;
    let runtime = Runtime::new(SourceBundle::default(), Default::default()).await?;
    let session = runtime.spawn(wasm).await?;
    // Each loop iteration retains the interpreter's own garbage (environments,
    // values), which nothing frees until reference counting (stage 4), so compare
    // with a builtin that makes no host call. A host call then adds only its
    // decoded result: the marshal buffer is reused, and raw-invoke frees its
    // slots and the host's result buffer (before stage 2, 56 B more per (now)
    // and 304 B more per (sessions)). tests/heap.rs checks the boundary exactly.
    let baseline = per_call(&session, "list").await?;
    for (verb, result_bytes) in [("now", 128), ("sessions", 1024)] {
        let extra = per_call(&session, verb).await? - baseline;
        println!("({verb}): {extra} B/call more than (list)");
        assert!(
            extra < result_bytes,
            "({verb}) retains {extra} B per call beyond the evaluation's own"
        );
    }
    runtime.shutdown().await?;
    Ok(())
}
