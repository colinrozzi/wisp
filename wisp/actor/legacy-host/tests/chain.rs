//! The chain recorder (`wisp-chain`): a session watches an actor's event chain
//! and reads its head, its newest events (hashes, types and sizes, never
//! payloads) and whether each event links to the one before it.

use std::collections::BTreeMap;
use std::path::Path;
use theater::ManifestConfig;
use wisp_interpreter_actor::{MANIFEST, Runtime, Session, chain::RING, source::SourceBundle};

async fn two_sessions() -> anyhow::Result<(Runtime, Session, Session)> {
    let runtime = Runtime::new(SourceBundle::new(BTreeMap::new())?, Default::default()).await?;
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let manifest = ManifestConfig::from_toml_str(MANIFEST)?;
    let alice = runtime
        .spawn_named(wasm.clone(), manifest.clone(), "alice".into())
        .await?;
    let bob = runtime.spawn_named(wasm, manifest, "bob".into()).await?;
    Ok((runtime, alice, bob))
}

/// alice's view of bob's chain through `verb`, e.g. `chain-head`.
async fn ask(alice: &Session, verb: &str, bob: &Session, rest: &str) -> anyhow::Result<String> {
    alice
        .evaluate(&format!("({verb} \"{}\"{rest})", bob.id))
        .await
}

/// bob's chain head as alice sees it: (hash-length count recording).
async fn head(alice: &Session, bob: &Session) -> anyhow::Result<String> {
    alice
        .evaluate(&format!(
            "(match (chain-head \"{}\") ((ok h) (match h ((chain-head hash count rec) (list (string-len hash) count rec)))) ((err e) e))",
            bob.id
        ))
        .await
}

/// The recorder takes an event just after the actor emits it, so an evaluation
/// can return before its last event is recorded. Wait until `head` is `want`.
async fn head_becomes(alice: &Session, bob: &Session, want: &str) -> anyhow::Result<String> {
    let mut seen = String::new();
    for _ in 0..100 {
        seen = head(alice, bob).await?;
        if seen == want {
            break;
        }
        tokio::time::sleep(std::time::Duration::from_millis(20)).await;
    }
    Ok(seen)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_watched_chain_is_recorded_linked_and_without_payloads() -> anyhow::Result<()> {
    let (runtime, alice, bob) = two_sessions().await?;

    // Not watched yet: an error, not an empty chain.
    assert!(
        ask(&alice, "chain-head", &bob, "")
            .await?
            .contains("not watching")
    );
    assert_eq!(
        ask(&alice, "chain-watch", &bob, "").await?,
        "(ok (tuple) (tuple) ())"
    );
    // Watching twice does not subscribe twice.
    assert_eq!(
        ask(&alice, "chain-watch", &bob, "").await?,
        "(ok (tuple) (tuple) ())"
    );
    assert_eq!(head(&alice, &bob).await?, "(0 0 true)");

    bob.evaluate("(define secret \"PAYLOAD-SECRET-42\")")
        .await?;
    bob.evaluate("(+ 1 2)").await?;
    // Two evaluations: a call and a result event each, every event once.
    assert_eq!(
        head_becomes(&alice, &bob, "(40 4 true)").await?,
        "(40 4 true)"
    );

    let tail = ask(&alice, "chain-tail", &bob, " 10").await?;
    assert!(!tail.contains("PAYLOAD-SECRET"), "{tail}");
    assert_eq!(tail.matches("(chain-event ").count(), 4, "{tail}");
    // Each event's parent is the hash before it.
    let hashes: Vec<&str> = tail.split('"').skip(1).step_by(2).collect();
    for event in hashes.chunks(3).collect::<Vec<_>>().windows(2) {
        assert_eq!(event[1][1], event[0][0], "{tail}");
    }
    assert!(
        ask(&alice, "chain-tail", &bob, " 1")
            .await?
            .contains(hashes[9])
    );
    assert_eq!(
        ask(&alice, "chain-verify", &bob, "").await?,
        "(ok chain-check (tuple) (chain-check true 4 4 0 0 false))"
    );

    // Bad arguments are errors, not traps.
    assert!(
        alice
            .evaluate("(chain-head \"nope\")")
            .await?
            .contains("not an actor id")
    );
    assert!(
        ask(&alice, "chain-tail", &bob, " -1")
            .await?
            .contains("n >= 0")
    );
    assert!(
        alice
            .evaluate("(chain-tail 1)")
            .await?
            .starts_with("error:")
    );

    runtime.shutdown().await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn the_ring_is_bounded_and_counts_what_it_dropped() -> anyhow::Result<()> {
    let (runtime, alice, bob) = two_sessions().await?;
    ask(&alice, "chain-watch", &bob, "").await?;
    let evaluations = RING / 2 + 10;
    for i in 0..evaluations {
        bob.evaluate(&i.to_string()).await?;
    }
    let count = 2 * evaluations;
    let settled = format!("(40 {count} true)");
    assert_eq!(head_becomes(&alice, &bob, &settled).await?, settled);
    assert_eq!(
        ask(&alice, "chain-verify", &bob, "").await?,
        format!(
            "(ok chain-check (tuple) (chain-check true {count} {RING} {} 0 false))",
            count - RING
        )
    );
    // The printer shortens long lists, so count in Wisp.
    let kept = alice
        .evaluate(&format!(
            "(match (chain-tail \"{}\" 100000) ((ok xs) (list-len xs)) ((err e) e))",
            bob.id
        ))
        .await?;
    assert_eq!(kept, RING.to_string());
    runtime.shutdown().await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn recording_stops_with_the_actor_and_its_ring_stays_readable() -> anyhow::Result<()> {
    let (runtime, alice, bob) = two_sessions().await?;
    ask(&alice, "chain-watch", &bob, "").await?;
    bob.evaluate("1").await?;
    bob.stop().await?;
    // The terminated event is the last one recorded; after it, nothing more.
    assert_eq!(
        head_becomes(&alice, &bob, "(40 3 false)").await?,
        "(40 3 false)"
    );
    assert!(
        ask(&alice, "chain-tail", &bob, " 1")
            .await?
            .contains("\"terminated\"")
    );
    // A stopped actor can't be watched again.
    assert!(
        ask(&alice, "chain-watch", &bob, "")
            .await?
            .contains("is not running")
    );
    assert!(
        ask(&alice, "chain-head", &bob, "")
            .await?
            .contains("not watching")
    );
    runtime.shutdown().await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_session_can_record_itself_and_unwatch() -> anyhow::Result<()> {
    let (runtime, alice, _bob) = two_sessions().await?;
    assert_eq!(
        alice.evaluate("(chain-watch (self))").await?,
        "(ok (tuple) (tuple) ())"
    );
    // Its own reads land on its own chain without blocking it.
    for _ in 0..5 {
        alice.evaluate("(chain-tail (self) 3)").await?;
    }
    let verified = alice.evaluate("(chain-verify (self))").await?;
    assert!(verified.contains("(chain-check true "), "{verified}");
    assert_eq!(
        alice.evaluate("(chain-unwatch (self))").await?,
        "(ok (tuple) (tuple) ())"
    );
    assert!(
        alice
            .evaluate("(chain-head (self))")
            .await?
            .contains("not watching")
    );
    assert!(
        alice
            .evaluate("(chain-unwatch (self))")
            .await?
            .contains("not watching")
    );
    runtime.shutdown().await
}
