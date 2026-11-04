// Bad input from a stranger must give an error, not trap the session (ticket
// 0050): `(unmarshal bytes)` on malformed CGRF, and host verbs called with too few
// arguments. Driven through a real actor, like invalid_utf8.rs.

use std::path::Path;
use wisp_interpreter_actor::{Grants, Runtime, Session, source::SourceBundle};

async fn session() -> anyhow::Result<Session> {
    let wasm = std::fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))?;
    let runtime = Runtime::new(SourceBundle::default(), Grants::default()).await?;
    runtime.spawn(wasm).await
}

async fn assert_alive(session: &Session) -> anyhow::Result<()> {
    assert_eq!(session.evaluate("(+ 40 2)").await?, "42");
    Ok(())
}

fn u32le(n: u32) -> Vec<u8> {
    n.to_le_bytes().to_vec()
}

fn pstr(s: &str) -> Vec<u8> {
    let mut out = u32le(s.len() as u32);
    out.extend_from_slice(s.as_bytes());
    out
}

/// A `[len:u32][cgrf]` blob: the CGRF v3 header (`count` nodes, `root`), then
/// each (kind, payload) node.
fn blob(count: u32, root: u32, nodes: &[(u8, Vec<u8>)]) -> Vec<u8> {
    let mut cgrf = b"CGRF".to_vec();
    cgrf.extend_from_slice(&3u16.to_le_bytes());
    cgrf.extend_from_slice(&0u16.to_le_bytes());
    cgrf.extend(u32le(count));
    cgrf.extend(u32le(root));
    for (kind, payload) in nodes {
        cgrf.extend_from_slice(&[*kind, 0, 0, 0]);
        cgrf.extend(u32le(payload.len() as u32));
        cgrf.extend_from_slice(payload);
    }
    let mut out = u32le(cgrf.len() as u32);
    out.extend(cgrf);
    out
}

fn tree(nodes: &[(u8, Vec<u8>)]) -> Vec<u8> {
    blob(nodes.len() as u32, nodes.len() as u32 - 1, nodes)
}

fn indices(idxs: &[u32]) -> Vec<u8> {
    let mut out = u32le(idxs.len() as u32);
    for i in idxs {
        out.extend(u32le(*i));
    }
    out
}

/// One of every node kind the decoder reads, under a tuple root.
fn every_kind() -> Vec<(u8, Vec<u8>)> {
    let cat = |parts: &[Vec<u8>]| parts.concat();
    vec![
        (2, u32le(42)),                                         // 0 s32
        (6, pstr("hi")),                                        // 1 string
        (6, pstr("x")),                                         // 2 string
        (7, cat(&[vec![6], indices(&[2])])),                    // 3 list<string>
        (21, cat(&[vec![12], u32le(3), vec![1, 2, 3]])),        // 4 array<u8>
        (2, u32le(7)),                                          // 5 s32
        (10, cat(&[vec![2, 1], u32le(5)])),                     // 6 option<s32> some
        (6, pstr("e")),                                         // 7 string
        (20, cat(&[vec![2, 6], u32le(1), vec![1], u32le(7)])),  // 8 result err
        (2, u32le(1)),                                          // 9 s32
        (9, cat(&[pstr("pt"), u32le(1), pstr("x"), u32le(9)])), // 10 record
        (6, pstr("v")),                                         // 11 string
        (
            8,
            cat(&[pstr("shape"), pstr("c"), u32le(0), indices(&[11])]),
        ), // 12 variant
        (5, 2.5f64.to_le_bytes().to_vec()),                     // 13 f64
        (11, indices(&[0, 1, 3, 4, 6, 8, 10, 12, 13])),         // 14 tuple
    ]
}

fn unmarshal_form(bytes: &[u8]) -> String {
    let items: Vec<String> = bytes.iter().map(|b| b.to_string()).collect();
    // bytes->string takes integers; string->bytes gives the u8 list unmarshal reads.
    format!(
        "(unmarshal (string->bytes (bytes->string (list {}))))",
        items.join(" ")
    )
}

/// Unmarshal BYTES, sent in pieces when the form would pass the eval input limit.
async fn unmarshal(session: &Session, bytes: &[u8]) -> anyhow::Result<String> {
    if bytes.len() < 800 {
        return session.evaluate(&unmarshal_form(bytes)).await;
    }
    session.evaluate(r#"(define blob "")"#).await?;
    for chunk in bytes.chunks(800) {
        let items: Vec<String> = chunk.iter().map(|b| b.to_string()).collect();
        let form = format!(
            "(define blob (string-append blob (bytes->string (list {}))))",
            items.join(" ")
        );
        session.evaluate(&form).await?;
    }
    session.evaluate("(unmarshal (string->bytes blob))").await
}

async fn rejected(session: &Session, bytes: &[u8], why: &str) -> anyhow::Result<()> {
    let out = unmarshal(session, bytes).await?;
    assert!(
        out.starts_with("(err ") && out.contains(why),
        "{why}: {out}"
    );
    assert_alive(session).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_unmarshal_well_formed_bytes() -> anyhow::Result<()> {
    let session = session().await?;
    let out = unmarshal(&session, &tree(&every_kind())).await?;
    for part in [
        "42",
        "\"hi\"",
        "\"x\"",
        "#<u8 3>",
        "7",
        "\"e\"",
        "(pt 1)",
        "(c \"v\")",
        "2.5",
    ] {
        assert!(out.contains(part), "{part}: {out}");
    }
    assert!(!out.starts_with("(err "), "{out}");
    // The interpreter's own marshal reads back.
    assert_eq!(
        session
            .evaluate(r#"(unmarshal (marshal (list 1 "two" (list 3.5 true))))"#)
            .await?,
        r#"(1 "two" (3.5f64 true))"#
    );
    // Nesting up to the limit is fine.
    let mut nodes = vec![(2, u32le(1))];
    for i in 0..200 {
        nodes.push((11, indices(&[i])));
    }
    let out = unmarshal(&session, &tree(&nodes)).await?;
    assert!(out.starts_with("(((("), "{out}"); // the printer stops at depth 64
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_unmarshal_malformed_bytes_is_an_error() -> anyhow::Result<()> {
    let session = session().await?;
    rejected(&session, &[], "length prefix").await?;
    rejected(&session, &[1, 0], "length prefix").await?;
    rejected(&session, &[255, 255, 255, 127, 1, 2, 3], "length prefix").await?;
    rejected(&session, &[3, 0, 0, 0, 1, 2, 3], "past the end").await?;

    let mut bad_magic = tree(&[(2, u32le(1))]);
    bad_magic[4] = b'X';
    rejected(&session, &bad_magic, "not a CGRF").await?;

    // Header claims more nodes than are there, or a root that isn't one.
    rejected(
        &session,
        &blob(1_000_000, 0, &[(2, u32le(1))]),
        "past the end",
    )
    .await?;
    rejected(&session, &blob(1, 5, &[(2, u32le(1))]), "out of range").await?;
    rejected(
        &session,
        &blob(1, u32::MAX, &[(2, u32le(1))]),
        "out of range",
    )
    .await?;

    // A payload length past the buffer, and a scalar payload too short.
    let mut long = tree(&[(2, u32le(1))]);
    long[24..28].copy_from_slice(&u32le(u32::MAX));
    rejected(&session, &long, "past the end").await?;
    rejected(&session, &tree(&[(5, vec![0; 3])]), "past the end").await?;

    // Strings, counts and child indices that point outside.
    rejected(&session, &tree(&[(6, u32le(1 << 30))]), "past the end").await?;
    rejected(&session, &tree(&[(11, indices(&[9]))]), "out of range").await?;
    rejected(&session, &tree(&[(11, u32le(u32::MAX))]), "past the end").await?;
    rejected(
        &session,
        &tree(&[(21, [vec![12], u32le(1 << 30)].concat())]),
        "past the end",
    )
    .await?;
    rejected(
        &session,
        &tree(&[(9, [pstr("r"), u32le(1 << 30)].concat())]),
        "past the end",
    )
    .await?;
    rejected(
        &session,
        &tree(&[(8, [pstr("v"), u32le(99)].concat())]),
        "past the end",
    )
    .await?;
    rejected(
        &session,
        &tree(&[(10, vec![2, 1, 9, 0, 0, 0])]),
        "out of range",
    )
    .await?;

    // A cycle, a node used twice, and nesting too deep (nodes or type tags).
    rejected(&session, &blob(1, 0, &[(11, indices(&[0]))]), "").await?;
    rejected(
        &session,
        &tree(&[(2, u32le(1)), (11, indices(&[0, 0]))]),
        "more than once",
    )
    .await?;
    let mut nodes = vec![(2, u32le(1))];
    for i in 0..300 {
        nodes.push((11, indices(&[i])));
    }
    rejected(&session, &tree(&nodes), "too deep").await?;
    let tags = [vec![7u8; 1000], vec![2], u32le(0)].concat();
    rejected(&session, &tree(&[(7, tags)]), "too deep").await?;

    rejected(&session, &tree(&[(99, vec![])]), "unsupported").await?;
    Ok(())
}

// Every byte of a blob with every node kind, set to a few values that make a
// length or index huge, zero or off by one: the session must survive each.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_unmarshal_survives_every_corrupted_byte() -> anyhow::Result<()> {
    let session = session().await?;
    let good = tree(&every_kind());
    for i in 0..good.len() {
        for v in [0u8, 0x7f, 0xff, good[i].wrapping_add(1)] {
            let mut bytes = good.clone();
            bytes[i] = v;
            // A dead session makes this call fail, which ends the test here.
            session.evaluate(&unmarshal_form(&bytes)).await?;
        }
    }
    assert_alive(&session).await
}

// A verb that takes one string, called with none, reads no argument.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_verbs_with_too_few_arguments_are_errors() -> anyhow::Result<()> {
    let session = session().await?;
    for form in [
        "(tcp-close)",
        "(tcp-connect)",
        "(process-kill)",
        "(http-req \"GET\")",
    ] {
        let out = session.evaluate(form).await?;
        assert!(out.starts_with("error"), "{form} -> {out}");
        assert_alive(&session).await?;
    }
    Ok(())
}
