//! `inspect`: what a session is running and how it is doing. Everything but the
//! heap and steps comes from the daemon itself (its notes and the chain-fed
//! [`health`] table), so a session that is busy or stuck still reports. The
//! heap and steps are the session's own `(memory-stats)`, asked with a short
//! deadline: a busy session shows `busy` there instead.
//!
//! Counts, names and hashes only: no message payloads, no file contents.
use crate::{Daemon, VERSION};
use std::fmt::Write;
use std::time::{Duration, Instant};
use theater::pack_bridge::Value;
use wisp_interpreter_actor::{Interpreter, health, resolve, short_id};

/// How long the session has to answer `(memory-stats)`.
const ASK: Duration = Duration::from_secs(1);

/// `inspect ID`: one session in full.
pub async fn session(daemon: &Daemon, target: &str) -> Result<String, String> {
    let interpreters = daemon
        .runtime
        .interpreters()
        .await
        .map_err(|e| format!("error: {e:#}"))?;
    let interpreter = resolve(&interpreters, target).map_err(|e| e.to_string())?;
    let now = Instant::now();
    let note = daemon.runtime.sessions.noted(&interpreter.id);
    let actor = daemon
        .runtime
        .health
        .get(&interpreter.id)
        .unwrap_or_default();
    let mut out = String::new();
    let _ = writeln!(
        out,
        "session {}\n  name:     {}\n  actor:    {}\n  version:  theater-repl {VERSION}\n  daemon:   up {}",
        short_id(&interpreters, &interpreter.id),
        interpreter.name,
        interpreter.id,
        ago(daemon.started, now),
    );
    match &note {
        Some(note) => {
            let _ = writeln!(
                out,
                "  age:      {}\n  source:   {}",
                ago(note.since, now),
                note.source
            );
            match &note.start {
                Some((path, sha256)) => {
                    let _ = writeln!(out, "  start:    {path} sha256 {sha256}");
                }
                None => out.push_str("  start:    -\n"),
            }
        }
        None => out.push_str("  age:      -\n  source:   -\n  start:    -\n"),
    }
    let _ = writeln!(out, "  busy:     {}", busy(&actor, now));
    let (min_in, min_out) = daemon.runtime.health.per_minute(&actor);
    let _ = writeln!(
        out,
        "  calls:    {}\n  messages: in {min_in}/min ({} total), out {min_out}/min ({} total)",
        actor.calls, actor.msgs_in, actor.msgs_out,
    );
    match &actor.last_error {
        Some((error, at)) => {
            let _ = writeln!(
                out,
                "  errors:   {}, last {} ago: {error}",
                actor.errors,
                ago(*at, now)
            );
        }
        None => {
            let _ = writeln!(out, "  errors:   {}", actor.errors);
        }
    }
    let _ = writeln!(out, "  timers:   {}", handles(&actor, "timer"));
    let _ = writeln!(
        out,
        "  sockets:  {} listening, {} connected",
        count(&actor, "listener"),
        count(&actor, "connection"),
    );
    out.push_str(&memory(daemon, interpreter).await);
    let _ = write!(out, "  files:    {}", actor.files.len());
    for (path, file) in &actor.files {
        let _ = write!(
            out,
            "\n    {path}  sha256 {}  {} bytes  read {} ago",
            file.sha256,
            file.bytes,
            ago(file.at, now),
        );
    }
    Ok(out)
}

/// `inspect` with no id: the daemon, and one line per session, from the
/// daemon's side only (no session is asked anything).
pub async fn all(daemon: &Daemon) -> Result<String, String> {
    let interpreters = daemon
        .runtime
        .sessions
        .list()
        .await
        .map_err(|e| format!("error: {e:#}"))?;
    let now = Instant::now();
    let mut out = format!(
        "theater-repl {VERSION}, daemon up {}, {} session(s)\n{:<9} {:<16} {:>6} {:>8} {:>7} {:>7} {:>6} {:>6} {:>7} {:>5}  BUSY",
        ago(daemon.started, now),
        interpreters.len(),
        "ID",
        "NAME",
        "AGE",
        "CALLS",
        "IN/MIN",
        "OUT/MIN",
        "ERRORS",
        "TIMERS",
        "SOCKETS",
        "FILES",
    );
    for interpreter in &interpreters {
        let actor = daemon
            .runtime
            .health
            .get(&interpreter.id)
            .unwrap_or_default();
        let (min_in, min_out) = daemon.runtime.health.per_minute(&actor);
        let age = daemon
            .runtime
            .sessions
            .noted(&interpreter.id)
            .map_or("-".into(), |note| ago(note.since, now));
        let _ = write!(
            out,
            "\n{:<9} {:<16} {:>6} {:>8} {:>7} {:>7} {:>6} {:>6} {:>7} {:>5}  {}",
            short_id(&interpreters, &interpreter.id),
            interpreter.name,
            age,
            actor.calls,
            min_in,
            min_out,
            actor.errors,
            count(&actor, "timer"),
            count(&actor, "listener") + count(&actor, "connection"),
            actor.files.len(),
            busy(&actor, now),
        );
    }
    Ok(out)
}

/// What `chain-tail --kind` takes, and the event-type prefixes each covers.
pub const KINDS: &[(&str, &[&str])] = &[
    ("fs", &["theater:simple/filesystem/"]),
    ("tcp", &["theater:simple/tcp/"]),
    (
        "http",
        &[
            "theater:simple/http-client/",
            "theater:simple/http-framework/",
        ],
    ),
    ("timer", &["theater:simple/timer/"]),
    ("msg", &["theater:simple/message-server-host/"]),
    ("process", &["theater:simple/process/"]),
];

/// `chain-tail ID [-n N] [--kind K]`: the session's newest host calls, with
/// their arguments and results in short: a string is cut at
/// [`STRING_CHARS`], bytes show only their count, so no file contents or
/// message payloads.
pub async fn chain_tail(
    daemon: &Daemon,
    target: &str,
    n: usize,
    kind: Option<&str>,
) -> Result<String, String> {
    let prefixes: Option<&[&str]> = match kind {
        None | Some("all") => None,
        Some(kind) => match KINDS.iter().find(|(name, _)| *name == kind) {
            Some((_, prefixes)) => Some(prefixes),
            None => {
                let names: Vec<&str> = KINDS.iter().map(|(name, _)| *name).collect();
                return Err(format!(
                    "error: no kind {kind}; one of all, {}",
                    names.join(", ")
                ));
            }
        },
    };
    let interpreters = daemon
        .runtime
        .interpreters()
        .await
        .map_err(|e| format!("error: {e:#}"))?;
    let interpreter = resolve(&interpreters, target).map_err(|e| e.to_string())?;
    let actor = daemon
        .runtime
        .health
        .get(&interpreter.id)
        .unwrap_or_default();
    let now = Instant::now();
    let wanted: Vec<&health::HostCall> = actor
        .tail
        .iter()
        .filter(|call| prefixes.is_none_or(|ps| ps.iter().any(|p| call.event_type.starts_with(p))))
        .collect();
    let mut lines = Vec::new();
    for call in &wanted[wanted.len().saturating_sub(n)..] {
        let shown = match call.decode() {
            Some((input, output)) => format!("{} -> {}", args(&input), short(&output)),
            None if call.data.is_none() => format!("({} bytes, too large to keep)", call.bytes),
            None => "(not decodable)".into(),
        };
        lines.push(format!(
            "{:>6} ago  {}{shown}",
            ago(call.at, now),
            call.event_type
        ));
    }
    if lines.is_empty() {
        return Ok(format!(
            "no host calls{} kept for {}",
            kind.map_or(String::new(), |k| format!(" of kind {k}")),
            interpreter.name
        ));
    }
    Ok(lines.join("\n"))
}

/// Characters of a string `chain-tail` shows.
const STRING_CHARS: usize = 60;
/// Items of a list `chain-tail` shows.
const ITEMS: usize = 6;

/// A host call's input, a tuple of its parameters, as `(a, b)`.
fn args(input: &Value) -> String {
    match input {
        Value::Tuple(items) => format!("({})", list(items)),
        other => format!("({})", short(other)),
    }
}

fn list(items: &[Value]) -> String {
    let mut shown: Vec<String> = items.iter().take(ITEMS).map(short).collect();
    if items.len() > ITEMS {
        shown.push(format!("… {} more", items.len() - ITEMS));
    }
    shown.join(", ")
}

/// A value in short: strings cut, bytes counted, lists capped.
fn short(v: &Value) -> String {
    match v {
        Value::String(s) if s.chars().count() > STRING_CHARS => {
            format!("{:?}…", s.chars().take(STRING_CHARS).collect::<String>())
        }
        Value::String(s) => format!("{s:?}"),
        Value::List { items, .. }
            if items.iter().all(|i| matches!(i, Value::U8(_))) && !items.is_empty() =>
        {
            format!("<{} bytes>", items.len())
        }
        Value::List { items, .. } => format!("[{}]", list(items)),
        Value::Tuple(items) => format!("({})", list(items)),
        Value::Option { value: None, .. } => "none".into(),
        Value::Option { value: Some(v), .. } => format!("some({})", short(v)),
        Value::Result { value: Ok(v), .. } => format!("ok({})", short(v)),
        Value::Result { value: Err(v), .. } => format!("err({})", short(v)),
        Value::Record { fields, .. } => {
            let shown: Vec<String> = fields
                .iter()
                .take(ITEMS)
                .map(|(name, v)| format!("{name}: {}", short(v)))
                .collect();
            format!("{{{}}}", shown.join(", "))
        }
        Value::Variant {
            case_name, payload, ..
        } if payload.is_empty() => case_name.clone(),
        Value::Variant {
            case_name, payload, ..
        } => format!("{case_name}({})", list(payload)),
        Value::Bool(b) => b.to_string(),
        Value::U8(n) => n.to_string(),
        Value::U16(n) => n.to_string(),
        Value::U32(n) => n.to_string(),
        Value::U64(n) => n.to_string(),
        Value::S8(n) => n.to_string(),
        Value::S16(n) => n.to_string(),
        Value::S32(n) => n.to_string(),
        Value::S64(n) => n.to_string(),
        Value::F32(n) => n.to_string(),
        Value::F64(n) => n.to_string(),
        Value::Char(c) => format!("{c:?}"),
        _ => "…".into(),
    }
}

/// The heap and steps lines, from the session's `(memory-stats)`.
async fn memory(daemon: &Daemon, interpreter: &Interpreter) -> String {
    let asked = async {
        let session = daemon.runtime.attach(interpreter.id).await.ok()?;
        session.evaluate("(memory-stats)").await.ok()
    };
    let Ok(Some(printed)) = tokio::time::timeout(ASK, asked).await else {
        return format!(
            "  heap:     busy (no answer in {}s)\n  steps:    busy\n",
            ASK.as_secs()
        );
    };
    let stat = |name: &str| stat(&printed, name).map_or("?".into(), |n| n.to_string());
    format!(
        "  heap:     {} bytes in use, {} pages of 64 KiB\n  steps:    most {} of limit {}\n",
        stat("heap-bytes"),
        stat("heap-pages"),
        stat("steps-most"),
        stat("step-limit"),
    )
}

/// The count after `("NAME" ` in a printed `(memory-stats)`.
fn stat(printed: &str, name: &str) -> Option<i64> {
    let key = format!("(\"{name}\" ");
    let rest = &printed[printed.find(&key)? + key.len()..];
    rest[..rest.find(')')?].trim().parse().ok()
}

fn busy(actor: &health::Actor, now: Instant) -> String {
    match &actor.busy {
        Some((function, since)) => format!("{} in {function}", ago(*since, now)),
        None => "no".into(),
    }
}

fn count(actor: &health::Actor, kind: &str) -> usize {
    actor.handles.get(kind).map_or(0, |ids| ids.len())
}

/// A handle kind's count, and its ids.
fn handles(actor: &health::Actor, kind: &str) -> String {
    match actor.handles.get(kind) {
        Some(ids) if !ids.is_empty() => {
            let ids: Vec<&str> = ids.iter().map(String::as_str).collect();
            format!("{} ({})", ids.len(), ids.join(", "))
        }
        _ => "0".into(),
    }
}

/// How long ago `then` was: `42s`, `5m03s`, `2h10m`, `3d04h`.
fn ago(then: Instant, now: Instant) -> String {
    let s = now.saturating_duration_since(then).as_secs();
    match s {
        0..60 => format!("{s}s"),
        60..3600 => format!("{}m{:02}s", s / 60, s % 60),
        3600..86400 => format!("{}h{:02}m", s / 3600, s % 3600 / 60),
        _ => format!("{}d{:02}h", s / 86400, s % 86400 / 3600),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stat_reads_a_count() {
        let printed =
            r#"(("heap-bytes" 1234) ("heap-pages" 20) ("steps-most" 9) ("step-limit" 1000000))"#;
        assert_eq!(stat(printed, "heap-bytes"), Some(1234));
        assert_eq!(stat(printed, "step-limit"), Some(1000000));
        assert_eq!(stat(printed, "nope"), None);
    }

    #[test]
    fn short_counts_bytes_and_cuts_strings() {
        let bytes = Value::List {
            elem_type: theater::pack_bridge::ValueType::U8,
            items: vec![Value::U8(1); 300],
        };
        let ok = Value::Result {
            ok_type: theater::pack_bridge::ValueType::U8,
            err_type: theater::pack_bridge::ValueType::String,
            value: Ok(Box::new(bytes)),
        };
        assert_eq!(short(&ok), "ok(<300 bytes>)");
        let long = Value::String("x".repeat(100));
        assert_eq!(short(&long), format!("{:?}…", "x".repeat(STRING_CHARS)));
        let input = Value::Tuple(vec![Value::String("m.wisp".into()), Value::U64(5)]);
        assert_eq!(args(&input), "(\"m.wisp\", 5)");
    }

    #[test]
    fn ago_reads_like_a_clock() {
        let now = Instant::now() + Duration::from_secs(200_000);
        let at = |s| now - Duration::from_secs(s);
        assert_eq!(ago(at(42), now), "42s");
        assert_eq!(ago(at(303), now), "5m03s");
        assert_eq!(ago(at(7800), now), "2h10m");
        assert_eq!(ago(at(100_000), now), "1d03h");
    }
}
