//! Daemon transport: one JSON request, one JSON response, per connection.
//!
//!   request:  {"op":"eval","id":"a1b2","form":"(…)"}   ops: new|eval|read|list|status|stop
//!   response: {"ok":true,"text":"…"}                   client prints text; ok=false → stderr, exit 1
//!
//! The daemon formats every result into `text`, so the client stays dumb: print it.
use anyhow::{Result, bail};
use serde_json::Value;
use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;

// A JSON-escaped 64 KiB form plus framing overhead fits comfortably.
const MAX_FRAME: usize = 1 << 20;

pub async fn read_line<R: AsyncBufRead + Unpin>(reader: &mut R) -> Result<Option<String>> {
    let mut line = Vec::new();
    loop {
        let buffer = reader.fill_buf().await?;
        if buffer.is_empty() {
            return if line.is_empty() {
                Ok(None)
            } else {
                Ok(Some(String::from_utf8(line)?))
            };
        }
        let count = buffer
            .iter()
            .position(|b| *b == b'\n')
            .map_or(buffer.len(), |i| i + 1);
        if line.len() + count > MAX_FRAME {
            bail!("input frame exceeds {MAX_FRAME} bytes");
        }
        let complete = buffer[count - 1] == b'\n';
        line.extend_from_slice(&buffer[..count]);
        reader.consume(count);
        if complete {
            return Ok(Some(String::from_utf8(line)?));
        }
    }
}

/// Client side: connect to the daemon, send one request, read one response.
pub async fn request(port: u16, req: Value) -> Result<(bool, String)> {
    let stream = TcpStream::connect(("127.0.0.1", port)).await.map_err(|error| {
        let hint = if port == 7777 {
            String::new()
        } else {
            format!(" -p {port}")
        };
        anyhow::anyhow!(
            "no theater-repl daemon on 127.0.0.1:{port} ({error})\nstart one with: theater-repl serve{hint}"
        )
    })?;
    let (read_half, mut write_half) = stream.into_split();
    let mut line = serde_json::to_string(&req)?;
    line.push('\n');
    write_half.write_all(line.as_bytes()).await?;
    let mut reader = BufReader::new(read_half);
    let Some(response) = read_line(&mut reader).await? else {
        bail!("daemon closed the connection without responding");
    };
    let value: Value = serde_json::from_str(&response)?;
    let ok = value.get("ok").and_then(Value::as_bool).unwrap_or(false);
    let text = value
        .get("text")
        .and_then(Value::as_str)
        .unwrap_or("")
        .to_string();
    Ok((ok, text))
}
