//! Local development transport: one JSON string per line, one actor per socket.
use crate::Session;
use anyhow::{Result, bail};
use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;

// Allows JSON escapes for all 4096 source bytes, with bounded framing overhead.
const MAX_FRAME: usize = 32768;

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
            bail!("input frame exceeds 32768 bytes");
        }
        let complete = buffer[count - 1] == b'\n';
        line.extend_from_slice(&buffer[..count]);
        reader.consume(count);
        if complete {
            return Ok(Some(String::from_utf8(line)?));
        }
    }
}

pub async fn connection(stream: TcpStream, session: Session) -> Result<()> {
    let (input, mut output) = stream.into_split();
    let result = async {
        let mut input = BufReader::new(input);
        while let Some(line) = read_line(&mut input).await? {
            let result = match serde_json::from_str::<String>(&line) {
                Ok(source) => session.evaluate(&source).await?,
                Err(_) => "error: expected a JSON string".into(),
            };
            output
                .write_all(serde_json::to_string(&result)?.as_bytes())
                .await?;
            output.write_all(b"\n").await?;
        }
        Ok(())
    }
    .await;
    let stopped = session.shutdown().await;
    result.and(stopped)
}
