use std::path::Path;
use tokio::{
    io::{AsyncWriteExt, BufReader},
    net::{TcpListener, TcpStream},
};
use wisp_interpreter_actor::{Runtime, adapter, source::SourceBundle, transport};

#[tokio::test]
async fn test_socket_framing_session_and_disconnect() -> anyhow::Result<()> {
    let output = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/socket-test");
    let wasm = adapter::build(&output)?;
    let runtime = Runtime::new(SourceBundle::default()).await?;
    // Mirror serve's resident actor so the node survives its last disconnect.
    let _listener_actor = runtime.spawn(wasm.clone()).await?;
    let listener = TcpListener::bind("127.0.0.1:0").await?;
    let mut client = BufReader::new(TcpStream::connect(listener.local_addr()?).await?);
    let (socket, _) = listener.accept().await?;
    let session = runtime.spawn(wasm.clone()).await?;
    let handle = session.handle.clone();
    let server = tokio::spawn(transport::connection(socket, session));
    // Fragmented first frame and coalesced subsequent frames, including a
    // multiline expression and an escaped quote in a Wisp string.
    client.get_mut().write_all(b"\"(define add").await?;
    client.get_mut().write_all(b"-two (lambda (x) (+ x 2)))\"\n\"(add-two\\n 40)\"\nnot-json\n\"(add-two 40)\"\n\"\\\"hello\\\"\"\n").await?;
    for expected in [
        "#<closure>",
        "42",
        "error: expected a JSON string",
        "42",
        "\"hello\"",
    ] {
        let line = transport::read_line(&mut client).await?.unwrap();
        assert_eq!(serde_json::from_str::<String>(&line)?, expected);
    }
    client.get_mut().shutdown().await?;
    server.await??;
    assert!(
        handle
            .call_function(
                adapter::EVALUATE.into(),
                theater::pack_bridge::Value::String("1".into())
            )
            .await
            .is_err()
    );
    let next = runtime.spawn(wasm).await?;
    assert!(next.evaluate("(add-two 40)").await?.contains("unbound"));
    assert_eq!(next.evaluate("(+ 40 2)").await?, "42");
    runtime.shutdown().await?;
    Ok(())
}

#[tokio::test]
async fn test_frame_size_is_bounded() {
    let bytes = vec![b'x'; 32769];
    let mut reader = BufReader::new(bytes.as_slice());
    assert!(
        transport::read_line(&mut reader)
            .await
            .unwrap_err()
            .to_string()
            .contains("exceeds")
    );
}
