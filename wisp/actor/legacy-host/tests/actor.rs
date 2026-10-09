use std::collections::BTreeMap;
use std::path::Path;
use theater::pack_bridge::{Value, ValueType};
use wisp_interpreter_actor::{Runtime, adapter, source::SourceBundle};
mod support;

#[tokio::test]
async fn test_actor_session_imports_and_recovery() -> anyhow::Result<()> {
    let output = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/actor-test");
    let wasm = adapter::build(&output)?;
    let bundle = SourceBundle::new(BTreeMap::from([
        (
            "lib/main.wisp".into(),
            "(include \"increment.wisp\")".into(),
        ),
        (
            "lib/increment.wisp".into(),
            "(fn increment ((x s32)) s32 (i32.add x 1))".into(),
        ),
    ]))?;
    let runtime = Runtime::new(bundle).await?;
    let first = runtime.spawn(wasm.clone()).await?;
    assert_eq!(
        first
            .evaluate("(define add-two (lambda (x) (+ x 2)))")
            .await?,
        "#<closure>"
    );
    assert_eq!(first.evaluate("(add-two 40)").await?, "42");
    assert_eq!(
        first
            .evaluate("(include \"lib/main.wisp\") (increment 41)")
            .await?,
        "42"
    );
    assert!(
        first
            .evaluate("(define marker 0) (include \"missing.wisp\")")
            .await?
            .starts_with("error:")
    );
    assert!(first.evaluate("marker").await?.contains("unbound"));
    assert!(
        first
            .evaluate("(include \"../outside.wisp\")")
            .await?
            .contains("escapes bundle")
    );
    assert!(first.evaluate("(/ 1 0)").await?.starts_with("error:"));
    assert!(
        first
            .evaluate("(define spin (lambda () (spin))) (spin)")
            .await?
            .starts_with("error:")
    );
    // Exercise the guest limit too, bypassing Session's host-side size check.
    let too_large = first
        .handle
        .call_function(adapter::EVALUATE.into(), Value::String(" ".repeat(4097)))
        .await?;
    assert!(matches!(too_large, Value::String(message) if message.starts_with("error:")));
    assert_eq!(first.evaluate("(add-two 40)").await?, "42");
    let second = runtime.spawn(wasm).await?;
    assert!(second.evaluate("(add-two 40)").await?.contains("unbound"));
    assert_eq!(first.evaluate("(increment 41)").await?, "42");
    // Discovery must use the actual export interface and function signature.
    let hashes = first.handle.get_export_hashes().await?;
    assert!(hashes.iter().any(|hash| hash.name == "theater:simple/wisp"));

    let caller = runtime.spawn(support::rpc_guest()).await?;
    for (source, expected) in [
        ("(define triple (lambda (x) (* x 3)))", "#<closure>"),
        ("(triple 14)", "42"),
        ("(add-two 40)", "42"),
    ] {
        let result = caller
            .handle
            .call_function(
                "test:rpc/relay.call".into(),
                Value::Tuple(vec![
                    Value::String(first.id.to_string()),
                    Value::String(adapter::EVALUATE.into()),
                    Value::String(source.into()),
                    Value::Option {
                        inner_type: ValueType::String,
                        value: None,
                    },
                ]),
            )
            .await?;
        // PackInstance unwraps the relay export's outer result at the host boundary.
        assert_eq!(result, Value::String(expected.into()));
    }
    let description = caller
        .handle
        .call_function(
            "test:rpc/relay.describe".into(),
            Value::String(first.id.to_string()),
        )
        .await?;
    let functions = support::items(support::field(&description, "exports"));
    let evaluation = functions
        .iter()
        .find(|function| {
            support::field(function, "name") == &Value::String(adapter::EVALUATE.into())
        })
        .expect("discoverable evaluation function");
    assert_eq!(
        support::items(support::field(evaluation, "params")).len(),
        1
    );
    assert_eq!(
        support::items(support::field(evaluation, "results")).len(),
        1
    );
    let param = &support::items(support::field(evaluation, "params"))[0];
    for ty in [
        support::field(param, "type"),
        &support::items(support::field(evaluation, "results"))[0],
    ] {
        assert!(
            matches!(ty, Value::Variant { case_name, payload, .. } if case_name == "scalar" && payload == &[Value::String("string".into())])
        );
    }
    assert_eq!(first.evaluate("(triple 14)").await?, "42");
    runtime.shutdown().await?;
    Ok(())
}
