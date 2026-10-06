//! A small ABI adapter until Wisp can express Pack's dynamic `value` init
//! parameter. Evaluation still runs entirely in the compiled Wisp evaluator.
use anyhow::{Context, Result, ensure};
use pack::abi::{Value, ValueType};
use pack::types::{Arena, Function, Param, Type};
use std::fmt::Write;
use std::path::Path;

pub const EVALUATE: &str = "theater:simple/wisp.evaluate";
pub const INIT: &str = "theater:simple/actor.init";
const METADATA_OFFSET: usize = 0xa000;

fn interface(name: &str, functions: Vec<Function>) -> Arena {
    let mut arena = Arena::new(name);
    for function in functions {
        arena.add_function(function);
    }
    arena
}

fn metadata() -> Result<Vec<u8>> {
    let mut package = Arena::new("package");
    let mut imports = Arena::new("imports");
    let result = Type::Result {
        ok: Box::new(Type::String),
        err: Box::new(Type::String),
    };
    imports.add_child(interface(
        "wisp-source",
        vec![
            Function::with_signature(
                "resolve-path",
                vec![
                    Param::new("base", Type::String),
                    Param::new("path", Type::String),
                ],
                vec![result.clone()],
            ),
            Function::with_signature(
                "read-source",
                vec![Param::new("path", Type::String)],
                vec![result],
            ),
        ],
    ));
    let mut exports = Arena::new("exports");
    exports.add_child(interface(
        "theater:simple/actor",
        vec![Function::with_signature(
            "init",
            vec![Param::new("config", Type::Value)],
            vec![Type::Result {
                ok: Box::new(Type::Tuple(vec![])),
                err: Box::new(Type::String),
            }],
        )],
    ));
    exports.add_child(interface(
        "theater:simple/wisp",
        vec![Function::with_signature(
            "evaluate",
            vec![Param::new("source", Type::String)],
            vec![Type::String],
        )],
    ));
    package.add_child(imports);
    package.add_child(exports);
    Ok(pack::metadata::encode_metadata_with_hashes(&package)?)
}

fn replace_once(text: &mut String, old: &str, new: &str) -> Result<()> {
    ensure!(
        text.matches(old).count() == 1,
        "compiler output changed: expected one {old:?}"
    );
    *text = text.replacen(old, new, 1);
    Ok(())
}

/// Build a self-contained evaluator module with Theater exports and metadata.
/// The checked WAT edits are deliberately confined to the adapter, not the
/// compiler or language. Fail closed if the compiler's layout changes.
pub fn build(output: &Path) -> Result<Vec<u8>> {
    std::fs::create_dir_all(output)?;
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let artifacts = wisp::compiler::compile(
        &root.join("actors/wisp-repl/actor.lisp"),
        &output.join("target/evaluator"),
        wisp::compiler::EmitOptions {
            wat: true,
            pact: false,
        },
    )?;
    let mut wat = std::fs::read_to_string(artifacts.wat.context("missing WAT")?)?;
    replace_once(
        &mut wat,
        "(export \"evaluate\")",
        &format!("(export \"{EVALUATE}\")"),
    )?;
    replace_once(&mut wat, "(export \"evaluate-from\")", "")?;
    // Bound each session's linear memory to 256 MiB; the bump heap is retained
    // until the actor is stopped. Start small and let __alloc grow it.
    replace_once(
        &mut wat,
        "(memory (export \"memory\") 32000 32000)",
        "(memory (export \"memory\") 16 4096)",
    )?;

    let metadata = metadata()?;
    ensure!(
        metadata.len() <= 0x2000,
        "actor metadata exceeds compiler reservation"
    );
    let start = wat
        .find(&format!("  (data (i32.const {METADATA_OFFSET})"))
        .context("missing metadata segment")?;
    // Replace only the data segment and __pack_types, ending before the heap.
    let heap = wat[start..]
        .find("  (global $__heap_ptr")
        .context("missing heap marker")?
        + start;
    let encoded: String = metadata.iter().map(|b| format!("\\{b:02x}")).collect();
    wat.replace_range(start..heap, &format!(
        "  (data (i32.const {METADATA_OFFSET}) \"{encoded}\")\n  (func (export \"__pack_types\") (param i32 i32) (result i32)\n    local.get 0 i32.const {METADATA_OFFSET} i32.store\n    local.get 1 i32.const {} i32.store i32.const 0)\n", metadata.len()));

    // Init intentionally ignores config: a fresh module is an empty session.
    // Return the real CGRF result<unit,string>, not a made-up string sentinel.
    let success = pack::abi::encode(&Value::Result {
        ok_type: ValueType::Tuple(vec![]),
        err_type: ValueType::String,
        value: Ok(Box::new(Value::Tuple(vec![]))),
    })?;
    let mut init = format!(
        "\n  (func (export \"{INIT}\") (param i32 i32 i32 i32) (result i32) (local $out i32)\n    i32.const {} call $__alloc local.set $out\n",
        success.len()
    );
    for (offset, byte) in success.iter().enumerate() {
        writeln!(
            init,
            "    local.get $out i32.const {byte} i32.store8 offset={offset}"
        )?;
    }
    writeln!(
        init,
        "    local.get 2 local.get $out i32.store\n    local.get 3 i32.const {} i32.store\n    i32.const 0)",
        success.len()
    )?;
    let close = wat.rfind(')').context("missing module end")?;
    wat.insert_str(close, &init);
    let wasm = wat::parse_str(&wat)?;
    std::fs::write(output.join("actor.wat"), wat)?;
    std::fs::write(output.join("actor.wasm"), &wasm)?;
    // Seed a new output directory without overwriting an edited manifest/bundle.
    for (name, content) in [
        ("manifest.toml", crate::MANIFEST),
        ("wisp.pact", include_str!("../wisp.pact")),
        ("source.pact", include_str!("../source.pact")),
        ("sources.json", include_str!("../sources.json")),
    ] {
        let path = output.join(name);
        if !path.exists() {
            std::fs::write(path, content)?;
        }
    }
    Ok(wasm)
}
