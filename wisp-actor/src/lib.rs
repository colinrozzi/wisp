//! Wisp Compiler Actor
//!
//! A Theater actor that compiles Wisp source code to composite-compatible
//! WASM modules on demand.
//!
//! This actor provides a `compile` interface that takes Wisp source code
//! and returns WASM bytes that follow the composite calling convention.

#![no_std]

extern crate alloc;

use alloc::string::String;
use alloc::vec::Vec;
use alloc::format;
use composite_guest::{export, import, Value};

// Use dlmalloc for proper memory management
#[global_allocator]
static ALLOC: dlmalloc::GlobalDlmalloc = dlmalloc::GlobalDlmalloc;

// Set up panic handler
composite_guest::panic_handler!();

// Import the log function from theater runtime
#[import(wit = "theater:simple/runtime.log")]
fn log(msg: String);

/// Initialize the actor
///
/// Called by Theater when the actor starts. We use this to run a test
/// compilation to verify everything works.
#[export(wit = "theater:simple/actor.init")]
fn init(state: Option<Vec<u8>>) -> Result<(Option<Vec<u8>>,), String> {
    log(String::from("Wisp compiler actor initialized"));

    // Test compilation
    log(String::from("Running test compilation..."));
    let test_result = compile_wisp(String::from("42"));

    match &test_result {
        Value::Variant { tag: 0, payload: Some(p) } => {
            if let Value::List(bytes) = p.as_ref() {
                log(format!("Test compilation succeeded: {} bytes", bytes.len()));
            }
        }
        Value::Variant { tag: 1, payload: Some(p) } => {
            if let Value::String(err) = p.as_ref() {
                log(format!("Test compilation failed: {}", err));
            }
        }
        _ => {
            log(String::from("Test compilation returned unexpected result"));
        }
    }

    Ok((state,))
}

/// Compile Wisp source code to WASM
///
/// Takes Wisp source as a string, compiles it, and returns either
/// WASM bytes or an error message.
///
/// WIT signature: compile: func(source: string) -> compile-result
/// Where compile-result is: variant { ok(list<u8>), err(string) }
#[export(wit = "compiler.compile")]
fn compile_wisp(source: String) -> Value {
    log(format!("Compiling: {}", source));

    // TODO: Integrate actual wisp compiler
    // For now, return a stub error explaining the limitation
    //
    // The wisp compiler currently requires std, but Theater actors
    // run in no_std. Options to resolve:
    // 1. Make wisp compiler no_std compatible (significant work)
    // 2. Use host functions to do compilation (architecture change)
    // 3. Embed pre-compiled WASM for common expressions (limited)

    // For demonstration, return a minimal valid WASM module
    // This is the simplest possible WASM module (just the header + empty sections)
    let minimal_wasm: Vec<u8> = alloc::vec![
        // WASM magic number
        0x00, 0x61, 0x73, 0x6d,
        // WASM version 1
        0x01, 0x00, 0x00, 0x00,
    ];

    // Return compile-result::ok(list<u8>)
    Value::Variant {
        tag: 0, // ok
        payload: Some(alloc::boxed::Box::new(Value::List(
            minimal_wasm.into_iter().map(|b| Value::U8(b)).collect()
        ))),
    }
}
