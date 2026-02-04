//! Pack composition with host functions
//!
//! This module documents the full Pack composition approach used by theater-repl.
//!
//! # Architecture
//!
//! The REPL uses Pack's `CompositionBuilder` to create a composition where:
//!
//! 1. **WASM-to-WASM linking**: The repl-actor can call the compiler's
//!    `compile-source` function directly, with no host bridge.
//!
//! 2. **Host functions**: The host provides functions that WASM cannot implement:
//!    - `theater:simple/runtime.log` - logging
//!    - `wisp:assembler/runtime.wat-to-wasm` - WAT to WASM assembly
//!    - `wisp:repl/helpers.*` - expression wrapping helpers
//!    - `theater:simple/supervisor.spawn-with-wasm` - actor spawning
//!
//! # Example
//!
//! ```ignore
//! use pack::runtime::CompositionBuilder;
//!
//! let composition = CompositionBuilder::new()
//!     // Add packages
//!     .add_package("compiler", compiler_wasm)
//!     .add_package("repl", repl_wasm)
//!     // Wire WASM-to-WASM calls
//!     .wire("repl", "wisp:compiler/compiler", "compile-source",
//!           "compiler", "compile-source")
//!     // Add host functions
//!     .add_host_function_typed("theater:simple/runtime", "log", |input| {
//!         println!("{:?}", input);
//!         Ok(Value::Tuple(vec![]))
//!     })
//!     .build()?;
//!
//! // Call functions on the composition
//! let result = composition.call("repl", "init", &input)?;
//! ```
//!
//! # Benefits
//!
//! - **True composition**: No serialization overhead between WASM modules
//! - **Flexible host functions**: Host can provide any function the WASM needs
//! - **Single runtime**: All packages share the same wasmtime engine
//! - **Clean API**: Builder pattern makes composition declarative
