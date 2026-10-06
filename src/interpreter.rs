//! Local host for the interpreter written in `interpreter/evaluator.lisp`.
//! One instance owns one session; evaluation itself runs entirely in Wisp/Wasm.

use std::path::Path;

use anyhow::{Context, Result, bail, ensure};
use pack::abi::Value;
use wasmtime::{Config, Engine, Instance, Memory, Module, Store, TypedFunc};

pub struct Interpreter {
    store: Store<()>,
    memory: Memory,
    alloc: TypedFunc<i32, i32>,
    evaluate: TypedFunc<(i32, i32, i32, i32), i32>,
}

impl Interpreter {
    /// Load an already compiled interpreter. Reuse this instance across inputs.
    pub fn load(path: impl AsRef<Path>) -> Result<Self> {
        let mut config = Config::new();
        config.wasm_tail_call(true);
        // Backstop for reader/printer work as well as evaluator execution.
        config.consume_fuel(true);
        let engine = Engine::new(&config)?;
        let module = Module::from_file(&engine, path)?;
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[])?;
        Ok(Self {
            memory: instance
                .get_memory(&mut store, "memory")
                .context("interpreter has no memory")?,
            alloc: instance.get_typed_func(&mut store, "__pack_alloc")?,
            evaluate: instance.get_typed_func(&mut store, "evaluate")?,
            store,
        })
    }

    /// Return the printed value or reader/evaluator diagnostic for an input.
    /// Host/Wasm failures are returned as errors. Earlier definitions survive.
    pub fn evaluate(&mut self, source: &str) -> Result<String> {
        // Check before copying arbitrarily large input into the guest heap.
        if source.len() > 4096 {
            return Ok("error: input exceeds 4096 bytes".into());
        }
        self.store.set_fuel(20_000_000)?;
        let input = pack::encode(&Value::String(source.into()))?;
        let ptr = self.alloc.call(&mut self.store, input.len() as i32)?;
        let slots = self.alloc.call(&mut self.store, 8)?;
        self.memory.write(&mut self.store, ptr as usize, &input)?;
        let status = self
            .evaluate
            .call(&mut self.store, (ptr, input.len() as i32, slots, slots + 4))?;
        ensure!(status == 0, "interpreter ABI returned status {status}");
        let mut pointer_and_length = [0; 8];
        self.memory
            .read(&self.store, slots as usize, &mut pointer_and_length)?;
        let ptr = u32::from_le_bytes(pointer_and_length[..4].try_into()?) as usize;
        let len = u32::from_le_bytes(pointer_and_length[4..].try_into()?) as usize;
        let end = ptr.checked_add(len).context("invalid output length")?;
        let bytes = self
            .memory
            .data(&self.store)
            .get(ptr..end)
            .context("interpreter output outside memory")?;
        match pack::decode(bytes)? {
            Value::String(text) => Ok(text),
            _ => bail!("interpreter returned a non-string value"),
        }
    }
}
