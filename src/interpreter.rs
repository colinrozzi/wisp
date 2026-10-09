//! Local host for the interpreter written in `wisp/interpreter/evaluator.wisp`.
//! One instance owns one session; evaluation itself runs entirely in Wisp/Wasm.

use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail, ensure};
use pack::abi::{Value, ValueType};
use wasmtime::{Caller, Config, Engine, Linker, Memory, Module, Store, TypedFunc};

const FILE_LIMIT: usize = 65536;
const SOURCE_LIMIT: usize = 1024 * 1024;

#[derive(Default)]
struct SourceBudget {
    bytes: usize,
    files: usize,
}

fn read_source(path: &Path) -> Result<String> {
    let file = File::open(path).with_context(|| format!("cannot open {}", path.display()))?;
    ensure!(
        file.metadata()?.is_file(),
        "not a regular source file: {}",
        path.display()
    );
    let mut bytes = Vec::new();
    file.take((FILE_LIMIT + 1) as u64).read_to_end(&mut bytes)?;
    ensure!(
        bytes.len() <= FILE_LIMIT,
        "source exceeds {FILE_LIMIT} bytes: {}",
        path.display()
    );
    String::from_utf8(bytes).with_context(|| format!("source is not UTF-8: {}", path.display()))
}

fn resolve_source(base: &str, path: &str) -> Result<PathBuf> {
    let parent = if base.is_empty() {
        Path::new(".")
    } else {
        Path::new(base)
            .parent()
            .context("source has no parent directory")?
    };
    let path = parent.join(path);
    path.canonicalize()
        .with_context(|| format!("cannot resolve {}", path.display()))
}

// Ordinary CGRF import returning Result<string, string>. Wisp performs all
// parsing, include deduplication, expansion, and evaluation.
fn source_import(
    caller: &mut Caller<'_, SourceBudget>,
    resolve: bool,
    ptr: i32,
    len: i32,
    out_ptr: i32,
    out_len: i32,
) -> Result<i32> {
    let memory = caller
        .get_export("memory")
        .and_then(|e| e.into_memory())
        .context("missing guest memory")?;
    ensure!(ptr >= 0 && len >= 0, "invalid source import buffer");
    let start = ptr as usize;
    let end = start
        .checked_add(len as usize)
        .context("source import buffer overflow")?;
    let input = pack::decode(
        memory
            .data(&*caller)
            .get(start..end)
            .context("source import buffer outside memory")?,
    )?;
    let result: Result<String> = (|| {
        if resolve {
            let Value::Tuple(args) = input else {
                bail!("resolve-path expects base and path")
            };
            let [Value::String(base), Value::String(path)] = args.as_slice() else {
                bail!("resolve-path expects two strings")
            };
            let canonical = resolve_source(base, path)?;
            Ok(canonical
                .to_str()
                .context("source path is not UTF-8")?
                .into())
        } else {
            let Value::String(path) = input else {
                bail!("read-source expects a path")
            };
            ensure!(caller.data().files < 256, "include graph exceeds 256 files");
            let text = read_source(Path::new(&path))?;
            ensure!(
                caller.data().bytes + text.len() <= SOURCE_LIMIT,
                "include graph exceeds {SOURCE_LIMIT} bytes"
            );
            caller.data_mut().bytes += text.len();
            caller.data_mut().files += 1;
            Ok(text)
        }
    })();
    let output = pack::encode(&Value::Result {
        ok_type: ValueType::String,
        err_type: ValueType::String,
        value: result
            .map(|s| Box::new(Value::String(s)))
            .map_err(|e| Box::new(Value::String(format!("{e:#}")))),
    })?;
    let alloc = caller
        .get_export("__pack_alloc")
        .and_then(|e| e.into_func())
        .context("missing guest allocator")?
        .typed::<i32, i32>(&*caller)?;
    let allocated = alloc.call(&mut *caller, output.len() as i32)?;
    memory.write(&mut *caller, allocated as usize, &output)?;
    memory.write(&mut *caller, out_ptr as usize, &allocated.to_le_bytes())?;
    memory.write(
        &mut *caller,
        out_len as usize,
        &(output.len() as i32).to_le_bytes(),
    )?;
    Ok(0)
}

pub struct Interpreter {
    store: Store<SourceBudget>,
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
        let mut store = Store::new(&engine, SourceBudget::default());
        let mut linker = Linker::new(&engine);
        linker.func_wrap(
            "wisp-source",
            "resolve-path",
            |mut caller: Caller<'_, SourceBudget>,
             ptr: i32,
             len: i32,
             out_ptr: i32,
             out_len: i32| {
                source_import(&mut caller, true, ptr, len, out_ptr, out_len)
            },
        )?;
        linker.func_wrap(
            "wisp-source",
            "read-source",
            |mut caller: Caller<'_, SourceBudget>,
             ptr: i32,
             len: i32,
             out_ptr: i32,
             out_len: i32| {
                source_import(&mut caller, false, ptr, len, out_ptr, out_len)
            },
        )?;
        // Theater rpc bridge. The local host has no Theater runtime, so these are
        // placeholders that only satisfy linking — a real host (or the actor)
        // supplies them over Theater's recording/replay path. Never invoked unless
        // a session evaluates the matching verb.
        for name in ["describe", "exports", "implements", "call"] {
            linker.func_wrap(
                "theater:simple/rpc",
                name,
                |_caller: Caller<'_, SourceBudget>, _: i32, _: i32, _: i32, _: i32| -> i32 { -1 },
            )?;
        }
        // Any other Theater host import the actor declares (self/store/timer/...)
        // traps if invoked — the local host has no Theater. One line instead of a
        // stub per import; real Theater supplies them all.
        linker.define_unknown_imports_as_traps(&module)?;
        let instance = linker.instantiate(&mut store, &module)?;
        Ok(Self {
            memory: instance
                .get_memory(&mut store, "memory")
                .context("interpreter has no memory")?,
            alloc: instance.get_typed_func(&mut store, "__pack_alloc")?,
            evaluate: instance.get_typed_func(&mut store, "evaluate-from")?,
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
        self.evaluate_from(source, "")
    }

    /// Read a source file and expand includes relative to that file. Included
    /// files are deduplicated by canonical path within this load, including cycles.
    pub fn load_file(&mut self, path: impl AsRef<Path>) -> Result<String> {
        let path = path
            .as_ref()
            .canonicalize()
            .with_context(|| format!("cannot resolve {}", path.as_ref().display()))?;
        let source = read_source(&path)?;
        self.evaluate_from(&source, path.to_str().context("source path is not UTF-8")?)
    }

    fn evaluate_from(&mut self, source: &str, base: &str) -> Result<String> {
        *self.store.data_mut() = SourceBudget {
            bytes: source.len(),
            files: usize::from(!base.is_empty()),
        };
        // Backstop only: the evaluator's own step (10k) and nesting (128) guards
        // fire first. host-builtin? is checked per lookup and grows with each wired
        // interface, so keep generous headroom above what those guards cost.
        self.store.set_fuel(200_000_000)?;
        let input = pack::encode(&Value::Tuple(vec![
            Value::String(source.into()),
            Value::String(base.into()),
        ]))?;
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
