// Memory baselines for docs/proposals/MEMORY.md: bytes allocated by the Wisp
// interpreter per evaluation, and by the self-hosted compiler building itself.
// Run: cargo test --release --test memory_baseline -- --ignored --nocapture --test-threads=1
use std::path::PathBuf;
use std::time::Instant;

use granite::compiler;
use wasmtime::*;

fn big<T: Send>(f: impl FnOnce() -> T + Send) -> T {
    std::thread::scope(|s| {
        std::thread::Builder::new()
            .stack_size(1 << 30)
            .spawn_scoped(s, f)
            .unwrap()
            .join()
            .unwrap()
    })
}

fn compile(src: &str, stem: &str) -> PathBuf {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out = root.join(format!("target/memory-baseline/{stem}"));
    let src = root.join(src);
    big(move || {
        compiler::compile(&src, &out, compiler::EmitOptions::default())
            .unwrap()
            .wasm
    })
}

struct Inst {
    store: Store<()>,
    memory: Memory,
    alloc: TypedFunc<i32, i32>,
    free: TypedFunc<(i32, i32), ()>,
    stats: TypedFunc<(), i32>,
    instance: Instance,
}

/// Heap counters from `__granite_heap_stats`.
#[derive(Clone, Copy)]
struct Heap {
    /// Bytes ever allocated, block headers included.
    total: i64,
    /// Bytes in live blocks.
    live: i64,
    pages: i64,
}

fn load(path: &PathBuf, max_stack: usize) -> Inst {
    let mut config = Config::new();
    config.wasm_tail_call(true);
    config.max_wasm_stack(max_stack);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_file(&engine, path).unwrap();
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);
    linker.define_unknown_imports_as_traps(&module).unwrap();
    let instance = linker.instantiate(&mut store, &module).unwrap();
    let memory = instance.get_memory(&mut store, "memory").unwrap();
    let alloc = instance.get_typed_func(&mut store, "__pack_alloc").unwrap();
    let free = instance.get_typed_func(&mut store, "__pack_free").unwrap();
    let stats = instance
        .get_typed_func(&mut store, "__granite_heap_stats")
        .unwrap();
    Inst {
        store,
        memory,
        alloc,
        free,
        stats,
        instance,
    }
}

impl Inst {
    fn heap(&mut self) -> Heap {
        let s = self.stats.call(&mut self.store, ()).unwrap() as usize;
        let mut b = [0u8; 16];
        self.memory.read(&self.store, s + 12, &mut b).unwrap();
        let word = |i: usize| u32::from_le_bytes(b[i..i + 4].try_into().unwrap()) as i64;
        let mut live = [0u8; 4];
        self.memory.read(&self.store, s, &mut live).unwrap();
        Heap {
            total: word(4) | word(8) << 32,
            live: u32::from_le_bytes(live) as i64,
            pages: word(0),
        }
    }
    /// Call an export the way the packr host does, freeing the input and output
    /// buffers after the call.
    fn call_str(&mut self, func: &str, input: pack::abi::Value) -> String {
        let f: TypedFunc<(i32, i32, i32, i32), i32> =
            self.instance.get_typed_func(&mut self.store, func).unwrap();
        let bytes = pack::encode(&input).unwrap();
        let ptr = self
            .alloc
            .call(&mut self.store, bytes.len() as i32)
            .unwrap();
        let slots = self.alloc.call(&mut self.store, 8).unwrap();
        self.memory
            .write(&mut self.store, ptr as usize, &bytes)
            .unwrap();
        let st = f
            .call(&mut self.store, (ptr, bytes.len() as i32, slots, slots + 4))
            .unwrap();
        assert_eq!(st, 0);
        let mut pl = [0u8; 8];
        self.memory
            .read(&self.store, slots as usize, &mut pl)
            .unwrap();
        let p = u32::from_le_bytes(pl[..4].try_into().unwrap()) as usize;
        let l = u32::from_le_bytes(pl[4..].try_into().unwrap()) as usize;
        let out = match pack::decode(&self.memory.data(&self.store)[p..p + l]).unwrap() {
            pack::abi::Value::String(s) => s,
            v => format!("{v:?}"),
        };
        for (ptr, len) in [(ptr, bytes.len() as i32), (p as i32, l as i32), (slots, 8)] {
            self.free.call(&mut self.store, (ptr, len)).unwrap();
        }
        out
    }
}

fn eval(i: &mut Inst, src: &str) -> String {
    i.call_str(
        "evaluate-from",
        pack::abi::Value::Tuple(vec![
            pack::abi::Value::String(src.into()),
            pack::abi::Value::String("".into()),
        ]),
    )
}

#[test]
#[ignore = "measurement; run with --ignored --nocapture"]
fn interpreter_growth() {
    let wasm = compile("wisp/interpreter/evaluator.wisp", "evaluator");
    big(move || {
        let mut i = load(&wasm, 512 << 20);
        let h0 = i.heap();
        println!(
            "evaluator: live after instantiate = {}, {} pages",
            h0.live, h0.pages
        );
        // allocated: every block allocated per eval; retained: what stays live
        // (the host's __pack_free reclaims the input and output buffers).
        let row = |i: &mut Inst, label: &str, src: &str, n: usize| {
            let first = eval(i, src);
            let a = i.heap();
            let t = Instant::now();
            for _ in 0..n {
                eval(i, src);
            }
            let dt = t.elapsed();
            let b = i.heap();
            println!(
                "{label:<40} -> {first:<14} {:>10} allocated/eval {:>10} retained/eval  {:>8.1} us/eval",
                (b.total - a.total) / n as i64,
                (b.live - a.live) / n as i64,
                dt.as_secs_f64() * 1e6 / n as f64
            );
        };
        row(&mut i, "(+ 1 2)", "(+ 1 2)", 2000);
        row(&mut i, "\"hello\"", "\"hello\"", 2000);
        row(
            &mut i,
            "(define x (list 1 2 3))",
            "(define x (list 1 2 3))",
            1000,
        );
        eval(
            &mut i,
            "(define count (lambda (n) (if (= n 0) 0 (count (- n 1)))))",
        );
        row(&mut i, "(count 100)", "(count 100)", 200);
        row(&mut i, "(count 1000)", "(count 1000)", 50);
        eval(&mut i, "(define on-message (lambda (e) (car e)))");
        row(
            &mut i,
            "(on-message (list 1 2))",
            "(on-message (list 1 2))",
            1000,
        );
        let h = i.heap();
        println!(
            "evaluator: at end {} allocated, {} live, {} pages",
            h.total, h.live, h.pages
        );
    });
}

#[test]
#[ignore = "measurement; run with --ignored --nocapture"]
fn self_compile() {
    let wasm = compile("examples/wisp-compiler.wisp", "wisp-compiler");
    let source = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/wisp-compiler.wisp"),
    )
    .unwrap();
    big(move || {
        let mut i = load(&wasm, 512 << 20);
        let h0 = i.heap();
        let t = Instant::now();
        let wat = i.call_str("compile-source", pack::abi::Value::String(source.clone()));
        let dt = t.elapsed();
        let h1 = i.heap();
        println!(
            "self-compile: source {} B, output {} B, {:.1} MiB allocated, {} pages, {:.2} s",
            source.len(),
            wat.len(),
            (h1.total - h0.total) as f64 / 1048576.0,
            h1.pages,
            dt.as_secs_f64()
        );
    });
}

#[test]
#[ignore = "measurement; run with --ignored --nocapture"]
fn gen2_self_compile() {
    let wasm = compile("examples/wisp-compiler.wisp", "wisp-compiler");
    let source = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/wisp-compiler.wisp"),
    )
    .unwrap();
    big(move || {
        let mut i = load(&wasm, 512 << 20);
        let gen2 = i.call_str("compile-source", pack::abi::Value::String(source.clone()));
        let mut config = Config::new();
        config.max_wasm_stack(512 << 20);
        config.wasm_tail_call(true);
        let engine = Engine::new(&config).unwrap();
        let module = Module::new(&engine, &gen2).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();
        let f = instance
            .get_typed_func::<i32, i32>(&mut store, "compile-source")
            .unwrap();
        let memory = instance.get_memory(&mut store, "memory").unwrap();
        let have = (memory.data_size(&store) / 65536) as u64;
        memory.grow(&mut store, 32768 - have).unwrap();
        let in_ptr: i32 = 0x6000_0000;
        let b = source.as_bytes();
        memory
            .write(&mut store, in_ptr as usize, &(b.len() as u32).to_le_bytes())
            .unwrap();
        memory.write(&mut store, in_ptr as usize + 4, b).unwrap();
        let t = Instant::now();
        let out = f.call(&mut store, in_ptr).unwrap();
        let dt = t.elapsed();
        let mut l = [0u8; 4];
        memory.read(&store, out as usize, &mut l).unwrap();
        let len = u32::from_le_bytes(l) as i64;
        let end = out as i64 + 4 + len;
        println!(
            "gen-2 self-compile: output {len} B, heap 0xC000 -> {end} ({:.1} MiB allocated), {:.2} s",
            (end - 0xC000) as f64 / 1048576.0,
            dt.as_secs_f64()
        );
    });
}
