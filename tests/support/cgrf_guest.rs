//! Minimal raw Pack ABI harness used by the interpreter's differential tests.
use pack::abi::Value;
use wasmtime::{Config, Engine, Instance, Memory, Module, Store};

pub struct Guest {
    pub store: Store<()>,
    pub instance: Instance,
    memory: Memory,
}

pub fn engine() -> Engine {
    let mut config = Config::new();
    config.wasm_tail_call(true);
    Engine::new(&config).unwrap()
}

impl Guest {
    pub fn new(module: &Module) -> Self {
        let mut store = Store::new(module.engine(), ());
        let instance = Instance::new(&mut store, module, &[]).unwrap();
        let memory = instance.get_memory(&mut store, "memory").unwrap();
        Self {
            store,
            instance,
            memory,
        }
    }

    pub fn call(&mut self, name: &str, input: Value) -> Value {
        let input = pack::encode(&input).unwrap();
        let alloc = self
            .instance
            .get_typed_func::<i32, i32>(&mut self.store, "__pack_alloc")
            .unwrap();
        let ptr = alloc.call(&mut self.store, input.len() as i32).unwrap();
        let slots = alloc.call(&mut self.store, 8).unwrap();
        self.memory
            .write(&mut self.store, ptr as usize, &input)
            .unwrap();
        let status = self
            .instance
            .get_typed_func::<(i32, i32, i32, i32), i32>(&mut self.store, name)
            .unwrap()
            .call(&mut self.store, (ptr, input.len() as i32, slots, slots + 4))
            .unwrap();
        assert_eq!(status, 0);
        let mut output = [0; 8];
        self.memory
            .read(&self.store, slots as usize, &mut output)
            .unwrap();
        let ptr = u32::from_le_bytes(output[..4].try_into().unwrap()) as usize;
        let len = u32::from_le_bytes(output[4..].try_into().unwrap()) as usize;
        pack::decode(&self.memory.data(&self.store)[ptr..ptr + len]).unwrap()
    }
}
