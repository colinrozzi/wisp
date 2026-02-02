use std::collections::HashMap;
use wisp::compiler::{
    Function, InlineValue, Type, compile_repl_expr, compile_repl_expr_pack,
    compile_repl_expr_pack_wat,
};

/// A runtime value that can be inlined during REPL compilation
#[derive(Debug, Clone)]
pub enum Value {
    // Scalars
    S32(i32),
    S64(i64),
    F32(f32),
    F64(f64),

    // String
    Str(String),

    // Compound types WITH explicit type info
    List {
        elem_type: Type,
        items: Vec<Value>,
    },
    Option {
        inner_type: Type,
        value: Option<Box<Value>>,
    },
    Result {
        ok_type: Type,
        err_type: Type,
        value: std::result::Result<Box<Value>, Box<Value>>,
    },

    // User-defined types - ordered fields, multi-value payload
    Record {
        type_name: String,
        fields: Vec<(String, Value)>,
    },
    Variant {
        type_name: String,
        case: String,
        payload: Vec<Value>,
    },
    // Note: Resources excluded for now
}

impl Value {
    /// Get the wisp Type for this value - uses explicit type fields
    pub fn get_type(&self) -> Type {
        match self {
            Value::S32(_) => Type::S32,
            Value::S64(_) => Type::S64,
            Value::F32(_) => Type::F32,
            Value::F64(_) => Type::F64,
            Value::Str(_) => Type::Str,
            Value::List { elem_type, .. } => Type::List(Box::new(elem_type.clone())),
            Value::Option { inner_type, .. } => Type::Option(Box::new(inner_type.clone())),
            Value::Result {
                ok_type, err_type, ..
            } => Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone())),
            Value::Record { type_name, .. } => Type::Record(type_name.clone()),
            Value::Variant { type_name, .. } => Type::Variant(type_name.clone()),
        }
    }

    /// Convert to InlineValue for compiler inlining - preserves type info
    pub fn to_inline(&self) -> InlineValue {
        match self {
            Value::S32(n) => InlineValue::S32(*n),
            Value::S64(n) => InlineValue::S64(*n),
            Value::F32(n) => InlineValue::F32(*n),
            Value::F64(n) => InlineValue::F64(*n),
            Value::Str(s) => InlineValue::Str(s.clone()),
            Value::List { elem_type, items } => InlineValue::List {
                elem_type: elem_type.clone(),
                items: items.iter().map(|v| v.to_inline()).collect(),
            },
            Value::Option { inner_type, value } => InlineValue::Option {
                inner_type: inner_type.clone(),
                value: value.as_ref().map(|v| Box::new(v.to_inline())),
            },
            Value::Result {
                ok_type,
                err_type,
                value,
            } => InlineValue::Result {
                ok_type: ok_type.clone(),
                err_type: err_type.clone(),
                value: match value {
                    Ok(v) => Ok(Box::new(v.to_inline())),
                    Err(v) => Err(Box::new(v.to_inline())),
                },
            },
            Value::Record { type_name, fields } => InlineValue::Record {
                type_name: type_name.clone(),
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.to_inline()))
                    .collect(),
            },
            Value::Variant {
                type_name,
                case,
                payload,
            } => InlineValue::Variant {
                type_name: type_name.clone(),
                case: case.clone(),
                payload: payload.iter().map(|v| v.to_inline()).collect(),
            },
        }
    }
}

/// State accumulated during a REPL session
pub struct ReplState {
    pub bindings: HashMap<String, Value>,
    pub functions: HashMap<String, Function>,
}

impl ReplState {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

impl Default for ReplState {
    fn default() -> Self {
        Self::new()
    }
}

/// Compile an expression with REPL context
/// Returns WASM package bytes (standard component model)
pub fn compile_repl(expr: &str, state: &ReplState) -> anyhow::Result<Vec<u8>> {
    // Convert bindings to InlineValue
    let bindings: HashMap<String, InlineValue> = state
        .bindings
        .iter()
        .map(|(k, v)| (k.clone(), v.to_inline()))
        .collect();

    // Collect functions as a Vec
    let functions: Vec<Function> = state.functions.values().cloned().collect();

    // Compile the expression
    compile_repl_expr(expr, &bindings, &functions)
}

/// Compile an expression with REPL context for Pack runtime
/// Returns raw WASM bytes (Pack package with Graph ABI encoding)
pub fn compile_repl_pack(expr: &str, state: &ReplState) -> anyhow::Result<Vec<u8>> {
    // Convert bindings to InlineValue
    let bindings: HashMap<String, InlineValue> = state
        .bindings
        .iter()
        .map(|(k, v)| (k.clone(), v.to_inline()))
        .collect();

    // Collect functions as a Vec
    let functions: Vec<Function> = state.functions.values().cloned().collect();

    // Compile the expression using Pack calling convention
    compile_repl_expr_pack(expr, &bindings, &functions)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_simple_literal() {
        let state = ReplState::new();
        let result = compile_repl("42", &state);
        assert!(
            result.is_ok(),
            "Failed to compile literal: {:?}",
            result.err()
        );
        let bytes = result.unwrap();
        assert!(!bytes.is_empty(), "Package bytes should not be empty");
    }

    #[test]
    fn test_compile_with_binding() {
        let mut state = ReplState::new();
        state.bindings.insert("x".to_string(), Value::S32(10));

        let result = compile_repl("x", &state);
        assert!(
            result.is_ok(),
            "Failed to compile with binding: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_compile_arithmetic_with_bindings() {
        let mut state = ReplState::new();
        state.bindings.insert("x".to_string(), Value::S32(10));
        state.bindings.insert("y".to_string(), Value::S32(20));

        let result = compile_repl("(i32.add x y)", &state);
        assert!(
            result.is_ok(),
            "Failed to compile arithmetic: {:?}",
            result.err()
        );
    }

    /// Helper to run a compiled package and get the i32 result
    fn run_eval_i32(wasm_bytes: &[u8]) -> anyhow::Result<i32> {
        use wasmtime::{Engine, Store, component::Component, component::Linker, component::Val};

        let engine = Engine::default();
        let component = Component::new(&engine, wasm_bytes)?;
        let mut store = Store::new(&engine, ());
        let linker = Linker::new(&engine);
        let instance = linker.instantiate(&mut store, &component)?;

        let eval_func = instance
            .get_func(&mut store, "eval")
            .ok_or_else(|| anyhow::anyhow!("eval function not found"))?;

        let mut results = vec![Val::S32(0)];
        eval_func.call(&mut store, &[], &mut results)?;
        eval_func.post_return(&mut store)?;

        match results.into_iter().next() {
            Some(Val::S32(n)) => Ok(n),
            other => anyhow::bail!("unexpected result: {:?}", other),
        }
    }

    #[test]
    fn test_run_literal() {
        let state = ReplState::new();
        let wasm = compile_repl("42", &state).expect("compile failed");
        let result = run_eval_i32(&wasm).expect("run failed");
        assert_eq!(result, 42);
    }

    #[test]
    fn test_run_with_binding() {
        let mut state = ReplState::new();
        state.bindings.insert("x".to_string(), Value::S32(100));

        let wasm = compile_repl("x", &state).expect("compile failed");
        let result = run_eval_i32(&wasm).expect("run failed");
        assert_eq!(result, 100);
    }

    #[test]
    fn test_run_arithmetic() {
        let mut state = ReplState::new();
        state.bindings.insert("x".to_string(), Value::S32(10));
        state.bindings.insert("y".to_string(), Value::S32(32));

        let wasm = compile_repl("(i32.add x y)", &state).expect("compile failed");
        let result = run_eval_i32(&wasm).expect("run failed");
        assert_eq!(result, 42);
    }

    #[test]
    fn test_run_nested_arithmetic() {
        let mut state = ReplState::new();
        state.bindings.insert("a".to_string(), Value::S32(2));
        state.bindings.insert("b".to_string(), Value::S32(3));
        state.bindings.insert("c".to_string(), Value::S32(4));

        // (a + b) * c = (2 + 3) * 4 = 20
        let wasm = compile_repl("(i32.mul (i32.add a b) c)", &state).expect("compile failed");
        let result = run_eval_i32(&wasm).expect("run failed");
        assert_eq!(result, 20);
    }

    // Tests for Pack package output
    #[test]
    fn test_compile_pack_literal() {
        use super::compile_repl_pack;
        let state = ReplState::new();
        let result = compile_repl_pack("42", &state);
        assert!(
            result.is_ok(),
            "Failed to compile pack literal: {:?}",
            result.err()
        );
        let bytes = result.unwrap();
        assert!(!bytes.is_empty(), "Pack bytes should not be empty");
        // Verify it's a WASM module (magic number \0asm)
        assert_eq!(&bytes[0..4], b"\0asm", "Should be valid WASM binary");
    }

    #[test]
    fn test_compile_pack_with_binding() {
        use super::compile_repl_pack;
        let mut state = ReplState::new();
        state.bindings.insert("x".to_string(), Value::S32(100));

        let result = compile_repl_pack("x", &state);
        assert!(
            result.is_ok(),
            "Failed to compile pack with binding: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_pack_wat_output() {
        use super::compile_repl_expr_pack_wat;
        let bindings: HashMap<String, InlineValue> = HashMap::new();
        let functions = vec![];

        let wat =
            compile_repl_expr_pack_wat("42", &bindings, &functions).expect("WAT generation failed");

        // Print the WAT for inspection
        eprintln!("Generated WAT:\n{}", wat);

        // Verify key parts of the WAT structure
        assert!(wat.contains("(module"), "Should contain module declaration");
        assert!(wat.contains("(memory"), "Should contain memory declaration");
        assert!(
            wat.contains("(export \"eval\")"),
            "Should export eval function"
        );
        assert!(
            wat.contains("(param $in_ptr i32)"),
            "Should have pack calling convention params"
        );
        assert!(
            wat.contains("(param $out_ptr i32)"),
            "Should have output pointer param"
        );
    }
}
