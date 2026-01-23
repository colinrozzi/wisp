use composite::abi::Value as CompositeValue;
use composite::Runtime;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use wisp_repl::{compile_repl_composite, ReplState, Value};

fn main() -> anyhow::Result<()> {
    println!("Wisp REPL v0.1.0 (composite runtime)");
    println!("Type expressions to evaluate. Use 'let name = expr' to bind values.");
    println!("Type :quit to exit.\n");

    let mut state = ReplState::new();
    let runtime = Runtime::new();

    let mut rl = DefaultEditor::new()?;

    loop {
        let readline = rl.readline("wisp> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                let _ = rl.add_history_entry(line);

                if line == ":quit" || line == ":q" {
                    break;
                }

                if line == ":bindings" {
                    println!("Current bindings:");
                    for (name, value) in &state.bindings {
                        println!("  {} = {:?}", name, value);
                    }
                    continue;
                }

                if line == ":help" {
                    println!("Commands:");
                    println!("  :quit, :q     - Exit the REPL");
                    println!("  :bindings     - Show current variable bindings");
                    println!("  :help         - Show this help");
                    println!("\nSyntax:");
                    println!("  let name = expr  - Bind result to a variable");
                    println!("  expr             - Evaluate and print result");
                    continue;
                }

                // Check for let binding: let name = expr
                if line.starts_with("let ") {
                    if let Some(result) = handle_let_binding(line, &mut state, &runtime) {
                        match result {
                            Ok((name, value)) => {
                                println!("{} = {:?}", name, value);
                            }
                            Err(e) => {
                                eprintln!("Error: {}", e);
                            }
                        }
                    }
                    continue;
                }

                // Regular expression evaluation
                match eval_expr(line, &state, &runtime) {
                    Ok(value) => {
                        println!("{:?}", value);
                    }
                    Err(e) => {
                        eprintln!("Error: {}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    println!("Goodbye!");
    Ok(())
}

fn handle_let_binding(
    line: &str,
    state: &mut ReplState,
    runtime: &Runtime,
) -> Option<anyhow::Result<(String, Value)>> {
    // Parse: let name = expr
    let rest = line.strip_prefix("let ")?.trim();
    let parts: Vec<&str> = rest.splitn(2, '=').collect();
    if parts.len() != 2 {
        return Some(Err(anyhow::anyhow!(
            "Invalid let syntax. Use: let name = expr"
        )));
    }

    let name = parts[0].trim().to_string();
    let expr = parts[1].trim();

    if name.is_empty() {
        return Some(Err(anyhow::anyhow!("Variable name cannot be empty")));
    }

    match eval_expr(expr, state, runtime) {
        Ok(value) => {
            state.bindings.insert(name.clone(), value.clone());
            Some(Ok((name, value)))
        }
        Err(e) => Some(Err(e)),
    }
}

fn eval_expr(expr: &str, state: &ReplState, runtime: &Runtime) -> anyhow::Result<Value> {
    // Compile the expression to composite WASM
    let wasm_bytes = compile_repl_composite(expr, state)?;

    // Load and instantiate with composite runtime
    let module = runtime.load_module(&wasm_bytes)?;
    let mut instance = module.instantiate()?;

    // Call eval with no arguments (empty tuple)
    let input = CompositeValue::Tuple(vec![]);
    let output = instance.call_with_value("eval", &input, 0)?;

    // Convert composite Value to our Value
    composite_to_repl_value(&output)
}

fn composite_to_repl_value(cv: &CompositeValue) -> anyhow::Result<Value> {
    match cv {
        CompositeValue::S32(n) => Ok(Value::S32(*n)),
        CompositeValue::S64(n) => Ok(Value::S64(*n)),
        CompositeValue::F32(n) => Ok(Value::F32(*n)),
        CompositeValue::F64(n) => Ok(Value::F64(*n)),
        CompositeValue::String(s) => Ok(Value::Str(s.clone())),
        CompositeValue::Option(inner) => Ok(Value::Option(
            inner
                .as_ref()
                .map(|v| composite_to_repl_value(v).map(Box::new))
                .transpose()?,
        )),
        CompositeValue::List(items) => Ok(Value::List(
            items
                .iter()
                .map(composite_to_repl_value)
                .collect::<anyhow::Result<Vec<_>>>()?,
        )),
        CompositeValue::Record(fields) => {
            let mut field_map = std::collections::HashMap::new();
            for (name, value) in fields {
                field_map.insert(name.clone(), composite_to_repl_value(value)?);
            }
            Ok(Value::Record {
                type_name: "record".to_string(), // We don't have type name in CGRF
                fields: field_map,
            })
        }
        CompositeValue::Variant { tag, payload } => Ok(Value::Variant {
            type_name: "variant".to_string(),
            case: format!("case_{}", tag),
            payload: payload
                .as_ref()
                .map(|v| composite_to_repl_value(v).map(Box::new))
                .transpose()?,
        }),
        other => Err(anyhow::anyhow!("Unsupported composite value: {:?}", other)),
    }
}
