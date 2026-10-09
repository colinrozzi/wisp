use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::collections::HashMap;
use wisp::compiler::{Function, InlineValue, Type, Value as CValue, eval_repl_expr};
use wisp_repl::{ReplState, Value};

fn main() -> anyhow::Result<()> {
    println!("Wisp REPL v0.1.0 (pack runtime)");
    println!("Type expressions to evaluate. Use 'let name = expr' to bind values.");
    println!("Type :quit to exit.\n");

    let mut state = ReplState::new();

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
                    if let Some(result) = handle_let_binding(line, &mut state) {
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
                match eval_expr(line, &state) {
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

    match eval_expr(expr, state) {
        Ok(value) => {
            state.bindings.insert(name.clone(), value.clone());
            Some(Ok((name, value)))
        }
        Err(e) => Some(Err(e)),
    }
}

fn eval_expr(expr: &str, state: &ReplState) -> anyhow::Result<Value> {
    // Inline the session's value bindings, bring its functions into scope, and
    // evaluate through the shared front/middle (parse + type-check) + eval back-end.
    let bindings: HashMap<String, InlineValue> = state
        .bindings
        .iter()
        .map(|(k, v)| (k.clone(), v.to_inline()))
        .collect();
    let functions: Vec<Function> = state.functions.values().cloned().collect();
    let (value, ty) = eval_repl_expr(expr, &bindings, &functions)?;
    eval_to_repl(&value, &ty)
}

/// Convert an eval `Value` plus its inferred static type into the REPL's typed
/// `Value`. The REPL evaluates expressions (no user record/variant construction),
/// so only scalars, strings, lists, options, and results arise here.
fn eval_to_repl(v: &CValue, ty: &Type) -> anyhow::Result<Value> {
    Ok(match (v, ty) {
        (CValue::Int(n), Type::S64 | Type::U64) => Value::S64(*n),
        (CValue::Int(n), _) => Value::S32(*n as i32),
        (CValue::Float(x), Type::F32) => Value::F32(*x as f32),
        (CValue::Float(x), _) => Value::F64(*x),
        (CValue::Str(s), _) => Value::Str(s.clone()),
        (CValue::List(items), Type::List(elem)) => Value::List {
            elem_type: (**elem).clone(),
            items: items
                .iter()
                .map(|i| eval_to_repl(i, elem))
                .collect::<anyhow::Result<Vec<_>>>()?,
        },
        (CValue::Opt(o), Type::Option(inner)) => Value::Option {
            inner_type: (**inner).clone(),
            value: o
                .as_ref()
                .map(|b| eval_to_repl(b, inner).map(Box::new))
                .transpose()?,
        },
        (CValue::Res(r), Type::Result(ok, err)) => Value::Result {
            ok_type: (**ok).clone(),
            err_type: (**err).clone(),
            value: match r {
                Ok(b) => Ok(Box::new(eval_to_repl(b, ok)?)),
                Err(b) => Err(Box::new(eval_to_repl(b, err)?)),
            },
        },
        (other, t) => anyhow::bail!("REPL cannot render {other} at type {t:?}"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repl_evaluates_via_eval_backend() {
        let state = ReplState::new();
        assert!(matches!(
            eval_expr("(i32.add 40 2)", &state).unwrap(),
            Value::S32(42)
        ));
        assert!(
            matches!(eval_expr("\"hello, λ\"", &state).unwrap(), Value::Str(s) if s == "hello, λ")
        );
        assert!(matches!(
            eval_expr("(some s32 42)", &state).unwrap(),
            Value::Option { inner_type: Type::S32, value: Some(v) } if matches!(*v, Value::S32(42))
        ));
    }
}
