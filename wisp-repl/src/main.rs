use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use wisp::compiler::{NullHost, Outcome, ReplSession};

fn main() -> anyhow::Result<()> {
    println!("Wisp REPL v0.1.0");
    println!(
        "Enter an expression, or define with (define name expr) / (fn name (params) ret body)."
    );
    println!("Type :quit to exit.\n");

    // The standalone REPL runs on the shared interpreter with no host: it is pure,
    // so any imported call is an error. The live Theater REPL is the same session
    // with a runtime-backed host instead.
    let mut session = ReplSession::new();
    let mut host = NullHost;
    let mut rl = DefaultEditor::new()?;

    loop {
        match rl.readline("wisp> ") {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                let _ = rl.add_history_entry(line);
                match line {
                    ":quit" | ":q" => break,
                    ":help" => {
                        print_help();
                        continue;
                    }
                    _ => {}
                }
                match session.feed(line, &mut host) {
                    Ok(Outcome::Defined(name)) => println!("defined {name}"),
                    Ok(Outcome::Bound { name, value, .. }) => println!("{name} = {value}"),
                    Ok(Outcome::Evaluated { value, .. }) => println!("{value}"),
                    Err(e) => eprintln!("Error: {e:#}"),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error: {err:?}");
                break;
            }
        }
    }

    println!("Goodbye!");
    Ok(())
}

fn print_help() {
    println!("Commands:");
    println!("  :quit, :q   - Exit the REPL");
    println!("  :help       - Show this help");
    println!();
    println!("Syntax:");
    println!("  (define name expr)                  - Bind a value");
    println!("  (fn name ((p type) ...) ret body)   - Define a function");
    println!("  expr                                - Evaluate and print");
}

#[cfg(test)]
mod tests {
    use super::*;
    use wisp::compiler::Value;

    #[test]
    fn repl_session_evaluates_and_accumulates() {
        let mut session = ReplSession::new();
        let mut host = NullHost;

        // A bare expression evaluates.
        assert!(matches!(
            session.feed("(i32.add 40 2)", &mut host).unwrap(),
            Outcome::Evaluated {
                value: Value::Int(42),
                ..
            }
        ));
        // Strings go through the same path.
        assert!(matches!(
            session.feed("\"hello, λ\"", &mut host).unwrap(),
            Outcome::Evaluated { value: Value::Str(s), .. } if s == "hello, λ"
        ));
        // A (define …) binds a value that later expressions see.
        assert!(matches!(
            session.feed("(define x 10)", &mut host).unwrap(),
            Outcome::Bound { .. }
        ));
        assert!(matches!(
            session.feed("(i32.mul x 4)", &mut host).unwrap(),
            Outcome::Evaluated {
                value: Value::Int(40),
                ..
            }
        ));
        // A (fn …) definition is callable afterward.
        assert!(matches!(
            session.feed("(fn inc ((n s32)) s32 (i32.add n 1))", &mut host).unwrap(),
            Outcome::Defined(n) if n == "inc"
        ));
        assert!(matches!(
            session.feed("(inc 41)", &mut host).unwrap(),
            Outcome::Evaluated {
                value: Value::Int(42),
                ..
            }
        ));
    }
}
