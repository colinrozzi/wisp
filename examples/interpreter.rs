//! Run with `cargo run --example interpreter`. Each line is a REPL input;
//! use `(begin ...)` to group expressions, and `:quit` or EOF to exit.

use std::io::{self, BufRead, IsTerminal, Write};
use std::path::Path;

use anyhow::Result;
use wisp::{compiler, interpreter::Interpreter};

fn main() -> Result<()> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let artifacts = compiler::compile(
        &root.join("interpreter/evaluator.lisp"),
        &root.join("target/interpreter/evaluator"),
        compiler::EmitOptions::default(),
    )?;
    let mut session = Interpreter::load(artifacts.wasm)?;
    let input = io::stdin();
    let interactive = input.is_terminal();
    if interactive {
        println!("Interpreted Wisp — :quit to exit");
    }
    let mut lines = input.lock().lines();
    loop {
        if interactive {
            print!("wisp> ");
            io::stdout().flush()?;
        }
        let Some(line) = lines.next() else { break };
        let line = line?;
        if matches!(line.trim(), ":quit" | ":q") {
            break;
        }
        if line.trim().is_empty() {
            continue;
        }
        match session.evaluate(&line) {
            Ok(output) => println!("{output}"),
            Err(error) => eprintln!("error: {error:#}"),
        }
    }
    Ok(())
}
