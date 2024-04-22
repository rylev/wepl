mod command;
mod evaluator;
mod runtime;
mod wit;

use std::collections::HashMap;

use anyhow::Context as _;
use clap::Parser;
use colored::*;
use rustyline::error::ReadlineError;

fn main() {
    if let Err(e) = _main() {
        print_error_prefix();
        eprintln!("{e}");
        if e.source().is_some() {
            eprintln!("\nCaused by:");
        }
        for e in e.chain().skip(1) {
            eprintln!("  {e}")
        }
    }
}

fn _main() -> anyhow::Result<()> {
    env_logger::init();

    let cli = Cli::parse();
    let component_bytes = std::fs::read(cli.component)?;
    let mut resolver = wit::WorldResolver::from_bytes(&component_bytes)?;
    let mut runtime = runtime::Runtime::init(component_bytes, &resolver, |import_name| {
        print_error_prefix();
        eprintln!("unimplemented import: {import_name}");
    })?;

    let mut rl = rustyline::DefaultEditor::new()?;
    if let Some(home) = home::home_dir() {
        let _ = rl.load_history(&home.join(".weplhistory"));
    }
    let world = resolver.world_name();
    println!("{}: {world}", "World".blue().bold());
    let mut scope = HashMap::default();
    let prompt = "> ".blue().bold().to_string();
    loop {
        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);
                let line = command::Cmd::parse(&line);
                match line {
                    Ok(Some(cmd)) => {
                        match cmd.run(&mut runtime, &mut resolver, &mut scope) {
                            Err(e) => {
                                print_error_prefix();
                                eprintln!("{e}");
                                // Refresh the runtime on error so we start fresh
                                runtime.refresh().context("error refreshing wasm runtime")?;
                            }
                            Ok(true) => {
                                let _ = rl.clear_screen();
                            }
                            _ => {}
                        }
                    }
                    Ok(None) => continue,
                    Err(e) => {
                        print_error_prefix();
                        eprintln!("{e}")
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(ReadlineError::WindowResized) => continue,
            Err(ReadlineError::Io(e)) => {
                print_error_prefix();
                eprintln!("reading from stdin failed: {e}");
                break;
            }
            Err(e) => {
                print_error_prefix();
                eprintln!("reading from stdin failed: {e}");
                break;
            }
        }
    }
    if let Some(home) = home::home_dir() {
        let _ = rl.save_history(&home.join(".weplhistory"));
    }

    Ok(())
}

fn print_error_prefix() {
    print_prefix("Error: ", colored::Color::Red)
}

fn print_prefix(prefix: &str, color: colored::Color) {
    use std::io::Write;
    let mut stderr = std::io::stderr();
    let _ = write!(&mut stderr, "{}", prefix.color(color).bold());
    let _ = stderr.flush();
}

/// The WebAssembly Component repl.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to component binary
    component: std::path::PathBuf,
}
