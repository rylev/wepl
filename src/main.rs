mod command;
mod runtime;
mod wit;

use clap::Parser;
use rustyline::error::ReadlineError;

#[tokio::main]
async fn main() {
    if let Err(e) = _main().await {
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

async fn _main() -> anyhow::Result<()> {
    env_logger::init();

    let cli = Cli::parse();
    let component_bytes = std::fs::read(cli.component)?;
    let querier = wit::Querier::from_bytes(&component_bytes)?;
    let mut runtime = runtime::Runtime::init(&component_bytes, &querier, |import_name| {
        print_error_prefix();
        eprintln!("unimplemented import: {import_name}");
    })
    .await?;

    let mut rl = rustyline::DefaultEditor::new()?;
    if let Some(home) = home::home_dir() {
        let _ = rl.load_history(&home.join(".weplhistory"));
    }
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);
                let line = command::Cmd::parse(&line);
                match line {
                    Ok(cmd) => {
                        if let Err(e) = cmd.run(&mut runtime, &querier).await {
                            print_error_prefix();
                            eprintln!("{e}");
                        }
                    }
                    Err(e) => eprintln!("Error parsing input: {e}"),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(ReadlineError::WindowResized) => continue,
            Err(ReadlineError::Io(e)) => {
                eprintln!("Error reading from stdin: {e}");
                break;
            }
            Err(e) => {
                eprintln!("Error reading from stdin: {e}");
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
    print_prefix("Error: ", termcolor::Color::Red)
}

fn print_prefix(prefix: &str, color: termcolor::Color) {
    use std::io::Write;
    use termcolor::WriteColor;
    let mut stderr = termcolor::StandardStream::stderr(termcolor::ColorChoice::Always);
    let _ = stderr.set_color(
        termcolor::ColorSpec::new()
            .set_fg(Some(color))
            .set_bold(true),
    );
    let _ = write!(&mut stderr, "{}", prefix);
    let _ = stderr.flush();
    let _ = stderr.reset();
}

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to component binary
    component: std::path::PathBuf,
}
