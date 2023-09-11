mod world;

use std::borrow::Cow;

use anyhow::{anyhow, bail, ensure, Context as _};
use clap::Parser;
use rustyline::error::ReadlineError;
use wasmtime::{
    component::{Component, Instance, Linker, Val},
    Config, Engine, Store,
};
use wasmtime_wasi::preview2::{Table, WasiCtx, WasiCtxBuilder, WasiView};
use wit_parser::FunctionKind;
use world::Querier;

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
    let querier = Querier::from_bytes(&component_bytes)?;
    let (mut store, instance) = init_store_and_instance(&component_bytes, &querier).await?;

    let mut rl = rustyline::DefaultEditor::new()?;
    if let Some(home) = home::home_dir() {
        let _ = rl.load_history(&home.join(".weplhistory"));
    }
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);
                let line = Cmd::parse(&line);
                match line {
                    Ok(cmd) => {
                        if let Err(e) = cmd.run(&instance, &mut store, &querier).await {
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

enum Cmd<'a> {
    BuiltIn { name: &'a str, args: Vec<&'a str> },
    CallFunction { name: &'a str, args: Vec<&'a str> },
}

impl<'a> Cmd<'a> {
    fn parse(s: &'a str) -> anyhow::Result<Self> {
        let s = s.trim();

        fn parse_builtin(input: &str) -> nom::IResult<&str, (&str, Vec<&str>)> {
            use nom::branch::alt;
            use nom::bytes::complete::tag;
            use nom::character::complete::{alpha1, multispace0};
            use nom::combinator::recognize;
            use nom::multi::many0_count;
            use nom::sequence::{delimited, pair};

            let ident_parser = recognize(pair(alpha1, many0_count(alt((alpha1, tag("-"))))));
            let mut ident_parser = delimited(multispace0, ident_parser, multispace0);
            let (rest, ident) = ident_parser(input)?;
            let args = rest
                .split(',')
                .map(|a| a.trim())
                .filter(|a| !a.is_empty())
                .collect();

            Ok((rest, (ident, args)))
        }

        if let Some(builtin) = s.strip_prefix('.') {
            match parse_builtin(builtin) {
                Ok((_, (name, args))) => return Ok(Cmd::BuiltIn { name, args }),
                _ => bail!("could not parse call to built-in function: {builtin}"),
            }
        }

        if let Some(export) = s.strip_prefix('?') {
            let export = export.trim();
            if export.contains(char::is_whitespace) {
                bail!("invalid export name '{export}'. Identifiers can't contain whitespace");
            }
            return Ok(Self::BuiltIn {
                name: "inspect",
                args: vec![export],
            });
        }

        // try to parse a function
        let open_paren = s.find('(').context("no open parenthesis present")?;
        let name = &s[..open_paren];
        ensure!(&s[s.len() - 1..] == ")", "Missing ending paren");
        let arg_list = &s[open_paren + 1..s.len() - 1];
        let args = arg_list
            .split(", ")
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect();

        Ok(Cmd::CallFunction { name, args })
    }

    async fn run(
        &self,
        instance: &Instance,
        store: &mut Store<Context>,
        querier: &Querier,
    ) -> anyhow::Result<()> {
        match self {
            Cmd::CallFunction { name, args } => {
                log::debug!("Calling function: {name} with args: {args:?}");
                let func_def = querier.exported_function(name)?;
                let func = instance
                    .exports(&mut *store)
                    .root()
                    .func(&func_def.name)
                    .with_context(|| format!("could not find function {name}' in instance"))?;

                let mut parsed_args = Vec::with_capacity(func_def.params.len());
                if func_def.params.len() != args.len() {
                    bail!(
                        "tried to call a function that has {} params with {} args",
                        func_def.params.len(),
                        args.len()
                    )
                }
                for ((param_name, param_type), arg) in func_def.params.iter().zip(args) {
                    let parsed_arg = match param_type {
                        wit_parser::Type::U8 => arg.parse::<u8>().map(Val::U8).map_err(Into::into),
                        wit_parser::Type::U16 => {
                            arg.parse::<u16>().map(Val::U16).map_err(Into::into)
                        }
                        wit_parser::Type::U32 => {
                            arg.parse::<u32>().map(Val::U32).map_err(Into::into)
                        }
                        wit_parser::Type::U64 => {
                            arg.parse::<u64>().map(Val::U64).map_err(Into::into)
                        }
                        wit_parser::Type::String => {
                            parse_string(arg).map(|s| Val::String(s.to_owned().into_boxed_str()))
                        }
                        p => todo!("handle params of type {p:?}"),
                    }
                    .map_err(|e| {
                        anyhow!(
                            "could not parse argument {param_name} as {} to function: {e}",
                            querier
                                .display_wit_type(param_type)
                                .unwrap_or(Cow::Borrowed("<DISPLAY_ERROR>"))
                        )
                    })?;
                    parsed_args.push(parsed_arg)
                }

                let mut results = vec![Val::Bool(Default::default()); func_def.results.len()];
                func.call_async(&mut *store, &parsed_args, &mut results)
                    .await?;
                println!(
                    "{}",
                    results
                        .into_iter()
                        .map(|v| format_val(&v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Cmd::BuiltIn {
                name: "exports",
                args: _,
            } => {
                for (export_name, export) in querier.world().exports.iter() {
                    let export_name = querier.world_item_name(export_name)?;
                    let export_type = match export {
                        wit_parser::WorldItem::Interface(_) => "interface",
                        wit_parser::WorldItem::Function(_) => "function",
                        wit_parser::WorldItem::Type(_) => "type",
                    };
                    println!("{export_name}: {export_type}");
                }
            }
            Cmd::BuiltIn {
                name: "inspect",
                args,
            } => {
                let &[name] = args.as_slice() else {
                    bail!(
                        "wrong number of arguments to inspect function. Expected 1 got {}",
                        args.len()
                    )
                };
                let export = querier.export(name)?;
                match export {
                    wit_parser::WorldItem::Interface(_) => todo!(),
                    wit_parser::WorldItem::Function(f) => {
                        let name = &f.name;
                        let mut params = Vec::new();
                        for (param_name, param_type) in &f.params {
                            let ty = querier.display_wit_type(param_type)?;
                            params.push(format!("{param_name}: {ty}"));
                        }
                        let params = params.join(", ");
                        let rets = match &f.results {
                            wit_parser::Results::Anon(t) => match t {
                                wit_parser::Type::String => {
                                    format!(" -> {}", querier.display_wit_type(t)?)
                                }
                                _ => todo!(),
                            },
                            wit_parser::Results::Named(_) => todo!(),
                        };
                        println!("{name}: func({params}){rets}")
                    }
                    wit_parser::WorldItem::Type(_) => todo!(),
                }
            }
            Cmd::BuiltIn {
                name: "imports",
                args,
            } => {
                let &[] = args.as_slice() else {
                    bail!(
                        "wrong number of arguments to imports function. Expected 0 got {}",
                        args.len()
                    )
                };
                for (import_name, import) in querier.world().imports.iter() {
                    let import_name = querier.world_item_name(import_name)?;
                    let import_type = match import {
                        wit_parser::WorldItem::Interface(_) => "interface",
                        wit_parser::WorldItem::Function(_) => "function",
                        wit_parser::WorldItem::Type(_) => "type",
                    };
                    println!("{import_name}: {import_type}");
                }
            }
            Cmd::BuiltIn { name, args: _ } => {
                bail!("Unrecognized built-in function '{name}'")
            }
        }
        Ok(())
    }
}

fn parse_string(arg: &str) -> anyhow::Result<&str> {
    let Some(arg) = arg.strip_prefix('"') else {
        bail!("missing open quote")
    };
    let Some(arg) = arg.strip_suffix('"') else {
        bail!("missing end quote")
    };
    Ok(arg)
}

fn format_val(val: &Val) -> String {
    match val {
        Val::String(s) => format!(r#""{s}""#),
        Val::Bool(b) => b.to_string(),
        Val::U8(u) => u.to_string(),
        Val::U16(u) => u.to_string(),
        Val::U32(u) => u.to_string(),
        Val::U64(u) => u.to_string(),
        Val::S8(s) => s.to_string(),
        Val::S16(s) => s.to_string(),
        Val::S32(s) => s.to_string(),
        Val::S64(s) => s.to_string(),
        Val::Float32(f) => f.to_string(),
        Val::Float64(f) => f.to_string(),
        Val::Char(c) => c.to_string(),
        Val::Option(o) => match o.value() {
            Some(o) => format!("Some({})", format_val(o)),
            None => "None".into(),
        },
        Val::Result(r) => match r.value() {
            Ok(Some(o)) => format!("Ok({})", format_val(o)),
            Ok(None) => "Ok".to_string(),
            Err(Some(e)) => format!("Err({})", format_val(e)),
            Err(None) => "Err".to_string(),
        },
        Val::List(_) => todo!(),
        Val::Record(_) => todo!(),
        Val::Tuple(_) => todo!(),
        Val::Variant(_) => todo!(),
        Val::Enum(_) => todo!(),
        Val::Flags(_) => todo!(),
        Val::Resource(_) => todo!(),
    }
}

async fn init_store_and_instance(
    component_bytes: &[u8],
    querier: &Querier,
) -> anyhow::Result<(Store<Context>, Instance)> {
    let engine = load_engine()?;
    let component = load_component(&engine, &component_bytes)?;
    let mut linker = Linker::<Context>::new(&engine);

    if querier.imports_wasi() {
        wasmtime_wasi::preview2::command::add_to_linker(&mut linker)?;
    }
    for (import_name, import) in querier.non_wasi_imports() {
        match import {
            wit_parser::WorldItem::Function(f) if f.kind == FunctionKind::Freestanding => {
                linker
                    .root()
                    .func_new(&component, &f.name, move |_ctx, _args, _rets| {
                        print_error_prefix();
                        eprintln!("unimplemented import: {import_name}");
                        Ok(())
                    })?;
            }
            i => todo!("Implement import: {i:?}"),
        }
    }
    let pre = linker
        .instantiate_pre(&component)
        .context("could not instantiate component")?;
    let mut store = build_store(&engine);
    let instance = pre.instantiate_async(&mut store).await?;
    Ok((store, instance))
}

fn build_store(engine: &Engine) -> Store<Context> {
    let mut table = Table::new();
    let mut builder = WasiCtxBuilder::new();
    let wasi = builder.build(&mut table).unwrap();
    let context = Context::new(table, wasi);
    Store::new(engine, context)
}

struct Context {
    table: Table,
    wasi: WasiCtx,
}

impl Context {
    fn new(table: Table, wasi: WasiCtx) -> Self {
        Self { table, wasi }
    }
}

impl WasiView for Context {
    fn table(&self) -> &Table {
        &self.table
    }

    fn table_mut(&mut self) -> &mut Table {
        &mut self.table
    }

    fn ctx(&self) -> &WasiCtx {
        &self.wasi
    }

    fn ctx_mut(&mut self) -> &mut WasiCtx {
        &mut self.wasi
    }
}

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to component binary
    component: std::path::PathBuf,
}

fn load_engine() -> anyhow::Result<Engine> {
    let mut config = Config::new();
    config.wasm_component_model(true);
    config.async_support(true);

    Engine::new(&config)
}

fn load_component(engine: &Engine, component_bytes: &[u8]) -> anyhow::Result<Component> {
    Component::new(engine, component_bytes)
}
