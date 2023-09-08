use std::borrow::Cow;

use anyhow::{anyhow, bail, ensure, Context as _};
use clap::Parser;
use rustyline::error::ReadlineError;
use wasmtime::{
    component::{Component, Instance, Linker, Val},
    Config, Engine, Store,
};
use wasmtime_wasi::preview2::{Table, WasiCtx, WasiCtxBuilder, WasiView};
use wit_component::DecodedWasm;
use wit_parser::{Resolve, World, WorldKey};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    env_logger::init();

    let cli = Cli::parse();
    let component_bytes = std::fs::read(cli.component)?;
    let (mut store, instance) = init_store_and_instance(&component_bytes).await?;

    let (resolve, world) = match wit_component::decode(&component_bytes)
        .context("could not decode given file as a WebAssembly component")?
    {
        DecodedWasm::Component(r, w) => (r, w),
        _ => bail!("found wit package instead of the expect WebAssembly component"),
    };
    let world = resolve.worlds.get(world).context("world not found")?;

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
                        if let Err(e) = cmd.run(&resolve, &instance, &mut store, &world).await {
                            eprintln!("Error executing command: {e}");
                        }
                    }
                    Err(e) => {
                        eprintln!("Error parsing input: {e}");
                        continue;
                    }
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

enum Cmd<'a> {
    ListExports,
    InspectExport { name: &'a str },
    CallFunction { name: &'a str, args: Vec<&'a str> },
}

impl<'a> Cmd<'a> {
    fn parse(s: &'a str) -> anyhow::Result<Self> {
        let s = s.trim();

        if let Some(command) = s.strip_prefix('.') {
            // Commands
            match command {
                "exports" => return Ok(Cmd::ListExports),
                _ => bail!("unrecognized command: {command}"),
            }
        }

        if let Some(export) = s.strip_prefix('?') {
            let export = export.trim();
            if export.contains(char::is_whitespace) {
                bail!("invalid export name '{export}'. Identifiers can't contain whitespace");
            }
            return Ok(Self::InspectExport { name: export });
        }

        // try to parse a function
        let open_paren = s.find('(').context("no open parenthesis present")?;
        let name = &s[..open_paren];
        ensure!(&s[s.len() - 1..] == ")", "Missing ending paren");
        let arg_list = &s[open_paren + 1..s.len() - 1];
        let args = arg_list.split(", ").map(|s| s.trim()).collect();

        Ok(Cmd::CallFunction { name, args })
    }

    async fn run(
        &self,
        resolve: &Resolve,
        instance: &Instance,
        store: &mut Store<Context>,
        world: &World,
    ) -> anyhow::Result<()> {
        match self {
            Cmd::CallFunction { name, args } => {
                log::debug!("Calling function: {name}");
                let func_def = world
                    .exports
                    .iter()
                    .find_map(|(_, kind)| match kind {
                        wit_parser::WorldItem::Function(f) if &f.name == name => Some(f),
                        _ => None,
                    })
                    .with_context(|| format!("Unrecognized function '{name}'"))?;
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
                            display_wit_type(param_type, resolve)
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
            Cmd::ListExports => {
                for (export_name, export) in world.exports.iter() {
                    let WorldKey::Name(export_name) = &export_name else {
                        continue;
                    };
                    let export_type = match export {
                        wit_parser::WorldItem::Interface(_) => "interface",
                        wit_parser::WorldItem::Function(_) => "function",
                        wit_parser::WorldItem::Type(_) => "type",
                    };
                    println!("{export_name}: {export_type}");
                }
            }
            Cmd::InspectExport { name } => {
                let export = world
                    .exports
                    .iter()
                    .find_map(|(export_name, export)| {
                        let WorldKey::Name(n) = export_name else {
                            return None;
                        };
                        (n == name).then_some(export)
                    })
                    .with_context(|| format!("no export with name '{name}'"))?;
                match export {
                    wit_parser::WorldItem::Interface(_) => todo!(),
                    wit_parser::WorldItem::Function(f) => {
                        let name = &f.name;
                        let mut params = Vec::new();
                        for (param_name, param_type) in &f.params {
                            let ty = display_wit_type(param_type, resolve)?;
                            params.push(format!("{param_name}: {ty}"));
                        }
                        let params = params.join(", ");
                        let rets = match &f.results {
                            wit_parser::Results::Anon(t) => match t {
                                wit_parser::Type::String => {
                                    format!(" -> {}", display_wit_type(t, resolve)?)
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
        }
        Ok(())
    }
}

fn display_wit_type<'a>(
    param_type: &wit_parser::Type,
    resolve: &Resolve,
) -> anyhow::Result<Cow<'a, str>> {
    let str = match param_type {
        wit_parser::Type::Bool => "bool",
        wit_parser::Type::U8 => "u8",
        wit_parser::Type::U16 => "u16",
        wit_parser::Type::U32 => "u32",
        wit_parser::Type::U64 => "u64",
        wit_parser::Type::S8 => "s8",
        wit_parser::Type::S16 => "s16",
        wit_parser::Type::S32 => "s32",
        wit_parser::Type::S64 => "s64",
        wit_parser::Type::Float32 => "float32",
        wit_parser::Type::Float64 => "float64",
        wit_parser::Type::String => "string",
        wit_parser::Type::Char => "char",
        wit_parser::Type::Id(id) => {
            let typ = resolve.types.get(*id).context("missing type definition")?;
            let name = typ.name.clone().context("type does not have a name")?;
            return Ok(Cow::Owned(name));
        }
    };
    Ok(Cow::Borrowed(str))
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
) -> anyhow::Result<(Store<Context>, Instance)> {
    let engine = load_engine()?;
    let component = load_component(&engine, &component_bytes)?;
    let mut linker = Linker::<Context>::new(&engine);

    wasmtime_wasi::preview2::command::add_to_linker(&mut linker)?;
    let pre = linker.instantiate_pre(&component)?;
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
