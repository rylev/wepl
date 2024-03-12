pub mod alt_parser;
pub mod parser;
mod tokenizer;
use std::collections::HashMap;

use anyhow::{bail, Context as _};
use colored::Colorize;
use wasmtime::component::Val;

use self::alt_parser::ItemIdent;
use self::tokenizer::TokenKind;

use super::runtime::Runtime;
use super::wit::WorldResolver;
use crate::evaluator::Evaluator;
use crate::wit::Expansion;

pub enum Cmd<'a> {
    BuiltIn {
        name: &'a str,
        args: Vec<tokenizer::Token<'a>>,
    },
    Eval(alt_parser::Expr<'a>),
    Assign {
        ident: &'a str,
        value: alt_parser::Expr<'a>,
    },
}

impl<'a> Cmd<'a> {
    pub fn parse(input: &'a str) -> anyhow::Result<Option<Cmd<'a>>> {
        let tokens = tokenizer::Token::tokenize(input)?;
        let line = alt_parser::Line::parse(tokens)
            .expect("TODO: using ? here leads to borrow checker errors");
        log::debug!("Parsed line: {line:?}");
        match line {
            alt_parser::Line::Expr(expr) => Ok(Some(Cmd::Eval(expr))),
            alt_parser::Line::Assignment(ident, value) => Ok(Some(Cmd::Assign { ident, value })),
            alt_parser::Line::BuiltIn(builtin) => Ok(Some(Cmd::BuiltIn {
                name: builtin.name,
                args: builtin.rest,
            })),
        }
    }

    /// Run the command
    ///
    /// Returns `Ok(true)` if the screen should be cleared
    pub fn run(
        self,
        runtime: &mut Runtime,
        querier: &mut WorldResolver,
        scope: &mut HashMap<String, Val>,
    ) -> anyhow::Result<bool> {
        let mut eval = Evaluator::new(runtime, querier, scope);
        match self {
            Cmd::Eval(expr) => match expr {
                alt_parser::Expr::Literal(l) => {
                    let val = eval.eval_literal(l, None)?;
                    println!("{}: {}", format_val(&val), val_as_type(&val));
                }
                alt_parser::Expr::Ident(ident) => match scope.get(ident) {
                    Some(val) => {
                        println!("{}: {}", format_val(val), val_as_type(val))
                    }
                    None => {
                        anyhow::bail!("no identifier '{ident}' in scope")
                    }
                },
                alt_parser::Expr::FunctionCall(func) => {
                    let results = eval.call_func(func.ident, func.args)?;
                    println!(
                        "{}",
                        results
                            .into_iter()
                            .map(|v| format!("{}", format_val(&v)))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                }
            },
            Cmd::Assign { ident, value } => {
                let val = eval.eval(value, None)?;
                println!("{}: {}", ident, val_as_type(&val));
                scope.insert(ident.into(), val);
            }
            Cmd::BuiltIn { name, args } if name == "exports" => {
                let &[] = args.as_slice() else {
                    bail!(
                        "wrong number of arguments to exports function. Expected 0 got {}",
                        args.len()
                    )
                };
                for (export_name, export) in querier.world().exports.iter() {
                    let export_name = querier.world_item_name(export_name);
                    if let Some(ty) = format_world_item(export, querier) {
                        println!("{}: {ty}", export_name.bold());
                    }
                }
            }
            Cmd::BuiltIn {
                name: "imports",
                args,
            } => {
                let include_wasi = match args.as_slice() {
                    [] => true,
                    [t] => match t.token() {
                        TokenKind::Flag("no-wasi") => false,
                        TokenKind::Flag(flag) => {
                            bail!("unrecognized flag for imports builtin '{flag}'")
                        }
                        _ => bail!("unrecognized token {}", t.input.str),
                    },
                    _ => {
                        bail!(
                            "wrong number of arguments to imports function. Expected 1 got {}",
                            args.len()
                        )
                    }
                };
                for (import_name, import) in querier.imports(include_wasi) {
                    let import_name = querier.world_item_name(import_name);
                    if let Some(ty) = format_world_item(import, querier) {
                        println!("{}: {ty}", import_name.bold());
                    }
                }
            }
            // Cmd::BuiltIn { name: "type", args } => {
            //     match args.as_slice() {
            //         &[name] => {
            //             let types = querier.types_by_name(&*name);
            //             for (interface, ty) in &types {
            //                 let typ = querier.display_wit_type_def(ty, Expansion::Expanded(1));
            //                 let name = &ty.name;
            //                 let interface = interface.and_then(|i| querier.interface_name(i));
            //                 let ident = match (interface, name) {
            //                     (Some(i), Some(n)) => format!("{i}#{n}: "),
            //                     (None, Some(n)) => format!("{n}: "),
            //                     _ => todo!(),
            //                 };
            //                 println!("{ident}{typ}");
            //             }
            //         }
            //         _ => bail!(
            //             "wrong number of arguments to inspect function. Expected 1 got {}",
            //             args.len()
            //         ),
            //     };
            // }
            Cmd::BuiltIn { name, args } if name == "compose" => {
                let &[token] = args.as_slice() else {
                    bail!(
                        "wrong number of arguments to compose function. Expected 1 got {}",
                        args.len()
                    )
                };
                let TokenKind::String(path) = token.token() else {
                    bail!("unrecognized token {}", token.input.str);
                };
                let adapter =
                    std::fs::read(&*path).context("could not read path to adapter module")?;
                runtime.compose(&adapter)?;
                *querier = WorldResolver::from_bytes(runtime.component_bytes())?;
            }
            Cmd::BuiltIn { name, args } if name == "link" => {
                let mut args = args.into_iter().collect();
                let Ok(Some(import_ident)) = ItemIdent::try_parse(&mut args) else {
                    bail!("import_ident is not a proper item identifier");
                };
                let Ok(Some(export_ident)) = ItemIdent::try_parse(&mut args) else {
                    bail!("export_ident is not a proper item identifier");
                };

                let Some(TokenKind::String(component)) = args.pop_front().map(|t| t.token()) else {
                    bail!("TODO");
                };
                let component_bytes = std::fs::read(component)
                    .with_context(|| format!("could not read component '{component}'"))?;
                runtime.stub(&querier, import_ident, export_ident, &component_bytes)?;
            }
            Cmd::BuiltIn {
                name: "inspect",
                args,
            } => {
                let mut args = args.into_iter().collect();
                let Ok(Some(ident)) = ItemIdent::try_parse(&mut args) else {
                    bail!("ident is not a proper item identifier");
                };
                match ident {
                    ItemIdent::Item(ident) => {
                        let f = querier
                            .exported_function(ident)
                            .or_else(|| querier.imported_function(ident));
                        match f {
                            Some(f) => println!("{}", format_function(f, querier)),
                            None => bail!("Could not find imported or exported function '{ident}'"),
                        }
                    }
                    ItemIdent::Interface(ident) => {
                        let i = querier
                            .exported_interface(ident)
                            .or_else(|| querier.imported_interface(ident));
                        match i {
                            Some(f) => println!("{}", format_interface(f, querier)),
                            None => {
                                bail!("Could not find imported or exported interface '{ident}'")
                            }
                        }
                    }
                }
            }
            Cmd::BuiltIn { name, args: _ } if name == "help" => print_help(),
            Cmd::BuiltIn { name, args: _ } if name == "clear" => return Ok(true),
            Cmd::BuiltIn { name, args: _ } => {
                bail!("Unrecognized built-in function '{name}'")
            }
        }
        Ok(false)
    }
}

fn print_help() {
    println!("Calling imports can be done like so:

> my-func(my-arg)

Variables can be saved as well:

> my-var = my-func(my-arg)

There are also builtin functions that can be called with a preceding '.'. Supported functions include:
  .imports                  print a list of all the component's imports
  .exports                  print a list of all the component's exports
  .link $function $wasm     satisfy the imported function `$func` with an export from the wasm component `$wasm`
  .compose $adapter         satisfy imports with the supplied adapter module (e.g., to compose with WASI-Virt adapter)
  .inspect $item            inspect an item `$item` in scope (`?` is alias for this built-in)")
}

fn format_world_item(item: &wit_parser::WorldItem, querier: &WorldResolver) -> Option<String> {
    match item {
        wit_parser::WorldItem::Function(f) => Some(format_function(f, querier)),
        wit_parser::WorldItem::Interface(id) => {
            let interface = querier.interface_by_id(*id).unwrap();
            if interface.functions.is_empty() {
                return None;
            }
            let output = format_interface(interface, querier);
            Some(output)
        }
        wit_parser::WorldItem::Type(_) => None,
    }
}

fn format_interface(interface: &wit_parser::Interface, querier: &WorldResolver) -> String {
    use std::fmt::Write;
    let mut output = String::from("{\n");
    for (_, fun) in &interface.functions {
        writeln!(
            &mut output,
            "    {}: {}",
            fun.name.bold(),
            format_function(fun, querier)
        )
        .unwrap();
    }
    output.push('}');
    output
}

fn format_function(f: &wit_parser::Function, querier: &WorldResolver) -> String {
    let mut params = Vec::new();
    for (param_name, param_type) in &f.params {
        let ty = querier.display_wit_type(param_type, Expansion::Collapsed);
        params.push(format!("{param_name}: {}", ty.italic()));
    }
    let params = params.join(", ");
    let rets = match &f.results {
        wit_parser::Results::Anon(t) => {
            let t = querier.display_wit_type(t, Expansion::Collapsed);
            format!(" -> {}", t.italic())
        }
        wit_parser::Results::Named(n) if n.is_empty() => String::new(),
        wit_parser::Results::Named(params) => {
            let params = params
                .iter()
                .map(|(name, t)| {
                    let t = querier.display_wit_type(t, Expansion::Collapsed);
                    format!("{name}: {t}")
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!(" -> {params}")
        }
    };
    format!("func({params}){rets}")
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
            Some(o) => format!("some({})", format_val(o)),
            None => "none".into(),
        },
        Val::Result(r) => match r.value() {
            Ok(Some(o)) => format!("ok({})", format_val(o)),
            Ok(None) => "ok".to_string(),
            Err(Some(e)) => format!("err({})", format_val(e)),
            Err(None) => "err".to_string(),
        },
        Val::List(l) => {
            let items = l
                .iter()
                .map(|value| format!("{}", format_val(value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{items}]")
        }
        Val::Record(r) => {
            let fields = r
                .fields()
                .map(|(key, value)| format!("{}: {}", key, format_val(value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {fields} }}")
        }
        Val::Tuple(_) => todo!(),
        Val::Variant(_) => todo!(),
        Val::Enum(_) => todo!(),
        Val::Flags(_) => todo!(),
        Val::Resource(_) => todo!(),
    }
}

fn val_as_type(val: &Val) -> &'static str {
    match val {
        Val::String(_) => "string",
        Val::Bool(_) => "bool",
        Val::U8(_) => "u8",
        Val::U16(_) => "u16",
        Val::U32(_) => "u32",
        Val::U64(_) => "u64",
        Val::S8(_) => "s8",
        Val::S16(_) => "s16",
        Val::S32(_) => "s32",
        Val::S64(_) => "s64",
        Val::Float32(_) => "float32",
        Val::Float64(_) => "float64",
        Val::Char(_) => "char",
        Val::Option(_) => "option",
        Val::Result(_) => "result",
        Val::List(_) => "list",
        Val::Record(_) => "record",
        Val::Tuple(_) => todo!(),
        Val::Variant(_) => todo!(),
        Val::Enum(_) => todo!(),
        Val::Flags(_) => todo!(),
        Val::Resource(_) => todo!(),
    }
}
