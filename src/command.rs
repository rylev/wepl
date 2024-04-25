pub mod parser;
mod tokenizer;
use std::collections::HashMap;
use std::borrow::Cow;

use anyhow::{bail, Context as _};
use colored::Colorize;
use wasmtime::component::Val;

use self::parser::Ident;
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
    Eval(parser::Expr<'a>),
    Assign {
        ident: &'a str,
        value: parser::Expr<'a>,
    },
}

impl<'a> Cmd<'a> {
    pub fn parse(input: &'a str) -> anyhow::Result<Option<Cmd<'a>>> {
        let tokens = tokenizer::Token::tokenize(input)?;
        let line = parser::Line::parse(tokens).map_err(|e| anyhow::anyhow!("{e}"))?;
        log::debug!("Parsed line: {line:?}");
        match line {
            parser::Line::Expr(expr) => Ok(Some(Cmd::Eval(expr))),
            parser::Line::Assignment(ident, value) => Ok(Some(Cmd::Assign { ident, value })),
            parser::Line::BuiltIn(builtin) => Ok(Some(Cmd::BuiltIn {
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
        resolver: &mut WorldResolver,
        scope: &mut HashMap<String, Val>,
    ) -> anyhow::Result<bool> {
        let mut eval = Evaluator::new(runtime, resolver, scope);
        match self {
            Cmd::Eval(expr) => match expr {
                parser::Expr::Literal(l) => {
                    let val = eval.eval_literal(l, None)?;
                    println!("{}: {}", format_val(&val), val_as_type(&val));
                }
                parser::Expr::Ident(ident) => match scope.get(ident) {
                    Some(val) => {
                        println!("{}: {}", format_val(val), val_as_type(val))
                    }
                    None => {
                        anyhow::bail!("no identifier '{ident}' in scope")
                    }
                },
                parser::Expr::FunctionCall(func) => {
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
            Cmd::BuiltIn {
                name: "exports",
                args,
            } => {
                let &[] = args.as_slice() else {
                    bail!(
                        "wrong number of arguments to exports function. Expected 0 got {}",
                        args.len()
                    )
                };
                for (export_name, export) in resolver.world().exports.iter() {
                    let export_name = resolver.world_item_name(export_name);
                    if let Some(ty) = format_world_item(export, resolver) {
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
                for (import_name, import) in resolver.imports(include_wasi) {
                    let import_name = resolver.world_item_name(import_name);
                    if let Some(ty) = format_world_item(import, resolver) {
                        println!("{}: {ty}", import_name.bold());
                    }
                }
            }
            Cmd::BuiltIn { name: "type", args } => {
                match args.as_slice() {
                    &[token] => {
                        let TokenKind::Ident(name) = token.token() else {
                            bail!("unrecognized token")
                        };
                        let types = resolver.types_by_name(name);
                        for (interface, ty) in &types {
                            let typ = resolver.display_wit_type_def(ty, Expansion::Expanded(1));
                            let name = &ty.name;
                            let interface = interface.and_then(|i| resolver.interface_name(i));
                            let ident = match (interface, name) {
                                (Some(i), Some(n)) => format!("{i}#{n}: "),
                                (None, Some(n)) => format!("{n}: "),
                                _ => todo!(),
                            };
                            println!("{ident}{typ}");
                        }
                    }
                    _ => bail!(
                        "wrong number of arguments to inspect function. Expected 1 got {}",
                        args.len()
                    ),
                };
            }
            Cmd::BuiltIn {
                name: "compose",
                args,
            } => {
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
                *resolver = WorldResolver::from_bytes(runtime.component_bytes())?;
            }
            Cmd::BuiltIn { name: "link", args } => {
                let mut args = args.into_iter().collect();
                let Ok(Some(import_ident)) = Ident::try_parse(&mut args) else {
                    bail!("import_ident is not a proper item identifier");
                };
                let Ok(Some(export_ident)) = Ident::try_parse(&mut args) else {
                    bail!("export_ident is not a proper item identifier");
                };

                let Some(TokenKind::String(component)) = args.pop_front().map(|t| t.token()) else {
                    bail!("component path is not a string");
                };
                let component_bytes = std::fs::read(component)
                    .with_context(|| format!("could not read component '{component}'"))?;
                runtime.stub(&resolver, import_ident, export_ident, &component_bytes)?;
            }
            Cmd::BuiltIn {
                name: "inspect",
                args,
            } => {
                let mut args = args.into_iter().collect();
                let Ok(Some(ident)) = Ident::try_parse(&mut args) else {
                    bail!("ident is not a proper item identifier");
                };
                match ident {
                    Ident::Item(ident) => {
                        let f = resolver
                            .exported_function(ident)
                            .or_else(|| resolver.imported_function(ident));
                        match f {
                            Some(f) => println!("{}", format_function(f, resolver)),
                            None => bail!("Could not find imported or exported function '{ident}'"),
                        }
                    }
                    Ident::Interface(ident) => {
                        let i = resolver
                            .exported_interface(ident)
                            .or_else(|| resolver.imported_interface(ident));
                        match i {
                            Some(f) => println!("{}", format_interface(f, resolver)),
                            None => {
                                bail!("Could not find imported or exported interface '{ident}'")
                            }
                        }
                    }
                }
            }
            Cmd::BuiltIn {
                name: "help",
                args: _,
            } => print_help(),
            Cmd::BuiltIn {
                name: "clear",
                args: _,
            } => return Ok(true),
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

fn format_world_item(item: &wit_parser::WorldItem, resolver: &WorldResolver) -> Option<String> {
    match item {
        wit_parser::WorldItem::Function(f) => Some(format_function(f, resolver)),
        wit_parser::WorldItem::Interface(id) => {
            let interface = resolver.interface_by_id(*id).unwrap();
            if interface.functions.is_empty() {
                return None;
            }
            let output = format_interface(interface, resolver);
            Some(output)
        }
        wit_parser::WorldItem::Type(_) => None,
    }
}

fn format_interface(interface: &wit_parser::Interface, resolver: &WorldResolver) -> String {
    use std::fmt::Write;
    let mut output = String::from("{\n");
    for (_, fun) in &interface.functions {
        writeln!(
            &mut output,
            "    {}: {}",
            fun.name.bold(),
            format_function(fun, resolver)
        )
        .unwrap();
    }
    output.push('}');
    output
}

fn format_function(f: &wit_parser::Function, resolver: &WorldResolver) -> String {
    let mut params = Vec::new();
    for (param_name, param_type) in &f.params {
        let ty = resolver.display_wit_type(param_type, Expansion::Collapsed);
        params.push(format!("{param_name}: {}", ty.italic()));
    }
    let params = params.join(", ");
    let rets = match &f.results {
        wit_parser::Results::Anon(t) => {
            let t = resolver.display_wit_type(t, Expansion::Collapsed);
            format!(" -> {}", t.italic())
        }
        wit_parser::Results::Named(n) if n.is_empty() => String::new(),
        wit_parser::Results::Named(params) => {
            let params = params
                .iter()
                .map(|(name, t)| {
                    let t = resolver.display_wit_type(t, Expansion::Collapsed);
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
        Val::Option(o) => match o {
            Some(o) => format!("some({})", format_val(o)),
            None => "none".into(),
        },
        Val::Result(r) => match r {
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
                .iter()
                .map(|(key, value)| format!("{}: {}", key, format_val(value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {fields} }}")
        }
        Val::Tuple(t) => {
            let items = t
                .iter()
                .map(format_val)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({items})")
        },
        Val::Variant(..) => todo!(),
        Val::Enum(_) => todo!(),
        Val::Flags(_) => todo!(),
        Val::Resource(_) => todo!(),
    }
}

fn val_as_type(val: &Val) -> Cow<'static, str> {
    match val {
        Val::String(_) => "string".into(),
        Val::Bool(_) => "bool".into(),
        Val::U8(_) => "u8".into(),
        Val::U16(_) => "u16".into(),
        Val::U32(_) => "u32".into(),
        Val::U64(_) => "u64".into(),
        Val::S8(_) => "s8".into(),
        Val::S16(_) => "s16".into(),
        Val::S32(_) => "s32".into(),
        Val::S64(_) => "s64".into(),
        Val::Float32(_) => "float32".into(),
        Val::Float64(_) => "float64".into(),
        Val::Char(_) => "char".into(),
        Val::Option(t) => if let Some(t) = t {
                format!("option<{}>", val_as_type(t)).into()
            } else {
                "option<type-unknown-because-variant-was-none>".into()
            },
        Val::Result(_) => "result".into(),
        Val::List(t) => {
            if ! t.is_empty() {
                format!("list<{}>", val_as_type(&t[0])).into()
            } else {
                "list<type-unknown-because-list-was-empty>".into()
            }
        }
        Val::Record(_) => "record".into(),
        Val::Tuple(t) => {
            let item_types = t
                .iter()
                .map(val_as_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("tuple<{item_types}>").into()
        },
        Val::Variant(..) => "variant_specific".into(),
        Val::Enum(_) => "enum_type".into(),
        Val::Flags(_) => "flags".into(),
        Val::Resource(_) => "resource_{}".into(),
    }
}
