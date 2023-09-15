mod parser;
use std::collections::HashMap;

use anyhow::{anyhow, bail, Context as _};
use colored::Colorize;
use log::debug;
use wasmtime::component::{self, Record, Val};

use crate::wit::Expansion;

use super::runtime::Runtime;
use super::wit::Querier;

pub enum Cmd<'a> {
    BuiltIn {
        name: &'a str,
        args: Vec<&'a str>,
    },
    Eval(parser::Expr<'a>),
    Assign {
        ident: &'a str,
        value: parser::Expr<'a>,
    },
}

impl<'a> Cmd<'a> {
    pub fn parse(s: &'a str) -> anyhow::Result<Option<Cmd<'a>>> {
        let s = s.trim();
        if s.is_empty() {
            return Ok(None);
        }

        // try to parse a function
        let (rest, line) = parser::Line::parse(s).map_err(|e| anyhow!("{e}"))?;
        if !rest.is_empty() {
            anyhow::bail!("unexpected end of input: '{rest}'");
        }
        log::debug!("Parsed line: {line:?}");
        match line {
            parser::Line::Expr(expr) => Ok(Some(Cmd::Eval(expr))),
            parser::Line::Assignment(ident, value) => Ok(Some(Cmd::Assign { ident, value })),
            parser::Line::Builtin(name, args) => Ok(Some(Cmd::BuiltIn { name, args })),
        }
    }

    /// Run the command
    ///
    /// Returns `Ok(true)` if the screen should be cleared
    pub fn run(
        self,
        runtime: &mut Runtime,
        querier: &mut Querier,
        scope: &mut HashMap<String, Val>,
    ) -> anyhow::Result<bool> {
        match self {
            Cmd::Eval(expr) => match expr {
                parser::Expr::Literal(l) => {
                    let val = literal_to_val(runtime, querier, scope, l, None)?;
                    println!("{}: {}", format_val(&val), val_as_type(&val));
                }
                parser::Expr::Ident(ident) => match scope.get(ident) {
                    Some(val) => {
                        println!("{}: {}", format_val(val), val_as_type(val))
                    }
                    None => {
                        let item = querier.export(ident).or_else(|| querier.import(ident));
                        match item {
                            Some(item) => {
                                let typ = format_world_item(item, querier);
                                println!("{ident}: {typ}");
                            }
                            None => {
                                anyhow::bail!("no identifier '{ident}' in scope")
                            }
                        }
                    }
                },
                parser::Expr::FunctionCall(name, args) => {
                    let results = call_func(runtime, querier, &*scope, name, args)?;
                    println!(
                        "{}",
                        results
                            .into_iter()
                            .map(|v| format!("{}: {}", format_val(&v), val_as_type(&v)))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            },
            Cmd::Assign { ident, value } => {
                let val = eval(runtime, querier, scope, value, None)?;
                println!("{}: {}", ident, val_as_type(&val));
                scope.insert(ident.to_owned(), val);
            }
            Cmd::BuiltIn {
                name: "exports",
                args,
            } => {
                let &[] = args.as_slice() else {
                    bail!(
                        "wrong number of arguments to imports function. Expected 0 got {}",
                        args.len()
                    )
                };
                for (export_name, export) in querier.world().exports.iter() {
                    let export_name = querier.world_item_name(export_name);
                    let typ = format_world_item(export, querier);
                    println!("{export_name}: {typ}");
                }
            }
            Cmd::BuiltIn {
                name: "imports",
                args,
            } => {
                let include_wasi = match args.as_slice() {
                    [] => true,
                    ["--no-wasi"] => false,
                    [flag] => {
                        bail!("unrecorgnized flag for imports builtin '{}'", flag)
                    }
                    _ => {
                        bail!(
                            "wrong number of arguments to imports function. Expected 0 got {}",
                            args.len()
                        )
                    }
                };
                for (import_name, import) in querier.imports(include_wasi) {
                    let import_name = querier.world_item_name(import_name);
                    let typ = format_world_item(import, querier);
                    println!("{}: {typ}", import_name.bold());
                }
            }
            Cmd::BuiltIn { name: "type", args } => {
                match args.as_slice() {
                    &[name] => {
                        let types = querier.types_by_name(name);
                        for (interface, ty) in &types {
                            let typ = querier.display_wit_type_def(ty, Expansion::Expanded(1));
                            let name = &ty.name;
                            let interface = interface.and_then(|i| querier.interface_name(i));
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
                let &[path] = args.as_slice() else {
                    bail!(
                        "wrong number of arguments to compose function. Expected 1 got {}",
                        args.len()
                    )
                };
                let adapter =
                    std::fs::read(path).context("could not read path to adapter module")?;
                runtime.compose(&adapter)?;
                *querier = Querier::from_bytes(runtime.component_bytes())?;
            }
            Cmd::BuiltIn { name: "link", args } => {
                let &[func_name, component] = args.as_slice() else {
                    bail!("wrong number of arguments. Expected 2 got {}", args.len())
                };

                let component_bytes = std::fs::read(component)
                    .with_context(|| format!("could not read component '{component}'"))?;
                querier.check_dynamic_import(func_name, &component_bytes)?;
                runtime.stub_function(func_name.into(), &component_bytes)?;
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

fn eval(
    runtime: &mut Runtime,
    querier: &Querier,
    scope: &HashMap<String, Val>,
    expr: parser::Expr<'_>,
    preferred_type: Option<&component::Type>,
) -> anyhow::Result<Val> {
    match expr {
        parser::Expr::Literal(l) => literal_to_val(runtime, querier, scope, l, preferred_type),
        parser::Expr::Ident(ident) => resolve_ident(ident, preferred_type, scope),
        parser::Expr::FunctionCall(name, mut args) => {
            debug!(
                "Checking for type constructor for {name} #args={} preferred_type={preferred_type:?}",
                args.len()
            );
            // If the preferred type has some sort of type constructor, try that first
            match preferred_type {
                Some(component::Type::Option(o)) if name == "some" && args.len() == 1 => {
                    let val = eval(runtime, querier, scope, args.remove(0), Some(&o.ty()))?;
                    return o.new_val(Some(val));
                }
                Some(component::Type::Result(r)) if args.len() == 1 => {
                    if let Some(ok) = r.ok() {
                        if name == "ok" {
                            let val = eval(runtime, querier, scope, args.remove(0), Some(&ok))?;
                            return r.new_val(Ok(Some(val)));
                        }
                    }
                    if let Some(err) = r.err() {
                        if name == "err" {
                            let val = eval(runtime, querier, scope, args.remove(0), Some(&err))?;
                            return r.new_val(Err(Some(val)));
                        }
                    }
                }
                _ => {}
            }

            let mut results = call_func(runtime, querier, scope, name, args)?;
            if results.len() != 1 {
                bail!(
                    "Expected function '{name}'to return one result but got {}",
                    results.len()
                )
            }
            Ok(results.remove(0))
        }
    }
}

fn lookup_in_scope(scope: &HashMap<String, Val>, ident: &str) -> anyhow::Result<Val> {
    scope
        .get(ident)
        .with_context(|| format!("no identifier '{ident}' in scope"))
        .cloned()
}

fn call_func(
    runtime: &mut Runtime,
    querier: &Querier,
    scope: &HashMap<String, Val>,
    name: &str,
    args: Vec<parser::Expr<'_>>,
) -> anyhow::Result<Vec<Val>> {
    log::debug!("Calling function: {name} with args: {args:?}");
    let func_def = querier
        .exported_function(name)
        .with_context(|| format!("no export with name '{name}'"))?;
    let mut evaled_args = Vec::with_capacity(func_def.params.len());
    if func_def.params.len() != args.len() {
        bail!(
            "tried to call a function that has {} params with {} args",
            func_def.params.len(),
            args.len()
        )
    }
    let func = runtime.get_func(name)?;
    let names = func_def.params.iter().map(|(n, _)| n);
    let types = func.params(&mut runtime.store);
    for (param_name, (param_type, arg)) in names.zip(types.iter().zip(args)) {
        let evaled_arg = eval(runtime, querier, scope, arg, Some(param_type))
            .map_err(|e| anyhow!("argument '{param_name}': {e}"))?;
        evaled_args.push(evaled_arg);
    }
    let results = runtime.call_func(&func_def.name, &evaled_args, func_def.results.len())?;
    Ok(results)
}

fn literal_to_val(
    runtime: &mut Runtime,
    querier: &Querier,
    scope: &HashMap<String, Val>,
    literal: parser::Literal<'_>,
    preferred_type: Option<&component::Type>,
) -> anyhow::Result<Val> {
    match literal {
        parser::Literal::Record(mut r) => {
            let ty = match preferred_type {
                Some(component::Type::Record(r)) => r,
                Some(t) => bail!("expected record got {t:?}"),
                None => bail!("cannot determine type of record"),
            };
            let mut values = Vec::new();
            let types = ty
                .fields()
                .enumerate()
                .map(|(index, field)| (field.name, index))
                .collect::<HashMap<_, _>>();
            // Sort the fields since wasmtime expects the fields to be in the defined order
            r.fields
                .sort_by(|(f1, _), (f2, _)| types.get(f1).unwrap().cmp(&types.get(f2).unwrap()));

            for ((name, field_expr), field_type) in r.fields.into_iter().zip(ty.fields()) {
                values.push((
                    name,
                    eval(runtime, querier, scope, field_expr, Some(&field_type.ty))?,
                ));
            }
            Ok(Val::Record(Record::new(ty, values)?))
        }
        parser::Literal::String(s) => {
            let val = Val::String(s.to_owned().into());
            match preferred_type {
                Some(component::Type::Result(r)) => r.new_val(match (r.ok(), r.err()) {
                    (Some(_), _) => Ok(Some(val)),
                    (_, Some(_)) => Err(Some(val)),
                    (None, None) => return Ok(val),
                }),
                _ => Ok(val),
            }
        }
        parser::Literal::Num(n) => match preferred_type {
            Some(component::Type::U8) => Ok(Val::U8(n.try_into()?)),
            _ => Ok(Val::S32(n.try_into()?)),
        },
    }
}

fn resolve_ident(
    ident: &str,
    preferred_type: Option<&component::Type>,
    scope: &HashMap<String, Val>,
) -> Result<Val, anyhow::Error> {
    debug!("Resolving ident {ident} with preferred type {preferred_type:?}");
    match preferred_type {
        Some(t) => match t {
            component::Type::Bool if ident == "true" => Ok(Val::Bool(true)),
            component::Type::Bool if ident == "false" => Ok(Val::Bool(false)),
            component::Type::Enum(e) => e.new_val(ident),
            component::Type::Variant(v) => match lookup_in_scope(scope, ident) {
                Ok(v) => Ok(v),
                Err(_) => v.new_val(ident, None),
            },
            component::Type::Option(o) if ident == "none" => o.new_val(None),
            component::Type::Option(o) => {
                o.new_val(Some(resolve_ident(ident, Some(&o.ty()), scope)?))
            }
            component::Type::Result(r) => r.new_val(match (r.ok(), r.err()) {
                (Some(o), _) => Ok(Some(resolve_ident(ident, Some(&o), scope)?)),
                (None, None) if ident == "ok" => Ok(None),
                (None, None) if ident == "err" => Err(None),
                _ => return lookup_in_scope(scope, ident),
            }),
            component::Type::Bool
            | component::Type::U8
            | component::Type::U16
            | component::Type::U32
            | component::Type::U64
            | component::Type::S8
            | component::Type::S16
            | component::Type::S32
            | component::Type::S64 => lookup_in_scope(scope, ident),
            t => todo!("handle ident '{ident}' with type {t:?}"),
        },
        None => lookup_in_scope(scope, ident),
    }
}

fn format_world_item(item: &wit_parser::WorldItem, querier: &Querier) -> String {
    match item {
        wit_parser::WorldItem::Function(f) => format_function(f, querier),
        wit_parser::WorldItem::Interface(id) => {
            use std::fmt::Write;
            let interface = querier.interface(*id).unwrap();
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
        wit_parser::WorldItem::Type(_) => "type".into(),
    }
}

fn format_function(f: &wit_parser::Function, querier: &Querier) -> String {
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
        wit_parser::Results::Named(_) => todo!(),
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
        Val::List(_) => todo!(),
        Val::Record(_) => todo!(),
        Val::Tuple(_) => todo!(),
        Val::Variant(_) => todo!(),
        Val::Enum(_) => todo!(),
        Val::Flags(_) => todo!(),
        Val::Resource(_) => todo!(),
    }
}
