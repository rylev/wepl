mod parser;
use std::collections::HashMap;

use anyhow::{anyhow, bail, ensure, Context as _};
use wasmtime::component::Val;

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
    pub fn parse(s: &'a str) -> anyhow::Result<Cmd<'a>> {
        let s = s.trim();

        // try to parse a function
        let (_, line) = parser::Line::parse(s).map_err(|e| anyhow!("{e}"))?;
        log::debug!("Parsed line: {line:?}");
        match line {
            parser::Line::Expr(expr) => Ok(Cmd::Eval(expr)),
            parser::Line::Assignment(ident, value) => Ok(Cmd::Assign { ident, value }),
            parser::Line::Builtin(name, args) => Ok(Cmd::BuiltIn { name, args }),
        }
    }

    /// Run the command
    ///
    /// Returns `Ok(true)` if the screen should be cleared
    pub fn run(
        self,
        runtime: &mut Runtime,
        querier: &Querier,
        scope: &mut HashMap<String, Val>,
    ) -> anyhow::Result<bool> {
        match self {
            Cmd::Eval(expr) => match expr {
                parser::Expr::Literal(l) => {
                    let val = literal_to_val(l, None);
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
                    let export_name = querier.world_item_name(export_name)?;
                    let typ = format_world_item(export, querier);
                    println!("{export_name}: {typ}");
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
                    let typ = format_world_item(import, querier);
                    println!("{import_name}: {typ}");
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
                let export = querier
                    .export(name)
                    .with_context(|| format!("no export with name '{name}'"))?;
                let typ = format_world_item(export, querier);
                println!("{name}: {typ}");
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

fn eval<'a>(
    runtime: &mut Runtime,
    querier: &Querier,
    scope: &HashMap<String, Val>,
    expr: parser::Expr<'a>,
    preferred_type: Option<&wit_parser::Type>,
) -> anyhow::Result<Val> {
    match expr {
        parser::Expr::Literal(l) => Ok(literal_to_val(l, preferred_type)),
        parser::Expr::Ident(n) => lookup_in_scope(scope, n),
        parser::Expr::FunctionCall(name, args) => {
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

fn call_func<'a>(
    runtime: &mut Runtime,
    querier: &Querier,
    scope: &HashMap<String, Val>,
    name: &str,
    args: Vec<parser::Expr<'a>>,
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
    for ((param_name, param_type), arg) in func_def.params.iter().zip(args) {
        let evaled_arg = eval(runtime, querier, scope, arg, Some(param_type))?;
        match param_type {
            wit_parser::Type::Bool => todo!(),
            wit_parser::Type::U8 => {
                ensure!(
                    matches!(evaled_arg, Val::U8(_)),
                    "arg '{}' type mismatch: expected value of type u8 got {}",
                    param_name,
                    querier.display_wit_type(param_type)
                );
            }
            wit_parser::Type::U16 => todo!(),
            wit_parser::Type::U32 => todo!(),
            wit_parser::Type::U64 => todo!(),
            wit_parser::Type::S8 => todo!(),
            wit_parser::Type::S16 => todo!(),
            wit_parser::Type::S32 => todo!(),
            wit_parser::Type::S64 => todo!(),
            wit_parser::Type::Float32 => todo!(),
            wit_parser::Type::Float64 => todo!(),
            wit_parser::Type::Char => todo!(),
            wit_parser::Type::String => {
                ensure!(
                    matches!(evaled_arg, Val::String(_)),
                    "arg '{}' type mismatch: expected value of type {} got {}",
                    param_name,
                    querier.display_wit_type(param_type),
                    val_as_type(&evaled_arg)
                );
            }
            wit_parser::Type::Id(_) => todo!(),
        }
        evaled_args.push(evaled_arg);
    }
    let results = runtime.call_func(&func_def.name, &evaled_args, func_def.results.len())?;
    Ok(results)
}

fn literal_to_val(l: parser::Literal<'_>, preferred_type: Option<&wit_parser::Type>) -> Val {
    match l {
        parser::Literal::String(s) => Val::String(s.to_owned().into()),
        parser::Literal::Num(n) => match preferred_type {
            Some(wit_parser::Type::U8) => Val::U8(n.try_into().unwrap()),
            _ => Val::S32(n.try_into().unwrap()),
        },
    }
}

fn format_world_item(item: &wit_parser::WorldItem, querier: &Querier) -> String {
    match item {
        wit_parser::WorldItem::Function(f) => {
            let mut params = Vec::new();
            for (param_name, param_type) in &f.params {
                let ty = querier.display_wit_type(param_type);
                params.push(format!("{param_name}: {ty}"));
            }
            let params = params.join(", ");
            let rets = match &f.results {
                wit_parser::Results::Anon(t) => {
                    let t = querier.display_wit_type(t);
                    format!(" -> {t}")
                }
                wit_parser::Results::Named(_) => todo!(),
            };
            format!("func({params}){rets}")
        }
        wit_parser::WorldItem::Interface(_) => "interface".into(),
        wit_parser::WorldItem::Type(_) => "type".into(),
    }
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
