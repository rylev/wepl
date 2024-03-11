use std::collections::HashMap;

use anyhow::{bail, Context};
use wasmtime::component::{self, List, Record, Val};

use crate::{
    command::parser::{self, FunctionIdent},
    runtime::Runtime,
    wit::WorldResolver,
};

pub struct Evaluator<'a> {
    runtime: &'a mut Runtime,
    querier: &'a WorldResolver,
    scope: &'a HashMap<String, Val>,
}

impl<'a> Evaluator<'a> {
    /// Create a new evaluator
    pub fn new(
        runtime: &'a mut Runtime,
        querier: &'a WorldResolver,
        scope: &'a HashMap<String, Val>,
    ) -> Self {
        Self {
            runtime,
            querier,
            scope,
        }
    }

    /// Evaluate the expression with the provided type hint
    pub fn eval(
        &mut self,
        expr: parser::Expr<'_>,
        type_hint: Option<&component::Type>,
    ) -> anyhow::Result<Val> {
        match expr {
            parser::Expr::Literal(l) => self.eval_literal(l, type_hint),
            parser::Expr::Ident(ident) => self.resolve_ident(&*ident, type_hint),
            parser::Expr::FunctionCall(ident, mut args) => {
                log::debug!(
                    "Checking for type constructor for {ident} #args={} type_hint={type_hint:?}",
                    args.len()
                );
                // If the preferred type has some sort of type constructor, try that first
                match type_hint {
                    Some(component::Type::Option(o))
                        if ident.interface.is_none()
                            && ident.function == "some"
                            && args.len() == 1 =>
                    {
                        let val = self.eval(args.remove(0), Some(&o.ty()))?;
                        return o.new_val(Some(val));
                    }
                    Some(component::Type::Result(r)) if args.len() == 1 => {
                        if let Some(ok) = r.ok() {
                            if ident.interface.is_none() && ident.function == "ok" {
                                let val = self.eval(args.remove(0), Some(&ok))?;
                                return r.new_val(Ok(Some(val)));
                            }
                        }
                        if let Some(err) = r.err() {
                            if ident.interface.is_none() && ident.function == "err" {
                                let val = self.eval(args.remove(0), Some(&err))?;
                                return r.new_val(Err(Some(val)));
                            }
                        }
                    }
                    _ => {}
                }

                let mut results = self.call_func(ident, args)?;
                if results.len() != 1 {
                    bail!(
                        "Expected function '{ident}' to return one result but got {}",
                        results.len()
                    )
                }
                Ok(results.remove(0))
            }
        }
    }

    /// Call the function
    pub fn call_func(
        &mut self,
        ident: FunctionIdent,
        args: Vec<parser::Expr<'_>>,
    ) -> anyhow::Result<Vec<Val>> {
        log::debug!("Calling function: {ident} with args: {args:?}");
        let func_def = self
            .querier
            .exported_function(ident)
            .with_context(|| format!("no function with name '{ident}'"))?;
        let mut evaled_args = Vec::with_capacity(func_def.params.len());
        if func_def.params.len() != args.len() {
            bail!(
                "tried to call a function that has {} params with {} args",
                func_def.params.len(),
                args.len()
            )
        }
        let func = self.runtime.get_func(ident)?;
        let names = func_def.params.iter().map(|(n, _)| n);
        let types = func.params(&mut self.runtime.store);
        for (param_name, (param_type, arg)) in names.zip(types.iter().zip(args)) {
            let evaled_arg = self
                .eval(arg, Some(param_type))
                .map_err(|e| anyhow::anyhow!("argument '{param_name}': {e}"))?;
            evaled_args.push(evaled_arg);
        }
        let results = self
            .runtime
            .call_func(func, &evaled_args, func_def.results.len())?;
        Ok(results)
    }

    /// Evaluate a literal using the provided type hint
    pub fn eval_literal(
        &mut self,
        literal: parser::Literal<'_>,
        type_hint: Option<&component::Type>,
    ) -> anyhow::Result<Val> {
        match literal {
            parser::Literal::List(list) => {
                match type_hint {
                    Some(component::Type::List(l)) => {
                        let mut values = Vec::new();
                        for item in list.items {
                            values.push(self.eval(item, Some(&l.ty()))?)
                        }
                        Ok(Val::List(List::new(&l, values.into_boxed_slice())?))
                    }
                    Some(component::Type::Option(o)) => match o.ty() {
                        component::Type::List(l) => {
                            let mut values = Vec::new();
                            for item in list.items {
                                values.push(self.eval(item, Some(&l.ty()))?)
                            }
                            Ok(Val::Option(component::OptionVal::new(
                                o,
                                Some(Val::List(List::new(&l, values.into_boxed_slice())?)),
                            )?))
                        }
                        t => bail!(
                            "type error - required option<{}> found = list",
                            display_component_type(&t)
                        ),
                    },
                    Some(t) => bail!(
                        "type error - required = {} found = list",
                        display_component_type(t)
                    ),
                    None => {
                        // TODO: try to find a list type that fits the shape of the literal
                        bail!("cannot determine type of list")
                    }
                }
            }
            parser::Literal::Record(mut r) => {
                let ty = match type_hint {
                    Some(component::Type::Record(r)) => r,
                    Some(t) => bail!(
                        "type error - required = {} found = record",
                        display_component_type(t)
                    ),
                    None => {
                        // TODO: try to find a record type that fits the shape of the literal
                        bail!("cannot determine type of record")
                    }
                };
                let mut values = Vec::new();
                let types = ty
                    .fields()
                    .enumerate()
                    .map(|(index, field)| (field.name, index))
                    .collect::<HashMap<_, _>>();
                // Sort the fields since wasmtime expects the fields to be in the defined order
                r.fields.sort_by(|(f1, _), (f2, _)| {
                    types
                        .get(f1.as_str())
                        .unwrap()
                        .cmp(types.get(f2.as_str()).unwrap())
                });

                for ((name, field_expr), field_type) in r.fields.into_iter().zip(ty.fields()) {
                    values.push((name.as_str(), self.eval(field_expr, Some(&field_type.ty))?));
                }
                Ok(Val::Record(Record::new(ty, values)?))
            }
            parser::Literal::String(s) => {
                let val = Val::String(s.as_str().to_owned().into());
                match type_hint {
                    Some(component::Type::Result(r)) => r.new_val(match (r.ok(), r.err()) {
                        (Some(_), _) => Ok(Some(val)),
                        (_, Some(_)) => Err(Some(val)),
                        (None, None) => return Ok(val),
                    }),
                    _ => Ok(val),
                }
            }
            parser::Literal::Num(n) => match type_hint {
                Some(component::Type::U8) => Ok(Val::U8(n.try_into()?)),
                _ => Ok(Val::S32(n.try_into()?)),
            },
        }
    }

    fn resolve_ident(
        &mut self,
        ident: &str,
        type_hint: Option<&component::Type>,
    ) -> Result<Val, anyhow::Error> {
        log::debug!("Resolving ident {ident} with type hint {type_hint:?}");
        match type_hint {
            Some(t) => match t {
                component::Type::Bool if ident == "true" => Ok(Val::Bool(true)),
                component::Type::Bool if ident == "false" => Ok(Val::Bool(false)),
                component::Type::Enum(e) => e.new_val(ident),
                component::Type::Variant(v) => match self.lookup_in_scope(ident) {
                    Ok(v) => Ok(v),
                    Err(_) => v.new_val(ident, None),
                },
                component::Type::Option(o) if ident == "none" => o.new_val(None),
                component::Type::Option(o) => {
                    o.new_val(Some(self.resolve_ident(ident, Some(&o.ty()))?))
                }
                component::Type::Result(r) => r.new_val(match (r.ok(), r.err()) {
                    (Some(o), _) => Ok(Some(self.resolve_ident(ident, Some(&o))?)),
                    (None, None) if ident == "ok" => Ok(None),
                    (None, None) if ident == "err" => Err(None),
                    _ => return self.lookup_in_scope(ident),
                }),
                component::Type::Bool
                | component::Type::U8
                | component::Type::U16
                | component::Type::U32
                | component::Type::U64
                | component::Type::S8
                | component::Type::S16
                | component::Type::S32
                | component::Type::S64
                | component::Type::String => self.lookup_in_scope(ident),
                t => todo!("handle ident '{ident}' with type {t:?}"),
            },
            None => self.lookup_in_scope(ident),
        }
    }

    fn lookup_in_scope(&self, ident: &str) -> anyhow::Result<Val> {
        self.scope
            .get(ident)
            .with_context(|| format!("no identifier '{ident}' in scope"))
            .cloned()
    }
}

fn display_component_type(ty: &component::Type) -> &'static str {
    match ty {
        component::Type::Bool => "bool",
        component::Type::S8 => "s8",
        component::Type::U8 => "u8",
        component::Type::S16 => "s16",
        component::Type::U16 => "u16",
        component::Type::S32 => "s32",
        component::Type::U32 => "u32",
        component::Type::S64 => "s64",
        component::Type::U64 => "u64",
        component::Type::Float32 => "float32",
        component::Type::Float64 => "float64",
        component::Type::Char => "char",
        component::Type::String => "string",
        component::Type::List(_) => "list",
        component::Type::Record(_) => "record",
        component::Type::Tuple(_) => "tuple",
        component::Type::Variant(_) => "variant",
        component::Type::Enum(_) => "enum",
        component::Type::Option(_) => "option",
        component::Type::Result(_) => "result",
        component::Type::Flags(_) => "flags",
        component::Type::Own(_) => "own",
        component::Type::Borrow(_) => "borrow",
    }
}
