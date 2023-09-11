use anyhow::Context as _;
use wasmtime::{
    component::{Component, Instance, Linker, Val},
    Config, Engine, Store,
};
use wasmtime_wasi::preview2::{Table, WasiCtx, WasiCtxBuilder, WasiView};
use wit_parser::FunctionKind;

use super::wit::Querier;

pub struct Runtime {
    store: Store<Context>,
    instance: Instance,
    linker: Linker<Context>,
    component: Component,
}

impl Runtime {
    pub async fn init(
        component_bytes: &[u8],
        querier: &Querier,
        stub_import: impl Fn(&str) + Sync + Send + Clone + 'static,
    ) -> anyhow::Result<Self> {
        let engine = load_engine()?;
        let component = load_component(&engine, &component_bytes)?;
        let mut linker = Linker::<Context>::new(&engine);
        linker.allow_shadowing(true);

        if querier.imports_wasi() {
            wasmtime_wasi::preview2::command::add_to_linker(&mut linker)?;
        }
        for (import_name, import) in querier.non_wasi_imports() {
            let stub_import = stub_import.clone();
            match import {
                wit_parser::WorldItem::Function(f) if f.kind == FunctionKind::Freestanding => {
                    linker
                        .root()
                        .func_new(&component, &f.name, move |_ctx, _args, _rets| {
                            stub_import(&import_name);
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

        Ok(Self {
            store,
            instance,
            linker,
            component,
        })
    }

    pub async fn call_func(
        &mut self,
        name: &str,
        args: &[Val],
        result_count: usize,
    ) -> anyhow::Result<Vec<Val>> {
        let func = self
            .instance
            .exports(&mut self.store)
            .root()
            .func(name)
            .with_context(|| format!("could not find function {name}' in instance"))?;
        let mut results = vec![Val::Bool(Default::default()); result_count];
        func.call_async(&mut self.store, args, &mut results).await?;
        func.post_return_async(&mut self.store).await?;
        Ok(results)
    }

    pub fn stub(&mut self) -> anyhow::Result<()> {
        self.linker
            .root()
            .func_new(&self.component, "get-string", |_ctx, _args, ret| {
                ret[0] = Val::String(String::from("wow").into());
                Ok(())
            })?;
        Ok(())
    }
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

fn load_engine() -> anyhow::Result<Engine> {
    let mut config = Config::new();
    config.wasm_component_model(true);
    config.async_support(true);

    Engine::new(&config)
}

fn load_component(engine: &Engine, component_bytes: &[u8]) -> anyhow::Result<Component> {
    Component::new(engine, component_bytes)
}
