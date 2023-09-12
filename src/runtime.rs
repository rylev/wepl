use std::sync::{Arc, Mutex};

use anyhow::Context as _;
use wasmtime::{
    component::{Component, Instance, Linker, Val},
    Config, Engine, Store,
};
use wasmtime_wasi::preview2::{Table, WasiCtx, WasiCtxBuilder, WasiView};
use wit_parser::FunctionKind;

use super::wit::Querier;

pub struct Runtime {
    engine: Engine,
    store: Store<Context>,
    instance: Instance,
    linker: Linker<Context>,
    component: (Component, Vec<u8>),
    import_impls: ImportImpls,
}

impl Runtime {
    pub fn init(
        component_bytes: Vec<u8>,
        querier: &Querier,
        stub_import: impl Fn(&str) + Sync + Send + Clone + 'static,
    ) -> anyhow::Result<Self> {
        let engine = load_engine()?;
        let component = load_component(&engine, &component_bytes)?;
        let mut linker = Linker::<Context>::new(&engine);
        linker.allow_shadowing(true);

        if querier.imports_wasi() {
            log::debug!("Linking with wasi");
            wasmtime_wasi::preview2::command::sync::add_to_linker(&mut linker)?;
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
                wit_parser::WorldItem::Interface(i) => {
                    let interface = querier.interface(*i).unwrap();
                    let mut root = linker.root();
                    let mut instance = root.instance(&import_name)?;
                    for (_, f) in interface.functions.iter() {
                        let stub_import = stub_import.clone();
                        let import_name = import_name.clone();
                        instance.func_new(&component, &f.name, move |_ctx, _args, _rets| {
                            stub_import(&import_name);
                            Ok(())
                        })?;
                    }
                }
                i => todo!("Implement import: {i:?}"),
            }
        }
        let pre = linker
            .instantiate_pre(&component)
            .context("could not instantiate component")?;
        let mut store = build_store(&engine);
        let instance = pre.instantiate(&mut store)?;
        let import_impls = ImportImpls::new(&engine);
        Ok(Self {
            engine,
            store,
            instance,
            linker,
            component: (component, component_bytes),
            import_impls,
        })
    }

    pub fn call_func(
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
        func.call(&mut self.store, args, &mut results)?;
        func.post_return(&mut self.store)?;
        Ok(results)
    }

    /// Stub a function with an export from the component encoded in `component_bytes`
    ///
    /// This function does not check that the component in `components_bytes` has the
    /// export needed.
    pub fn stub_function(&mut self, name: String, component_bytes: &[u8]) -> anyhow::Result<()> {
        let component = load_component(&self.engine, component_bytes)?;
        let mut linker = Linker::<ImportImplsContext>::new(&self.engine);
        wasmtime_wasi::preview2::command::sync::add_to_linker(&mut linker)?;
        let instance =
            linker.instantiate(&mut *self.import_impls.store.lock().unwrap(), &component)?;
        let func = instance
            .get_func(&mut *self.import_impls.store.lock().unwrap(), &name)
            .unwrap();

        let store = self.import_impls.store.clone();
        self.linker.root().func_new(
            &self.component.0,
            &name.clone(),
            move |_ctx, args, results| {
                let mut store = store.lock().unwrap();
                func.call(&mut *store, args, results)?;
                func.post_return(&mut *store)?;
                Ok(())
            },
        )?;
        self.refresh()?;
        Ok(())
    }

    pub fn set_component(&mut self, component: Vec<u8>) -> anyhow::Result<()> {
        self.component = (Component::from_binary(&self.engine, &component)?, component);
        self.refresh()
    }

    pub fn compose(&mut self, adapter: &[u8]) -> Result<(), anyhow::Error> {
        let temp = std::env::temp_dir();
        let tmp_virt = temp.join("virt.wasm");
        std::fs::write(&tmp_virt, adapter)?;
        let tmp_component = temp.join("component.wasm");
        std::fs::write(&tmp_component, &self.component.1)?;

        let bytes = wasm_compose::composer::ComponentComposer::new(
            &tmp_component,
            &wasm_compose::config::Config {
                definitions: vec![tmp_virt],
                ..Default::default()
            },
        )
        .compose()?;
        self.set_component(bytes)
    }

    pub fn component_bytes(&self) -> &[u8] {
        &self.component.1
    }

    /// Get a new instance
    pub fn refresh(&mut self) -> anyhow::Result<()> {
        self.store = build_store(&self.engine);
        self.instance = self
            .linker
            .instantiate(&mut self.store, &self.component.0)?;
        Ok(())
    }
}

/// A collection of instances that implement the main components imports
struct ImportImpls {
    store: Arc<Mutex<Store<ImportImplsContext>>>,
}

impl ImportImpls {
    fn new(engine: &Engine) -> Self {
        let mut table = Table::new();
        let mut builder = WasiCtxBuilder::new();
        builder.inherit_stdout();
        let wasi = builder.build(&mut table).unwrap();
        let context = ImportImplsContext::new(table, wasi);
        let store = Store::new(engine, context);

        Self {
            store: Arc::new(Mutex::new(store)),
        }
    }
}

fn build_store(engine: &Engine) -> Store<Context> {
    let mut table = Table::new();
    let mut builder = WasiCtxBuilder::new();
    builder.inherit_stdout().inherit_stderr();
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

    Engine::new(&config)
}

fn load_component(engine: &Engine, component_bytes: &[u8]) -> anyhow::Result<Component> {
    Component::new(engine, component_bytes)
}

struct ImportImplsContext {
    table: Table,
    wasi: WasiCtx,
}

impl ImportImplsContext {
    fn new(table: Table, wasi: WasiCtx) -> Self {
        Self { table, wasi }
    }
}

impl WasiView for ImportImplsContext {
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
