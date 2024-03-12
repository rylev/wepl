use std::borrow::Cow;

use anyhow::Context;
use wit_component::DecodedWasm;
use wit_parser::{
    Function, Interface, InterfaceId, Package, Resolve, TypeDef, TypeId, World, WorldId, WorldItem,
    WorldKey,
};

use crate::command::parser;

/// A resolver for a wit world.
pub struct WorldResolver {
    resolve: Resolve,
    world_id: WorldId,
}

impl WorldResolver {
    /// Create new instance.
    ///
    /// Panics if the `world_id` is not found in the `resolve`.
    pub fn new(resolve: Resolve, world_id: WorldId) -> Self {
        let this = Self { resolve, world_id };
        // Ensure the world can be resolved
        let _ = this.world();
        this
    }

    /// Create a new instance from the given bytes.
    pub fn from_bytes(component_bytes: &[u8]) -> anyhow::Result<Self> {
        let (resolve, world) = match wit_component::decode(component_bytes)
            .context("could not decode given file as a WebAssembly component")?
        {
            DecodedWasm::Component(r, w) => (r, w),
            _ => anyhow::bail!("found wit package instead of the expect WebAssembly component"),
        };
        Ok(Self::new(resolve, world))
    }

    /// Get the exported function by the given `FunctionIdent`.
    pub fn exported_function(&self, ident: parser::ItemIdent) -> Option<&Function> {
        match ident.interface {
            Some(i) => {
                let interface = self.exported_interface(i)?;
                interface.functions.get(ident.item)
            }
            None => {
                if let WorldItem::Function(f) = &self.export(ident.item)? {
                    Some(f)
                } else {
                    None
                }
            }
        }
    }

    /// Get the imported function by the given `FunctionIdent`.
    pub fn imported_function(&self, ident: parser::ItemIdent) -> Option<&Function> {
        match ident.interface {
            Some(i) => {
                let interface = self.imported_interface(i)?;
                interface.functions.get(ident.item)
            }
            None => {
                if let WorldItem::Function(f) = &self.import(ident.item)? {
                    Some(f)
                } else {
                    None
                }
            }
        }
    }

    /// Get the exported interface by the given `InterfaceIdent`.
    pub fn exported_interface(&self, ident: parser::InterfaceIdent) -> Option<&Interface> {
        self.interface_in_items(ident, self.world().exports.iter())
    }

    /// Get the imported interface by the given `InterfaceIdent`.
    pub fn imported_interface(&self, ident: parser::InterfaceIdent) -> Option<&Interface> {
        self.interface_in_items(ident, self.world().imports.iter())
    }

    pub fn export(&self, name: &str) -> Option<&WorldItem> {
        self.get_world_item_by_name(self.world().exports.iter(), name)
    }

    pub fn import(&self, name: &str) -> Option<&WorldItem> {
        self.get_world_item_by_name(self.world().imports.iter(), name)
    }

    pub fn interface_by_id(&self, id: InterfaceId) -> Option<&wit_parser::Interface> {
        self.resolve.interfaces.get(id)
    }

    pub fn type_by_id(&self, id: TypeId) -> Option<&TypeDef> {
        self.resolve.types.get(id)
    }

    pub(crate) fn types_by_name(&self, name: &str) -> Vec<(Option<&InterfaceId>, &TypeDef)> {
        let mut types = Vec::new();
        for (_, t) in &self.resolve.types {
            if t.name.as_deref().map(|n| n == name).unwrap_or_default() {
                let interface = if let wit_parser::TypeOwner::Interface(i) = &t.owner {
                    Some(i)
                } else {
                    None
                };
                types.push((interface, t));
            }
        }
        types
    }

    pub fn display_wit_type<'a>(
        &'a self,
        param_type: &wit_parser::Type,
        expansion: Expansion,
    ) -> Cow<'a, str> {
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
                let typ = self
                    .resolve
                    .types
                    .get(*id)
                    .expect("found type id for type not present in resolver");
                return self.display_wit_type_def(typ, expansion);
            }
        };
        Cow::Borrowed(str)
    }

    pub fn display_wit_type_def(&self, typ: &TypeDef, expansion: Expansion) -> Cow<'_, str> {
        let display = match &typ.kind {
            wit_parser::TypeDefKind::Option(o) => {
                format!("option<{}>", self.display_wit_type(o, Expansion::Collapsed))
            }
            wit_parser::TypeDefKind::Result(r) => {
                let ok =
                    r.ok.as_ref()
                        .map(|o| self.display_wit_type(o, Expansion::Collapsed));
                let err = r
                    .err
                    .as_ref()
                    .map(|o| self.display_wit_type(o, Expansion::Collapsed));
                match (ok, err) {
                    (Some(ok), Some(err)) => format!("result<{ok}, {err}>"),
                    (Some(t), _) => format!("result<{t}>"),
                    (_, Some(t)) => format!("result<_, {t}>"),
                    _ => "result".into(),
                }
            }
            wit_parser::TypeDefKind::Type(t) => return self.display_wit_type(t, expansion),
            wit_parser::TypeDefKind::List(t) => {
                format!("list<{}>", self.display_wit_type(t, Expansion::Collapsed))
            }
            wit_parser::TypeDefKind::Tuple(t) => {
                format!(
                    "tuple<{}>",
                    t.types
                        .iter()
                        .map(|t| self.display_wit_type(t, Expansion::Collapsed))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            wit_parser::TypeDefKind::Enum(e) => match expansion {
                Expansion::Expanded(col) => {
                    let fields = e
                        .cases
                        .iter()
                        .map(|c| format!("{}{}", " ".repeat(col as usize * 4), c.name))
                        .collect::<Vec<_>>()
                        .join(",\n");
                    format!(
                        "enum {{\n{fields}\n{}}}",
                        " ".repeat((col - 1) as usize * 4)
                    )
                }
                Expansion::Collapsed => typ.name.clone().unwrap(),
            },
            wit_parser::TypeDefKind::Record(r) => match expansion {
                Expansion::Expanded(col) => {
                    let fields = r
                        .fields
                        .iter()
                        .map(|f| {
                            format!(
                                "{}: {}",
                                f.name,
                                self.display_wit_type(&f.ty, Expansion::Collapsed)
                            )
                        })
                        .map(|f| format!("{}{}", " ".repeat(col as usize * 4), f))
                        .collect::<Vec<_>>()
                        .join(",\n");
                    format!(
                        "record {{\n{fields}\n{}}}",
                        " ".repeat((col - 1) as usize * 4)
                    )
                }
                Expansion::Collapsed => typ.name.clone().unwrap(),
            },
            wit_parser::TypeDefKind::Variant(v) => match expansion {
                Expansion::Expanded(col) => {
                    let cases = v
                        .cases
                        .iter()
                        .map(|c| {
                            let data = match c.ty {
                                Some(ty) => {
                                    let ty = self.display_wit_type(&ty, Expansion::Collapsed);
                                    format!("({ty})")
                                }
                                None => String::new(),
                            };
                            format!("{}{}", c.name, data)
                        })
                        .map(|f| format!("{}{}", " ".repeat(col as usize * 4), f))
                        .collect::<Vec<_>>()
                        .join(",\n");
                    format!(
                        "variant {{\n{cases}\n{}}}",
                        " ".repeat((col - 1) as usize * 4)
                    )
                }
                Expansion::Collapsed => typ.name.clone().unwrap(),
            },
            // TODO: Fill these in with more information.
            wit_parser::TypeDefKind::Resource => format!("resource<...>"),
            wit_parser::TypeDefKind::Handle(_) => format!("handle<...>"),
            wit_parser::TypeDefKind::Flags(_) => format!("flags<...>"),
            wit_parser::TypeDefKind::Future(_) => format!("future<...>"),
            wit_parser::TypeDefKind::Stream(_) => format!("stream<...>"),
            wit_parser::TypeDefKind::Unknown => unreachable!(),
        };
        Cow::Owned(display)
    }

    /// Whether the wasi cli (0.2.0) package is imported.
    ///
    /// Note that this is being used as a heuristic to determine whether to
    /// link wasi command.
    pub fn imports_wasi_cli(&self) -> bool {
        for package in self.package_dependencies() {
            if package.name.namespace == "wasi"
                && package.name.name == "cli"
                && package
                    .name
                    .version
                    .as_ref()
                    .map(|v| v.major == 0 && v.minor == 2 && v.patch == 0)
                    .unwrap_or(false)
            {
                return true;
            }
        }
        false
    }

    /// All packages that are imported dependencies of the current world.
    pub fn package_dependencies(&self) -> impl Iterator<Item = &Package> {
        self.world()
            .imports
            .iter()
            .filter_map(move |(import_name, _)| {
                if let WorldKey::Interface(interface_id) = import_name {
                    let interface = self.resolve.interfaces.get(*interface_id).unwrap();
                    interface
                        .package
                        .and_then(|package_id| self.resolve.packages.get(package_id))
                } else {
                    None
                }
            })
    }

    pub fn world_item_name(&self, name: &WorldKey) -> String {
        self.resolve.name_world_key(name)
    }

    pub fn interface_name(&self, interface: &InterfaceId) -> Option<String> {
        self.resolve.id_of(*interface)
    }

    pub fn world(&self) -> &World {
        self.resolve
            .worlds
            .get(self.world_id)
            .expect("world_id is not found in the resolved wit package")
    }

    pub(crate) fn imports(
        &self,
        include_wasi_cli: bool,
    ) -> impl Iterator<Item = (&WorldKey, &WorldItem)> {
        self.world()
            .imports
            .iter()
            .filter(move |(_, item)| match item {
                WorldItem::Interface(id) if !include_wasi_cli => {
                    let interface = self.interface_by_id(*id).unwrap();
                    let Some(package) = interface.package else {
                        return true;
                    };
                    let package = self.resolve.packages.get(package).unwrap();
                    !(package.name.namespace == "wasi"
                        // This is not 100% correct.
                        // We're assuming that all interfaces in these packages are handled by `wasmtime-wasi` 
                        // command implementation. This should be true for many components so we'll leave
                        // the hack for now.
                        && ["cli", "io", "filesystem", "random"].contains(&package.name.name.as_str()))
                }
                _ => true,
            })
    }

    pub(crate) fn world_name(&self) -> String {
        let world = self.world();
        let world_package = if let Some(world_package) = world.package {
            let world_package = self.resolve.packages.get(world_package).unwrap();
            format!("{}/", world_package.name)
        } else {
            String::new()
        };
        format!("{world_package}{}", world.name)
    }

    /// Get an interface by its ident from a list of world items.
    fn interface_in_items<'a>(
        &self,
        ident: parser::InterfaceIdent,
        items: impl Iterator<Item = (&'a WorldKey, &'a WorldItem)>,
    ) -> Option<&Interface> {
        let item = self.get_world_item_by_name(items, &ident.to_string())?;
        if let WorldItem::Interface(i) = item {
            return self.interface_by_id(*i);
        }
        None
    }

    fn get_world_item_by_name<'a>(
        &self,
        mut items: impl Iterator<Item = (&'a WorldKey, &'a WorldItem)>,
        name: &str,
    ) -> Option<&'a WorldItem> {
        items.find_map(|(export_name, export)| {
            let export_name = self.resolve.name_world_key(export_name);
            (export_name == name).then_some(export)
        })
    }
}

pub enum Expansion {
    Expanded(u8),
    Collapsed,
}
