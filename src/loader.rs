use std::time::Instant;

use anyhow::{Context, Result};
use ra_ap_base_db::SourceDatabase;
use ra_ap_hir::{db::HirDatabase, AdtId, Crate, Enum, Function, Impl, Module, Struct, Trait, Type};
use ra_ap_hir_def::resolver::{HasResolver, Resolver};
use ra_ap_ide::{CrateId, RootDatabase};
use ra_ap_load_cargo::LoadCargoConfig;
use ra_ap_proc_macro_api::ProcMacroServer;
use ra_ap_project_model::{CargoConfig, ProjectManifest, ProjectWorkspace, RustLibSource};
use ra_ap_vfs::{AbsPathBuf, Vfs};

use crate::{abs_path, crate_path, parse_find::GetId, timer_end, timer_start, Semantics};

pub struct BaguaProject {
    pub db: RootDatabase,
    pub vfs: Vfs,
    pub proc_macro: ProcMacroServer,

    pub root_crate: Crate,
    pub bc_crates: Vec<BoundedContext>,

    pub bagua: Bagua,
}

pub struct Bagua {
    pub entity_trait: EntityTrait,
    pub usecase_trait: Trait,
    pub repository_trait: RepositoryTrait,
    pub trait_subset: Trait,

    pub struct_field: Enum,
    pub struct_foreign_entities: Enum,
}

#[derive(Debug, Clone, Copy)]
pub struct EntityTrait(pub Trait);

#[derive(Debug, Clone, Copy)]
pub struct RepositoryTrait(pub Trait);

pub struct BoundedContext {
    pub krate: Crate,
}

pub struct Usecase {
    pub module: Module,
}

impl RepositoryTrait {
    pub fn method_find(&self, db: &RootDatabase) -> Option<Function> {
        self.0.items(db).iter().find_map(|it| match it {
            ra_ap_hir::AssocItem::Function(function) => {
                if function.name(db).symbol().to_string() == "find" {
                    Some(function.clone())
                } else {
                    None
                }
            }
            _ => None,
        })
    }
}

impl BaguaProject {
    pub fn load<P: AsRef<std::path::Path>>(path: P) -> anyhow::Result<Self> {
        let path = path.as_ref();
        let cargo_config = CargoConfig {
            sysroot: Some(RustLibSource::Discover),
            ..Default::default()
        };
        let no_progress = &|_| ();

        let path = AbsPathBuf::assert_utf8(path.to_path_buf());
        let manifest = ProjectManifest::discover_single(&path)?;

        let now = Instant::now();
        let mut workspace = ProjectWorkspace::load(manifest.clone(), &cargo_config, no_progress)?;

        let root_krate_name = match &workspace.kind {
            ra_ap_project_model::ProjectWorkspaceKind::Cargo {
                cargo,
                error,
                build_scripts,
                rustc,
                cargo_config_extra_env,
                set_test,
            } => {
                let crates = cargo.packages().collect::<Vec<_>>();
                crates.iter().find_map(|k| {
                    let krate = &cargo[*k];
                    if krate.is_member && &krate.manifest == manifest.manifest_path() {
                        Some(krate.name.clone())
                    } else {
                        None
                    }
                })
            }
            ra_ap_project_model::ProjectWorkspaceKind::Json(project_json) => todo!(),
            ra_ap_project_model::ProjectWorkspaceKind::DetachedFile {
                file,
                cargo,
                cargo_config_extra_env,
                set_test,
            } => todo!(),
        };
        let root_krate_name = root_krate_name.unwrap();

        println!("load workspace: {:?}", now.elapsed());

        let now = Instant::now();
        let load_cargo_config = LoadCargoConfig {
            load_out_dirs_from_check: true,
            with_proc_macro_server: ra_ap_load_cargo::ProcMacroServerChoice::Sysroot,
            prefill_caches: false,
        };

        let bs = workspace.run_build_scripts(&cargo_config, no_progress)?;
        workspace.set_build_scripts(bs);
        println!("load build scripts: {:?}", now.elapsed());

        let now = Instant::now();
        let (db, vfs, proc_macro) = ra_ap_load_cargo::load_workspace(
            workspace.clone(),
            &cargo_config.extra_env,
            &load_cargo_config,
        )?;
        println!("load workspace: {:?}", now.elapsed());

        let now = Instant::now();
        let crate_graph = db.crate_graph();
        let root_krate = crate_graph
            .iter()
            .find_map(|id| {
                let krate = &crate_graph[id];
                match &krate.origin {
                    ra_ap_base_db::CrateOrigin::Local { repo, name } => {
                        let name = name.as_ref().map(|n| n.as_str());
                        if name == Some(&root_krate_name) {
                            Some(Crate::from(id))
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            })
            .unwrap();
        println!("root krate found: {:?}", now.elapsed());
        let bc_crates = db
            .crate_graph()
            .iter()
            .filter_map(|krate_id| {
                let krate = &crate_graph[krate_id];
                match &krate.origin {
                    ra_ap_base_db::CrateOrigin::Local {
                        repo,
                        name: Some(name),
                    } => {
                        if name.as_str().starts_with("bc") {
                            Some(BoundedContext {
                                krate: Crate::from(krate_id),
                            })
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            })
            .collect::<Vec<_>>();
        let bagua_crate = db
            .crate_graph()
            .iter()
            .find_map(|krate_id| {
                let krate = &crate_graph[krate_id];
                if krate
                    .display_name
                    .as_ref()
                    .map(|n| n.crate_name().symbol().as_str())
                    == Some("bagua_dev")
                {
                    Some(Crate::from(krate_id))
                } else {
                    None
                }
            })
            .context("bagua crate not found")?;

        let uc_trait = usecase_trait(&db, root_krate)?;
        let entity_trait = entity_trait(&db, root_krate)?;
        let repository_trait = repository_trait(&db, root_krate)?;
        let struct_field = struct_field(&db, root_krate)?;
        let struct_foreign_entities = struct_foreign_entities(&db, root_krate)?;
        let trait_subset = trait_subset(&db, root_krate)?;

        let project = BaguaProject {
            db,
            vfs,
            proc_macro: proc_macro.unwrap(),
            root_crate: root_krate,
            bc_crates,
            bagua: Bagua {
                entity_trait: EntityTrait(entity_trait),
                usecase_trait: uc_trait,
                repository_trait: RepositoryTrait(repository_trait),
                struct_field,
                struct_foreign_entities,
                trait_subset,
            },
        };

        Ok(project)
    }
}

fn trait_subset(db: &RootDatabase, krate: Crate) -> Result<Trait> {
    let resolver = krate.root_module().id().resolver(db);
    let path = abs_path!(bagua::entity::subset::Subset);
    resolve_trait(db, &resolver, path)
}

fn struct_foreign_entities(db: &RootDatabase, krate: Crate) -> Result<Enum> {
    let resolver = krate.root_module().id().resolver(db);
    let path = abs_path!(bagua::entity::foreign::ForeignEntities);
    resolve_enum(db, &resolver, path)
}

fn struct_field(db: &RootDatabase, krate: Crate) -> Result<Enum> {
    let resolver = krate.root_module().id().resolver(db);
    let path = abs_path!(bagua::entity::field::Field);
    resolve_enum(db, &resolver, path)
}

fn repository_trait(db: &RootDatabase, krate: Crate) -> Result<Trait> {
    let start = timer_start!();
    let resolver = krate.root_module().id().resolver(db);
    timer_end!(start, "resolve root module");

    let path = abs_path!(bagua::repository::Repository);
    resolve_trait(db, &resolver, path)
}

fn entity_trait(db: &RootDatabase, krate: Crate) -> Result<Trait> {
    let start = timer_start!();
    let resolver = krate.root_module().id().resolver(db);
    timer_end!(start, "resolve root module");

    let path = abs_path!(bagua::entity::Entity);

    resolve_trait(db, &resolver, path)
}

fn resolve_enum(db: &RootDatabase, resolver: &Resolver, path: ra_ap_hir::ModPath) -> Result<Enum> {
    let resolved = resolver.resolve_module_path_in_items(db, &path);
    match resolved.take_types() {
        Some(def) => match def {
            ra_ap_hir::ModuleDefId::AdtId(AdtId::EnumId(enum_id)) => Ok(Enum::from(enum_id)),
            _ => {
                anyhow::bail!("Could not find struct {:?}", path)
            }
        },
        None => anyhow::bail!("Could not find struct {:?}", path),
    }
}

fn resolve_trait(
    db: &RootDatabase,
    resolver: &Resolver,
    path: ra_ap_hir::ModPath,
) -> Result<Trait> {
    let resolved = resolver.resolve_module_path_in_items(db, &path);
    match resolved.take_types() {
        Some(def) => match def {
            ra_ap_hir::ModuleDefId::TraitId(trait_id) => Ok(Trait::from(trait_id)),
            _ => {
                anyhow::bail!("")
            }
        },
        None => anyhow::bail!("Could not find trait {:?}", path),
    }
}

fn usecase_trait(db: &RootDatabase, krate: Crate) -> Result<Trait> {
    let start = timer_start!();
    let resolver = krate.root_module().id().resolver(db);
    timer_end!(start, "resolve root module");

    let path = abs_path!(bagua::usecase::UseCase);

    let resolved = resolver.resolve_module_path_in_items(db, &path);
    match resolved.take_types() {
        Some(def) => match def {
            ra_ap_hir::ModuleDefId::TraitId(trait_id) => Ok(Trait::from(trait_id)),
            _ => {
                anyhow::bail!("")
            }
        },
        None => anyhow::bail!("Could not find trait UseCase"),
    }
}

impl BoundedContext {
    pub fn use_cases(&self, db: &RootDatabase) -> Vec<Usecase> {
        let path = crate_path!(application);
        let resolver = self.krate.root_module().id().resolver(db);
        let resolved = resolver.resolve_module_path_in_items(db, &path);
        let module = resolved
            .take_types()
            .and_then(|id| {
                if let ra_ap_hir::ModuleDefId::ModuleId(module_id) = id {
                    Some(Module::from(module_id))
                } else {
                    None
                }
            })
            .unwrap();

        let uc_mods = use_case_mods(module, db)
            .unwrap()
            .into_iter()
            .map(|m| Usecase { module: m })
            .collect();

        uc_mods
    }
}

fn use_case_mods(module: Module, db: &RootDatabase) -> Result<Vec<Module>> {
    let mut mods = Vec::new();
    inner_recur(module, db, &mut mods)?;
    return Ok(mods);

    fn inner_recur(module: Module, db: &RootDatabase, mods: &mut Vec<Module>) -> Result<()> {
        for x in module.children(db) {
            inner_recur(x, db, mods)?;

            let impls = x.impl_defs(db);

            for impl_ in impls {
                if !is_use_case(impl_, db) {
                    continue;
                }

                mods.push(x);
            }
        }

        Ok(())
    }
}

fn is_use_case(impl_: ra_ap_hir::Impl, db: &RootDatabase) -> bool {
    let impl_trait = impl_.trait_(db);
    if let Some(impl_trait) = impl_trait {
        let name = impl_trait.name(db).symbol().to_string();
        if name == "UseCase" {
            return true;
        }
    }
    false
}

impl Usecase {
    pub fn debug_print(&self, db: &RootDatabase) {
        let sema = Semantics::new(db);
        let source = self.module.as_source_file_id(db).unwrap();
        let file = sema.parse(source);
        dbg!(&file);
    }
}

impl Bagua {
    pub fn impls_subset(&self, db: &RootDatabase, ty: &Type) -> bool {
        ty.impls_trait(db, self.trait_subset, &[])
    }
}
