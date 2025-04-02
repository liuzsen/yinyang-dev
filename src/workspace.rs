use std::{
    path::{Path, PathBuf},
    time::Instant,
};

use anyhow::Context;
use ra_ap_base_db::RootQueryDb;
use ra_ap_hir::{
    db::{DefDatabase, ExpandDatabase, HirDatabase},
    Crate, HasSource, HirDisplay, HirFileId, Impl, Module, Name, Semantics, Trait, TypeInfo,
};
use ra_ap_ide::{AnalysisHost, RootDatabase};
use ra_ap_ide_db::{base_db::SourceDatabase, defs::IdentClass};
use ra_ap_load_cargo::LoadCargoConfig;
use ra_ap_proc_macro_api::ProcMacroClient;
// use ra_ap_proc_macro_api::ProcMacroServer;
use ra_ap_project_model::{CargoConfig, ProjectManifest, ProjectWorkspace, RustLibSource};
use ra_ap_span::EditionedFileId;
use ra_ap_syntax::{ast::{self, HasName}, syntax_editor::Element, AstNode};
use ra_ap_vfs::{AbsPathBuf, AnchoredPath, FileId, Vfs, VfsPath};

pub struct Workspace {
    pub root_crate: Crate,
    pub host: AnalysisHost,
    pub vfs: Vfs,
    pub proc_macro: ProcMacroClient,
}

impl Workspace {
    pub fn load(path: &Path) -> anyhow::Result<Workspace> {
        let cargo_config = CargoConfig {
            sysroot: Some(RustLibSource::Discover),
            ..Default::default()
        };
        let no_progress = &|_| ();

        let path = AbsPathBuf::assert_utf8(path.to_path_buf());
        let manifest = ProjectManifest::discover_single(&path)?;

        let now = Instant::now();
        let mut workspace = ProjectWorkspace::load(manifest.clone(), &cargo_config, no_progress)?;

        let root_krate_name = match &workspace.kind{
            ra_ap_project_model::ProjectWorkspaceKind::Cargo { cargo, error, build_scripts, rustc, } => {
                let crates = cargo.packages().collect::<Vec<_>>();
                crates.iter().find_map(|k| {
                    let krate = &cargo[*k];
                    if krate.is_member && &krate.manifest == manifest.manifest_path() {
                        Some(krate.name.clone())
                    } else {
                        None
                    }
                })
            },
            ra_ap_project_model::ProjectWorkspaceKind::Json(project_json) => todo!(),
            ra_ap_project_model::ProjectWorkspaceKind::DetachedFile { file, cargo, } => todo!(),
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
        let crate_graph = db.all_crates();

        let root_krate = crate_graph.iter().find_map(|id| {
            let krate = id.clone();
            // let krate = &crate_graph[id];
            match &krate.data(&db).origin {
                ra_ap_base_db::CrateOrigin::Local { repo, name } => {
                    let name = name.as_ref().map(|n| {n.as_str()});
                    if name == Some(&root_krate_name) {
                        Some(Crate::from(krate))
                    } else {
                        None
                    }
                },
                _ => {None}
            }
        }).unwrap();
        println!("root krate found: {:?}", now.elapsed());

        let host = AnalysisHost::with_database(db);

        Ok(Workspace {
            root_crate: root_krate,
            host,
            vfs,
            proc_macro: proc_macro.context("Failed to load proc macro server")?,
        })
    }

    pub fn bounded_contexts(&self) -> anyhow::Result<Vec<BoundedContext>> {
        let db = self.host.raw_database();
        // let graph = db.crate_graph();
        // let crates = graph.iter();
        let crates = db.all_crates();

        let mut bcs = vec![];
        for krate_id in crates.into_iter() {
            let krate = ra_ap_hir::Crate::from(krate_id.clone());

            if krate.origin(db).is_local() {
                let crate_display_name = krate.display_name(db).unwrap();
                dbg!(&crate_display_name);
                let canonical_name = crate_display_name.canonical_name().as_str();
                dbg!(&canonical_name);

                if canonical_name.starts_with("bc_") || canonical_name.starts_with("bc-") {
                    let root_file_id = krate.root_file(db);
                    let path = self.vfs.file_path(root_file_id);
                    let path = path.parent().unwrap();
                    let path = path.as_path().unwrap().to_owned();

                    bcs.push(BoundedContext {
                        name: canonical_name.to_string(),
                        path,
                        krate,
                    });
                }
            }
        }

        Ok(bcs)
    }
}

pub struct BoundedContext {
    pub name: String,
    pub path: AbsPathBuf,
    pub krate: Crate,
}

pub fn bagua_entity_trait(host: &AnalysisHost) -> anyhow::Result<Trait> {
    let db = host.raw_database();
    // let graph = db.crate_graph();
    // let crates = graph.iter();
    let crates = db.all_crates();

    for krate_id in crates.into_iter() {
        let krate = ra_ap_hir::Crate::from(krate_id.clone());

        let display_name = krate.display_name(db).unwrap();
        if display_name.canonical_name().as_str().contains("bagua") {
            let modules = krate.modules(db);

            for module in modules {
                if module
                    .name(db)
                    .map_or(false, |name| name.as_str() == "entity")
                {
                    let declarations = module.declarations(db);
                    for declaration in declarations {
                        match declaration {
                            ra_ap_hir::ModuleDef::Trait(trait_) => {
                                if trait_.name(db).as_str() == "Entity" {
                                    return Ok(trait_);
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    anyhow::bail!("Bagua entity trait not found")
}

pub fn bagua_usecase_trait(host: &AnalysisHost) -> anyhow::Result<Trait> {
    let db = host.raw_database();
    let crates = db.all_crates();

    // 遍历所有 crate
    for krate_id in crates.into_iter() {
        let krate = ra_ap_hir::Crate::from(krate_id.clone());
        let display_name = match krate.origin(db) {
            ra_ap_base_db::CrateOrigin::Rustc { name } => todo!(),
            ra_ap_base_db::CrateOrigin::Local { repo, name } => todo!(),
            ra_ap_base_db::CrateOrigin::Library { repo, name } => {
                Some(name)
            },
            ra_ap_base_db::CrateOrigin::Lang(lang_crate_origin) => todo!(),
        };
        // let display_name = graph[krate_id].display_name.as_ref();

        // 找到 bagua crate
        if let Some(krate_name) = display_name {
            if krate_name.as_str().contains("bagua") {
                let krate = ra_ap_hir::Crate::from(krate_id.clone());

                let modules = krate.modules(db);

                for module in modules {
                    if module
                        .name(db)
                        .map_or(false, |name| name.as_str() == "usecase")
                    {
                        let declarations = module.declarations(db);
                        for declaration in declarations {
                            match declaration {
                                ra_ap_hir::ModuleDef::Trait(trait_) => return Ok(trait_),
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
    }

    anyhow::bail!("Bagua usecase module not found")
}

impl BoundedContext {
    fn find_impl_use_case_in_application(
        &self,
        host: &AnalysisHost,
        application_module: Module,
        uc_trait: Trait,
        vfs: &Vfs,
    ) -> anyhow::Result<Vec<UseCase>> {
        let db = host.raw_database();

        let mut use_cases = Vec::new();
        let mut stack = vec![application_module];
        while let Some(module) = stack.pop() {
            let children = module.children(db);
            use_cases.extend(self.find_impl_use_case_in_module(host, &module, uc_trait, vfs));
            stack.extend(children);
        }

        Ok(use_cases)
    }

    fn find_impl_use_case_in_module(
        &self,
        host: &AnalysisHost,
        module: &Module,
        usecase_trait: Trait,
        vfs: &Vfs,
    ) -> Option<UseCase> {
        let db = host.raw_database();
        let impl_defs = module.impl_defs(db);

        let id = module.definition_source(db).file_id;
        let file_id = id.file_id().unwrap();
        let path = vfs
            .file_path(file_id.file_id())
            .as_path()
            .unwrap()
            .to_owned();

        for impl_def in impl_defs {
            if impl_def
                .trait_ref(db)
                .map_or(false, |t| t.trait_() == usecase_trait)
            {
                return Some(UseCase {
                    name: module.name(db).unwrap().as_str().to_string(),
                    path,
                    entities: Vec::new(),
                    impl_: impl_def,
                });
            }
        }

        None
    }

    pub fn use_cases(&self, host: &AnalysisHost, vfs: &Vfs) -> anyhow::Result<Vec<UseCase>> {
        let uc_trait = bagua_usecase_trait(host)?;
        let db = host.raw_database();
        let modules = self.krate.root_module().children(db);

        for module in modules {
            let name = module.name(db).unwrap();
            if name.as_str() == "application" {
                // find `impl UseCase for ...` in `application` module recursively
                let use_cases =
                    self.find_impl_use_case_in_application(host, module, uc_trait, vfs)?;
                return Ok(use_cases);
            }
        }

        Ok(vec![])
    }
}

pub struct UseCase {
    pub name: String,
    pub path: AbsPathBuf,
    pub entities: Vec<EntityInUseCase>,
    pub impl_: Impl,
}

impl UseCase {
    pub fn used_entities(
        &self,
        host: &AnalysisHost,
        vfs: &Vfs,
    ) -> anyhow::Result<Vec<EntityInUseCase>> {
        let db = host.raw_database();
        let mut entities = Vec::new();

        let semantics = Semantics::new(db);
        let (file_id, _) = vfs
            .file_id(&VfsPath::new_real_path(dbg!(self.path.to_string())))
            .unwrap();

        let file_id = EditionedFileId::current_edition(file_id);
        let source_file = semantics.parse(ra_ap_base_db::EditionedFileId::new(db, file_id));
        let impl_ = source_file
            .syntax()
            .children()
            .find_map(|c| {
                if let Some(impl_block) = ast::Impl::cast(c.clone()) {
                    if impl_block.trait_().map_or(false, |t| {
                        let path_str = t.syntax().text().to_string();
                        dbg!(&path_str);
                        // 检查是否是 bagua::UseCase 或 ::bagus::UseCase
                        path_str == "UseCase" || path_str == "::bagua::UseCase"
                    }) {
                        return Some(impl_block);
                    }
                }
                None
            })
            .unwrap();
        let impl_ = semantics.to_impl_def(&impl_).unwrap();

        // let impl_ = semantics.parse(semantics.hir_file_for(self.impl_).file_id().unwrap());
        // 遍历 impl 块中的所有项
        let entity_trait = bagua_entity_trait(host)?;
        for item in impl_.items(db) {
            if let ra_ap_hir::AssocItem::Function(function) = item {
                if function.name(db).as_str() == "execute" {
                    let source = function.source(db).unwrap();
                    let body = source.value.body().unwrap();

                    // 使用语义分析器获取语法节点
                    for stmt in body.statements() {
                        if let ast::Stmt::LetStmt(let_stmt) = stmt {
                            dbg!(&let_stmt.to_string());

                            if let Some(init) = let_stmt.initializer() {
                                if let Some(ty) = semantics.type_of_expr(&init) {
                                    if is_entity(&ty, db, impl_.module(db)) {
                                        if dbg!(ty.original.impls_trait(db, entity_trait, &[])) {}
                                        if &self.path.to_string() == "/home/sen/web-rearch/yinyang-dev/sample-project/bc-user/src/application/user/update.rs" {
                                            dbg!(&init);
                                            
                                            analyze_repository_find(&init, &semantics);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(entities)
    }
}


fn analyze_repository_find(
    expr: &ast::Expr,
    semantics: &Semantics<'_, RootDatabase>,
) -> Option<(String, String)> {
    // (Entity name, Subset name)
    // 1. 检查是否是方法调用
    if let ast::Expr::MacroExpr(mac) = expr {
        let call = mac.macro_call()?;
        let tree = call.token_tree()?;
        for c in tree.token_trees_and_tokens() {
            match c {
                ra_ap_syntax::NodeOrToken::Node(sub_tree) => {},
                ra_ap_syntax::NodeOrToken::Token(token) => {
                    let find_method_syn_node = semantics.descend_into_macros(token).into_iter().find_map(|token| {
                        let p = token.parent()?;

                        dbg!(&token);
                        dbg!(p.to_string());
                        dbg!(p.kind());
                        println!("");
                        
                        if token.text() == "find" {
                            Some(p)
                        } else {
                            None
                        }
                    });

                    let Some(find_method_syn_node) = find_method_syn_node else {
                        continue;
                    };
                    dbg!(&find_method_syn_node);

                    let defs = IdentClass::classify_node(semantics, &find_method_syn_node)?.definitions();
                    for (def, subs) in defs {
                        dbg!(&def); 
                        let module = def.module(semantics.db).unwrap();
                        dbg!(module.name(semantics.db));
                        let krate = module.krate();
                        dbg!(krate.display_name(semantics.db).unwrap());

                        match def {
                            ra_ap_ide_db::defs::Definition::Trait(t) => {

                            },
                            _ => {}
                        }
                    }

                    
                    // let def = semantics.to_def(&find_method_node)?;
                },
            }
        }
    }

    // if let Some(call) = expr
    //     .syntax()
    //     .descendants()
    //     .find_map(|d| {
    //         dbg!(&d);
    //         dbg!(ra_ap_syntax::ast::CallExpr::cast(d))
    //     })
    // {
    //     // 2. 检查调用的函数
    //     if let Some(callee) = call.expr() {
    //         if let Some(ty) = semantics.type_of_expr(&callee) {
    //             dbg!(ty);
    //             // let type_str = ty
    //             //     .original
    //             //     .display_source_code(db, ty.original.module(db), true)
    //             //     .unwrap_or_default()
    //             //     .to_string();

    //             // // 3. 检查是否是 Repository::find
    //             // if type_str.contains("Repository") {
    //             //     // 4. 获取泛型参数
    //             //     if let Some(generic_args) = call.generic_arg_list() {
    //             //         dbg!(&generic_args.to_string()); // 显式泛型参数
    //             //     }

    //             //     // 5. 从返回类型推断 Entity 和 Subset
    //             //     if let Some(ret_ty) = semantics.type_of_expr(expr) {
    //             //         dbg!(&ret_ty); // 返回类型信息
    //             //     }
    //             // }
    //         }
    //     }
    // }
    None
}

fn is_entity(ty: &TypeInfo, db: &RootDatabase, module: Module) -> bool {
    let ty_str = dbg!(ty
        .original
        .display_source_code(db, From::from(module), true)
        .unwrap()
        .to_string());
    // for test
    if ty_str == "User" {
        true
    } else {
        false
    }
}

pub struct EntityInUseCase {
    need_fields: Vec<String>,
    from_subset: EntitySubset,
}

pub struct EntitySubset {
    name: String,
    fields: Vec<String>,
}
