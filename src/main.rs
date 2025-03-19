use std::path::Path;

use anyhow::Result;
use ra_ap_base_db::SourceDatabase;
use ra_ap_hir::{db::ExpandDatabase, HasSource, Semantics};
use ra_ap_ide::{TextSize, TryToNav};
use ra_ap_ide_db::defs::{Definition, IdentClass};
use ra_ap_paths::Utf8Path;
use ra_ap_project_model::{CargoConfig, ProjectManifest, ProjectWorkspace};
use ra_ap_syntax::{
    ast::{Fn, HasArgList, HasName},
    syntax_editor::Element,
    AstNode, SourceFile,
};
use ra_ap_vfs::{AbsPath, VfsPath};
use yinyang::{
    field_checker, field_checker2, field_checker4, field_checker5, loader, parse_find,
    trait_ref::parse_trait_ref,
    workspace::{self, Workspace},
};

fn main() -> Result<()> {
    // bagua_project()?;

    // aa();
    // aa_project()?;
    // parse_find::parse_find()?;
    // parse_trait_ref()?;

    let project = loader::BaguaProject::load(
        "/home/sen/framework-dev/yinyang-dev/test-projects/field-check",
    )?;

    let violations = field_checker5::FieldChecker::new(&project).check()?;
    dbg!(&violations);
    Ok(())
}

fn aa() {
    panic!()
}

trait Repo {
    fn find<T>(&self) -> T
    where
        Self: Loader<T>,
    {
        self.load()
    }
}

trait Loader<T> {
    fn load(&self) -> T;
}

struct UC<R> {
    ab: R,
}

impl<R> UC<R>
where
    R: Repo,
    R: Loader<i32>,
{
    fn aa(&self) {
        let a = self.ab.find();
    }
}

fn aa_project() -> Result<()> {
    let root_dir = Path::new("/home/sen/web-rearch/yinyang-dev/.local/aa");
    let root_dir = AbsPath::assert(Utf8Path::from_path(root_dir).unwrap());
    let Workspace {
        host,
        vfs,
        proc_macro,
        root_crate,
    } = Workspace::load(root_dir.as_ref()).unwrap();

    let manifest = ProjectManifest::discover_single(root_dir)?;
    let cargo_config = CargoConfig::default();
    let workspace = ProjectWorkspace::load(manifest, &cargo_config, &|_| ())?;
    let workspace_root = workspace.workspace_root();
    dbg!(&workspace_root);

    // let s = SourceFile::parse(
    //     include_str!("../.local/tt.rs"),
    //     ra_ap_ide::Edition::Edition2021,
    // );
    // let s = s.tree();
    // dbg!(&s);

    let db = host.raw_database();
    let graph = db.crate_graph();
    let crates = graph.iter();

    for crate_id in crates {
        let crate_data = &graph[crate_id];
        if crate_data.origin.is_local() {
            let root_mod_path = vfs.file_path(crate_data.root_file_id);
            if root_mod_path.starts_with(&VfsPath::new_real_path(root_dir.to_string())) {
                let krate = ra_ap_hir::Crate::from(crate_id);
                let lib = krate.root_module();
                let lib_file_id = lib.as_source_file_id(db).unwrap();

                let semantics = Semantics::new(db);
                let lib_file = semantics.parse(lib_file_id);
                let lib_file = lib_file.syntax();

                let a_trait = lib_file
                    .descendants()
                    .find_map(|x| {
                        let t = ra_ap_syntax::ast::Trait::cast(x.clone());
                        t
                    })
                    .unwrap();
                dbg!(&a_trait);

                let trait_def = semantics
                    .find_node_at_offset_with_descend::<ra_ap_syntax::ast::Trait>(
                        lib_file,
                        a_trait.syntax().text_range().end(),
                    )
                    .and_then(|t| semantics.to_def(&t))
                    .unwrap();
                dbg!(&trait_def);

                let a_call = lib_file.token_at_offset(TextSize::new(50));
                dbg!(&a_call);
                let a_call = match a_call {
                    ra_ap_syntax::TokenAtOffset::None => todo!(),
                    ra_ap_syntax::TokenAtOffset::Single(a) => a,
                    ra_ap_syntax::TokenAtOffset::Between(_, _) => todo!(),
                };
                let a_call_node = dbg!(a_call.parent()).unwrap();

                // 1. 直接从语法节点获取泛型参数
                if let Some(generic_args) = a_call_node
                    .children()
                    .find_map(ra_ap_syntax::ast::GenericArgList::cast)
                {
                    dbg!(&generic_args);
                }

                let a_call_node_def = semantics
                    .find_node_at_offset_with_descend::<ra_ap_syntax::ast::MethodCallExpr>(
                        lib_file,
                        a_call_node.text_range().end(),
                    )
                    .unwrap();

                let defs = IdentClass::classify_node(&semantics, &a_call_node)
                    .unwrap()
                    .definitions();

                for def in defs {
                    dbg!(&def);
                    match def {
                        ra_ap_ide_db::defs::Definition::Macro(_) => todo!(),
                        ra_ap_ide_db::defs::Definition::Field(field) => todo!(),
                        ra_ap_ide_db::defs::Definition::TupleField(tuple_field) => todo!(),
                        ra_ap_ide_db::defs::Definition::Module(module) => todo!(),
                        ra_ap_ide_db::defs::Definition::Function(function) => {
                            let fn_ = function.source(db).unwrap().value;
                            dbg!(&fn_);

                            let impl_ = fn_
                                .syntax()
                                .ancestors()
                                .find_map(|a| ra_ap_syntax::ast::Impl::cast(a))
                                .unwrap();
                            dbg!(&impl_);
                            let t = dbg!(impl_.trait_()).unwrap();
                            let trait_ref = match t {
                                ra_ap_syntax::ast::Type::PathType(path_type) => path_type,
                                _ => {
                                    todo!()
                                }
                            };

                            let seg = trait_ref.path().unwrap().segment().unwrap();
                            let trait_ref = semantics
                                .find_node_at_offset_with_descend::<ra_ap_syntax::ast::NameRef>(
                                    lib_file,
                                    seg.syntax().text_range().end(),
                                )
                                .unwrap();

                            if let Some(def) =
                                IdentClass::classify_node(&semantics, trait_ref.syntax())
                                    .and_then(|class| class.definitions().get(0).cloned())
                            {
                                dbg!(def == Definition::Trait(trait_def));
                            }
                        }
                        ra_ap_ide_db::defs::Definition::Adt(adt) => todo!(),
                        ra_ap_ide_db::defs::Definition::Variant(variant) => todo!(),
                        ra_ap_ide_db::defs::Definition::Const(_) => todo!(),
                        ra_ap_ide_db::defs::Definition::Static(_) => todo!(),
                        ra_ap_ide_db::defs::Definition::Trait(_) => todo!(),
                        ra_ap_ide_db::defs::Definition::TraitAlias(trait_alias) => todo!(),
                        ra_ap_ide_db::defs::Definition::TypeAlias(type_alias) => todo!(),
                        ra_ap_ide_db::defs::Definition::SelfType(_) => todo!(),
                        ra_ap_ide_db::defs::Definition::GenericParam(generic_param) => todo!(),
                        ra_ap_ide_db::defs::Definition::Local(local) => todo!(),
                        ra_ap_ide_db::defs::Definition::Label(label) => todo!(),
                        ra_ap_ide_db::defs::Definition::DeriveHelper(derive_helper) => todo!(),
                        ra_ap_ide_db::defs::Definition::BuiltinType(builtin_type) => todo!(),
                        ra_ap_ide_db::defs::Definition::BuiltinLifetime(static_lifetime) => todo!(),
                        ra_ap_ide_db::defs::Definition::BuiltinAttr(builtin_attr) => todo!(),
                        ra_ap_ide_db::defs::Definition::ToolModule(tool_module) => todo!(),
                        ra_ap_ide_db::defs::Definition::ExternCrateDecl(extern_crate_decl) => {
                            todo!()
                        }
                        ra_ap_ide_db::defs::Definition::InlineAsmRegOrRegClass(_) => todo!(),
                        ra_ap_ide_db::defs::Definition::InlineAsmOperand(inline_asm_operand) => {
                            todo!()
                        }
                    }
                    // dbg!(def.try_to_nav(db).unwrap());
                }
            }
        }
    }

    Ok(())
}

fn bagua_project() -> Result<()> {
    let sample_project = Path::new("/home/sen/web-rearch/yinyang-dev/sample-project");
    let workspace = Workspace::load(sample_project).unwrap();
    dbg!(workspace::bagua_usecase_trait(&workspace.host));

    for bc in workspace.bounded_contexts()? {
        dbg!(&bc.name);
        let ucs = bc.use_cases(&workspace.host, &workspace.vfs)?;

        for uc in ucs {
            dbg!(&uc.name);
            dbg!(&uc.path);
            uc.used_entities(&workspace.host, &workspace.vfs)?;
        }
    }

    Ok(())
}
