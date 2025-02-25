use std::path::Path;

use anyhow::{bail, Context, Result};
use ra_ap_base_db::SourceDatabase;
use ra_ap_hir::{
    db::{DefDatabase, HirDatabase},
    Crate, HasSource, ModPath, Module, Name, PathKind, Semantics, Struct, Trait, TraitRef,
};
use ra_ap_hir_def::{resolver::HasResolver, ImplId, ModuleId};
use ra_ap_hir_expand::__path;
use ra_ap_hir_ty::{Interner, TyBuilder};
use ra_ap_ide::{AnalysisHost, RootDatabase, TryToNav};
use ra_ap_ide_db::defs::IdentClass;
use ra_ap_paths::Utf8Path;
use ra_ap_span::SyntaxContextId;
use ra_ap_syntax::{
    ast::{self, edit::AstNodeEdit, HasGenericParams, HasName, HasTypeBounds},
    ted, AstNode, SyntaxNode,
};
use ra_ap_vfs::AbsPath;

use crate::workspace::Workspace;

#[macro_export]
macro_rules! timer_start {
    () => {{
        let start = std::time::Instant::now();
        start
    }};
}

#[macro_export]
macro_rules! timer_end {
    ($start:expr, $msg:expr) => {{
        let elapsed = $start.elapsed();
        println!("{}: {:?}", $msg, elapsed);
    }};
}

#[macro_export]
macro_rules! ra_path {
    ($kind:expr, $start:ident $(:: $seg:ident)*) => {
        {
            use ra_ap_hir_expand::name::Name;
            use ra_ap_hir_expand::mod_path::ModPath;
            use ra_ap_span::SyntaxContextId;

            ModPath::from_segments(
                $kind,
                vec![
                    Name::new(stringify!($start), SyntaxContextId::from_u32(1)),
                    $(Name::new(stringify!($seg), SyntaxContextId::from_u32(1)),)*
                ],
            )
        }
    };
}

#[macro_export]
macro_rules! crate_path {
    ($start:ident $(:: $seg:ident)*) => {
        {
            $crate::ra_path!(ra_ap_hir::PathKind::Crate, $start $(:: $seg)*)
        }
    };
}

#[macro_export]
macro_rules! abs_path {
    ($start:ident $(:: $seg:ident)*) => {
        {
            $crate::ra_path!(ra_ap_hir::PathKind::Abs, $start $(:: $seg)*)
        }
    };
}

pub fn parse_find() -> Result<()> {
    let root_dir = Path::new("/home/sen/web-rearch/yinyang-dev/sample-project");
    let root_dir = AbsPath::assert(Utf8Path::from_path(root_dir).unwrap());
    let Workspace {
        host,
        vfs,
        proc_macro,
        root_crate,
    } = Workspace::load(root_dir.as_ref()).unwrap();

    let db = host.raw_database();
    let bagua_ctx = BaguaContext::load(db, root_crate)?;

    let bc_user_crate = bc_user_crate(&host)?;
    let start = timer_start!();
    let resolver = bc_user_crate.root_module().id().resolver(db);
    timer_end!(start, "resolve bc_user_crate");

    let app_mod_path = crate_path!(application);
    let resolved = resolver.resolve_module_path_in_items(db, &app_mod_path);
    let application_module = match resolved.take_types().context("no application module")? {
        ra_ap_hir::ModuleDefId::ModuleId(module_id) => Module::from(module_id),
        _ => anyhow::bail!("no application module"),
    };

    let mods = use_case_mods(application_module, db)?;

    for mod_ in mods {
        analyze_use_case_mod(mod_, db, &bagua_ctx)?;
    }

    Ok(())
}

pub trait GetId {
    type Id;

    fn id(&self) -> Self::Id;
}

impl GetId for Module {
    type Id = ModuleId;

    fn id(&self) -> Self::Id {
        <Self::Id>::from(*self)
    }
}

struct CrateCtx {
    workspase: Crate,
    bagua: BaguaContext,
}

struct BaguaContext {
    use_case_trait: Trait,
}

impl BaguaContext {
    fn load(db: &RootDatabase, ws: Crate) -> Result<Self> {
        let uc_trait = Self::usecase_trait(db, ws)?;

        Ok(Self {
            use_case_trait: uc_trait,
        })
    }

    fn usecase_trait(db: &RootDatabase, ws: Crate) -> Result<Trait> {
        let start = timer_start!();
        let resolver = ws.root_module().id().resolver(db);
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
}

pub struct EntityVariant {
    subset: EntitySubset,
}

pub struct EntitySubset {
    def: Struct,
}

fn analyze_use_case_mod(module: Module, db: &RootDatabase, bagua: &BaguaContext) -> Result<()> {
    let mod_file_id = module.as_source_file_id(db).context("no source file id")?;
    let sema = Semantics::new(db);
    let mod_file = sema.parse(mod_file_id);

    let uc_impl = mod_file
        .syntax()
        .children()
        .find_map(|c| {
            if let Some(ra_ap_syntax::ast::Item::Impl(impl_)) = AstNode::cast(c) {
                let trait_ = impl_.trait_();
                if let Some(ra_ap_syntax::ast::Type::PathType(trait_path)) = trait_ {
                    let path = trait_path.path().unwrap();
                    let def = sema.resolve_trait(&path).unwrap();

                    if def == bagua.use_case_trait {
                        return Some(impl_);
                    }
                }

                None
            } else {
                None
            }
        })
        .context("no use case impl")?;

    let execute_fn = uc_impl
        .syntax()
        .descendants()
        .find_map(|c| {
            if let Some(ra_ap_syntax::ast::Item::Fn(fn_)) = AstNode::cast(c) {
                // if fn_.name().unwrap().text() == "execute" {
                // }
                return Some(fn_);
            }
            None
        })
        .context("no execute fn")?;

    let execute_fn_body = execute_fn.body().context("no execute fn body")?;

    for stmt in execute_fn_body.stmt_list().unwrap().statements() {
        // dbg!(&stmt);
        match stmt {
            ra_ap_syntax::ast::Stmt::ExprStmt(expr_stmt) => {}
            ra_ap_syntax::ast::Stmt::Item(item) => {}
            ra_ap_syntax::ast::Stmt::LetStmt(let_stmt) => {
                if let Some(init) = let_stmt.initializer() {
                    let subset_struct = match init {
                        ast::Expr::MacroExpr(macro_expr) => {
                            let call = macro_expr.macro_call().unwrap();
                            let expanded = expand_macro_recur(&sema, &call).unwrap();
                            let find_method_call = expanded.descendants().find_map(|x| {
                                if let Some(ra_ap_syntax::ast::Expr::MethodCallExpr(method_call)) =
                                    AstNode::cast(x)
                                {
                                    dbg!(&method_call);
                                    let name = method_call.name_ref().unwrap();
                                    if name.text() == "find" {
                                        return Some(method_call);
                                        // if let Some(fn_def) = sema.resolve_method_call(&method_call)
                                        // {
                                        //     todo!()
                                        // }
                                    }
                                }
                                None
                            });
                            let subset_struct = if let Some(method_call) = find_method_call {
                                let args = method_call.get_or_create_generic_arg_list();
                                if let Some(subset_ty) = args.generic_args().next() {
                                    match subset_ty {
                                        ast::GenericArg::TypeArg(type_arg) => {
                                            if let ast::Type::PathType(path_type) =
                                                type_arg.ty().unwrap()
                                            {
                                                let def = sema
                                                    .resolve_path(&path_type.path().unwrap())
                                                    .unwrap();
                                                let subset_struct = match def {
                                                    ra_ap_hir::PathResolution::Def(module_def) => {
                                                        match module_def {
                                                            ra_ap_hir::ModuleDef::Adt(adt) => {
                                                                adt.as_struct().unwrap()
                                                            }
                                                            _ => {
                                                                todo!()
                                                            }
                                                        }
                                                    }
                                                    _ => {
                                                        todo!()
                                                    }
                                                };
                                                let fields = subset_struct.fields(db);
                                                for field in fields {
                                                    dbg!(field.name(db));
                                                }

                                                Some(subset_struct)
                                            } else {
                                                None
                                            }
                                        }
                                        _ => None,
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            };

                            subset_struct
                        }
                        _ => None,
                    };
                    let pat = let_stmt.pat().unwrap();
                    dbg!(&pat);
                    let name = match pat {
                        ast::Pat::IdentPat(ident_pat) => ident_pat.name().unwrap(),
                        _ => {
                            continue;
                        }
                    };
                    let entity_var = IdentClass::classify_node(&sema, &name.syntax()).unwrap();
                    match entity_var {
                        IdentClass::NameClass(name_class) => {
                            match name_class {
                                ra_ap_ide_db::defs::NameClass::Definition(definition) => {
                                    let usages = definition.usages(&sema).all();
                                    // dbg!(&usages);
                                    for (_, usages) in usages {
                                        for usage in usages {
                                            let name_ref = usage.name;
                                            match name_ref {
                                            ra_ap_ide_db::search::FileReferenceNode::NameRef(name_ref) => {
                                                // if usage.range.start() > 1166.into() {
                                                //     let a = dbg!(name_ref.syntax().ancestors().last());
                                                // }

                                                let expr = 
                                                name_ref.syntax().ancestors().find(|a| {
                                                    ast::Stmt::can_cast(a.kind()) || ast::MacroStmts::can_cast(a.kind())
                                                });
                                                
                                                // name_ref.syntax().siblings(direction)
                                                dbg!(expr);
                                                dbg!(name_ref);
                                                println!("==================");
                                            },
                                            _ => {}
                                        }
                                        }
                                    }
                                    // dbg!(definition.try_to_nav(db));
                                }
                                _ => {}
                            }
                        }
                        IdentClass::NameRefClass(name_ref_class) => todo!(),
                        IdentClass::Operator(operator_class) => todo!(),
                    }
                }
            }
        }
    }

    Ok(())
}

fn expand_macro_recur(
    sema: &Semantics<'_, RootDatabase>,
    macro_call: &ast::MacroCall,
) -> Option<SyntaxNode> {
    let expanded = sema.expand(macro_call)?.clone_for_update();
    expand(sema, expanded, ast::MacroCall::cast, expand_macro_recur)
}

fn expand<T: AstNode>(
    sema: &Semantics<'_, RootDatabase>,
    expanded: SyntaxNode,
    f: impl FnMut(SyntaxNode) -> Option<T>,
    exp: impl Fn(&Semantics<'_, RootDatabase>, &T) -> Option<SyntaxNode>,
) -> Option<SyntaxNode> {
    let children = expanded.descendants().filter_map(f);
    let mut replacements = Vec::new();

    for child in children {
        if let Some(new_node) = exp(sema, &child) {
            // check if the whole original syntax is replaced
            if expanded == *child.syntax() {
                return Some(new_node);
            }
            replacements.push((child, new_node));
        }
    }

    replacements
        .into_iter()
        .rev()
        .for_each(|(old, new)| ted::replace(old.syntax(), new));
    Some(expanded)
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

type BB = String;

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

pub fn bc_user_crate(host: &AnalysisHost) -> Result<Crate> {
    let crate_graph = host.raw_database().crate_graph();
    let crate_id = crate_graph
        .crates_in_topological_order()
        .into_iter()
        .find_map(|x| {
            let krate = &crate_graph[x];
            if krate
                .display_name
                .as_ref()
                .unwrap()
                .crate_name()
                .to_string()
                == "bc_user"
            {
                return Some(x);
            }
            None
        });

    Ok(Crate::from(crate_id.unwrap().clone()))
}
