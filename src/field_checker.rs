use anyhow::{Context, Result};
use ra_ap_base_db::SourceDatabase;
use ra_ap_hir::{Adt, HasSource, ModuleDef, Semantics};
use ra_ap_hir_def::hir;
use ra_ap_hir_expand::builtin::quote::ToTokenTree;
use ra_ap_ide::RootDatabase;
use ra_ap_ide_db::{defs::NameClass, LineIndexDatabase};
use ra_ap_syntax::{
    ast::{self, HasArgList, HasGenericArgs, HasName},
    ted, AstNode, SourceFile, SyntaxNode,
};

use crate::{
    entity::{Entity, FieldPath, Subset, SubsetField},
    loader::{BaguaProject, Usecase},
};

pub struct FieldChecker {
    project: BaguaProject,
}

impl FieldChecker {
    pub fn new(project: BaguaProject) -> Self {
        Self { project }
    }
}

#[derive(Debug)]
pub struct FieldAccessViolation {
    pub local_def: Position,
    pub allowed_fields: Vec<FieldPath>,
    pub violations: Vec<AccessStack>,
}

#[derive(Debug)]
pub struct AccessStack {
    pub access_stack: Vec<Position>,
}

#[derive(Debug)]
pub struct Position {
    pub file_path: String,
    pub line: u32,
}

impl FieldChecker {
    pub fn check(&self) -> anyhow::Result<Vec<FieldAccessViolation>> {
        let mut violations = Vec::new();
        for bc in &self.project.bc_crates {
            let ucs = bc.use_cases(&self.project.db);
            for uc in ucs {
                violations.extend(self.check_uc(&uc)?);
            }
        }

        Ok(violations)
    }

    pub fn check_uc(&self, uc: &Usecase) -> anyhow::Result<Vec<FieldAccessViolation>> {
        let db = &self.project.db;
        let sema = Semantics::new(&self.project.db);
        let file_id = uc
            .module
            .as_source_file_id(db)
            .context("uc modult has no file id ")?;
        let mod_syn = UcFileSyn(sema.parse(file_id));
        let uc_impl = mod_syn.uc_impl()?;
        let exec_fn = uc_impl.execute_fn()?;

        let line_index = db.line_index(file_id.file_id());
        let mod_path = self.project.vfs.file_path(file_id.file_id());

        let mut violations = Vec::new();
        for local_def in exec_fn.entity_local_defs(&sema)? {
            let mut stacks = Vec::new();

            // let ty = sema
            //     .type_of_pat(&local_def.pat())
            //     .context("cannot get type of local def")?
            //     .original
            //     .as_adt()
            //     .context("Entity variant is not an ADT")?
            //     .as_struct()
            //     .context("Entity variant is not a struct")?;

            dbg!(&local_def);

            for usage in dbg!(local_def.usages(&sema)) {
                if let Some(field_path) = usage.access_field_path() {
                    dbg!(&field_path);
                    if !local_def.subset.contains(&field_path) {
                        let name_ref = usage.name_ref();
                        let line_col = line_index.line_col(name_ref.syntax().text_range().start());
                        stacks.push(AccessStack {
                            access_stack: vec![Position {
                                file_path: mod_path.to_string(),
                                line: line_col.line,
                            }],
                        });
                    }
                }
            }

            if !stacks.is_empty() {
                let file_path = self.project.vfs.file_path(file_id.file_id());
                let line_col = line_index.line_col(local_def.pat().syntax().text_range().start());
                violations.push(FieldAccessViolation {
                    local_def: Position {
                        file_path: file_path.to_string(),
                        line: line_col.line,
                    },
                    allowed_fields: local_def
                        .subset
                        .fields
                        .into_iter()
                        .map(|f| f.path)
                        .collect(),
                    violations: stacks,
                });
            }
        }

        Ok(violations)
    }
}

struct UcFileSyn(SourceFile);
struct UcImpl(ast::Impl);
struct UcExecuteFn(ast::Fn);

impl UcFileSyn {
    fn uc_impl(&self) -> Result<UcImpl> {
        let uc_impl = self
            .0
            .syntax()
            .descendants()
            .find_map(|node| {
                let impl_ = ast::Impl::cast(node)?;

                let trait_ = impl_.trait_()?;
                if let ast::Type::PathType(path) = trait_ {
                    let path = path.path()?;
                    if path.segment()?.name_ref()?.text() == "UseCase" {
                        return Some(impl_);
                    }
                }

                None
            })
            .context("uc impl not found in uc file")?;
        let uc_impl = UcImpl(uc_impl);

        Ok(uc_impl)
    }
}

impl UcImpl {
    fn execute_fn(&self) -> Result<UcExecuteFn> {
        let execute_fn = self
            .0
            .assoc_item_list()
            .context("uc impl has no assoc item list")?
            .assoc_items()
            .find_map(|item| {
                let method = ast::Fn::cast(item.syntax().clone())?;
                if method.name()?.text() == "execute" {
                    return Some(method);
                }
                None
            })
            .context("execute fn not found in uc impl")?;
        let execute_fn = UcExecuteFn(execute_fn);

        Ok(execute_fn)
    }
}

impl UcExecuteFn {
    fn entity_local_defs(&self, sema: &Semantics<RootDatabase>) -> Result<Vec<EntityLocalDef>> {
        let defs = self.entity_local_defs_inner(sema)?.unwrap_or_default();
        Ok(defs)
    }

    fn entity_local_defs_inner(
        &self,
        sema: &Semantics<RootDatabase>,
    ) -> Result<Option<Vec<EntityLocalDef>>> {
        let let_stmts = self
            .0
            .body()
            .context("uc execute fn has no body")?
            .statements()
            .filter_map(|s| {
                let stmt = ast::LetStmt::cast(s.syntax().clone())?;
                Some(stmt)
            })
            .collect::<Vec<_>>();

        let mut defs = Vec::new();
        for stmt in let_stmts {
            let def = if let Some(pat) = stmt.pat() {
                if let ast::Pat::IdentPat(ident_pat) = pat {
                    ident_pat.name().context("ident pat has no name")?
                } else {
                    continue;
                }
            } else {
                continue;
            };
            let expr = stmt.initializer().context("let stmt has no initializer")?;
            let subset = match expr {
                ast::Expr::MacroExpr(macro_expr) => subset_struct_in_macro_call(sema, macro_expr),
                _ => {
                    continue;
                }
            };
            if let Some(subset) = subset {
                defs.push(EntityLocalDef { subset, def });
            }
        }

        Ok(Some(defs))
    }
}

fn subset_struct_in_macro_call(
    sema: &Semantics<RootDatabase>,
    macro_expr: ast::MacroExpr,
) -> Option<Subset> {
    let call = macro_expr.macro_call().unwrap();
    let expanded = expand_macro_recur(&sema, &call).unwrap();
    let find_method_call = expanded.descendants().find_map(|x| {
        if let Some(ra_ap_syntax::ast::Expr::MethodCallExpr(method_call)) = AstNode::cast(x) {
            // dbg!(&method_call);
            let name = method_call.name_ref().unwrap();
            if name.text() == "find" {
                return Some(method_call);
            }
        }
        None
    })?;

    let struct_ = find_method_call
        .generic_arg_list()?
        .generic_args()
        .find_map(|g| {
            if let ast::GenericArg::TypeArg(type_arg) = g {
                let ty = type_arg.ty()?;
                if let ast::Type::PathType(path_type) = ty {
                    if let ra_ap_hir::PathResolution::Def(ModuleDef::Adt(Adt::Struct(struct_))) =
                        sema.resolve_path(&path_type.path().unwrap())?
                    {
                        return Some(struct_);
                    }
                }
            }

            None
        })?;
    let struct_ = struct_.source(sema.db).unwrap();

    let mut subset_fields = Vec::new();
    if let ast::FieldList::RecordFieldList(fields) = struct_.value.field_list()? {
        let fields = fields.fields();
        for field in fields {
            let name = field.name()?.text().to_string();
            let path = FieldPath {
                qualifier: None,
                name,
            };
            let subset_field = SubsetField { path };
            subset_fields.push(subset_field);
        }
    }

    let name = struct_.value.name()?;

    let subset = Subset {
        name,
        fields: subset_fields,
    };

    Some(subset)
}

#[derive(Debug)]
struct EntityLocalDef {
    subset: Subset,
    def: ast::Name,
}

#[derive(Debug)]
enum EntityVariantUsage {
    FieldAccess(ast::FieldExpr),
    MethodCall(ast::MethodCallExpr),
}

impl EntityLocalDef {
    fn pat(&self) -> ast::Pat {
        self.def
            .syntax()
            .ancestors()
            .find_map(ast::Pat::cast)
            .unwrap()
    }

    fn usages(&self, sema: &Semantics<RootDatabase>) -> Vec<EntityVariantUsage> {
        let mut local_usages = Vec::new();
        if let NameClass::Definition(def) = NameClass::classify(sema, &self.def).unwrap() {
            let usages = def.usages(sema).all();
            for (_, usages) in usages {
                for usage in usages {
                    dbg!(&usage);

                    let name_ref = match usage.name {
                        ra_ap_ide_db::search::FileReferenceNode::NameRef(name_ref) => name_ref,
                        _ => continue,
                    };
                    let name = name_ref.syntax().clone();

                    for x in name.ancestors() {
                        if let Some(expr) = ast::FieldExpr::cast(x.clone()) {
                            local_usages.push(EntityVariantUsage::FieldAccess(expr));
                        }
                        if let Some(expr) = ast::MethodCallExpr::cast(x) {
                            local_usages.push(EntityVariantUsage::MethodCall(expr));
                        }
                    }
                }
            }
        }

        local_usages
    }
}

impl EntityVariantUsage {
    fn access_field_path(&self) -> Option<FieldPath> {
        match self {
            EntityVariantUsage::FieldAccess(field_expr) => {
                let name = field_expr.name_ref()?;
                Some(FieldPath {
                    qualifier: None,
                    name: name.text().to_string(),
                })
            }
            EntityVariantUsage::MethodCall(_) => None,
        }
    }

    fn name_ref(&self) -> ast::NameRef {
        match self {
            EntityVariantUsage::FieldAccess(field_expr) => field_expr.name_ref().unwrap(),
            EntityVariantUsage::MethodCall(method_call) => method_call.name_ref().unwrap(),
        }
    }
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
