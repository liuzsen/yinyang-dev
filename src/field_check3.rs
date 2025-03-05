use std::collections::HashMap;

use ra_ap_hir::{HasSource, Module};
use ra_ap_ide::SearchScope;
use ra_ap_ide_db::defs::{Definition, NameClass, NameRefClass};
use ra_ap_syntax::{
    ast::{self, HasGenericParams, HasName},
    AstNode,
};

use crate::{
    bc_context::UseCaseMod,
    entity::FieldPath,
    helper,
    loader::{Bagua, BaguaProject},
    Database, Semantics,
};

pub struct FieldChecker<'a> {
    project: &'a BaguaProject,
}

pub struct EntityLocalVar {
    def: ast::Name,
    fields: HashMap<FieldPath, FieldState>,
}

pub enum FieldState {
    Unloaded,
    Unchanged,
    Set,
}

pub struct FieldAccessViolation {
    entity_var_def: ast::Name,
    trace: Vec<FieldAccessExpr>,
}

pub enum FieldAccessExpr {
    FieldExpr(ast::FieldExpr),
}

impl<'a> FieldChecker<'a> {
    pub fn new(project: &'a BaguaProject) -> Self {
        Self { project }
    }

    pub fn check_uc_mod(&self, db: &Database, module: Module) -> Option<Vec<FieldAccessViolation>> {
        let sema = Semantics::new(db);
        let mod_file_id = module.as_source_file_id(db)?;
        let file = sema.parse(mod_file_id);
        let use_case = UseCaseMod::from_file(&file)?;

        let repo_field = use_case.input.repo_field()?;
        let repo_field_name = repo_field.name()?;

        let scope = SearchScope::single_file(mod_file_id);

        let repo_usages = Definition::Field(sema.to_def(&repo_field)?)
            .usages(&sema)
            .set_scope(Some(&scope))
            .all();

        let mut defs = vec![];

        for repo_usage in repo_usages.into_iter().next()?.1 {
            match repo_usage.name {
                ra_ap_ide_db::search::FileReferenceNode::Name(name) => todo!(),
                ra_ap_ide_db::search::FileReferenceNode::NameRef(name_ref) => {
                    defs.extend(EntityLocalVar::load_from_repo_usage(
                        &sema,
                        name_ref,
                        &self.project.bagua,
                    ));
                }
                ra_ap_ide_db::search::FileReferenceNode::Lifetime(lifetime) => todo!(),
                ra_ap_ide_db::search::FileReferenceNode::FormatStringEntry(_, text_range) => {
                    todo!()
                }
            }
        }

        let mut violations = vec![];
        for def in defs {
            violations.extend(def.check_voilations(&sema, &use_case, &self.project.bagua));
        }

        if violations.is_empty() {
            None
        } else {
            Some(violations)
        }
    }
}

impl EntityLocalVar {
    fn check_voilations(
        &self,
        sema: &Semantics,
        mod_: &UseCaseMod,
        bagua: &Bagua,
    ) -> Vec<FieldAccessViolation> {
        todo!()
    }

    fn load_from_repo_usage(
        sema: &Semantics,
        repo_usage: ast::NameRef,
        bagua: &Bagua,
    ) -> Option<EntityLocalVar> {
        let repo_expr = repo_usage
            .syntax()
            .ancestors()
            .find_map(|it| it.ancestors().find_map(ast::Expr::cast))?;
        let let_stmt = repo_expr
            .syntax()
            .ancestors()
            .find_map(|it| it.ancestors().find_map(ast::LetStmt::cast))?;
        let pat = let_stmt.pat()?;

        let token_find = repo_expr.syntax().descendants_with_tokens().find_map(|it| {
            let Some(token) = it.into_token() else {
                return None;
            };
            if token.text() == "find" {
                Some(token)
            } else {
                None
            }
        });

        match token_find {
            Some(token_find) => {
                if !helper::is_method_call_of_find(sema, &token_find, bagua) {
                    return None;
                }
                todo!()
            }
            None => todo!(),
        }

        todo!()
    }

    fn find_entity_binding_recursive(
        sema: &Semantics,
        let_stmt: &ast::LetStmt,
    ) -> Option<ast::Name> {
        let pat = let_stmt.pat()?;

        let (bind_name, ty) = match pat {
            ast::Pat::IdentPat(ident_pat) => {
                (ident_pat.name()?, sema.type_of_binding_in_pat(&ident_pat)?)
            }
            ast::Pat::TupleStructPat(tuple_struct_pat) => {
                let ident_pat = tuple_struct_pat
                    .fields()
                    .next()?
                    .syntax()
                    .descendants()
                    .find_map(ast::IdentPat::cast)?;
                (ident_pat.name()?, sema.type_of_binding_in_pat(&ident_pat)?)
            }
            _ => return None,
        };

        match ty.as_adt()? {
            ra_ap_hir::Adt::Struct(struct_) => {
                let struct_ast = struct_.source(sema.db)?.value;
                if struct_ast.generic_param_list().is_none() {
                    return Some(bind_name);
                }
            }
            _ => {
                let usages = helper::usages_in_current_file(sema, &bind_name)?;
                let stmts = usages.into_iter().flat_map(|(_, refs)| {
                    refs.into_iter().filter_map(|it| match it.name {
                        ra_ap_ide_db::search::FileReferenceNode::NameRef(name_ref) => {
                            name_ref.syntax().ancestors().find_map(ast::LetStmt::cast)
                        }
                        _ => None,
                    })
                });
                for stmt in stmts {
                    let bind = Self::find_entity_binding_recursive(sema, &stmt);
                    if bind.is_some() {
                        return bind;
                    }
                }
            }
        }

        None
    }
}
