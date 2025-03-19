use anyhow::{Context, Result};
use ra_ap_hir::{
    db::{DefDatabase, HirDatabase, InternDatabase},
    Function, HasSource, HirDisplay, InFile, Struct, Trait, Type,
};
use ra_ap_hir_def::{type_ref::TraitBoundModifier, FunctionId};
use ra_ap_hir_ty::{Interner, TyKind};
use ra_ap_ide_db::{
    defs::{Definition, NameClass, NameRefClass},
    documentation::HasDocs,
};
use ra_ap_syntax::{
    ast::{self, HasName},
    AstNode, AstToken,
};

use crate::{
    entity::{NamedRefPath, Subset},
    loader::{BaguaProject, Usecase},
    Semantics,
};

pub struct FieldChecker {
    project: BaguaProject,
}

#[derive(Debug)]
pub struct FieldAccessViolation {
    pub local_def: Position,
    pub allowed_fields: Vec<NamedRefPath>,
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
    pub fn new(project: BaguaProject) -> Self {
        Self { project }
    }

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

    fn check_uc(&self, uc: &Usecase) -> anyhow::Result<Vec<FieldAccessViolation>> {
        let db = &self.project.db;
        let sema = Semantics::new(&self.project.db);
        let file_id = uc
            .module
            .as_source_file_id(db)
            .context("uc module has no file id ")?;
        let mod_syn = UcFileSyn(sema.parse(file_id));
        // dbg!(&mod_syn.0);

        let uc_impl = mod_syn.uc_impl(&self.project)?;
        let exec_fn = uc_impl.execute_fn()?;
        exec_fn.entity_local_defs(&sema, &self.project);

        Ok(vec![])
    }
}

struct UcFileSyn(ast::SourceFile);

#[derive(Debug, Clone)]
struct UcImpl(ast::Impl);

struct UcExecuteFn {
    fn_: ast::Fn,
    impl_: UcImpl,
}

#[derive(Debug)]
struct EntityLocalDef {
    subset: Subset,
    def: ast::Name,
}

impl UcFileSyn {
    fn uc_impl(&self, bagua: &BaguaProject) -> Result<UcImpl> {
        let impl_ = self
            .0
            .syntax()
            .descendants()
            .find_map(|node| {
                if let Some(impl_) = ast::Impl::cast(node) {
                    return Some(impl_);
                }
                None
            })
            .context("uc impl not found")?;

        Ok(UcImpl(impl_))
    }
}

impl UcImpl {
    fn execute_fn(&self) -> Result<UcExecuteFn> {
        let node = self.0.syntax();
        let fn_ = node
            .descendants()
            .find_map(|node| {
                if let Some(fn_) = ast::Fn::cast(node) {
                    let is_execute = fn_
                        .name()
                        .map(|name| name.text() == "execute")
                        .unwrap_or(false);
                    if is_execute {
                        return Some(fn_);
                    }
                }
                None
            })
            .context("uc execute fn not found")?;

        Ok(UcExecuteFn {
            fn_,
            impl_: self.clone(),
        })
    }
}

impl UcExecuteFn {
    fn entity_local_defs(&self, sema: &Semantics, project: &BaguaProject) -> Vec<EntityLocalDef> {
        for stmt in self.fn_.body().unwrap().statements() {
            self.get_subset_ty(sema, &stmt);
            // if let Some(def) = self.find_def_binding(sema, &stmt, project) {
            //     // dbg!(def);
            //     // dbg!(stmt.syntax().text());
            //     // println!("==================");
            // }
        }
        vec![]
    }

    fn get_subset_ty(&self, sema: &Semantics, stmt: &ast::Stmt) -> Option<Type> {
        let ast::Stmt::LetStmt(let_stmt) = stmt else {
            return None;
        };
        let init = let_stmt.initializer()?;
        let call_ident = init.syntax().descendants_with_tokens().find_map(|node| {
            let token = node.into_token()?;
            if token.text() != "find" {
                return None;
            }
            if ast::Ident::can_cast(token.kind()) {
                Some(token)
            } else {
                None
            }
        })?;

        let call_ident = sema.descend_into_macros(call_ident).into_iter().next()?;
        let name_ref = ast::NameRef::cast(call_ident.parent()?)?;
        let expr = name_ref.syntax().ancestors().find_map(|node| {
            if let Some(expr) = ast::MethodCallExpr::cast(node.clone()) {
                return Some(FindExpr::MethodCall(expr));
            }
            if let Some(expr) = ast::PathExpr::cast(node) {
                return Some(FindExpr::PathExpr(expr));
            }
            None
        })?;

        enum FindExpr {
            MethodCall(ast::MethodCallExpr),
            PathExpr(ast::PathExpr),
        }

        let s: ast::Struct = todo!();
        match s.field_list()? {
            ast::FieldList::RecordFieldList(record_field_list) => {
                for f in record_field_list.fields() {
                    Definition::Field(sema.to_def(&f)?).usages(sema);
                }

                todo!()
            }
            ast::FieldList::TupleFieldList(tuple_field_list) => todo!(),
        }

        match expr {
            FindExpr::MethodCall(method_call_expr) => {
                todo!()
            }
            FindExpr::PathExpr(path_expr) => todo!(),
        }

        // let name_ref_class = NameRefClass::classify(sema, &name_ref)?;

        // let function = match name_ref_class {
        //     NameRefClass::Definition(definition) => match definition {
        //         ra_ap_ide_db::defs::Definition::Function(function) => function,
        //         _ => return None,
        //     },
        //     _ => {
        //         return None;
        //     }
        // };

        // dbg!(&name_ref_class);
        let fn_ = stmt.syntax().ancestors().find_map(ast::Fn::cast)?;
        let fn_hir = sema.to_def(&fn_)?;
        let infre_res = sema
            .db
            .infer(ra_ap_hir_def::DefWithBodyId::FunctionId(FunctionId::from(
                fn_hir,
            )));

        let (body, source_map) =
            sema.db
                .body_with_source_map(ra_ap_hir_def::DefWithBodyId::FunctionId(FunctionId::from(
                    fn_hir,
                )));

        let init = init
            .syntax()
            .descendants()
            .find_map(|e| ast::MethodCallExpr::cast(e))?;

        let file_id = sema.hir_file_for(init.syntax());
        let in_file = InFile::new(file_id, ast::Expr::from(init.clone()));

        let expr_hir_id = match source_map.node_expr(in_file.as_ref())? {
            ra_ap_hir_def::hir::ExprOrPatId::ExprId(idx) => idx,
            ra_ap_hir_def::hir::ExprOrPatId::PatId(idx) => return None,
        };

        dbg!(init.syntax());
        let (fun_id, subtitution) = dbg!(infre_res.method_resolution(expr_hir_id))?;
        let fun_ast = sema.source(Function::from(fun_id))?;
        dbg!(&fun_ast.value.syntax().text());

        // ra_ap_hir::Struct::from(value)

        let args: Vec<chalk_ir::GenericArg<ra_ap_hir_ty::Interner>> =
            subtitution.interned().to_vec();
        for arg in args {
            let arg = arg.interned();
            match arg {
                chalk_ir::GenericArgData::Ty(ty) => {
                    let a = ty.data(Interner);
                    if let TyKind::Adt(adt_id, _) = a.kind {
                        let id = adt_id.0;
                        let struct_id = match id {
                            ra_ap_hir::AdtId::StructId(struct_id) => struct_id,
                            _ => return None,
                        };
                        let struct_def = Struct::from(struct_id);
                        let source = struct_def.source(sema.db);
                        dbg!(&source);

                        // sema.resolve_type(ty)
                        // let struct_def = sema.db.lookup_intern_struct(struct_id);
                    }

                    let t = ty.interned();
                }
                chalk_ir::GenericArgData::Lifetime(lifetime) => todo!(),
                chalk_ir::GenericArgData::Const(_) => todo!(),
            }
        }

        None
    }

    fn find_def_binding(
        &self,
        sema: &Semantics,
        stmt: &ast::Stmt,
        project: &BaguaProject,
    ) -> Option<ast::Name> {
        let ast::Stmt::LetStmt(let_stmt) = stmt else {
            return None;
        };

        let init = let_stmt.initializer()?;
        if !self.is_find_method_call(&init) {
            return None;
        }

        let pat = let_stmt.pat()?;
        let name = pat.get_name()?;
        let entity_trait = project.bagua.entity_trait;

        let ty = match pat {
            ast::Pat::TupleStructPat(tuple_struct_pat) => {
                let ident_pat = tuple_struct_pat.fields().next()?.get_ident_pat()?;
                sema.type_of_binding_in_pat(&ident_pat)?
            }
            ast::Pat::IdentPat(ident_pat) => sema.type_of_binding_in_pat(&ident_pat)?,
            _ => {
                return None;
            }
        };
        if is_entity_type(&ty, sema, entity_trait.0) {
            dbg!(&name);
            dbg!(stmt.syntax().text());
            println!("==================");

            return Some(name);
        }

        if let NameClass::Definition(def) = NameClass::classify(sema, &name).unwrap() {
            for (_, usages) in def.usages(sema).all() {
                for usage in usages {
                    let stmt = usage.name.syntax().ancestors().find_map(ast::Stmt::cast)?;
                    if let Some(def) = self.find_def_binding(sema, &stmt, project) {
                        return Some(def);
                    }
                }
            }
        }

        fn is_entity_type(ty: &Type, sema: &Semantics, entity_trait: Trait) -> bool {
            let Some(adt) = ty.as_adt() else {
                return false;
            };
            match adt {
                ra_ap_hir::Adt::Struct(_) => ty.impls_trait(sema.db, entity_trait, &[]),
                _ => false,
            }
        }

        None
    }

    fn tuple_struct_pat_local_def(
        &self,
        sema: &Semantics,
        pat: &ast::TupleStructPat,
    ) -> Option<EntityLocalDef> {
        let ident_pat = pat.fields().next()?.get_ident_pat()?;

        let ty = sema.type_of_binding_in_pat(&ident_pat)?;
        let display = ty.display(sema.db, ra_ap_ide::Edition::Edition2021);
        println!("{}", display);

        // let ty = ty.as_adt()?;
        // match ty {
        //     Adt::Struct(struct_) => {}
        //     Adt::Union(union) => todo!(),
        //     Adt::Enum(_) => todo!(),
        // }

        None
    }

    fn is_find_method_call(&self, expr: &ast::Expr) -> bool {
        true
    }
}

trait PatHelper {
    fn get_name(&self) -> Option<ast::Name>;

    fn get_ident_pat(&self) -> Option<ast::IdentPat>;
}

impl PatHelper for ast::Pat {
    fn get_ident_pat(&self) -> Option<ast::IdentPat> {
        self.syntax().descendants().find_map(ast::IdentPat::cast)
    }

    fn get_name(&self) -> Option<ast::Name> {
        self.syntax().descendants().find_map(ast::Name::cast)
    }
}
