use std::collections::HashMap;

use anyhow::Context;
use anyhow::Result;
use chalk_ir::TyKind;
use ra_ap_hir::{
    db::{DefDatabase, HirDatabase},
    Adt, HasSource, InFile, Struct,
};
use ra_ap_hir_def::{hir::ExprId, DefWithBodyId, FunctionId};
use ra_ap_hir_ty::{InferenceResult, Interner};
use ra_ap_ide_db::defs::{Definition, NameClass};
use ra_ap_syntax::ast::HasAttrs;
use ra_ap_syntax::{
    ast::{self, HasName as _},
    AstNode, AstToken, NodeOrToken, SyntaxNode, SyntaxToken,
};

use crate::{
    entity::NamedRefPath,
    helper,
    loader::{Bagua, BaguaProject, Usecase},
    Semantics,
};

pub struct FieldChecker<'a> {
    project: &'a BaguaProject,
}

#[derive(Debug, Clone)]
pub struct FieldAccessViolation {
    entity_var_def: ast::Name,
    trace: Vec<FieldAccessExpr>,
}

#[derive(Debug, Clone)]
pub enum FieldAccessExpr {
    FieldExpr(ast::FieldExpr),
}

#[derive(Debug, Clone)]
pub struct EntityLocalVar {
    local_bind: EntityBinding,
    fields: HashMap<NamedRefPath, FieldState>,
}

#[derive(Debug, Clone)]
pub enum FieldState {
    Unloaded,
    Unchanged,
    Set,
}

#[derive(Debug, Clone)]
struct EntityBinding {
    pat: ast::IdentPat,
    entity: Struct,
}

impl<'a> FieldChecker<'a> {
    pub fn new(project: &'a BaguaProject) -> Self {
        Self { project }
    }

    pub fn check(&self) -> anyhow::Result<Vec<FieldAccessViolation>> {
        let mut violations = Vec::new();
        for bc in &self.project.bc_crates {
            let ucs = bc.use_cases(&self.project.db);
            for uc in ucs {
                let sema = Semantics::new(&self.project.db);
                play_fn_access(&sema, &uc);
                // self.check_uc(&uc)?;
                // uc.debug_print(&self.project.db);
            }
        }

        Ok(violations)
    }

    fn check_uc(&self, uc: &Usecase) -> anyhow::Result<Vec<FieldAccessViolation>> {
        let sema = Semantics::new(&self.project.db);
        let file_id = uc
            .module
            .as_source_file_id(&self.project.db)
            .context("uc module not found")?;
        let file_ast = sema.parse(file_id);
        dbg!(&file_ast);
        let entity_local_defs = self.entity_local_defs(&sema, &file_ast);
        dbg!(&entity_local_defs);

        Ok(vec![])
    }

    fn entity_local_defs(
        &self,
        sema: &Semantics,
        file_ast: &ast::SourceFile,
    ) -> Vec<EntityLocalVar> {
        let method_find_call_stmts = file_ast
            .syntax()
            .descendants()
            .filter_map(|node| self.method_find_call_stmt(&sema, node));

        let mut entity_local_vars = Vec::new();
        for (pat, subset_ty) in method_find_call_stmts {
            let entity_local_var = self.entity_local_var(&sema, pat, subset_ty);
            entity_local_vars.extend(entity_local_var);
        }

        entity_local_vars
    }

    fn entity_local_var(
        &self,
        sema: &Semantics,
        pat: ast::Pat,
        subset_ty: Struct,
    ) -> Option<EntityLocalVar> {
        let local_var = self.track_entity_binding(sema, pat)?;
        let entity_fields = local_var.fields(sema).ok()?;
        let subset_fields = self.subset_fields(sema, subset_ty, &self.project.bagua);

        let mut fields = HashMap::new();
        for field in entity_fields {
            fields.insert(field, FieldState::Unloaded);
        }
        for field in subset_fields {
            fields.insert(field, FieldState::Unchanged);
        }

        Some(EntityLocalVar {
            local_bind: local_var,
            fields,
        })
    }

    fn method_find_call_stmt(
        &self,
        sema: &Semantics,
        node: SyntaxNode,
    ) -> Option<(ast::Pat, Struct)> {
        let stmt = ast::LetStmt::cast(node)?;
        let call_expr = stmt.initializer()?;
        let pat = stmt.pat()?;

        let fn_ = stmt.syntax().ancestors().find_map(ast::Fn::cast)?;
        let fn_hir = sema.to_def(&fn_)?;
        let fn_id = DefWithBodyId::FunctionId(FunctionId::from(fn_hir));

        let method_call_expr = MethodFindCallExpr::try_new(call_expr, &sema, &self.project.bagua)?;
        let expr_hir_id = method_call_expr.clone().hir_id(sema, fn_id)?;

        let infer = sema.db.infer(fn_id);
        let subtitution = match &method_call_expr {
            MethodFindCallExpr::PathExpr(_) => {
                let func_ty = infer.type_of_expr_or_pat(From::from(expr_hir_id))?;

                if let TyKind::FnDef(_fn_def, subs) = func_ty.kind(Interner) {
                    subs.clone()
                } else {
                    return None;
                }
            }
            MethodFindCallExpr::MethodCallExpr(_) => {
                let (_fun_id, subtitution) = infer.method_resolution(expr_hir_id).unwrap();
                // dbg!(&subtitution);
                subtitution
            }
        };

        let subset_arg: chalk_ir::GenericArg<ra_ap_hir_ty::Interner> =
            subtitution.interned().to_vec().into_iter().next()?;
        match subset_arg.interned() {
            chalk_ir::GenericArgData::Ty(ty) => {
                let ty_data = ty.interned();
                if let TyKind::Adt(adt_id, _) = ty_data.kind {
                    let id = adt_id.0;
                    let struct_id = match id {
                        ra_ap_hir::AdtId::StructId(struct_id) => struct_id,
                        _ => return None,
                    };
                    let struct_def = Struct::from(struct_id);
                    // dbg!(struct_def.source(sema.db));
                    return Some((pat, struct_def));
                }
            }
            _ => {}
        }

        None
    }

    fn subset_fields(&self, sema: &Semantics, ty: Struct, bagua: &Bagua) -> Vec<NamedRefPath> {
        fn subset_fields_opt(
            sema: &Semantics,
            ty: Struct,
            bagua: &Bagua,
        ) -> Option<Vec<NamedRefPath>> {
            let subset_ast = reparse_struct(sema, ty).unwrap();
            let fields = match subset_ast.field_list()? {
                ast::FieldList::RecordFieldList(record_field_list) => record_field_list,
                ast::FieldList::TupleFieldList(_) => return None,
            };
            let fields = fields.fields();
            let mut subset_fields = vec![];
            for field in fields {
                let field_name = field.name()?;
                let field_type = field.ty()?;
                let current_field_path = NamedRefPath::new_segment(field_name.text().to_string());

                let field_type_hir = sema.resolve_type(&field_type)?;
                if bagua.impls_subset(sema.db, &field_type_hir) {
                    let grouped_fields = subset_fields_opt(sema, ty, bagua)?;
                    let grouped_fields = grouped_fields
                        .into_iter()
                        .map(|field| current_field_path.join(&field));
                    subset_fields.extend(grouped_fields);
                } else {
                    subset_fields.push(current_field_path);
                }
            }

            Some(subset_fields)
        }

        subset_fields_opt(sema, ty, bagua).unwrap()
    }

    fn track_entity_binding(&self, sema: &Semantics, pat: ast::Pat) -> Option<EntityBinding> {
        let ident_pat = pat.syntax().descendants().find_map(ast::IdentPat::cast)?;
        let ty = sema.type_of_pat(&ast::Pat::IdentPat(ident_pat.clone()))?;
        if let Adt::Struct(s) = ty.original.as_adt()? {
            return Some(EntityBinding {
                pat: ident_pat,
                entity: s,
            });
        }
        let usages = helper::usages_in_current_file(sema, &ident_pat.name()?)?;
        for (_, usages) in usages {
            for usage in usages {
                let stmt = usage.name.syntax().ancestors().find_map(ast::Stmt::cast)?;

                let pat = stmt.syntax().descendants().find_map(ast::Pat::cast)?;
                let entity_binding = self.track_entity_binding(sema, pat);
                if entity_binding.is_some() {
                    return entity_binding;
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
enum MethodFindCallExpr {
    PathExpr(ast::PathExpr),
    MethodCallExpr(ast::MethodCallExpr),
}

impl MethodFindCallExpr {
    fn try_new(expr: ast::Expr, sema: &Semantics, bagua: &Bagua) -> Option<Self> {
        let token_find = expr.syntax().descendants_with_tokens().find_map(|nt| {
            let token = nt.into_token()?;
            if token.text() == "find" {
                return Some(token);
            }
            None
        })?;
        let token_find = sema.descend_into_macros(token_find).into_iter().next()?;

        if !helper::is_method_call_of_find(sema, &token_find, bagua) {
            return None;
        }

        let expr = token_find.parent_ancestors().find_map(|node| {
            if let Some(expr) = ast::PathExpr::cast(node.clone()) {
                return Some(MethodFindCallExpr::PathExpr(expr));
            }
            if let Some(expr) = ast::MethodCallExpr::cast(node) {
                return Some(MethodFindCallExpr::MethodCallExpr(expr));
            }
            None
        });

        expr
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            MethodFindCallExpr::PathExpr(expr) => expr.syntax(),
            MethodFindCallExpr::MethodCallExpr(expr) => expr.syntax(),
        }
    }

    fn to_expr(self) -> ast::Expr {
        match self {
            MethodFindCallExpr::PathExpr(expr) => ast::Expr::from(expr),
            MethodFindCallExpr::MethodCallExpr(expr) => ast::Expr::from(expr),
        }
    }

    fn hir_id(self, sema: &Semantics, fn_id: DefWithBodyId) -> Option<ExprId> {
        let (_, source_map) = sema.db.body_with_source_map(fn_id);
        let file_id = sema.hir_file_for(self.syntax());
        let in_file = InFile::new(file_id, self.to_expr());

        match source_map.node_expr(in_file.as_ref())? {
            ra_ap_hir_def::hir::ExprOrPatId::ExprId(idx) => Some(idx),
            ra_ap_hir_def::hir::ExprOrPatId::PatId(_idx) => None,
        }
    }
}

struct FieldGroup {
    scalar_like: Vec<String>,
    group: Vec<(String, FieldGroup)>,
}

impl FieldGroup {
    fn flatten_fields(self, sema: &Semantics) -> Vec<NamedRefPath> {
        let mut fields = vec![];
        for scalar in self.scalar_like {
            fields.push(NamedRefPath::new_segment(scalar));
        }
        for (group, group_fields) in self.group {
            let prefix = NamedRefPath::new_segment(group);
            let group_fields = group_fields.flatten_fields(sema);
            for field in group_fields {
                fields.push(prefix.join(&field));
            }
        }
        fields
    }

    fn parse_from_entity_ast(sema: &Semantics, node: ast::Struct) -> Result<Self> {
        let field_list = node.field_list().context("entity field list not found")?;
        let ast::FieldList::RecordFieldList(field_list) = field_list else {
            return Err(anyhow::anyhow!(
                "entity field list is not a record field list"
            ));
        };

        let mut scalar_like = vec![];
        let mut group = vec![];
        for field in field_list.fields() {
            let field_type = field_type(&field).context("field type not found")?;
            let field_name = field.name().unwrap().text().to_string();
            match field_type {
                FieldType::Scalar => scalar_like.push(field_name),
                FieldType::Group => {
                    let ty = sema
                        .resolve_type(&field.ty().unwrap())
                        .context("field type not found")?;
                    let struct_ = ty
                        .as_adt()
                        .context("field type is not an adt")?
                        .as_struct()
                        .context("field type is not a struct")?;

                    let ast = struct_.source(sema.db).unwrap();
                    let origin_ast = ast.original_ast_node_rooted(sema.db).unwrap();
                    let field_group = FieldGroup::parse_from_entity_ast(sema, origin_ast.value)?;
                    group.push((field_name, field_group));
                }
                FieldType::SysId => {
                    scalar_like.push(field_name);
                }
            }
        }

        Ok(Self { scalar_like, group })
    }
}

fn field_type(field: &ast::RecordField) -> Option<FieldType> {
    let name = field.name()?;
    let attrs = field.attrs();

    for attr in attrs {
        let meta = attr.meta()?;
        let path = meta.path()?;
        let token_tree = meta.token_tree()?;
        if path.to_string() != "entity" {
            continue;
        }
        let group = token_tree
            .syntax()
            .descendants_with_tokens()
            .find_map(|nt| {
                let token = nt.into_token()?;
                if token.text() == "group" {
                    return Some(FieldType::Group);
                }
                None
            });
        if group.is_some() {
            return Some(FieldType::Group);
        }
    }

    if name.text() == "id" {
        return Some(FieldType::SysId);
    }

    Some(FieldType::Scalar)
}

fn reparse_struct(sema: &Semantics, node: Struct) -> Result<ast::Struct> {
    let source = node.source(sema.db).unwrap();
    let name = source.value.name().context("struct name not found")?;
    let file = sema.parse_or_expand(source.file_id);
    let struct_ = file
        .descendants()
        .find_map(|node| {
            if let Some(struct_) = ast::Struct::cast(node) {
                if struct_.name()? == name {
                    return Some(struct_);
                }
            }
            None
        })
        .context("struct not found")?;

    Ok(struct_)
}

pub enum FieldType {
    SysId,
    Scalar,
    Group,
}

impl EntityBinding {
    fn fields(&self, sema: &Semantics) -> Result<Vec<NamedRefPath>> {
        let source = self.entity.source(sema.db).unwrap();
        let origin_ast = source
            .original_ast_node_rooted(sema.db)
            .context("entity source not found")?;
        let struct_name = origin_ast.value.name().context("entity name not found")?;
        let origin_file = sema.parse(origin_ast.file_id);
        let struct_ast = origin_file
            .syntax()
            .descendants()
            .find_map(|node| {
                if let Some(struct_) = ast::Struct::cast(node) {
                    if struct_.name()? == struct_name {
                        return Some(struct_);
                    }
                }
                None
            })
            .context("struct not found")?;

        let field_group = FieldGroup::parse_from_entity_ast(sema, struct_ast)?;
        let fields = field_group.flatten_fields(sema);

        Ok(fields)
    }
}

struct EntityFindStmt {
    stmt: ast::Stmt,
    subset: Struct,
}

struct FieldAccessTrace {}

impl EntityLocalVar {}

fn play_fn_access(sema: &Semantics, uc: &Usecase) {
    let file_id = uc
        .module
        .as_source_file_id(sema.db)
        .context("uc module not found")
        .unwrap();
    let file_ast = sema.parse(file_id);
    let fn_ = file_ast
        .syntax()
        .children()
        .find_map(ast::Fn::cast)
        .unwrap();

    play_fn(sema, fn_);
}

fn play_fn(sema: &Semantics, fn_: ast::Fn) -> Option<()> {
    dbg!(&fn_);
    // let aa_arg = fn_.param_list()?.params().next().unwrap();
    // let bind = aa_arg
    //     .pat()
    //     .unwrap()
    //     .syntax()
    //     .descendants()
    //     .find_map(ast::IdentPat::cast)?;
    // let usages = helper::usages_in_current_file(sema, &bind.name()?)?;
    let stmts = fn_.body()?.statements();
    for stmt in stmts {
        dbg!(stmt.syntax().text());
        let exprs = stmt.syntax().descendants().filter_map(ast::Expr::cast);
        for expr_ast in exprs {
            let expr_ty = sema.type_of_expr(&ast::Expr::from(expr_ast.clone()))?;
            dbg!(expr_ast.syntax().text());
            dbg!(&expr_ast);
            dbg!(&expr_ty);
            println!("");
        }

        println!("==================================================")
    }
    // for (_, usages) in usages {
    //     for usage in usages {
    //         let name_ref = usage.name.as_name_ref().unwrap();
    //         let stmt = name_ref.syntax().ancestors().find_map(ast::Stmt::cast)?;
    //         dbg!(stmt.syntax().text());
    //         let exprs = stmt.syntax().descendants().filter_map(ast::Expr::cast);
    //         for expr_ast in exprs {
    //             let expr_ty = sema.type_of_expr(&ast::Expr::from(expr_ast.clone()))?;
    //             dbg!(expr_ast.syntax().text());
    //             dbg!(&expr_ast);
    //             dbg!(&expr_ty);
    //             println!("");
    //         }

    //         // let expr = name_ref
    //         //     .syntax()
    //         //     .ancestors()
    //         //     .find_map(ast::FieldExpr::cast)?;
    //         // let expr_ty = sema.type_of_expr(&ast::Expr::from(expr.clone()))?;
    //         // dbg!(&expr);
    //         // dbg!(&expr_ty);

    //         // let Some(ref_expr) = expr.syntax().ancestors().find_map(ast::RefExpr::cast) else {
    //         //     continue;
    //         // };
    //         // let ref_expr_ty = sema.type_of_expr(&ast::Expr::from(ref_expr.clone()))?;
    //         // dbg!(&ref_expr);
    //         // dbg!(&ref_expr_ty);

    //         println!("-----------------")
    //     }
    // }

    None
}

// pub enum EntityUsageKind {
//     ScalarField(EntityFieldErxpr),
//     GroupFieldExpr(Box<EntityUsageKind>),
// }

// pub enum EntityFieldExpr {
//     Ref,
//     MethodCall,
//     Arg,
// }

mod aa {
    use std::{collections::HashMap, sync::Arc};

    use ra_ap_syntax::{ast, SmolStr};

    use crate::entity::NamedRefPath;

    pub struct FieldGroup {
        fields: Arc<HashMap<String, Field>>,
    }

    pub enum VirtualFieldGroup {
        Struct { fields: Arc<HashMap<String, Field>> },
        Enum {},
    }

    pub enum Field {
        AlwaysLoaded,
        ScalarField,
        GroupField(Box<FieldGroup>),
    }

    pub enum FieldAccessExpr {
        // `entity.name` in `entity.name.len()`
        FieldExpr(ast::FieldExpr),
        // `&entity.name` in `let _: &MyString = &entity.name;`
        // `&entity.group.g1` in `let _: &MyString = &entity.group.g1;`
        // `&(&entity.name)` in `let _: &MyString = &(&entity.name);`
        RefFieldExpr(ast::RefExpr),
        // `*(&entity.name)` in `let _: &MyString = &(&entity.name);`
        // `*bb.group.g1` in `let bb_g1: u32 = *bb.group.g1;`
        DerefFieldExpr(ast::FieldExpr),
    }

    pub enum FieldReassign {
        LetBind(ast::Pat, ast::Expr),
        MethodCallArg(ast::CallExpr, ast::Expr),
    }

    pub enum EntityFieldTraceKind {
        FieldAccess(FieldAccessExpr),
        FieldReassign(FieldReassign),
    }

    pub struct FieldAccessNode {
        kind: EntityFieldTraceKind,
    }

    pub struct FieldAccessChain {
        nodes: Vec<FieldAccessNode>,
        field_path: NamedRefPath,
    }
    pub struct FieldGroupAccessNode {}

    fn aa() {}
}

mod bb {
    use std::{collections::HashMap, sync::Arc};

    use ra_ap_hir::TypeInfo;
    use ra_ap_syntax::{
        ast::{self, HasName, NameRef},
        match_ast, AstNode, SyntaxKind,
    };

    use crate::{entity::NamedRefPath, helper, Semantics};

    pub enum FieldAccessExpr {
        // `entity.name` in `entity.name.len()`
        FieldExpr(ast::FieldExpr),
        // `&entity.name` in `let _: &MyString = &entity.name;`
        // `&entity.group.g1` in `let _: &MyString = &entity.group.g1;`
        // `&(&entity.name)` in `let _: &MyString = &(&entity.name);`
        RefFieldExpr(ast::RefExpr),
        // `*(&entity.name)` in `let _: &MyString = &(&entity.name);`
        // `*bb.group.g1` in `let bb_g1: u32 = *bb.group.g1;`
        DerefFieldExpr(ast::PrefixExpr),
    }

    pub enum Rebind {
        LetBind(ast::Pat),
        MethodArg(ast::Pat),
        ReturnValue(ast::Expr),
        StructRecord(ast::NameRef),
    }

    pub enum FieldAccessNode {
        FieldAccess(FieldAccessExpr),
        Rebind(Rebind),
        GroupSelf(ast::Expr),
        GroupSelfRef(ast::RefExpr),
    }

    pub struct FieldAccessChain {
        nodes: Vec<FieldAccessNode>,
        scalar_deref: EntityFieldDerefAccess,
        field_path: NamedRefPath,
    }

    pub enum FieldType {
        Scalar,
        Group,
    }

    impl FieldAccessChain {
        fn track(sema: &Semantics, def: super::EntityLocalVar) -> Vec<Self> {
            let usages =
                helper::usages_in_current_file(sema, &def.local_bind.pat.name().unwrap()).unwrap();

            for (_, usages) in usages {
                for usage in usages {
                    let name_ref = usage.name.as_name_ref().unwrap();
                    todo!()
                }
            }
            todo!()
        }

        fn field_group_usage(
            sema: &Semantics,
            ref_expr: &ast::PathExpr,
            group: &FieldGroup,
        ) -> Option<FieldAccessNode> {
            let parent = ref_expr.syntax().parent()?;
            if ast::FieldExpr::can_cast(parent.kind()) {
                let parent = ast::FieldExpr::cast(parent).unwrap();
                let field_name = parent.name_ref()?;
                let field = group.fields.get(field_name.ident_token()?.text()).unwrap();
                match field {
                    Field::AlwaysLoaded => {}
                    Field::ScalarField => {}
                    Field::GroupField(field_group) => todo!(),
                }

                todo!()
            }

            // let exprs = name_ref
            //     .syntax()
            //     .ancestors()
            //     .take_while(|n| !ast::Stmt::can_cast(n.kind()))
            //     .find_map(ast::Expr::cast);

            todo!()
        }

        fn scalar_access(sema: &Semantics, expr: ast::FieldExpr) -> Option<EntityFieldDerefAccess> {
            if is_field_deref(sema, expr.clone()) {
                todo!()
                // return Some(EntityFieldDerefAccess::new(expr));
            }

            let exprs = expr
                .syntax()
                .ancestors()
                .skip(1)
                .take_while(|n| ScalarFieldDeRef::can_cast(n.kind()))
                .filter_map(|n| ScalarFieldDeRef::cast(n))
                .collect::<Vec<_>>();
            let outter_expr = exprs.last().cloned()?;

            for expr in exprs {
                if is_field_deref(sema, expr) {
                    todo!()
                }
            }
            let parent = outter_expr.syntax().parent()?;
            match parent.kind() {
                SyntaxKind::LET_STMT => {
                    let parent = ast::LetStmt::cast(parent).unwrap();
                    let pat = parent.pat();
                }
                SyntaxKind::METHOD_CALL_EXPR => {
                    let parent = ast::MethodCallExpr::cast(parent).unwrap();
                    sema.resolve_method_call(&parent);
                }
                SyntaxKind::ARG_LIST => {
                    let arg_list = ast::ArgList::cast(parent).unwrap();
                    let call_expr = ast::CallExpr::cast(arg_list.syntax().parent()?);
                }
                _ => {}
            }

            todo!()
        }
    }

    enum ScalarAssign {
        LetBind(ast::LetStmt),
        MethodCall(ast::MethodCallExpr),
        CallExpr(ast::CallExpr),
    }

    impl ScalarAssign {
        fn track_trace(self, sema: &Semantics) -> Option<Vec<FieldAccessNode>> {
            todo!()
        }
    }

    #[derive(Clone)]
    enum ScalarFieldDeRef {
        Ref(ast::RefExpr),
        Deref(ast::PrefixExpr),
    }

    impl From<ScalarFieldDeRef> for ast::Expr {
        fn from(value: ScalarFieldDeRef) -> Self {
            match value {
                ScalarFieldDeRef::Ref(ref_expr) => todo!(),
                ScalarFieldDeRef::Deref(prefix_expr) => todo!(),
            }
        }
    }

    impl AstNode for ScalarFieldDeRef {
        fn can_cast(kind: ra_ap_syntax::SyntaxKind) -> bool
        where
            Self: Sized,
        {
            match kind {
                ra_ap_syntax::SyntaxKind::REF_EXPR => true,
                ra_ap_syntax::SyntaxKind::PREFIX_EXPR => true,
                _ => false,
            }
        }

        fn cast(syntax: ra_ap_syntax::SyntaxNode) -> Option<Self>
        where
            Self: Sized,
        {
            match syntax.kind() {
                ra_ap_syntax::SyntaxKind::REF_EXPR => {
                    Some(Self::Ref(ast::RefExpr::cast(syntax).unwrap()))
                }
                ra_ap_syntax::SyntaxKind::PREFIX_EXPR => {
                    Some(Self::Deref(ast::PrefixExpr::cast(syntax).unwrap()))
                }
                _ => None,
            }
        }

        fn syntax(&self) -> &ra_ap_syntax::SyntaxNode {
            match self {
                ScalarFieldDeRef::Ref(ref_expr) => ref_expr.syntax(),
                ScalarFieldDeRef::Deref(prefix_expr) => prefix_expr.syntax(),
            }
        }
    }

    struct AccessStmt {
        pat: Option<ast::Pat>,
        exprs: Vec<ast::Expr>,
    }

    impl AccessStmt {
        fn new(inner_expr: ast::Expr) -> Self {
            let mut ancestors = inner_expr.syntax().ancestors();
            let mut exprs = vec![];
            let mut pat = None;
            while let Some(ancestor) = ancestors.next() {
                if ast::Expr::can_cast(ancestor.kind()) {
                    exprs.push(ast::Expr::cast(ancestor).unwrap());
                } else if ast::Stmt::can_cast(ancestor.kind()) {
                    let stmt = ast::Stmt::cast(ancestor).unwrap();
                    if let ast::Stmt::LetStmt(let_stmt) = stmt {
                        pat = let_stmt.pat();
                    }
                    break;
                } else {
                    break;
                }
            }

            Self { pat, exprs }
        }
    }

    struct EntityFieldDerefAccess(ast::Expr);

    impl EntityFieldDerefAccess {
        fn new<T>(expr: T) -> Self
        where
            ast::Expr: From<T>,
        {
            todo!()
        }
    }

    fn is_field_deref<T>(sema: &Semantics, expr: T) -> bool
    where
        ast::Expr: From<T>,
    {
        todo!()
    }

    pub struct FieldGroup {
        fields: Arc<HashMap<String, Field>>,
    }

    pub enum Field {
        AlwaysLoaded,
        ScalarField,
        GroupField(Box<FieldGroup>),
    }
}

mod cc {
    use std::{collections::HashMap, sync::Arc};

    use ra_ap_syntax::{
        ast::{self, HasName},
        AstNode, SyntaxKind, SyntaxNode,
    };

    use crate::{entity::NamedRefPath, helper, Semantics};

    pub struct TraceNodeRightValueAccess {
        node: TraceNodeRightValue,
        deref_triggered: bool,
        field_path: Option<NamedRefPath>,
    }

    pub enum TraceNodeRightValue {
        FieldExpr(ast::FieldExpr),
        RefFieldExpr(ast::RefExpr),
        StarDeref(ast::PrefixExpr),
        PassSelf(ast::PathExpr),
    }

    #[derive(Clone)]
    pub enum TraceNode {
        FieldExpr(ast::FieldExpr),
        RefFieldExpr(ast::RefExpr),
        StarDeref(ast::PrefixExpr),
        PassSelf(ast::PathExpr),
        Bind(ast::Pat),
    }

    pub enum DerefTrigger {
        FieldExpr(ast::FieldExpr),
        RefFieldExpr(ast::RefExpr),
        StarDeref(ast::PrefixExpr),
    }

    pub struct AccessTrace {
        nodes: Vec<TraceNode>,
        deref_at: DerefTrigger,
        field_path: NamedRefPath,
    }

    pub struct FieldGroup {
        fields: Arc<HashMap<String, Field>>,
    }

    pub enum Field {
        AlwaysLoaded,
        ScalarField,
        GroupField(Box<FieldGroup>),
    }

    impl AccessTrace {
        fn track(sema: &Semantics, bind: ast::Pat, group: FieldGroup) -> Vec<AccessTrace> {
            let ident = match bind.clone() {
                ast::Pat::IdentPat(ident_pat) => ident_pat,
                _ => {
                    todo!()
                }
            };
            let usages = helper::usages_in_current_file(sema, &ident.name().unwrap()).unwrap();

            let mut nodes = vec![TraceNode::Bind(bind)];
            let mut traces = vec![];

            for (_, usages) in usages {
                for usage in usages {
                    let name_ref = usage.name.as_name_ref().unwrap();
                    let right_value =
                        TraceNodeRightValue::from_name_ref(sema, name_ref, &group).unwrap();
                    nodes.push(right_value.node.to_trace_node());

                    if right_value.deref_triggered {
                        traces.push(AccessTrace {
                            nodes: nodes.clone(),
                            deref_at: todo!(),
                            field_path: right_value.field_path.unwrap(),
                        })
                    } else {
                        todo!()
                    }

                    todo!()
                }
            }

            todo!()
        }
    }

    impl TraceNodeRightValue {
        fn to_trace_node(self) -> TraceNode {
            todo!()
        }

        fn from_name_ref(
            sema: &Semantics,
            group_ref: &ast::NameRef,
            field_group: &FieldGroup,
        ) -> Option<TraceNodeRightValueAccess> {
            let path_expr = group_ref
                .syntax()
                .ancestors()
                .find_map(ast::PathExpr::cast)?;

            let mut res_expr = Self::PassSelf(path_expr.clone());

            loop {
                let Some(parent_expr) = res_expr.syntax().parent().and_then(ast::Expr::cast) else {
                    break;
                };

                let ty = sema.type_of_expr(&parent_expr)?;

                match parent_expr {
                    ast::Expr::FieldExpr(field_expr) => {
                        let name_ref = field_expr.name_ref()?;
                        let field = name_ref.text();
                        res_expr = Self::FieldExpr(field_expr);
                        match field_group.fields.get(field.as_str())? {
                            Field::AlwaysLoaded => return None,
                            Field::ScalarField => {
                                let is_deref = ty.adjusted.is_some();
                                if is_deref {
                                    return Some(TraceNodeRightValueAccess {
                                        node: res_expr,
                                        deref_triggered: is_deref,
                                        field_path: todo!(),
                                    });
                                }
                            }
                            Field::GroupField(field_group) => {}
                        };
                    }
                    _ => {
                        todo!()
                    }
                }
            }

            todo!()
            // Some(res_expr)
        }

        fn syntax(&self) -> SyntaxNode {
            todo!()
        }
    }
}

mod dd {
    use std::{collections::HashMap, sync::Arc};

    use ra_ap_syntax::{
        ast::{self, HasName},
        AstNode, SyntaxKind, SyntaxNode,
    };

    use crate::{entity::NamedRefPath, helper, Semantics};

    #[derive(Clone)]
    pub struct TraceNodeRightValue {
        node: RightTraceNode,
        kind: NodeTypeKind,
        field_path: NamedRefPath,
        deref_triggered: bool,
    }

    #[derive(Clone, Copy)]
    pub enum NodeTypeKind {
        Scalar,
        FieldGroup,
    }

    impl NodeTypeKind {
        /// Returns `true` if the node type kind is [`Scalar`].
        ///
        /// [`Scalar`]: NodeTypeKind::Scalar
        #[must_use]
        pub fn is_scalar(&self) -> bool {
            matches!(self, Self::Scalar)
        }

        /// Returns `true` if the node type kind is [`FieldGroup`].
        ///
        /// [`FieldGroup`]: NodeTypeKind::FieldGroup
        #[must_use]
        pub fn is_field_group(&self) -> bool {
            matches!(self, Self::FieldGroup)
        }
    }

    #[derive(Clone)]
    pub enum RightTraceNode {
        FieldExpr(ast::FieldExpr),
        RefFieldExpr(ast::RefExpr),
        StarDeref(ast::PrefixExpr),
        PassSelf(ast::PathExpr),
    }

    #[derive(Clone)]
    pub enum TraceNode {
        RightValue(RightTraceNode),
        LeftValue(ast::Pat),
        ReturnValue(RightTraceNode),
    }

    pub enum DerefTrigger {
        FieldExpr(ast::FieldExpr),
        RefFieldExpr(ast::RefExpr),
        StarDeref(ast::PrefixExpr),
    }

    pub struct AccessTrace {
        nodes: Vec<TraceNode>,
        field_path: NamedRefPath,
    }

    pub struct FieldGroup {
        fields: Arc<HashMap<String, Field>>,
    }

    pub enum Field {
        AlwaysLoaded,
        ScalarField,
        GroupField(Box<FieldGroup>),
    }

    fn track_field_group(
        sema: &Semantics,
        bind_to: ast::Pat,
        field_group: FieldGroup,
    ) -> Option<Vec<AccessTrace>> {
        let name = match &bind_to {
            ast::Pat::IdentPat(ident_pat) => ident_pat,
            _ => {
                todo!()
            }
        };
        let usages = helper::usages_in_current_file(sema, &name.name()?)?;
        let head_node = TraceNode::LeftValue(bind_to.clone());
        let mut traces = vec![];
        for (_, usages) in usages {
            for usage in usages {
                let name_ref = usage.name.as_name_ref()?;
                let path_expr = name_ref
                    .syntax()
                    .parent()?
                    .parent()?
                    .parent()
                    .map(ast::PathExpr::cast)??;
                let right_value = extract_right_value_node(sema, path_expr)?;
                let current_nodes = vec![
                    head_node.clone(),
                    TraceNode::RightValue(right_value.node.clone()),
                ];

                if right_value.deref_triggered {
                    traces.push(AccessTrace {
                        nodes: current_nodes,
                        field_path: right_value.field_path,
                    });
                    break;
                }

                let kind = right_value.kind;
                if let Some(assign_to) = right_value_assign_to(sema, right_value.clone()) {
                    let bind_to = match assign_to {
                        RightValueAssignTo::LetBind(pat) => pat,
                        RightValueAssignTo::MethodCall(method_call_expr) => todo!(),
                        RightValueAssignTo::Arg(call_expr, index) => {
                            todo!()
                        }
                        RightValueAssignTo::Return(group, path, ..) => {
                            traces.push(AccessTrace {
                                nodes: current_nodes,
                                field_path: path,
                            });
                            break;
                        }
                    };
                    if kind.is_field_group() {
                        let field_group = field_group
                            .get_field(&right_value.field_path)?
                            .to_field_group()?;
                        if let Some(mut sub_traces) = track_field_group(sema, bind_to, field_group)
                        {
                            for sub_trace in &mut sub_traces {
                                sub_trace.field_path =
                                    right_value.field_path.join(&sub_trace.field_path);
                                let mut nodes = current_nodes.clone();
                                nodes.extend(sub_trace.nodes.drain(..));
                                sub_trace.nodes = nodes;
                            }
                            traces.extend(sub_traces);
                        }
                    } else {
                        let nodess = track_scalar_field(sema, bind_to);
                        for mut nodes in nodess {
                            let mut current_nodes = current_nodes.clone();
                            current_nodes.extend(nodes.drain(..));
                            traces.push(AccessTrace {
                                nodes: current_nodes,
                                field_path: right_value.field_path.clone(),
                            });
                        }
                    }
                }
            }
        }

        todo!()
    }

    struct VirtualAssign(ast::Expr);

    struct VirtualFieldGroup(ast::Adt);

    mod aa {
        use std::collections::HashMap;

        use ra_ap_syntax::ast;

        pub struct FieldGroup {
            fields: HashMap<FieldPath, Field>,
        }

        pub struct Field {
            path: FieldPath,
            ty: ast::Type,
            kind: FieldKind,
        }

        pub enum FieldKind {
            AlwaysLoaded,
            ScalarField,
            GroupField,
        }

        pub struct FieldPath {
            segments: Vec<PathSegment>,
        }

        enum PathSegment {
            RecordField(String),
            EnumVariant(String),
            TupleField(u8),
        }
    }

    impl FieldGroup {
        fn get_field(&self, path: &NamedRefPath) -> Option<Field> {
            todo!()
        }
    }

    impl Field {
        fn to_field_group(self) -> Option<FieldGroup> {
            todo!()
        }
    }

    enum RightValueAssignTo {
        LetBind(ast::Pat),
        MethodCall(ast::MethodCallExpr),
        Arg(ast::CallExpr, usize),
        Return(FieldGroup, NamedRefPath, ast::Expr),
    }

    fn right_value_assign_to(
        sema: &Semantics,
        rv: TraceNodeRightValue,
    ) -> Option<RightValueAssignTo> {
        todo!()
    }

    fn extract_right_value_node(
        sema: &Semantics,
        group_ref: ast::PathExpr,
    ) -> Option<TraceNodeRightValue> {
        todo!()
    }

    fn track_scalar_field(sema: &Semantics, bind_to: ast::Pat) -> Vec<Vec<TraceNode>> {
        todo!()
    }
}

mod ee {
    use std::collections::HashMap;

    use ra_ap_syntax::ast;

    pub struct FieldGroup {
        fields: HashMap<FieldPath, Field>,
    }

    #[derive(Clone)]
    pub struct Field {
        path: FieldPath,
        ty: ast::Type,
        kind: FieldKind,
    }

    #[derive(Clone, Copy)]
    pub enum FieldKind {
        AlwaysLoaded,
        ScalarField,
        GroupField,
        Iterator,
    }

    impl FieldKind {
        /// Returns `true` if the field kind is [`GroupField`].
        ///
        /// [`GroupField`]: FieldKind::GroupField
        #[must_use]
        pub fn is_group_field(&self) -> bool {
            matches!(self, Self::GroupField)
        }
    }

    #[derive(PartialEq, Eq, Hash, Clone)]
    pub struct FieldPath {
        segments: Vec<PathSegment>,
    }

    #[derive(PartialEq, Eq, Hash, Clone)]
    enum PathSegment {
        RecordField(String),
        EnumVariant(String),
        TupleField(u8),
    }

    impl FieldPath {
        fn starts_with(&self, base: &FieldPath) -> bool {
            todo!()
        }

        fn strip_prefix(&self, prefix: &FieldPath) -> Self {
            todo!()
        }
    }

    impl FieldGroup {
        fn get_field_group(&self, path: FieldPath) -> Option<FieldGroup> {
            let group = self.fields.get(&path)?;
            if !group.kind.is_group_field() {
                return None;
            }
            let fields = self
                .fields
                .values()
                .filter_map(|f| {
                    if !f.path.starts_with(&group.path) {
                        return None;
                    }
                    let mut f = f.clone();
                    f.path = f.path.strip_prefix(&group.path);

                    Some((f.path.clone(), f))
                })
                .collect();

            Some(FieldGroup { fields })
        }
    }

    struct Node {
        node_kind: NodeKind,
        deref_triggered: bool,
        // field_or_root: FieldOrRoot,
    }

    enum FieldOrRoot {
        Root(FieldGroup),
        Field(Field, FieldGroup),
    }

    enum NodeKind {
        LetBindTo(ast::Pat),
        ArgBind(ast::Pat),
        RecordFieldBind {
            field: ast::NameRef,
            is_return: bool,
        },
        ReturnGroup(ReadNode),
        ReturnScalar(ReadNode),
        Read {
            node: ReadNode,
        },
        ReturnTmpGroup {
            read: ReadNode,
            group: FieldGroup,
            path: FieldPath,
        },
        MethodCall(ast::MethodCallExpr, Option<NewGroupOrScalar>),
        PathExpr(ast::PathExpr),
    }

    enum TempExpr {
        MethodCall(ast::MethodCallExpr),
        PathExpr(ast::PathExpr),
    }

    enum NewGroupOrScalar {
        Group(FieldGroup, FieldPath),
        Scalar(FieldPath),
    }

    enum ReadNode {
        FieldExpr(ast::FieldExpr),
        RefFieldExpr(ast::RefExpr),
    }
}

mod ff {
    use std::collections::HashMap;

    use anyhow::{bail, Context, Result};
    use ra_ap_hir::HasSource;
    use ra_ap_ide::RootDatabase;
    use ra_ap_syntax::{
        ast::{self, HasName},
        AstNode, SyntaxKind, SyntaxNode,
    };

    use crate::{entity::NamedRefPath, helper, Semantics};

    #[derive(Clone)]
    pub enum TraceNode {
        RightValue(RightTraceNode),
        LeftValue(LeftTraceNode),
    }

    #[derive(Clone)]
    pub enum LeftTraceNode {
        LetBind(ast::Pat),
        MethodArg(ast::Pat),
    }

    #[derive(Clone)]
    pub enum RightTraceNode {
        FieldExpr(ast::FieldExpr),
        RefExpr(ast::RefExpr),
        DerefExpr(ast::PrefixExpr),
        PassSelf(ast::PathExpr),
    }

    pub struct FieldGroup {
        fields: HashMap<NamedRefPath, Field>,
    }

    #[derive(Clone)]
    pub struct Field {
        path: NamedRefPath,
        ty: ast::Type,
        kind: FieldKind,
    }

    #[derive(Clone, Copy)]
    pub enum FieldKind {
        AlwaysLoaded,
        ScalarField,
        GroupField,
    }

    impl FieldKind {
        #[must_use]
        pub fn is_group_field(&self) -> bool {
            matches!(self, Self::GroupField)
        }
    }

    #[derive(Clone)]
    pub struct AccessChain {
        nodes: Vec<TraceNode>,
    }

    pub struct AccessTrace {
        chain: AccessChain,
        field_path: NamedRefPath,
    }

    fn track_entity_fields(
        sema: &Semantics,
        let_bind_to: ast::IdentPat,
        field_group: FieldGroup,
    ) -> Result<Vec<AccessTrace>> {
        todo!()
    }

    impl AccessChain {
        fn new(nodes: Vec<TraceNode>) -> Self {
            Self { nodes }
        }

        fn extend(&mut self, nodes: Vec<TraceNode>) {
            self.nodes.extend(nodes);
        }
    }

    fn track_field_group(
        sema: &Semantics,
        bind_to: LeftTraceNode,
        field_group: FieldGroup,
    ) -> Result<Vec<AccessTrace>> {
        let name = match &bind_to {
            LeftTraceNode::LetBind(pat) | LeftTraceNode::MethodArg(pat) => match pat {
                ast::Pat::IdentPat(ident_pat) => ident_pat.name().unwrap(),
                _ => todo!(),
            },
        };

        let usages = helper::usages_in_current_file(sema, &name).context("no usages")?;
        let head_node = TraceNode::LeftValue(bind_to.clone());

        let mut traces = vec![];
        for (_, usages) in usages {
            for usage in usages {
                let name_ref = usage.name.as_name_ref().context("no name ref")?;
                let path_expr = name_ref
                    .syntax()
                    .parent()
                    .context("no parent expr")?
                    .parent()
                    .context("no parent expr")?
                    .parent()
                    .map(ast::PathExpr::cast)
                    .context("no parent path expr")?
                    .context("not a path expr")?;

                let Some(right_value) = extract_right_value_node(sema, &field_group, path_expr)?
                else {
                    continue;
                };

                let scope_chain = AccessChain::new(vec![
                    head_node.clone(),
                    TraceNode::RightValue(right_value.node.clone()),
                ]);

                if right_value.deref_triggered {
                    traces.push(AccessTrace {
                        chain: scope_chain,
                        field_path: right_value.field_path,
                    });
                    break;
                }

                let kind = right_value.kind;
                if let Some(assign_to) = right_value_assign_to(sema, right_value.clone())? {
                    let bind_to = match assign_to {
                        RightValueAssignTo::LetBind(pat) => LeftTraceNode::LetBind(pat),
                        RightValueAssignTo::FnArg(pat) => LeftTraceNode::MethodArg(pat),
                    };

                    if kind.is_field_group() {
                        let field_group = field_group
                            .get_field(&right_value.field_path)
                            .context("no field")?
                            .to_field_group()
                            .context("not a field group")?;

                        let mut sub_traces = track_field_group(sema, bind_to, field_group)?;
                        for sub_trace in &mut sub_traces {
                            sub_trace.field_path =
                                right_value.field_path.join(&sub_trace.field_path);
                            let mut chain = scope_chain.clone();
                            chain.extend(std::mem::take(&mut sub_trace.chain.nodes));
                            sub_trace.chain = chain;
                        }
                        traces.extend(sub_traces);
                    } else {
                        let chains = track_scalar_field(sema, bind_to);
                        for chain in chains {
                            let mut scope_chain = scope_chain.clone();
                            scope_chain.extend(chain.nodes);
                            traces.push(AccessTrace {
                                chain: scope_chain,
                                field_path: right_value.field_path.clone(),
                            });
                        }
                    }
                }
            }
        }

        todo!()
    }

    #[derive(Clone)]
    pub struct TraceNodeRightValue {
        node: RightTraceNode,
        kind: NodeKind,
        field_path: NamedRefPath,
        deref_triggered: bool,
    }

    #[derive(Clone, Copy)]
    pub enum NodeKind {
        Scalar,
        FieldGroup,
    }

    impl NodeKind {
        /// Returns `true` if the node type kind is [`FieldGroup`].
        ///
        /// [`FieldGroup`]: NodeTypeKind::FieldGroup
        #[must_use]
        pub fn is_field_group(&self) -> bool {
            matches!(self, Self::FieldGroup)
        }
    }

    fn extract_right_value_node(
        sema: &Semantics,
        field_group: &FieldGroup,
        path_expr: ast::PathExpr,
    ) -> Result<Option<TraceNodeRightValue>> {
        let mut field_path = NamedRefPath::new_this();
        let mut deref_triggered = false;
        let mut node = RightTraceNode::PassSelf(path_expr);
        let mut kind = NodeKind::Scalar;
        loop {
            let Some(parent_expr) = node.syntax().parent().and_then(ast::Expr::cast) else {
                break;
            };

            deref_triggered = is_deref_triggered(sema, &parent_expr)?;

            match parent_expr {
                ast::Expr::FieldExpr(field_expr) => {
                    let name_ref = field_expr.name_ref().context("no name ref")?;
                    field_path.push(name_ref.text().to_string());

                    let field = field_group
                        .get_field(&field_path)
                        .context("no field in group")?;

                    match field.kind {
                        FieldKind::AlwaysLoaded => return Ok(None),
                        FieldKind::ScalarField => kind = NodeKind::Scalar,
                        FieldKind::GroupField => kind = NodeKind::FieldGroup,
                    };
                    node = RightTraceNode::FieldExpr(field_expr);
                }
                _ => {
                    break;
                }
            }
            if deref_triggered {
                break;
            }
        }

        let right_value = TraceNodeRightValue {
            node,
            kind,
            field_path,
            deref_triggered,
        };

        Ok(Some(right_value))
    }

    fn is_deref_triggered(sema: &Semantics, expr: &ast::Expr) -> Result<bool> {
        let ty = sema.type_of_expr(expr).context("cannot resolve type")?;
        todo!()
    }

    enum RightValueAssignTo {
        LetBind(ast::Pat),
        FnArg(ast::Pat),
    }

    fn right_value_assign_to(
        sema: &Semantics,
        rv: TraceNodeRightValue,
    ) -> Result<Option<RightValueAssignTo>> {
        if is_returnning(rv.node.syntax()) {
            bail!("You should not return an Entity field");
        }

        let parent = rv
            .node
            .syntax()
            .parent()
            .context("right value node has no parent")?;

        match parent.kind() {
            SyntaxKind::METHOD_CALL_EXPR => {
                let fn_call = ast::MethodCallExpr::cast(parent).unwrap();
                let fn_ref = fn_call.name_ref().context("no fn name ref")?;
                let pat = get_param_pat(sema, fn_ref, 0)?;
                Ok(Some(RightValueAssignTo::FnArg(pat)))
            }
            SyntaxKind::ARG_LIST => {
                let arg_list = ast::ArgList::cast(parent).unwrap();
                let index = arg_list
                    .args()
                    .position(|arg| arg.syntax() == rv.node.syntax())
                    .unwrap();
                let fn_ref = arg_list
                    .syntax()
                    .prev_sibling()
                    .context("arg list has no prev sibling")?;
                let name_ref = fn_ref
                    .descendants()
                    .find_map(ast::NameRef::cast)
                    .context("no fn name ref")?;

                let pat = get_param_pat(sema, name_ref, index)?;
                Ok(Some(RightValueAssignTo::FnArg(pat)))
            }
            SyntaxKind::LET_STMT => {
                let let_stmt = ast::LetStmt::cast(parent).unwrap();
                let pat = let_stmt.pat().context("no pat in let stmt")?;
                Ok(Some(RightValueAssignTo::LetBind(pat)))
            }
            SyntaxKind::RECORD_EXPR_FIELD => {
                bail!("You should not assign an entity field to a record field");
            }
            _ => {
                bail!("unknown assignable node");
            }
        }
    }

    fn is_returnning(expr: &SyntaxNode) -> bool {
        let mut node = expr.clone();
        loop {
            let Some(parent) = node.parent() else {
                return false;
            };

            let Some(expr) = ast::Expr::cast(parent) else {
                break;
            };

            if matches!(expr, ast::Expr::ReturnExpr(_)) {
                return true;
            }
            node = expr.syntax().clone();
        }
        let Some(stmt_list) = node.parent() else {
            return false;
        };
        let Some(block_expr) = stmt_list.parent() else {
            return false;
        };
        let Some(fn_) = block_expr.parent() else {
            return false;
        };

        if ast::Fn::can_cast(fn_.kind()) {
            return true;
        }

        false
    }

    #[test]
    fn test_is_the_fn_return() {
        let code = r#"
fn main() {
    println!("Hello, world!")
}
        "#;
        let parse = ast::SourceFile::parse(code, ra_ap_ide::Edition::CURRENT);
        let file = parse.ok();
    }

    fn get_param_pat(sema: &Semantics, name_ref: ast::NameRef, index: usize) -> Result<ast::Pat> {
        let Some(fn_) = helper::resolve_fn(sema, &name_ref) else {
            bail!("cannot resolve fn");
        };
        let fn_ = fn_
            .to_fn()
            .context("you should not assign an entity field to an enum variant")?;

        let source = fn_.source(sema.db).context("fn has no source")?;
        let params = source.value.param_list().context("fn has no param list")?;
        let param = params.params().skip(index).next().context("no param")?;
        let pat = param.pat().context("no pat in param")?;

        Ok(pat)
    }

    impl FieldGroup {
        fn get_field(&self, path: &NamedRefPath) -> Option<Field> {
            self.fields.get(path).cloned()
        }

        fn get_group_field(&self, path: &NamedRefPath) -> Option<FieldGroup> {
            let field = self.fields.get(path).cloned()?;
            let field_paths = self
                .fields
                .iter()
                .filter_map(|(p, _)| {
                    if p.starts_with(path) {
                        Some(p.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            todo!()
        }
    }

    impl Field {
        fn to_field_group(self) -> Option<FieldGroup> {
            todo!()
        }
    }

    fn track_scalar_field(sema: &Semantics, bind_to: LeftTraceNode) -> Vec<AccessChain> {
        todo!()
    }

    impl RightTraceNode {
        fn syntax(&self) -> &SyntaxNode {
            match self {
                RightTraceNode::FieldExpr(field_expr) => field_expr.syntax(),
                RightTraceNode::RefExpr(ref_expr) => ref_expr.syntax(),
                RightTraceNode::DerefExpr(prefix_expr) => prefix_expr.syntax(),
                RightTraceNode::PassSelf(path_expr) => path_expr.syntax(),
            }
        }
    }
}

mod gg {
    use std::collections::HashMap;

    use anyhow::{bail, Context, Result};
    use ra_ap_hir::{sym::rustc_builtin_macro, Adt, HasSource, Type};
    use ra_ap_syntax::{
        ast::{self, HasName},
        match_ast, AstNode, SyntaxNode,
    };

    use crate::{helper, loader::Bagua, Semantics};

    #[derive(Clone)]
    pub struct Trace {
        nodes: Vec<TraceNode>,
    }

    impl Trace {
        fn push(&mut self, node: TraceNode) {
            self.nodes.push(node);
        }
    }

    #[derive(Clone)]
    pub struct FnCallPoint {
        ast: ast::Expr,
        parent: Option<Box<FnCallPoint>>,
    }

    #[derive(Clone)]
    pub enum TraceNode {
        Left(LeftTraceNode),
        Right(RightTraceNode),
    }

    #[derive(Clone)]
    pub struct TraceCtx {
        scope: GroupScope,
        path_in_scope: ReferPath,
        path_in_entity: ReferPath,
        fn_call_point: Option<FnCallPoint>,
    }

    #[derive(Clone)]
    pub struct RightTraceNode {
        ast: ast::Expr,
        trace_ctx: TraceCtx,
        deref_triggered: bool,
    }

    #[derive(Clone)]
    pub struct LeftTraceNode {
        ast: ast::Pat,
        trace_ctx: TraceCtx,
    }

    fn left_node_traces(
        sema: &Semantics,
        node: LeftTraceNode,
        bagua: &Bagua,
    ) -> Result<Vec<Trace>> {
        let ident = match node.ast.clone() {
            ast::Pat::IdentPat(ident_pat) => ident_pat,
            ast::Pat::RecordPat(record_pat) => {
                todo!()
            }
            _ => todo!(),
        };

        let usages = helper::usages_in_current_file(sema, &ident.name().unwrap()).unwrap();
        let base_trace = Trace {
            nodes: vec![TraceNode::Left(node.clone())],
        };
        let mut traces = vec![];
        for (_, usages) in usages {
            for usage in usages {
                let name_ref = usage.name.as_name_ref().context("no name ref")?;

                let Some(node) = expand_group_ref_to_node(sema, name_ref, &node.trace_ctx, bagua)?
                else {
                    continue;
                };
                let deref_triggered = node.deref_triggered;
                if deref_triggered {
                    let mut trace = base_trace.clone();
                    trace.push(TraceNode::Right(node));
                    traces.push(trace);
                } else {
                    let mut right_nexts = vec![node];
                    loop {
                        let next_node = right_nexts.last().unwrap().next_node(sema)?;
                        if let Some(next_node) = next_node {
                            match next_node {
                                TraceNode::Left(left_trace_node) => {
                                    let sub_traces =
                                        left_node_traces(sema, left_trace_node, bagua)?;
                                    let bridge_nodes = right_nexts
                                        .into_iter()
                                        .map(|node| TraceNode::Right(node))
                                        .collect::<Vec<_>>();
                                    for sub_trace in sub_traces {
                                        let mut trace = base_trace.clone();
                                        trace.nodes.extend(bridge_nodes.clone());
                                        trace.nodes.extend(sub_trace.nodes);
                                        traces.push(trace);
                                    }

                                    break;
                                }
                                TraceNode::Right(right_trace_node) => {
                                    if right_trace_node.deref_triggered {
                                        let mut trace = base_trace.clone();
                                        trace.push(TraceNode::Right(right_trace_node));
                                        traces.push(trace);
                                    } else {
                                        right_nexts.push(right_trace_node.clone());
                                    }
                                }
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        Ok(traces)
    }

    fn is_returning(expr: &ast::Expr) -> Result<bool> {
        todo!()
    }

    impl RightTraceNode {
        fn next_node(&self, sema: &Semantics) -> Result<Option<TraceNode>> {
            if is_returning(&self.ast)? {
                let fn_call_point = self
                    .trace_ctx
                    .fn_call_point
                    .clone()
                    .context("no fn call point")?;
                let mut ctx = self.trace_ctx.clone();
                ctx.fn_call_point = fn_call_point.parent.map(|p| *p);

                let node = TraceNode::Right(RightTraceNode {
                    ast: fn_call_point.ast,
                    trace_ctx: ctx,
                    deref_triggered: false,
                });

                return Ok(Some(node));
            }

            let parent = self
                .ast
                .syntax()
                .parent()
                .context("right node has no parent")?;
            if let Some(let_stmt) = ast::LetStmt::cast(parent.clone()) {
                let pat = let_stmt.pat().context("no pat in let stmt")?;
                return Ok(Some(TraceNode::Left(LeftTraceNode {
                    ast: pat,
                    trace_ctx: self.trace_ctx.clone(),
                })));
            }
            if let Some(arg_list) = ast::ArgList::cast(parent.clone()) {
                return self.extract_fn_node(sema, arg_list).map(Some);
            }

            if let Some(record_field) = ast::RecordExprField::cast(parent.clone()) {
                bail!("record_field not supported yet")
            }

            if let Some(match_expr) = ast::MatchExpr::cast(parent.clone()) {
                return Ok(None);
            }
            if let Some(let_expr) = ast::LetExpr::cast(parent.clone()) {
                return Ok(None);
            }

            Ok(None)
        }

        fn extract_fn_node(
            &self,
            sema: &Semantics,
            arg_list: ast::ArgList,
        ) -> Result<TraceNode, anyhow::Error> {
            let index = arg_list
                .args()
                .position(|arg| arg.syntax() == self.ast.syntax())
                .unwrap();
            let fn_ref = arg_list
                .syntax()
                .prev_sibling()
                .context("arg list has no prev sibling")?;
            let fn_ref = ast::NameRef::cast(fn_ref).context("no fn ref")?;
            let fn_ = helper::resolve_fn(sema, &fn_ref).context("no fn")?;
            let fn_ = fn_.to_fn().context("not a fn")?;
            let source = fn_.source(sema.db).context("fn has no source")?;
            let params = source.value.param_list().context("fn has no param list")?;
            let param = params.params().skip(index).next().context("no param")?;
            let pat = param.pat().context("no pat in param")?;

            return Ok(TraceNode::Left(LeftTraceNode {
                ast: pat,
                trace_ctx: self.trace_ctx.clone(),
            }));
        }
    }

    fn expand_group_ref_to_node(
        sema: &Semantics,
        name_ref: &ast::NameRef,
        trace_ctx: &TraceCtx,
        bagua: &Bagua,
    ) -> Result<Option<RightTraceNode>> {
        let path_expr = name_ref
            .syntax()
            .ancestors()
            .find_map(ast::PathExpr::cast)
            .context("no path expr")?;
        let mut res_expr = ast::Expr::PathExpr(path_expr);
        let mut is_deref = false;
        let mut trace_ctx = trace_ctx.clone();
        let mut is_scalar = false;
        loop {
            let Some(parent_expr) = res_expr.syntax().parent().and_then(ast::Expr::cast) else {
                break;
            };

            match parent_expr {
                ast::Expr::FieldExpr(field_expr) => {
                    let name_ref = field_expr.name_ref().context("no name ref")?;
                    let field_name = name_ref.text();
                    res_expr = ast::Expr::FieldExpr(field_expr);

                    let field = trace_ctx
                        .scope
                        .get_struct_name_field(&field_name)
                        .context("no field")?;

                    match field {
                        Field::NotEntityField => return Ok(None),
                        Field::ScalarField => {
                            is_deref = check_if_deref_triggered(sema, &res_expr, bagua)?;
                            let path_seg = PathSegment::NamedField(field_name.to_string());
                            let mut path_in_scope = trace_ctx.path_in_scope.clone();
                            path_in_scope.push(path_seg.clone());
                            trace_ctx.path_in_scope = path_in_scope;
                            trace_ctx.path_in_entity.push(path_seg);
                            is_scalar = true;
                        }
                        Field::GroupField(group_scope) => {
                            if trace_ctx.scope.is_real_entity_group {
                                let path_seg = PathSegment::NamedField(field_name.to_string());
                                trace_ctx.path_in_entity.push(path_seg);
                            }
                            trace_ctx.scope = group_scope.clone();
                            trace_ctx.path_in_scope = ReferPath::This;
                        }
                    }
                }
                ast::Expr::RefExpr(ref_expr) => {
                    res_expr = ast::Expr::RefExpr(ref_expr);
                    if is_scalar {
                        is_deref = check_if_deref_triggered(sema, &res_expr, bagua)?;
                    }
                }
                ast::Expr::PrefixExpr(prefix_expr) => {
                    if is_scalar {
                        is_deref = check_prefix_expr_deref(sema, &prefix_expr, &res_expr, bagua)?;
                    }
                    res_expr = ast::Expr::PrefixExpr(prefix_expr);
                }
                _ => {
                    break;
                }
            }
            if is_deref {
                break;
            }
        }

        let node = RightTraceNode {
            ast: res_expr,
            trace_ctx,
            deref_triggered: is_deref,
        };

        Ok(Some(node))
    }

    fn check_prefix_expr_deref(
        sema: &Semantics,
        deref_expr: &ast::PrefixExpr,
        child: &ast::Expr,
        bagua: &Bagua,
    ) -> Result<bool> {
        let child_ty = sema
            .type_of_expr(child)
            .context("cannot get type of expr")?;
        if !is_entity_field(sema, &child_ty.original, bagua)? {
            return Ok(false);
        }
        let ty = sema
            .type_of_expr(&ast::Expr::PrefixExpr(deref_expr.clone()))
            .context("cannot get type of expr")?;
        let original = ty.original.strip_references();
        if !is_entity_field(sema, &original, bagua)? {
            Ok(true)
        } else {
            Ok(ty.has_adjustment())
        }
    }

    fn entity_field_deref_triggered(sema: &Semantics, ty: &Type) -> Result<bool> {
        todo!()
    }

    fn is_entity_field(sema: &Semantics, ty: &Type, bagua: &Bagua) -> Result<bool> {
        let Some(Adt::Enum(original)) = ty.strip_references().as_adt() else {
            return Ok(false);
        };
        Ok(original == bagua.struct_field)
    }

    fn check_if_deref_triggered(sema: &Semantics, expr: &ast::Expr, bagua: &Bagua) -> Result<bool> {
        let ty = sema.type_of_expr(expr).context("cannot get type of expr")?;
        if !is_entity_field(sema, &ty.original, bagua)? {
            return Ok(false);
        }

        Ok(ty.has_adjustment())
    }

    impl GroupScope {
        fn get_struct_name_field(&self, name: &str) -> Option<Field> {
            todo!()
        }
    }

    #[derive(Clone)]
    pub struct GroupScope {
        fields: HashMap<PathSegment, Field>,
        is_real_entity_group: bool,
    }

    pub struct FieldName(String);

    #[derive(Clone)]
    pub enum Field {
        NotEntityField,
        ScalarField,
        GroupField(GroupScope),
    }

    #[derive(Clone)]
    pub enum ReferPath {
        This,
        Field(FieldPath),
    }

    impl ReferPath {
        fn strip_start(&mut self, base: ReferPath) {
            todo!()
        }

        fn push(&mut self, seg: PathSegment) {
            match self {
                ReferPath::This => {
                    *self = ReferPath::Field(FieldPath {
                        segments: vec![seg],
                    })
                }
                ReferPath::Field(field_path) => {
                    field_path.segments.push(seg);
                }
            }
        }
    }

    #[derive(Clone)]
    pub enum ThisOrChild {
        This,
        Child(PathSegment),
    }

    #[derive(Clone)]
    pub struct FieldPath {
        segments: Vec<PathSegment>,
    }

    #[derive(Clone)]
    enum PathSegment {
        NamedField(String),
        TupleIndex(u8),
        UnionStructIndex(u8),
        EnumNamedField(String, String),
        EnumTupleIndex(String, u8),
    }

    pub enum TraceNodeAst {
        Left(ast::Pat),
        Right(ast::Expr),
    }
}
