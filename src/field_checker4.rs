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
    entity::FieldPath,
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
    fields: HashMap<FieldPath, FieldState>,
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

    fn subset_fields(&self, sema: &Semantics, ty: Struct, bagua: &Bagua) -> Vec<FieldPath> {
        fn subset_fields_opt(
            sema: &Semantics,
            ty: Struct,
            bagua: &Bagua,
        ) -> Option<Vec<FieldPath>> {
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
                let current_field_path = FieldPath::new_scalar(field_name.text().to_string());

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
    fn flatten_fields(self, sema: &Semantics) -> Vec<FieldPath> {
        let mut fields = vec![];
        for scalar in self.scalar_like {
            fields.push(FieldPath::new_scalar(scalar));
        }
        for (group, group_fields) in self.group {
            let prefix = FieldPath::new_scalar(group);
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
    fn fields(&self, sema: &Semantics) -> Result<Vec<FieldPath>> {
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

    use crate::entity::FieldPath;

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
        field_path: FieldPath,
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

    use crate::{entity::FieldPath, helper, Semantics};

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
        field_path: FieldPath,
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

    use ra_ap_syntax::ast;

    use crate::entity::FieldPath;

    pub enum TraceNode {
        FieldExpr(ast::FieldExpr),
        RefFieldExpr(ast::RefExpr),
        StarDeref(ast::PrefixExpr),
        PassSelf(ast::NameRef),
        Bind(ast::Pat),
    }

    pub struct AccessTrace {
        nodes: Vec<TraceNode>,
        field_path: FieldPath,
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
