use std::collections::HashMap;
use std::fmt::Debug;

use anyhow::bail;
use anyhow::Context as _;
use anyhow::Result;
use chalk_ir::TyKind;
use indexmap::IndexMap;
use ra_ap_hir::HasSource;
use ra_ap_hir::HirFileId;
use ra_ap_hir::{
    db::{DefDatabase, HirDatabase},
    Adt, InFile, Struct,
};
use ra_ap_hir_def::{hir::ExprId, DefWithBodyId, FunctionId};
use ra_ap_hir_ty::Interner;
use ra_ap_ide_db::LineIndexDatabase;
use ra_ap_syntax::ast::HasAttrs;
use ra_ap_syntax::{
    ast::{self, HasName},
    AstNode, SyntaxNode,
};

use crate::entity::NamedRefPath;
use crate::{
    entity::FieldName,
    helper,
    loader::{Bagua, BaguaProject, Usecase},
    Semantics,
};

pub struct FieldChecker<'a> {
    project: &'a BaguaProject,
}

#[derive(Debug, Clone)]
pub struct FieldAccessViolation {
    entity_var: ast::Name,
    trace: tracer::Trace,
}

#[derive(Debug, Clone)]
pub struct EntityLocalVar {
    pat: ast::IdentPat,
    entity_ty: Struct,
    field_group: FieldGroup,
}

#[derive(Debug, Clone)]
pub enum Field {
    Group(FieldGroup),
    Scalar(ScalarField),
}

#[derive(Debug, Clone)]
pub struct FieldGroup {
    def: Option<ast::RecordField>,
    fields: IndexMap<FieldName, Field>,
}

#[derive(Debug, Clone)]
pub struct ScalarField {
    def: ast::RecordField,
    state: FieldState,
}

#[derive(Debug, Clone)]
enum FieldState {
    Unloaded,
    Unchanged,
    Set,
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
                // play_fn_access(&sema, &uc);
                self.check_uc(&uc)?;
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

        let entity_local_defs = self.entity_local_defs(&sema, &file_ast)?;
        // dbg!(&entity_local_defs);

        for entity_var in entity_local_defs {
            let field_group = entity_var.field_group.to_any_filed_group();
            let init_node = tracer::LeftTraceNode {
                ast: From::from(ast::Pat::IdentPat(entity_var.pat.clone())),
                trace_ctx: tracer::TraceCtx {
                    scope: field_group,
                    path_in_scope: tracer::ReferPath::new_this(),
                    path_in_entity: NamedRefPath::new_this(),
                    fn_call_point: None,
                },
                file_id: HirFileId::from(file_id),
            };
            let traces = init_node.gen_traces(&sema, &self.project.bagua)?;

            for trace in traces {
                let out = trace.pretty_print(sema.db, &self.project.vfs);
                dbg!(trace);
                println!("{out}");
            }
        }

        Ok(vec![])
    }

    fn entity_local_defs(
        &self,
        sema: &Semantics,
        file_ast: &ast::SourceFile,
    ) -> Result<Vec<EntityLocalVar>> {
        let method_find_call_stmts = file_ast
            .syntax()
            .descendants()
            .filter_map(|node| self.method_find_call_stmt(&sema, node));

        let mut entity_local_vars = Vec::new();
        for (pat, subset_ty) in method_find_call_stmts {
            let entity_local_var = self.entity_local_var(&sema, pat, subset_ty)?;
            entity_local_vars.extend(entity_local_var);
        }

        Ok(entity_local_vars)
    }

    fn entity_local_var(
        &self,
        sema: &Semantics,
        pat: ast::Pat,
        subset_ty: Struct,
    ) -> Result<Option<EntityLocalVar>> {
        let Some((ident, ty)) = self.track_entity_binding(sema, pat) else {
            return Ok(None);
        };
        let mut field_group = FieldGroup::from_entity_binding(sema, ty)?;
        let subset_fields = parse_subset_ty(sema, subset_ty, &self.project.bagua)?;

        for loaded_field in subset_fields {
            let field = field_group
                .get_mut(&loaded_field)
                .expect("unknown subset field");
            match field {
                Field::Group(_field_group) => panic!("unexpected group field in subset"),
                Field::Scalar(scalar_field) => scalar_field.state = FieldState::Unchanged,
            }
        }

        Ok(Some(EntityLocalVar {
            pat: ident,
            entity_ty: ty,
            field_group,
        }))
    }

    fn track_entity_binding(
        &self,
        sema: &Semantics,
        pat: ast::Pat,
    ) -> Option<(ast::IdentPat, Struct)> {
        let ident_pat = pat.syntax().descendants().find_map(ast::IdentPat::cast)?;
        let ty = sema.type_of_pat(&ast::Pat::IdentPat(ident_pat.clone()))?;
        if let Adt::Struct(ty) = ty.original.as_adt()? {
            return Some((ident_pat, ty));
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

trait AstErrContext<T> {
    fn ast_err_ctx<A>(self, ast: &A, ctx: &'static str) -> anyhow::Result<T>
    where
        A: ra_ap_syntax::AstNode + Debug;
}

impl<T> AstErrContext<T> for Option<T> {
    fn ast_err_ctx<A>(self, ast: &A, ctx: &'static str) -> anyhow::Result<T>
    where
        A: ra_ap_syntax::AstNode + Debug,
    {
        match self {
            Some(v) => Ok(v),
            None => {
                let location = ast.syntax().index();
                Err(anyhow::anyhow!("{ctx}: {ast:?}"))
            }
        }
    }
}

fn parse_subset_ty(sema: &Semantics, ty: Struct, bagua: &Bagua) -> Result<Vec<NamedRefPath>> {
    fn subset_fields_opt(sema: &Semantics, ty: Struct, bagua: &Bagua) -> Result<Vec<NamedRefPath>> {
        let subset_ast = reparse_struct(sema, ty)?;
        let fields = match subset_ast
            .field_list()
            .ast_err_ctx(&subset_ast, "no field list")?
        {
            ast::FieldList::RecordFieldList(record_field_list) => record_field_list,
            ast::FieldList::TupleFieldList(_) => {
                bail!("Subset struct must be a recorded struct")
            }
        };

        let fields = fields.fields();
        let mut subset_fields = vec![];
        for field in fields {
            let field_name = field.name().ast_err_ctx(&field, "no name")?;
            let field_type = field.ty().ast_err_ctx(&field, "no ty")?;
            let current_field_path = NamedRefPath::new_segment(field_name.text().to_string());

            let field_type_hir = sema
                .resolve_type(&field_type)
                .ast_err_ctx(&field_type, "cannot resolve ty")?;
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

        Ok(subset_fields)
    }

    subset_fields_opt(sema, ty, bagua)
}

impl FieldGroup {
    fn to_any_filed_group(self) -> tracer::AnyFieldGroup {
        let mut fields: HashMap<tracer::ReferSegment, tracer::AnyField> = HashMap::new();
        for (name, field) in self.fields {
            match field {
                Field::Group(field_group) => {
                    fields.insert(
                        tracer::ReferSegment::NamedField(name.to_string()),
                        tracer::AnyField::GroupField(field_group.to_any_filed_group()),
                    );
                }
                Field::Scalar(_scalar_field) => {
                    fields.insert(
                        tracer::ReferSegment::NamedField(name.to_string()),
                        tracer::AnyField::ScalarField,
                    );
                }
            }
        }

        tracer::AnyFieldGroup {
            fields,
            is_real: true,
        }
    }

    fn get_mut(&mut self, field_path: &NamedRefPath) -> Option<&mut Field> {
        assert!(!field_path.is_this());

        if let Some(seg) = field_path.segment() {
            return self.fields.get_mut(seg);
        }

        let (field_name, remain) = field_path.split_at_1();
        match self.fields.get_mut(field_name)? {
            Field::Group(field_group) => field_group.get_mut(&remain),
            field @ Field::Scalar(_) => {
                assert!(remain.is_empty());
                Some(field)
            }
        }
    }

    fn from_entity_binding(sema: &Semantics, entity: Struct) -> Result<Self> {
        let source = entity.source(sema.db).unwrap();
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

        let fields = FieldGroup::parse_fields(sema, struct_ast)?;
        Ok(Self { def: None, fields })
    }

    fn parse_fields(sema: &Semantics, node: ast::Struct) -> Result<IndexMap<FieldName, Field>> {
        let field_list = node.field_list().context("entity field list not found")?;
        let ast::FieldList::RecordFieldList(field_list) = field_list else {
            return Err(anyhow::anyhow!(
                "entity field list is not a record field list"
            ));
        };

        let mut fields = IndexMap::new();
        for field in field_list.fields() {
            let field_type = field_type(&field).context("field type not found")?;
            let field_name = FieldName(field.name().unwrap().text().to_string());
            match field_type {
                FieldType::Scalar => {
                    fields.insert(
                        field_name,
                        Field::Scalar(ScalarField {
                            def: field,
                            state: FieldState::Unloaded,
                        }),
                    );
                }
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
                    let field_group = FieldGroup::parse_fields(sema, origin_ast.value)?;
                    fields.insert(
                        field_name,
                        Field::Group(FieldGroup {
                            def: Some(field),
                            fields: field_group,
                        }),
                    );
                }
                FieldType::SysId => {
                    fields.insert(
                        field_name,
                        Field::Scalar(ScalarField {
                            def: field,
                            state: FieldState::Set,
                        }),
                    );
                }
            }
        }

        Ok(fields)
    }
}

pub enum FieldType {
    SysId,
    Scalar,
    Group,
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

mod tracer {
    use std::collections::HashMap;

    use anyhow::{bail, Context, Result};
    use ra_ap_hir::{
        db::{ExpandDatabase, InternDatabase},
        Adt, HasSource, HirFileId, InFileWrapper, ToolModule, Type,
    };
    use ra_ap_hir_def::FieldId;
    use ra_ap_ide_db::{label, LineIndexDatabase};
    use ra_ap_syntax::{
        ast::{self, HasArgList, HasName},
        AstNode, SyntaxNode,
    };
    use ra_ap_vfs::{FileId, Vfs};

    use crate::{
        entity::NamedRefPath,
        helper::{self, FnCall},
        loader::Bagua,
        Database, Semantics,
    };

    #[derive(Clone, Debug)]
    pub struct ReferPath {
        // empty segments means `This`, smiliar to `.` in unix file path
        segments: Vec<ReferSegment>,
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub enum ReferSegment {
        NamedField(String),
        TupleIndex(u8),
        EnumNamedField(String, String),
        EnumUnionIndex(String, u8),
        UnionStructIndex(u8),
    }

    #[derive(Clone, Debug)]
    pub struct AnyFieldGroup {
        pub fields: HashMap<ReferSegment, AnyField>,
        pub is_real: bool,
    }

    #[derive(Clone, Debug)]
    pub enum AnyField {
        NotEntityField,
        ScalarField,
        GroupField(AnyFieldGroup),
    }

    #[derive(Clone, Debug)]
    pub struct Trace {
        nodes: Vec<TraceNode>,
    }

    impl Trace {
        fn push(&mut self, node: TraceNode) {
            self.nodes.push(node);
        }

        pub fn pretty_print(&self, db: &Database, vfs: &Vfs) -> String {
            let mut output = self.pretty_print_simple(db, vfs);

            output += "Trace: \n";
            for node in &self.nodes {
                let code = &node.ast().text().to_string();
                let location = node.location(db, vfs);

                output += "  ";
                output += &format!("`{code}`");
                output += " --> ";
                output += &location;
                output += "\n";
            }

            output
        }

        pub fn pretty_print_simple(&self, db: &Database, vfs: &Vfs) -> String {
            let mut output = String::new();

            let last = self.nodes.last().unwrap();
            output += "Violation detected!\n";

            let field = last.path_in_entity().dot_string();
            output += &format!("Field: {field}\n");

            let code = &last.ast().text().to_string();
            output += &format!("Code: `{code}`\n");

            let location = &last.location(db, vfs);
            output += &format!("At: {location}\n");

            output
        }
    }

    #[derive(Clone, Debug)]
    pub struct FnCallPoint {
        ast: ast::Expr,
        file_id: HirFileId,
        parent: Option<Box<FnCallPoint>>,
    }

    #[derive(Clone, Debug)]
    pub struct TraceCtx {
        pub scope: AnyFieldGroup,
        pub path_in_scope: ReferPath,
        pub path_in_entity: NamedRefPath,
        pub fn_call_point: Option<FnCallPoint>,
    }

    #[derive(Clone, Debug)]
    pub enum TraceNode {
        Left(LeftTraceNode),
        Right(RightTraceNode),
    }

    #[derive(Clone, Debug)]
    pub struct LeftTraceNode {
        pub ast: LeftTraceNodeAst,
        pub file_id: HirFileId,
        pub trace_ctx: TraceCtx,
    }

    #[derive(Clone, Debug, derive_more::From)]
    pub enum LeftTraceNodeAst {
        Pat(ast::Pat),
        SelfArg(ast::SelfParam),
    }

    impl LeftTraceNodeAst {
        fn syntax(&self) -> &SyntaxNode {
            match self {
                LeftTraceNodeAst::Pat(pat) => pat.syntax(),
                LeftTraceNodeAst::SelfArg(self_param) => self_param.syntax(),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct RightTraceNode {
        pub ast: ast::Expr,
        pub file_id: HirFileId,
        pub trace_ctx: TraceCtx,
        pub deref_triggered: bool,
    }

    impl TraceCtx {
        fn get_tracing_field(&self) -> Option<&AnyField> {
            self.scope.get_field(&self.path_in_scope)
        }

        fn is_self(&self) -> bool {
            self.path_in_entity.is_this()
        }

        fn is_entity_self(&self) -> bool {
            if self.path_in_entity.is_this() {
                assert!(self.path_in_scope.is_this());
                return true;
            }
            false
        }
    }

    impl ReferPath {
        fn is_this(&self) -> bool {
            self.segments.is_empty()
        }

        pub fn new_this() -> Self {
            Self { segments: vec![] }
        }

        pub fn push_named_field(&mut self, field: String) {
            self.segments.push(ReferSegment::NamedField(field));
        }
    }

    impl TraceNode {
        fn path_in_entity(&self) -> &NamedRefPath {
            match self {
                TraceNode::Left(left_trace_node) => &left_trace_node.trace_ctx.path_in_entity,
                TraceNode::Right(right_trace_node) => &right_trace_node.trace_ctx.path_in_entity,
            }
        }
        fn location(&self, db: &Database, vfs: &Vfs) -> String {
            let file_wrapper = match self {
                TraceNode::Left(left_trace_node) => InFileWrapper::new(
                    left_trace_node.file_id,
                    left_trace_node.ast.syntax().clone(),
                ),
                TraceNode::Right(right_trace_node) => InFileWrapper::new(
                    right_trace_node.file_id,
                    right_trace_node.ast.syntax().clone(),
                ),
            };

            dbg!(&file_wrapper);

            let real = file_wrapper.original_file_range_with_macro_call_body(db);
            let line_index = db.line_index(real.file_id.file_id());
            let line_col = line_index.line_col(real.range.start());

            let path = vfs.file_path(real.file_id.file_id());

            format!("{path}:{}:{}", line_col.line + 1, line_col.col + 1)
        }

        fn ast(&self) -> &SyntaxNode {
            match self {
                TraceNode::Left(left_trace_node) => left_trace_node.ast.syntax(),
                TraceNode::Right(right_trace_node) => right_trace_node.ast.syntax(),
            }
        }

        fn file_id(&self) -> HirFileId {
            match self {
                TraceNode::Left(left_trace_node) => left_trace_node.file_id,
                TraceNode::Right(right_trace_node) => right_trace_node.file_id,
            }
        }
    }

    impl AnyField {
        fn is_scalar(&self) -> bool {
            matches!(self, AnyField::ScalarField)
        }
    }
    impl LeftTraceNode {
        fn is_scalar(&self) -> bool {
            if self.trace_ctx.is_self() {
                return false;
            }

            let field = self
                .trace_ctx
                .get_tracing_field()
                .expect("node field not found");

            field.is_scalar()
        }

        pub fn gen_traces(self, sema: &Semantics, bagua: &Bagua) -> Result<Vec<Trace>> {
            let ident = match self.ast.clone() {
                LeftTraceNodeAst::Pat(pat) => match pat {
                    ast::Pat::IdentPat(ident_pat) => {
                        ident_pat.name().context("IdentPat without Name")?
                    }
                    _ => {
                        todo!()
                    }
                },
                LeftTraceNodeAst::SelfArg(self_param) => self_param.name().unwrap(),
            };

            let usages = helper::usages_in_current_file(sema, &ident).unwrap();
            let base_trace = Trace {
                nodes: vec![TraceNode::Left(self.clone())],
            };
            let mut traces = vec![];
            for (_, usages) in usages {
                for usage in usages {
                    let name_ref = usage.name.as_name_ref().context("no name ref")?;
                    let file_id = sema.hir_file_for(name_ref.syntax());

                    let Some(right_node) = expand_ref_to_node(
                        sema,
                        name_ref,
                        &self.trace_ctx,
                        bagua,
                        file_id,
                        self.is_scalar(),
                    )?
                    else {
                        continue;
                    };
                    let deref_triggered = right_node.deref_triggered;
                    if deref_triggered {
                        let mut trace = base_trace.clone();
                        trace.push(TraceNode::Right(right_node));
                        traces.push(trace);
                    } else {
                        let mut right_nexts = vec![right_node];
                        loop {
                            let next_node = right_nexts.last().unwrap().next_node(sema, bagua)?;
                            if let Some(next_node) = next_node {
                                match next_node {
                                    TraceNode::Left(left_trace_node) => {
                                        let sub_traces = left_trace_node.gen_traces(sema, bagua)?;
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
                                            let bridge_nodes = right_nexts
                                                .into_iter()
                                                .map(|node| TraceNode::Right(node))
                                                .collect::<Vec<_>>();
                                            trace.nodes.extend(bridge_nodes);
                                            trace.push(TraceNode::Right(right_trace_node));
                                            traces.push(trace);
                                            break;
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
    }

    impl RightTraceNode {
        fn is_returning(&self, sema: &Semantics) -> Result<bool> {
            let expr = &self.ast;
            // let infile = InFileWrapper::new(self.file_id, self.ast.clone());
            // let origin = infile.original_ast_node_rooted(sema.db);
            // dbg!(origin);

            // // let call = sema.might_be_inside_macro_call(token)

            // let Some(expr) = sema.original_syntax_node_rooted(self.ast.syntax()) else {
            //     let m_call_id = self.file_id.macro_file().unwrap().macro_call_id;
            //     let call = sema.db.lookup_intern_macro_call(m_call_id);
            //     dbg!(call);

            //     bail!("cannot map node to origin node: {expr:#?}");
            // };
            // let expr = ast::Expr::cast(expr).context("not a expr")?;

            let Some(parent) = expr.syntax().parent() else {
                todo!()
            };
            if ast::ReturnExpr::can_cast(parent.kind()) {
                return Ok(true);
            }

            if ast::ExprStmt::can_cast(parent.kind()) {
                return Ok(false);
            }

            if ast::StmtList::can_cast(parent.kind()) {
                let grandpa = parent.parent().context("expr without parent")?;
                if ast::BlockExpr::can_cast(grandpa.kind()) {
                    let grandgrandpa = grandpa.parent().context("expr without parent")?;
                    if ast::Fn::can_cast(grandgrandpa.kind()) {
                        return Ok(true);
                    }
                }
            }

            Ok(false)
        }

        pub fn next_node(&self, sema: &Semantics, bagua: &Bagua) -> Result<Option<TraceNode>> {
            if self.is_returning(sema)? {
                let fn_call_point = self
                    .trace_ctx
                    .fn_call_point
                    .clone()
                    .context("no fn call point")?;
                let mut ctx = self.trace_ctx.clone();
                ctx.fn_call_point = fn_call_point.parent.map(|p| *p);

                let deref_triggered = check_if_deref_triggered(sema, &fn_call_point.ast, bagua)?;

                let node = TraceNode::Right(RightTraceNode {
                    ast: fn_call_point.ast,
                    trace_ctx: ctx,
                    deref_triggered,
                    file_id: fn_call_point.file_id,
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
                    ast: From::from(pat),
                    trace_ctx: self.trace_ctx.clone(),
                    file_id: self.file_id,
                })));
            }
            if let Some(arg_list) = ast::ArgList::cast(parent.clone()) {
                let index = arg_list
                    .args()
                    .position(|arg| arg.syntax() == self.ast.syntax())
                    .expect("no arg match");
                let fn_call = FnCall::try_from_arg_list(arg_list)?;

                return self
                    .next_fn_arg_node(sema, fn_call, NodeArgIndex::Index(index))
                    .map(|n| Some(TraceNode::Left(n)));
            }
            if let Some(method_call) = ast::MethodCallExpr::cast(parent.clone()) {
                let fn_call = FnCall::from_method_call(method_call);
                return self
                    .next_fn_arg_node(sema, fn_call, NodeArgIndex::Self_)
                    .map(|n| Some(TraceNode::Left(n)));
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

        fn next_fn_arg_node(
            &self,
            sema: &Semantics,
            fn_call: FnCall,
            index: NodeArgIndex, // arg_index: usize,
        ) -> Result<LeftTraceNode, anyhow::Error> {
            let fn_ = fn_call.resolve(sema)?;
            let fn_source = fn_.source(sema.db).context("fn has no source")?;
            let _file_ast = sema.parse_or_expand(dbg!(fn_source.file_id)); // don't delete this line
                                                                           // dbg!(_file_ast);
            let fn_ast = fn_source.value.clone();

            let params = fn_ast.param_list().context("fn has no param list")?;
            dbg!(params.params().next());
            dbg!(params.self_param());

            let pat = match index {
                NodeArgIndex::Self_ => {
                    let p = params.self_param().context("no SelfParams")?;
                    LeftTraceNodeAst::SelfArg(p)
                }
                NodeArgIndex::Index(index) => {
                    let param = params
                        .params()
                        .skip(index)
                        .next()
                        .with_context(|| format!("no param: {:#?}", fn_source))?;
                    let pat = param.pat().context("no pat in param")?;
                    LeftTraceNodeAst::Pat(pat)
                }
            };
            let mut trace_ctx = self.trace_ctx.clone();
            trace_ctx.fn_call_point = Some(FnCallPoint {
                ast: fn_call.to_expr(),
                file_id: self.file_id,
                parent: trace_ctx.fn_call_point.clone().map(Box::new),
            });

            return Ok(dbg!(LeftTraceNode {
                ast: pat,
                trace_ctx,
                file_id: fn_source.file_id,
            }));
        }
    }

    fn fn_call_ast(arg_list: &ast::ArgList) -> Result<ast::Expr> {
        let parent = arg_list
            .syntax()
            .parent()
            .context("arg list without parent")?;
        if let Some(method_call) = ast::MethodCallExpr::cast(parent.clone()) {
            return Ok(ast::Expr::MethodCallExpr(method_call));
        }

        if let Some(call_expr) = ast::CallExpr::cast(parent) {
            return Ok(ast::Expr::CallExpr(call_expr));
        }

        bail!("no fn-call-ast found")
    }

    enum NodeArgIndex {
        Self_,
        Index(usize),
    }

    impl AnyFieldGroup {
        fn get_field(&self, path: &ReferPath) -> Option<&AnyField> {
            self.get_field_by_segments(&path.segments)
        }

        fn get_field_by_segments(&self, segments: &[ReferSegment]) -> Option<&AnyField> {
            assert!(segments.len() != 0);

            let child = self.fields.get(&segments[0]);
            if segments.len() == 1 {
                return child;
            }

            let Some(child) = child else {
                return None;
            };

            match child {
                AnyField::GroupField(any_field_group) => {
                    any_field_group.get_field_by_segments(&segments[1..])
                }
                _ => None,
            }
        }

        fn get_struct_name_field(&self, name: &str) -> Option<AnyField> {
            self.fields
                .get(&ReferSegment::NamedField(name.to_owned()))
                .cloned()
        }
    }

    fn is_entity_field(sema: &Semantics, ty: &Type, bagua: &Bagua) -> Result<bool> {
        let Some(Adt::Enum(original)) = ty.strip_references().as_adt() else {
            return Ok(false);
        };
        Ok(original == bagua.struct_field)
    }

    fn check_if_deref_triggered(sema: &Semantics, expr: &ast::Expr, bagua: &Bagua) -> Result<bool> {
        let ty = sema.type_of_expr(expr).context("cannot get type of expr")?;
        dbg!(&expr);
        dbg!(&ty);

        let original = ty.original.strip_references();

        let Some(Adt::Enum(field_wrapper_ty)) = original.strip_references().as_adt() else {
            return Ok(false);
        };
        let Some(adjusted) = ty.adjusted.map(|t| t.strip_references()) else {
            return Ok(false);
        };
        dbg!(&original);
        dbg!(&adjusted);

        if field_wrapper_ty != bagua.struct_field {
            return Ok(false);
        }
        if original == adjusted {
            return Ok(false);
        }

        Ok(true)
    }

    fn check_prefix_expr_deref(
        sema: &Semantics,
        deref_expr: &ast::PrefixExpr,
        bagua: &Bagua,
    ) -> Result<bool> {
        let child = deref_expr
            .expr()
            .context("prefix expr without child expr")?;
        let child_ty = sema
            .type_of_expr(&child)
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

    fn expand_ref_to_node(
        sema: &Semantics,
        name_ref: &ast::NameRef,
        trace_ctx: &TraceCtx,
        bagua: &Bagua,
        file_id: HirFileId,
        mut is_scalar: bool,
    ) -> Result<Option<RightTraceNode>> {
        let path_expr = name_ref
            .syntax()
            .ancestors()
            .find_map(ast::PathExpr::cast)
            .context("no path expr")?;
        let mut res_expr = ast::Expr::PathExpr(path_expr);
        let mut is_deref = false;
        let mut trace_ctx = trace_ctx.clone();
        loop {
            match &res_expr {
                ast::Expr::FieldExpr(field_expr) => {
                    let name_ref = field_expr.name_ref().context("no name ref")?;
                    let field_name = name_ref.text();

                    let field = trace_ctx
                        .scope
                        .get_struct_name_field(&field_name)
                        .context("no field")?;

                    match field {
                        AnyField::NotEntityField => return Ok(None),
                        AnyField::ScalarField => {
                            is_deref = check_if_deref_triggered(sema, &res_expr, bagua)?;
                            trace_ctx.path_in_entity.push(field_name.to_string());
                            trace_ctx
                                .path_in_scope
                                .push_named_field(field_name.to_string());
                            is_scalar = true;
                        }
                        AnyField::GroupField(group_scope) => {
                            if trace_ctx.scope.is_real {
                                trace_ctx.path_in_entity.push(field_name.to_string());
                            }
                            trace_ctx.scope = group_scope.clone();
                            trace_ctx.path_in_scope = ReferPath::new_this();
                        }
                    }
                }
                ast::Expr::RefExpr(_) => {
                    if is_scalar {
                        is_deref = check_if_deref_triggered(sema, &res_expr, bagua)?;
                    }
                }
                ast::Expr::PrefixExpr(prefix_expr) => {
                    if is_scalar {
                        is_deref = check_prefix_expr_deref(sema, &prefix_expr, bagua)?;
                    }
                }
                ast::Expr::PathExpr(_) => {
                    if is_scalar {
                        is_deref = check_if_deref_triggered(sema, &res_expr, bagua)?;
                    }
                }
                _ => {
                    bail!("unknown right node: {:#?}", res_expr);
                }
            }
            if is_deref {
                break;
            }

            let Some(parent_expr) = res_expr.syntax().parent().and_then(ast::Expr::cast) else {
                break;
            };
            if matches!(parent_expr, ast::Expr::MethodCallExpr(_)) {
                break;
            }
            dbg!(parent_expr.syntax().text());
            res_expr = parent_expr;
        }

        let right_node = RightTraceNode {
            ast: res_expr,
            trace_ctx,
            deref_triggered: is_deref,
            file_id,
        };
        dbg!(&right_node);

        Ok(Some(right_node))
    }
}
