use anyhow::{bail, Context};
use ra_ap_hir::{Function, ModuleDef, ToolModule, Variant};
use ra_ap_hir_def::hir;
use ra_ap_ide::SearchScope;
use ra_ap_ide_db::{
    defs::{Definition, NameClass, NameRefClass},
    search::UsageSearchResult,
};
use ra_ap_span::EditionedFileId;
use ra_ap_syntax::{
    ast::{self, HasArgList},
    ted, AstNode, SyntaxNode, SyntaxToken,
};

use crate::{loader::Bagua, Semantics};

fn expand_macro_recur(sema: &Semantics, macro_call: &ast::MacroCall) -> Option<SyntaxNode> {
    let expanded = sema.expand(macro_call)?.clone_for_update();
    expand(sema, expanded, ast::MacroCall::cast, expand_macro_recur)
}

fn expand<T: AstNode>(
    sema: &Semantics,
    expanded: SyntaxNode,
    f: impl FnMut(SyntaxNode) -> Option<T>,
    exp: impl Fn(&Semantics, &T) -> Option<SyntaxNode>,
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

pub fn is_method_call_of_find(sema: &Semantics, token: &SyntaxToken, bagua: &Bagua) -> bool {
    fn is_method_call_of_find_inner(
        sema: &Semantics,
        token: &SyntaxToken,
        bagua: &Bagua,
    ) -> Option<bool> {
        let name_ref = token.parent_ancestors().find_map(ast::NameRef::cast)?;
        let name_ref_class = NameRefClass::classify(sema, &name_ref)?;
        if let NameRefClass::Definition(Definition::Function(function)) = name_ref_class {
            let is_method_find = function == bagua.repository_trait.method_find(sema.db)?;
            return Some(is_method_find);
        }
        None
    }

    is_method_call_of_find_inner(sema, token, bagua).unwrap_or(false)
}

pub fn usages_in_current_file(sema: &Semantics, name: &ast::Name) -> Option<UsageSearchResult> {
    if let Some(NameClass::Definition(def)) = NameClass::classify(sema, &name) {
        // let file_id = sema.hir_file_for(&name.syntax()).file_id().unwrap();

        let usages = def
            .usages(sema)
            // .in_scope(&SearchScope::single_file(file_id))
            .all();
        return Some(usages);
    }

    None
}

pub fn current_fn(stmt: &ast::Stmt) -> Option<ast::Fn> {
    stmt.syntax().ancestors().find_map(ast::Fn::cast)
}

pub enum FunctionLike {
    Fn(Function),
    Variant(Variant),
}

impl FunctionLike {
    pub fn to_fn(self) -> Option<Function> {
        match self {
            FunctionLike::Fn(fn_) => Some(fn_),
            FunctionLike::Variant(_) => None,
        }
    }
}

pub fn resolve_fn(sema: &Semantics, name_ref: &ast::NameRef) -> Option<FunctionLike> {
    let parent = name_ref.syntax().parent()?;

    if let Some(path) = ast::PathSegment::cast(parent.clone()).map(|it| it.parent_path()) {
        let resolution = dbg!(sema.resolve_path(dbg!(&path)))?;
        match resolution {
            ra_ap_hir::PathResolution::Def(ModuleDef::Function(fn_)) => {
                return Some(FunctionLike::Fn(fn_));
            }
            ra_ap_hir::PathResolution::Def(ModuleDef::Variant(variant)) => {
                return Some(FunctionLike::Variant(variant));
            }
            _ => return None,
        }
    }
    if let Some(fn_) = parent.parent().and_then(ast::MethodCallExpr::cast) {
        return sema.resolve_method_call(&fn_).map(FunctionLike::Fn);
    }

    None
}

pub enum FnCall {
    CallExpr(ast::CallExpr),
    MethodCall(ast::MethodCallExpr),
}

impl FnCall {
    pub fn from_method_call(m: ast::MethodCallExpr) -> Self {
        Self::MethodCall(m)
    }

    pub fn try_from_arg_list(arg_list: ast::ArgList) -> anyhow::Result<Self> {
        let parent = arg_list
            .syntax()
            .parent()
            .context("ArgList without parent")?;
        if let Some(method_call) = ast::MethodCallExpr::cast(parent.clone()) {
            return Ok(Self::MethodCall(method_call));
        }

        if let Some(method_call) = ast::CallExpr::cast(parent) {
            return Ok(Self::CallExpr(method_call));
        }

        bail!("not a function call")
    }

    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            FnCall::CallExpr(call_expr) => call_expr.syntax(),
            FnCall::MethodCall(method_call_expr) => method_call_expr.syntax(),
        }
    }

    pub fn to_expr(self) -> ast::Expr {
        match self {
            FnCall::CallExpr(call_expr) => ast::Expr::CallExpr(call_expr),
            FnCall::MethodCall(method_call_expr) => ast::Expr::MethodCallExpr(method_call_expr),
        }
    }

    pub fn resolve(&self, sema: &Semantics) -> anyhow::Result<Function> {
        match self {
            FnCall::CallExpr(call_expr) => {
                let path_expr = call_expr
                    .syntax()
                    .first_child()
                    .context("call_expr without the first child")?;
                let path_expr = ast::PathExpr::cast(path_expr).context("not a PathExpr")?;
                let path = path_expr.path().context("a PathExpr without path")?;
                let resolution = sema.resolve_path(&path).context("cannot resolve fn path")?;
                match resolution {
                    ra_ap_hir::PathResolution::Def(ModuleDef::Function(fn_)) => {
                        return Ok(fn_);
                    }
                    _ => bail!("not a fn"),
                }
            }
            FnCall::MethodCall(method_call_expr) => {
                return sema
                    .resolve_method_call(method_call_expr)
                    .context("cannot resolve fn");
            }
        };
    }
}
