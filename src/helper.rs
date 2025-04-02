use anyhow::{bail, Context, Result};
use ra_ap_hir::{Function, MacroFileId, ModuleDef, ToolModule, Variant};
use ra_ap_hir_def::hir;
use ra_ap_ide::SearchScope;
use ra_ap_ide_db::{
    defs::{Definition, NameClass, NameRefClass},
    search::UsageSearchResult,
};
use ra_ap_span::EditionedFileId;
use ra_ap_syntax::{
    ast::{self, HasArgList},
    ted, AstNode, SyntaxKind, SyntaxNode, SyntaxToken,
};

use crate::{loader::Bagua, Database, Semantics};

// fn expand_macro_recur(sema: &Semantics, macro_call: &ast::MacroCall) -> Option<SyntaxNode> {
//     let expanded = sema
//         .expand(MacroFileId {
//             macro_call_id: macro_call,
//         })
//         .value
//         .clone_for_update();
//     expand(sema, expanded, ast::MacroCall::cast, expand_macro_recur)
// }

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
        if let NameRefClass::Definition(Definition::Function(function), substits) = name_ref_class {
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

pub fn is_returning(sema: &Semantics, expr: ast::Expr) -> Result<bool> {
    use SyntaxKind::*;

    let ancestors = sema.ancestors_with_macros(expr.syntax().clone());
    for anc in ancestors {
        let kind = anc.kind();
        if matches!(kind, SyntaxKind::LET_STMT) {
            return Ok(false);
        }
        if matches!(kind, SyntaxKind::FN) {
            return Ok(true);
        }
        if kind == SyntaxKind::RETURN_EXPR {
            return Ok(true);
        }
        if matches!(kind, MATCH_ARM | MATCH_ARM_LIST | STMT_LIST | MACRO_CALL) {
            continue;
        }

        if let Some(expr_stmt) = ast::ExprStmt::cast(anc.clone()) {
            let first_child = expr_stmt.syntax().first_child().unwrap();
            if first_child.kind() == SyntaxKind::BREAK_EXPR {
                continue;
            } else {
                return Ok(false);
            }
        }

        if !ast::Expr::can_cast(kind) {
            return Ok(false);
        }
    }

    Ok(false)
}

#[cfg(test)]
mod tests {
    use ra_ap_ide::RootDatabase;
    use ra_ap_syntax::{ast, AstNode};
    use ra_ap_test_fixture::WithFixture;

    use crate::Semantics;

    #[test]
    fn test_is_returning() {
        assert!(check_if_returning(returning_case_1()));
        assert!(check_if_returning(returning_case_2()));
        assert!(check_if_returning(returning_case_3()));
        assert!(check_if_returning(loop_break()));
        assert!(!check_if_returning(not_returning()));
    }

    fn check_if_returning(code: &str) -> bool {
        let (db, file_id, selection) = RootDatabase::with_range_or_offset(code);
        let sema = Semantics::new(&db);
        let file = sema.parse(ra_ap_base_db::EditionedFileId::new(&db, file_id));
        dbg!(&file);
        let token = file
            .syntax()
            .covering_element(dbg!(selection.expect_range()));
        let token = sema.descend_into_macros_no_opaque(token.into_token().unwrap())[0].clone();
        let expr = token.parent_ancestors().find_map(ast::Expr::cast).unwrap();
        super::is_returning(&sema, expr).unwrap()
    }

    fn loop_break() -> &'static str {
        r#"
fn aa() -> u8 {
    loop {
        break $02$0;
    }
}
        "#
    }

    fn not_returning() -> &'static str {
        r#"
fn bb() -> &'static str {
    let a = "";

    $0a$0;
    todo!()
}
        "#
    }

    fn returning_case_3() -> &'static str {
        r#"
macro_rules! echo {
    ($($tt:tt)*) => {
        $($tt)*
    };
}

fn bb() -> &'static str {
    let a = "";

    a;

    match true {
        true => echo!(a),
        false => {
            if true {
                {
                    a
                }
            } else {
                return { $0a$0 };
            }
        }
    }
}
        "#
    }

    fn returning_case_2() -> &'static str {
        r#"
macro_rules! echo {
    ($($tt:tt)*) => {
        $($tt)*
    };
}

fn bb() -> &'static str {
    let a = "";

    a;

    match true {
        true => echo!(a),
        false => {
            if true {
                {
                    $0a$0
                }
            } else {
                return { a };
            }
        }
    }
}
        "#
    }

    fn returning_case_1() -> &'static str {
        r#"
macro_rules! echo {
    ($($tt:tt)*) => {
        $($tt)*
    };
}

fn bb() -> &'static str {
    let a = "";

    a;

    match true {
        true => echo!($0a$0),
        false => {
            if true {
                {
                    a
                }
            } else {
                return { a };
            }
        }
    }
}
        "#
    }
}
