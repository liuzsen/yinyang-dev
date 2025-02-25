use std::path::Path;

use anyhow::Result;
use ra_ap_base_db::SourceDatabase;
use ra_ap_hir::{db::HirDatabase, Crate, HasSource, HirDisplay, Module, TraitRef};
use ra_ap_hir_def::{resolver::HasResolver, ImplId};
use ra_ap_hir_ty::{Interner, TyBuilder};
use ra_ap_ide::RootDatabase;
use ra_ap_paths::Utf8Path;
use ra_ap_vfs::AbsPath;

use crate::{crate_path, parse_find::GetId, workspace::Workspace};

pub fn parse_trait_ref() -> Result<()> {
    let root_dir = Path::new("/home/sen/web-rearch/yinyang-dev/.local/aa");
    let root_dir = AbsPath::assert(Utf8Path::from_path(root_dir).unwrap());
    let Workspace {
        host,
        vfs,
        proc_macro,
        root_crate,
    } = Workspace::load(root_dir.as_ref()).unwrap();

    let db = host.raw_database();

    let root_mod = root_crate.root_module();
    let resolver = root_mod.id().resolver(db);

    let path = crate_path!(bb);
    let resolved = resolver.resolve_module_path_in_items(db, &path);
    let items = resolved.take_types().unwrap();
    let bb_mod = Module::from(items.module(db).unwrap());

    let bb_fn = bb_mod
        .declarations(db)
        .into_iter()
        .find_map(|item| {
            if let ra_ap_hir::ModuleDef::Function(function) = item {
                Some(function)
            } else {
                None
            }
        })
        .unwrap();

    let body_src = bb_fn.source(db).unwrap().value;
    let body_block = body_src.body().unwrap();
    let stmts = body_block.stmt_list().unwrap();

    for stmt in stmts.statements() {
        dbg!(stmt);
    }

    Ok(())
}

macro_rules! identity {
    ($($tt:tt)*) => {
        $($tt)*
    };
}
fn bb() -> Result<(), String> {
    let a = 1;
    let b = { 1 };
    let c = 1 == 1;
    let d = { 1 == 1 };
    let e = identity!(String::from("a"));
    let f = 1i32 as u8;
    let g = format!("{}", 1);
    let h = if true { 1 } else { 2 };
    let i = Err(String::from("a"))?;

    Ok(())
}
