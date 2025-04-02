use ra_ap_hir::Crate;
use ra_ap_ide::RootDatabase;
use ra_ap_proc_macro_api::ProcMacroClient;
use ra_ap_vfs::Vfs;

use super::bagua::Bagua;

pub struct BaguaProject {
    bagua: Bagua,
    ra: Ra,
    workspace: Workspace,
}

pub struct Workspace {
    root_crate: Crate,
    bc_crates: Vec<BoundedContext>,
}

pub struct BoundedContext {
    pub krate: Crate,
}

pub struct Ra {
    db: RootDatabase,
    vfs: Vfs,
    proc_macro: ProcMacroClient,
}

impl BaguaProject {
    pub fn load<P: AsRef<std::path::Path>>(path: P) -> anyhow::Result<Self> {
        todo!()
    }
}

impl Ra {
    pub fn load<P: AsRef<std::path::Path>>(path: P) -> anyhow::Result<Self> {
        todo!()
    }
}
