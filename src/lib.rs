pub mod bc_context;
pub mod entity;
pub mod field_check3;
pub mod field_checker;
pub mod field_checker2;
pub mod field_checker4;
pub mod field_checker5;
pub mod helper;
pub mod loader;
pub mod parse_find;
pub mod trait_ref;
pub mod workspace;

type Database = ra_ap_ide::RootDatabase;
type Semantics<'a> = ra_ap_hir::Semantics<'a, Database>;
