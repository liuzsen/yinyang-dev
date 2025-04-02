use std::path::Path;

use anyhow::Result;
use ra_ap_base_db::SourceDatabase;
use ra_ap_hir::{db::ExpandDatabase, HasSource, Semantics};
use ra_ap_ide::{TextSize, TryToNav};
use ra_ap_ide_db::defs::{Definition, IdentClass};
use ra_ap_paths::Utf8Path;
use ra_ap_project_model::{CargoConfig, ProjectManifest, ProjectWorkspace};
use ra_ap_syntax::{
    ast::{Fn, HasArgList, HasName},
    syntax_editor::Element,
    AstNode, SourceFile,
};
use ra_ap_vfs::{AbsPath, VfsPath};
use yinyang::{
    field_checker5, loader, parse_find,
    trait_ref::parse_trait_ref,
    workspace::{self, Workspace},
};

fn main() -> Result<()> {
    // bagua_project()?;

    // aa();
    // aa_project()?;
    // parse_find::parse_find()?;
    // parse_trait_ref()?;

    let project = loader::BaguaProject::load(
        "/home/sen/framework-dev/yinyang-dev/test-projects/field-check",
    )?;

    let violations = field_checker5::FieldChecker::new(&project).check()?;
    dbg!(&violations);
    Ok(())
}

trait Repo {
    fn find<T>(&self) -> T
    where
        Self: Loader<T>,
    {
        self.load()
    }
}

trait Loader<T> {
    fn load(&self) -> T;
}

struct UC<R> {
    ab: R,
}

impl<R> UC<R>
where
    R: Repo,
    R: Loader<i32>,
{
    fn aa(&self) {
        let a = self.ab.find();
    }
}

fn bagua_project() -> Result<()> {
    let sample_project = Path::new("/home/sen/web-rearch/yinyang-dev/sample-project");
    let workspace = Workspace::load(sample_project).unwrap();
    dbg!(workspace::bagua_usecase_trait(&workspace.host));

    for bc in workspace.bounded_contexts()? {
        dbg!(&bc.name);
        let ucs = bc.use_cases(&workspace.host, &workspace.vfs)?;

        for uc in ucs {
            dbg!(&uc.name);
            dbg!(&uc.path);
            uc.used_entities(&workspace.host, &workspace.vfs)?;
        }
    }

    Ok(())
}
