use std::path::Path;

use anyhow::Result;
use yinyang::workspace::{self, Workspace};

fn main() -> Result<()> {
    let sample_project = Path::new("/home/sen/web-rearch/yinyang-dev/sample-project");
    let workspace = Workspace::load(sample_project).unwrap();
    dbg!(workspace::bagua_usecase_trait(&workspace.host));

    for bc in workspace.bounded_contexts()? {
        dbg!(&bc.name);
        let ucs = bc.use_cases(&workspace.host, &workspace.vfs)?;

        for uc in ucs {
            dbg!(&uc.name);
            dbg!(&uc.path);
        }
    }

    Ok(())
}
