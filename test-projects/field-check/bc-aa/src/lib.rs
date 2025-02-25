#![allow(async_fn_in_trait)]

pub mod adapters;
pub mod application;
pub mod domain;
pub mod infrastructure;

pub mod init;
pub mod settings;

pub async fn init(settings: &settings::Settings) -> anyhow::Result<()> {
    init::run_all(settings).await?;
    Ok(())
}
