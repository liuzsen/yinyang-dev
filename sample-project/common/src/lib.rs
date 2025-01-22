#![allow(async_fn_in_trait)]

pub mod init;
pub mod logger;
pub mod postgres;
pub mod settings;

pub use settings::Settings;

pub async fn init(settings: &Settings) -> anyhow::Result<()> {
    init::run_all(settings).await?;

    Ok(())
}
