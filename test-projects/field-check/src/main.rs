use tracing::info;

pub mod http;
pub mod settings;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let settings = settings::load_from_convention_files()?;

    init(&settings).await?;

    let server = http::build_server(&settings.http).await?;
    server.await?;

    Ok(())
}

async fn init(settings: &settings::Settings) -> anyhow::Result<()> {
    common::init(&settings.common).await?;

    bc_aa::init(&settings.bc_aa).await?;

    info!("init success! 🎉");

    Ok(())
}
