use anyhow::Context;
use bagua::InitFunction;
use diesel::Connection;
use diesel_migrations::{embed_migrations, EmbeddedMigrations, MigrationHarness};
use tracing::info;
pub const MIGRATIONS: EmbeddedMigrations = embed_migrations!("migrations");

#[InitFunction]
pub async fn init(s: &crate::settings::Settings) -> anyhow::Result<()> {
    init_inner(&s.db_url).await?;

    Ok(())
}

async fn init_inner(url: &str) -> anyhow::Result<()> {
    run_pg_migrations(url).context("error running pg migrations")?;

    Ok(())
}

pub fn run_pg_migrations(url: &str) -> anyhow::Result<()> {
    info!(url, "Running migrations");
    let mut conn = diesel::pg::PgConnection::establish(url)?;
    run_migrations::<diesel::pg::Pg>(&mut conn)
}

pub fn run_migrations<DB: diesel::backend::Backend>(
    connection: &mut impl MigrationHarness<DB>,
) -> anyhow::Result<()> {
    connection
        .run_pending_migrations(MIGRATIONS)
        .map_err(|err| anyhow::anyhow!(err))?;

    Ok(())
}
