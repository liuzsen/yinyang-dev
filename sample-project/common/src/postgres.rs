use anyhow::Context;
use bagua::{
    db::{diesel::pg_pool::PgConn, ConnectionPool},
    Provider,
};
use tracing::info;

#[bagua::InitFunction(priority = 1)]
async fn init(settings: &crate::settings::Settings) -> anyhow::Result<()> {
    info!("[init] Postgres connection pool");
    bagua::db::diesel::pg_pool::init(&settings.postgres)
        .await
        .with_context(|| {
            format!(
                "Failed to init postgres connection pool. Config = {:#?}",
                settings.postgres
            )
        })
}

#[derive(Clone, Provider)]
pub struct StaticDieselPgPool {}

impl ConnectionPool for StaticDieselPgPool {
    type Connection = PgConn;

    async fn get_conn(&self) -> anyhow::Result<PgConn> {
        self.get_conn().await
    }
}

impl StaticDieselPgPool {
    pub async fn get_conn(&self) -> anyhow::Result<PgConn> {
        bagua::db::diesel::pg_pool::pg_conn().await
    }
}
