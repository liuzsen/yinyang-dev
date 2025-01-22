use serde::Deserialize;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Settings {
    pub tracing: crate::logger::TracingCfg,
    pub postgres: bagua::db::diesel::pg_pool::PgPoolConfig,
}
