use bagua::{db::diesel::DieselSqlRunner, Provider};
use diesel::pg::Pg;

pub mod dao;

pub mod user;

#[derive(Provider)]
pub struct BasicRepo<T> {
    adapter: T,
}

pub trait PgSqlRunner: DieselSqlRunner<Pg> {}

impl<T: DieselSqlRunner<Pg>> PgSqlRunner for T {}
