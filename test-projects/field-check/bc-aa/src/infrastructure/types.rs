#![allow(unused)]

pub type ConnectionPool = common::postgres::StaticDieselPgPool;
pub type DbAdapter = bagua::db::diesel::DbAdapterDiesel<ConnectionPool>;
pub type SqlRunner = bagua::db::diesel::DbAdapterDiesel<ConnectionPool>;
pub type TxnManager = bagua::db::diesel::TxnManagerDiesel<DbAdapter>;
pub type Repository = crate::adapters::spi_db::BasicRepo<SqlRunner>;

pub type HttpRequest = bagua::http::actix_web_impl::HttpRequestImpl;
pub type HttpJsonBody<T> = bagua::http::actix_web_impl::HttpJsonBodyImpl<T>;
pub type HttpJsonQuery<T> = bagua::http::actix_web_impl::HttpJsonQueryImpl<T>;
