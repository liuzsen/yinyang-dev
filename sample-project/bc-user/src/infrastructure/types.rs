#![allow(unused)]

use crate::{adapters::api_http::UserCredential, domain::user::UserId};

pub type ConnectionPool = common::postgres::StaticDieselPgPool;
pub type DbAdapter = bagua::db::diesel::DbAdapterDiesel<ConnectionPool>;
pub type SqlRunner = bagua::db::diesel::DbAdapterDiesel<ConnectionPool>;
pub type TxnManager = bagua::db::diesel::TxnManagerDiesel<DbAdapter>;
pub type Repository = crate::adapters::spi_db::BasicRepo<SqlRunner>;

pub type HttpRequest = bagua::http::actix_web_impl::HttpRequestImpl;
pub type HttpJsonBody<T> = bagua::http::actix_web_impl::HttpJsonBodyImpl<T>;
pub type HttpJsonQuery<T> = bagua::http::actix_web_impl::HttpJsonQueryImpl<T>;
pub type HttpCredential = crate::infrastructure::credential::CredentialExtractor;

pub type UserHttpCredential = crate::infrastructure::credential::CredentialExtractor;

pub type EmailService = crate::adapters::spi_email::EmailServiceImpl;

pub mod user {
    pub mod create {
        use crate::infrastructure::types::*;
        pub type UseCase = crate::application::user::create::Executor<Repository, EmailService>;
        pub type Adapter = crate::adapters::api_http::user::create::AdapterImpl;
    }

    pub mod update {
        use crate::infrastructure::types::*;
        pub type UseCase = crate::application::user::update::Executor<Repository>;
        pub type Adapter = crate::adapters::api_http::user::update::AdapterImpl;
    }
}
