use bagua::{biz_ok, ensure_biz, repository::Repository, usecase::UseCase, Provider};
use serde::Serialize;

use crate::{
    application::traits::EmailService,
    domain::user::{Email, Password, User, UserId, UserModel, UserRole, Username},
};

pub struct Input {
    pub email: Email,
    pub email_code: String,
    pub name: Username,
    pub password: Password,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Output {
    pub id: UserId,
}

pub enum Error {
    EmailCodeInvalid,
    UserAlreadyExists,
}

#[derive(Provider)]
pub struct Executor<R, E> {
    repo: R,
    email_service: E,
}

impl<R, E> UseCase for Executor<R, E>
where
    R: Repository<User>,
    E: EmailService,
{
    type Input = Input;

    type Output = Output;

    type Error = Error;

    async fn execute(
        &mut self,
        params: Self::Input,
    ) -> bagua::result::BizResult<Self::Output, Self::Error> {
        let Input {
            email,
            email_code,
            name,
            password,
        } = params;
        ensure_biz!(
            self.email_service
                .verify_email_code(&email, &email_code)
                .await?,
            Error::EmailCodeInvalid
        );

        let user = UserModel {
            email,
            name,
            password,
            role: UserRole::User,
        }
        .build_entity();

        ensure_biz!(
            self.repo.save(&user).await?.is_effected(),
            Error::UserAlreadyExists
        );

        biz_ok!(Output { id: user.id })
    }
}
