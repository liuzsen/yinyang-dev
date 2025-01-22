use bagua::{
    biz_ok, ensure_biz, ensure_exist,
    repository::{Repository, SubsetLoader},
    usecase::UseCase,
    Provider,
};
use serde::Serialize;

use crate::domain::user::{Password, User, UserError, UserId, UserWithPassword};

pub struct Input {
    id: UserId,
    old_password: Password,
    new_password: Password,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Output {}

pub enum Error {
    UserNotFound,
    UserError(UserError),
}

#[derive(Provider)]
pub struct Executor<R> {
    repo: R,
}

impl<R> UseCase for Executor<R>
where
    R: Repository<User>,
    R: SubsetLoader<UserWithPassword>,
{
    type Input = Input;

    type Output = Output;

    type Error = Error;

    async fn execute(
        &mut self,
        params: Self::Input,
    ) -> bagua::result::BizResult<Self::Output, Self::Error> {
        let Input {
            id,
            old_password,
            new_password,
        } = params;
        let mut user = ensure_exist!(self.repo.find(id).await?, Error::UserNotFound);
        ensure_biz!(user
            .update_password(&old_password, new_password)
            .map_err(Error::UserError));

        biz_ok!(Output {})
    }
}
