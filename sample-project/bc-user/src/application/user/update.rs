use bagua::{
    biz_ok, ensure_exist,
    repository::{Repository, SubsetLoader},
    usecase::UseCase,
    Provider,
};
use serde::Serialize;

use crate::domain::user::{User, UserId, UserMini, UserUpdater};

pub struct Input {
    pub id: UserId,
    pub updater: UserUpdater,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Output {}

pub enum Error {
    UserNotFound,
}

#[derive(Provider)]
pub struct Executor<R> {
    repo: R,
}

impl<R> UseCase for Executor<R>
where
    R: Repository<User>,
    R: SubsetLoader<UserMini>,
{
    type Input = Input;

    type Output = Output;

    type Error = Error;

    async fn execute(
        &mut self,
        params: Self::Input,
    ) -> bagua::result::BizResult<Self::Output, Self::Error> {
        let Input { id, updater } = params;
        let mut user = ensure_exist!(self.repo.find(id).await?, Error::UserNotFound);
        user.update_fields(updater);

        biz_ok!(Output {})
    }
}
