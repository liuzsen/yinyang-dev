use bagua::{
    biz_ok, ensure_exist,
    repository::{Repository, SubsetLoader},
    usecase::UseCase,
    Provider,
};
use serde::{Deserialize, Serialize};

use crate::{
    application::identity,
    domain::aa::{AAId, AAWithOnlyId, MyString, AA},
};

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Input {
    id: AAId,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Output {}

pub enum Error {
    AANotFound,
}

#[derive(Provider)]
pub struct Executor<R> {
    repo: R,
}

impl<R> UseCase for Executor<R>
where
    R: Repository<AA>,
    R: SubsetLoader<AAWithOnlyId>,
{
    type Input = Input;

    type Output = Output;

    type Error = Error;

    async fn execute(
        &mut self,
        params: Self::Input,
    ) -> bagua::result::BizResult<Self::Output, Self::Error> {
        let Input { id } = params;
        let repo1 = &mut self.repo;
        let repo2 = identity(repo1);
        let repo3 = Some(repo2);
        find_helper(repo3.unwrap(), id).await;

        let repo = &mut self.repo;
        let _aa = repo.find(id).await?.unwrap(); // entity
        let Some(_aa /* entity */) = repo.find(id).await? else {
            todo!()
        };

        let aa = self.repo.find(id).await;
        let aa = aa;
        if let Ok(Some(_aa)) = identity(aa) {} // entity

        let mut _aa: AA = ensure_exist!(
            Repository::find::<AAWithOnlyId, AAId>(&mut self.repo, id).await?,
            Error::AANotFound
        ); // entity

        biz_ok!(Output {})
    }
}

async fn find_helper<A>(a: &mut A, id: AAId)
where
    A: Repository<AA>,
    A: SubsetLoader<AAWithOnlyId>,
{
    let b = a;
    let c = b;
    let _aa = Repository::find(c, id).await.unwrap().unwrap(); // entity
    let _aa = Repository::find(c, id).await.unwrap().unwrap(); // entity
}
