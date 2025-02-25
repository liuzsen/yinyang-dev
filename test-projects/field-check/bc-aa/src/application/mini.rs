use bagua::{
    biz_err, biz_ok,
    repository::{Repository, SubsetLoader},
    usecase::UseCase,
    Provider,
};
use serde::{Deserialize, Serialize};

use crate::domain::{AAId, AAWithOnlyId, AA};

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
        let aa = self.repo.find::<AAWithOnlyId, AAId>(id).await?;
        let Some(mut aa) = aa else {
            return biz_err!(Error::AANotFound);
        };

        let _ = &aa.name1; // violate
        let _ = &aa.group.g1; // violate

        let _ = super::identity(&aa.group).g1; // violate
        let _ = aa.get_group().g2; // violate
        let _ = aa.get_group().get_g2(); // violate

        aa.set_name1("name1".to_string()); // ok
        aa.set_g1(1); // ok
        aa.set_name2_if_empty("name2".to_string()); // violate

        biz_ok!(Output {})
    }
}
