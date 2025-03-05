use std::collections::HashSet;

use bagua::{
    biz_err, biz_ok, ensure_exist,
    repository::{Repository, SubsetLoader},
    usecase::UseCase,
    Provider,
};
use serde::{Deserialize, Serialize};

use crate::domain::aa::{AAId, AAReadOnly, AAWithOnlyId, BBId, MyString, AA};

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

        let aa: Option<AA> = self.repo.find(id).await?;
        let name_opt = aa.as_ref().map(|aa| &*aa.name);

        let Some(mut aa) = aa else {
            return biz_err!(Error::AANotFound);
        };

        access(&mut aa);

        todo!()
    }
}

fn access(aa: &mut AA) {
    let _: &MyString = &aa.name; // panic
    let _: usize = aa.name.len(); // panic
    let _: usize = aa.name.inner.len(); // panic
    let _: &u32 = &aa.group.g1; // panic
    let _: &HashSet<BBId> = aa.foreigns.origin_value_ref(); // panic

    let _: &u32 = &super::identity(&aa.group).g1; // panic
    let _: &String = &aa.get_group().g2; // panic
    let _: &String = &aa.get_group().get_g2(); // panic

    aa.set_name("name1".to_string()); // ok
    aa.set_g1(1); // ok
    aa.set_name_if_empty("name2".to_string()); // panic

    let AAReadOnly {
        id: _,
        name,
        foreigns,
        group,
        ..
    } = aa.read_only();
    let AAReadOnly {
        id: _,
        name,
        foreigns,
        group,
        ..
    } = &**aa;

    let _: &MyString = &name; // panic
    let _: &HashSet<BBId> = foreigns.origin_value_ref(); // panic
    let _: &u32 = &super::identity(&group).g1; // panic
    let _: &String = &group.g2; // panic
}
