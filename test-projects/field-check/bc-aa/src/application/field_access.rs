use std::collections::HashSet;

use bagua::{
    biz_err, biz_ok, ensure_exist,
    entity::field::Field,
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

struct BB<'a, T> {
    group: &'a T,
    unused: u8,
}

fn access(entity: &mut AA) -> &MyString {
    {
        // all ok

        match &entity.name {
            bagua::entity::field::Field::Unloaded => {}
            bagua::entity::field::Field::Unchanged(_) => {}
            bagua::entity::field::Field::Set(_) => {}
        }
        let Field::Unchanged(name) = &entity.name else {
            todo!()
        };
        if let Field::Unchanged(name) = &entity.name {
            todo!()
        }
        if entity.name.changed_ref().is_some() {
            todo!();
        }
    }
    &entity.name;
    let name = &&*&*&entity.name;
    let name = name;
    let name_: &MyString = &&*&name;
    let name_len: usize = name.len();

    let group = &entity.group;
    let group = group;
    let g1: &u32 = &group.g1;

    let bb = BB {
        group: &entity.group,
        unused: 0,
    };

    let bb_g1: u32 = *bb.group.g1;

    let _: &MyString = entity.name.this(); // panic
    let _: usize = entity.name.len(); // panic
    let _: usize = entity.name.inner.len(); // panic
    let _: &u32 = &entity.group.g1; // panic
    let _: &HashSet<BBId> = entity.foreigns.origin_value_ref(); // panic

    let _: &MyString = &(&entity.name); // panic
    let _: u32 = *(&entity.group).g1; // panic
    let _: &u32 = &super::identity(&entity.group).g1; // panic
    let _: &String = &entity.get_group().g2; // panic
    let _: &String = &entity.get_group().get_g2(); // panic

    entity.set_name("name1".to_string()); // ok
    entity.set_g1(1); // ok
    entity.set_name_if_empty("name2".to_string()); // panic

    let AAReadOnly {
        id: _,
        name,
        foreigns,
        group,
        ..
    } = entity.read_only();
    let AAReadOnly {
        id: _,
        name,
        foreigns,
        group,
        ..
    } = &**entity;

    let _: &MyString = &name; // panic
    let _: &HashSet<BBId> = foreigns.origin_value_ref(); // panic
    let _: &u32 = &super::identity(&group).g1; // panic
    let _: &String = &group.g2; // panic

    &entity.name
}

trait This {
    fn this(&self) -> &Self {
        self
    }
}

impl<T> This for Field<T> {}
