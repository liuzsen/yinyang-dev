use std::collections::HashSet;

use bagua::{
    biz_err, biz_ok, ensure_exist,
    entity::field::Field,
    repository::{Repository, SubsetLoader},
    usecase::UseCase,
    Provider,
};
use serde::{Deserialize, Serialize};

use crate::{
    domain::aa::{AAId, AAReadOnly, AAWithName, AAWithOnlyId, BBId, MyString, AA},
    output,
};

use super::{This, ThisField};

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
    R: SubsetLoader<AAWithName>,
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

        simple_access(&mut aa);

        todo!()
    }
}

struct BB<'a, T> {
    gp: &'a T,
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
    let aa = Some(&entity.name);
    if true {
        if let Some(aa) = aa {
            todo!()
        }
    } else {
        let b = aa.unwrap();
    }
    let name = &&*&*&entity.name;
    let name = name;
    let name_: &MyString = &&*&name;
    let name_len: usize = name.len();

    let group = &entity.group;
    let group = group;
    let g1: &u32 = &group.g1;

    let bb = BB {
        gp: &entity.group,
        unused: 0,
    };

    let bb_g1 = &bb.gp.g1;
    let bb_g1: u32 = **&bb.gp.g1;
    let bb_g1: &u32 = *&&bb.gp.g1;

    let _: &MyString = entity.name.this_ref(); // panic
    let _: usize = entity.name.len(); // panic
    let _: usize = entity.name.inner.len(); // panic
    let _: &u32 = &entity.group.g1; // panic
    let _: &HashSet<BBId> = entity.foreigns.origin_value_ref(); // panic

    let _: &MyString = &(&entity.name); // panic
    let _: u32 = *(&entity.group).g1; // panic
    let _: &u32 = &super::identity(&entity.group).this().g1; // panic
    let _: &u32 = &crate::application::aa::identity(&entity.group).this().g1; // panic
    let _: &String = &entity.get_group().g2; // panic
    let _: &String = &entity.get_group().get_g2(); // panic

    entity.set_name("name1".to_string()); // ok
    entity.set_g1(1); // ok
    entity.set_name_if_empty("name2".to_string()); // panic

    let AAReadOnly {
        id: _,
        name: nn,
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

    if true {
        return &entity.name;
    }

    (&entity.name)
}

fn simple_access(entity: &mut AA) {
    let name = &&*&*&entity.name;
    let name: &&Field<MyString> = name;
    // let name_: &MyString = &&*&name;
    // let name_len: usize = name.len();

    // let name_len: usize = name.this().len();
    let this_name: &&Field<MyString> = name.this();
    output!(this_name.len());
    // let name_len: usize = name.this_field().len();

    // let group = &entity.group;
    // let group = group;
    // let g1: &u32 = &group.g1;

    // let _: &MyString = entity.name.this(); // panic
    // let _: usize = entity.name.len(); // panic
    // let _: usize = entity.name.inner.len(); // panic
    // let _: &u32 = &entity.group.g1; // panic

    // let _: &MyString = &(&entity.name); // panic
    // let _: u32 = *(&entity.group).g1; // panic
    //
    //
    // let _: &u32 = &super::identity(&entity.group).this().g1; // panic
    // let _: &String = &entity.get_group().g2; // panic
    // let _: &String = &entity.get_group().get_g2(); // panic

    // entity.set_name("name1".to_string()); // ok
    // entity.set_g1(1); // ok
    // entity.set_name_if_empty("name2".to_string()); // panic
}
