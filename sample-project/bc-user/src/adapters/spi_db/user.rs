use bagua::{
    db::{diesel::SqlErrorDiesel, primitives::PersistentObject},
    repository::{DeleteEffect, Repository, SaveEffect, SubsetLoader, UpdateEffect},
};
use diesel::{ExpressionMethods, QueryDsl, SelectableHelper};

use crate::{
    domain::user::{User, UserIdent, UserMini},
    schema::users,
};

use super::{
    dao::user::{InsertMirrorUser, UpdateMirrorUser, UserMiniPo},
    BasicRepo, PgSqlRunner,
};

impl<R: PgSqlRunner> Repository<User> for BasicRepo<R> {
    async fn save(&mut self, entity: &User) -> anyhow::Result<bagua::repository::SaveEffect> {
        let InsertMirrorUser { main } = PersistentObject::from_domain_object(entity);
        let sql = diesel::insert_into(users::table)
            .values(main)
            .on_conflict_do_nothing();
        let effect: usize = self.adapter.sql_execute(sql).await?;

        if effect == 1 {
            Ok(SaveEffect::Ok)
        } else {
            Ok(SaveEffect::Conflict)
        }
    }

    async fn update(&mut self, entity: &User) -> anyhow::Result<bagua::repository::UpdateEffect> {
        let UpdateMirrorUser { main } = PersistentObject::from_domain_object(entity);
        if main.has_changed() {
            let sql = diesel::update(users::table).set(main);
            if let Err(err) = self.adapter.sql_execute(sql).await {
                let err: SqlErrorDiesel = err;
                if err.is_conflict() {
                    return Ok(UpdateEffect::Conflict);
                } else {
                    return Err(err.into());
                }
            }
        }

        Ok(UpdateEffect::Ok)
    }

    async fn delete<I>(&mut self, id: I) -> anyhow::Result<bagua::repository::DeleteEffect>
    where
        for<'a> <User as bagua::entity::Entity>::Id<'a>: From<I>,
    {
        let id = <User as bagua::entity::Entity>::Id::from(id);
        let effect: usize = match id {
            crate::domain::user::UserIdent::SysId(cow) => {
                let sql = diesel::delete(users::table).filter(users::id.eq(&*cow));
                self.adapter.sql_execute(sql).await?
            }
            crate::domain::user::UserIdent::Email(cow) => {
                let sql = diesel::delete(users::table).filter(users::email.eq(cow.as_str()));
                self.adapter.sql_execute(sql).await?
            }
        };

        if effect == 1 {
            Ok(DeleteEffect::Ok)
        } else {
            Ok(DeleteEffect::NotFound)
        }
    }

    async fn exists<I>(&mut self, id: I) -> anyhow::Result<bool>
    where
        for<'a> <User as bagua::entity::Entity>::Id<'a>: From<I>,
    {
        let id = <User as bagua::entity::Entity>::Id::from(id);
        match id {
            crate::domain::user::UserIdent::SysId(cow) => {
                let predicate = users::table.filter(users::id.eq(&*cow));
                Ok(self.adapter.sql_exists(predicate).await?)
            }
            crate::domain::user::UserIdent::Email(cow) => {
                let predicate = users::table.filter(users::email.eq(cow.as_str()));
                Ok(self.adapter.sql_exists(predicate).await?)
            }
        }
    }
}

impl<R: PgSqlRunner> SubsetLoader<UserMini> for BasicRepo<R> {
    async fn load<I>(&mut self, id: I) -> anyhow::Result<Option<UserMini>>
    where
        for<'a> <<UserMini as bagua::entity::subset::Subset>::Entity as bagua::entity::Entity>::Id<'a>:
            From<I>,
    {
        let id = UserIdent::from(id);
        let po: Option<UserMiniPo> = match id {
            UserIdent::SysId(cow) => {
                let sql = users::table
                    .select(UserMiniPo::as_select())
                    .filter(users::id.eq(&cow))
                    .for_update();

                self.adapter.sql_result(sql).await?
            }
            UserIdent::Email(cow) => {
                let sql = users::table
                    .filter(users::email.eq(cow.as_str()))
                    .select(UserMiniPo::as_select())
                    .for_update();
                self.adapter.sql_result(sql).await?
            }
        };

        Ok(po.map(From::from))
    }
}
