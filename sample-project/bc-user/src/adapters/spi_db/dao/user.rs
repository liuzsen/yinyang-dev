use bagua::{
    db::primitives::{PersistentObject, ToDbPrimitive, ToDomainPremitive, WrapperType},
    diesel_smallint_enum, diesel_sql_type_wrapper, HasChanged,
};
use diesel::prelude::*;

use crate::{
    domain::user::{Email, Password, User, UserId, UserMini, UserReadOnly, UserRole, Username},
    schema::users,
};

diesel_sql_type_wrapper!(
    pub struct Username(pub(crate) String);
);

diesel_sql_type_wrapper!(
    pub struct Password(pub(crate) String);
);

diesel_sql_type_wrapper!(
    pub struct Email(String);
);

diesel_smallint_enum! {
    pub enum UserRole {
        Admin = 1,
        User = 2,
    }
}

impl ToDbPrimitive<'_, UserRole> for UserRole {
    fn to_db_primitive(&'_ self) -> UserRole {
        *self
    }
}

impl WrapperType for Email {
    type InnerType = String;

    fn get_inner(&self) -> &Self::InnerType {
        self.inner()
    }

    fn from_inner(s: Self::InnerType) -> Result<Self, String> {
        Email::try_from(s).map_err(|err| format!("invalid Email format: {:?}", err))
    }
}

impl WrapperType for Username {
    type InnerType = String;

    fn get_inner(&self) -> &Self::InnerType {
        self.inner()
    }

    fn from_inner(s: Self::InnerType) -> Result<Self, String> {
        Username::try_from(s).map_err(|err| format!("invalid Username format: {:?}", err))
    }
}

impl WrapperType for Password {
    type InnerType = String;

    fn get_inner(&self) -> &Self::InnerType {
        self.inner()
    }

    fn from_inner(s: Self::InnerType) -> Result<Self, String> {
        Password::try_from(s).map_err(|err| format!("invalid Password format: {:?}", err))
    }
}

pub struct InsertMirrorUser<'a> {
    pub main: InsertPoUser<'a>,
}

#[derive(Insertable)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(table_name = users)]
pub struct InsertPoUser<'a> {
    pub id: &'a UserId,
    pub email: &'a Email,
    pub name: &'a Username,
    pub password: &'a Password,
    pub role: UserRole,
}

pub struct UpdateMirrorUser<'a> {
    pub main: UpdatePoUser<'a>,
}

#[derive(AsChangeset, HasChanged)]
#[diesel(table_name = users)]
pub struct UpdatePoUser<'a> {
    pub id: &'a UserId,
    pub email: Option<&'a Email>,
    pub name: Option<&'a Username>,
    pub password: Option<&'a Password>,
    pub role: Option<UserRole>,
}

impl<'a> PersistentObject<'a, User> for InsertMirrorUser<'a> {
    fn from_domain_object(do_obj: &'a User) -> Self {
        let UserReadOnly {
            id,
            email,
            name,
            password,
            role,
        } = do_obj.read_only();

        Self {
            main: InsertPoUser {
                id,
                email: ToDbPrimitive::to_db_primitive(email.value_ref()),
                name: ToDbPrimitive::to_db_primitive(name.value_ref()),
                password: ToDbPrimitive::to_db_primitive(password.value_ref()),
                role: ToDbPrimitive::to_db_primitive(role.value_ref()),
            },
        }
    }
}

impl<'a> PersistentObject<'a, User> for UpdateMirrorUser<'a> {
    fn from_domain_object(do_obj: &'a User) -> Self {
        let UserReadOnly {
            id,
            email,
            name,
            password,
            role,
        } = do_obj.read_only();

        Self {
            main: UpdatePoUser {
                id,
                email: email
                    .changed_ref()
                    .map(|v| ToDbPrimitive::to_db_primitive(v)),
                name: name
                    .changed_ref()
                    .map(|v| ToDbPrimitive::to_db_primitive(v)),
                password: password
                    .changed_ref()
                    .map(|v| ToDbPrimitive::to_db_primitive(v)),
                role: role
                    .changed_ref()
                    .map(|v| ToDbPrimitive::to_db_primitive(v)),
            },
        }
    }
}

#[derive(Queryable, Selectable)]
#[diesel(table_name = users)]
pub struct UserMiniPo {
    pub id: UserId,
}

impl From<UserMiniPo> for UserMini {
    fn from(po: UserMiniPo) -> Self {
        UserMini {
            id: po.id.to_domain_primitive(),
        }
    }
}
