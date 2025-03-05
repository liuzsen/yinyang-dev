use std::collections::HashSet;

use bagua::{flake_id, Entity, FieldGroup};
use serde::{Deserialize, Serialize};

flake_id!(AAId, @serde);
flake_id!(BBId, @serde);

#[Entity]
#[subset(AAWithOnlyId{id})]
pub struct AA {
    id: AAId,
    name: MyString,

    #[entity(foreign)]
    foreigns: HashSet<BBId>,

    #[entity(group)]
    group: AAGroup,
}

#[derive(Serialize, Deserialize)]
pub struct MyString {
    pub inner: String,
}

#[FieldGroup]
pub struct AAGroup {
    g1: u32,
    g2: String,
}

impl AA {
    pub fn set_name(&mut self, name: String) {
        self.name.set(MyString { inner: name });
    }

    pub fn set_name_if_empty(&mut self, name: String) {
        if self.name.is_empty() {
            self.name.set(MyString { inner: name });
        }
    }

    pub fn set_g1(&mut self, g1: u32) {
        self.group.set_g1(g1);
    }

    pub fn get_group(&self) -> &AAGroup {
        &self.group
    }
}

impl AAGroup {
    pub fn set_g1(&mut self, g1: u32) {
        self.g1.set(g1);
    }

    pub fn get_g2(&self) -> &String {
        &self.g2
    }
}

mod aa {
    // Recursive expansion of flake_id! macro
    // =======================================

    impl AAId {
        pub fn generate() -> AAId {
            use ::std::sync::{Mutex, OnceLock};
            use bagua::flaken::Flaken;
            static FLAKE_ID_GENERATOR: OnceLock<Mutex<Flaken>> = OnceLock::new();
            let f = FLAKE_ID_GENERATOR.get_or_init(|| {
                let f = bagua::flaken::Flaken::default();
                Mutex::new(f)
            });
            let mut lock = f.lock().unwrap();
            AAId(lock.next() as i64)
        }
    }
    impl ::serde::Serialize for AAId {
        fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error>
        where
            S: ::serde::Serializer,
        {
            serializer.serialize_str(&self.0.to_string())
        }
    }
    impl<'de> ::serde::Deserialize<'de> for AAId {
        fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error>
        where
            D: ::serde::Deserializer<'de>,
        {
            let id = String::deserialize(deserializer)?;
            let id = id.parse().map_err(serde::de::Error::custom)?;
            Ok(Self(id))
        }
    }

    use aaaaa::*;
    mod aaaaa {
        pub use bagua::derive_more;
    }

    #[derive(
        Debug,
        PartialEq,
        PartialOrd,
        Eq,
        Hash,
        Clone,
        Copy,
        bagua::derive_more::From,
        bagua::derive_more::Display,
        bagua::derive_more::FromStr,
    )]
    pub struct AAId(pub i64);

    impl Default for AAId {
        fn default() -> Self {
            Self::generate()
        }
    }
    impl<'a> From<&'a AAId> for i64 {
        fn from(id: &'a AAId) -> Self {
            id.0
        }
    }
    impl<'a> From<&'a AAId> for AAId {
        fn from(id: &'a AAId) -> Self {
            *id
        }
    }
    impl bagua::entity::SysId for AAId {
        fn generate() -> Self {
            Self::generate()
        }
    }
    impl AAId {
        pub fn from_str(s: &str) -> Result<Self, std::num::ParseIntError> {
            Ok(Self(s.parse()?))
        }
    }
}

impl MyString {
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}
