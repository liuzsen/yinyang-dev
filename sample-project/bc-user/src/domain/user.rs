use argon2::password_hash::{
    rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString,
};
use argon2::Argon2;
use bagua::{flake_id, Entity};
use email_address::EmailAddress;

flake_id!(UserId, @diesel-pg, @serde);

#[Entity]
#[subset(UserWithPassword {password})]
pub struct User {
    id: UserId,

    #[entity(biz_id, no_update)]
    email: Email,

    name: Username,

    #[entity(no_update)]
    password: Password,

    #[entity(no_update)]
    role: UserRole,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum UserRole {
    Admin,
    User,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Email(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Username(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Password(String);

#[derive(Debug)]
pub enum UserNameError {
    TooShort,
    TooLong,
    InvalidCharacter,
}

#[derive(Debug)]
pub enum PasswordError {
    TooShort,
    TooLong,
    InvalidCharacter,
}

#[derive(Debug)]
pub enum EmailError {
    InvalidFormat,
}

impl TryFrom<String> for Email {
    type Error = EmailError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if EmailAddress::is_valid(value.as_str()) {
            Ok(Email(value))
        } else {
            Err(EmailError::InvalidFormat)
        }
    }
}

impl TryFrom<String> for Username {
    type Error = UserNameError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.len() < 2 {
            Err(UserNameError::TooShort)
        } else if value.len() > 32 {
            Err(UserNameError::TooLong)
        } else {
            Ok(Username(value))
        }
    }
}

impl TryFrom<String> for Password {
    type Error = PasswordError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let argon2 = Self::argon2();
        let salt = SaltString::generate(&mut OsRng);
        let Ok(password_hash) = argon2.hash_password(value.as_bytes(), &salt) else {
            return Err(PasswordError::InvalidCharacter);
        };
        Ok(Password(password_hash.to_string()))
    }
}

impl Password {
    fn argon2() -> Argon2<'static> {
        Argon2::default()
    }

    pub fn verify(&self, password: &str) -> bool {
        let argon2 = Self::argon2();
        let parsed_hash = PasswordHash::new(&self.0).unwrap();
        argon2
            .verify_password(password.as_bytes(), &parsed_hash)
            .is_ok()
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn inner(&self) -> &String {
        &self.0
    }
}

impl Email {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn inner(&self) -> &String {
        &self.0
    }
}

impl Username {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn inner(&self) -> &String {
        &self.0
    }
}

pub enum UserError {
    OldPasswordNotMatch,
    OldPasswordEqualNewPassword,
}

impl User {
    pub fn update_password(&mut self, old: &Password, new: Password) -> Result<(), UserError> {
        if self.password != old {
            return Err(UserError::OldPasswordNotMatch);
        }
        if self.password == new {
            return Err(UserError::OldPasswordEqualNewPassword);
        }

        self.password.set(new);

        Ok(())
    }
}
