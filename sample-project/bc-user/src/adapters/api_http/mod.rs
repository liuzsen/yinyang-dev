use bagua::{http::biz_err::BizError, BizErrorEnum};

use crate::domain::user::{EmailError, PasswordError, UserId, UserNameError};

pub mod user;

pub struct UserCredential {
    pub id: UserId,
}

#[BizErrorEnum]
#[base_biz_code = 1000]
pub enum BizErrorKind {
    UserNotFound,
    UserAlreadyExists,
    UserNameTooShort,
    UserNameTooLong,
    UserNameInvalidCharacter,
    UserPasswordTooShort,
    UserPasswordTooLong,
    UserPasswordInvalidCharacter,
    UserEmailInvalidFormat,
}

impl From<UserNameError> for BizError {
    fn from(value: UserNameError) -> Self {
        match value {
            UserNameError::TooShort => BizErrorKind::UserNameTooShort,
            UserNameError::TooLong => BizErrorKind::UserNameTooLong,
            UserNameError::InvalidCharacter => BizErrorKind::UserNameInvalidCharacter,
        }
    }
}

impl From<PasswordError> for BizError {
    fn from(value: PasswordError) -> Self {
        match value {
            PasswordError::TooShort => BizErrorKind::UserPasswordTooShort,
            PasswordError::TooLong => BizErrorKind::UserPasswordTooLong,
            PasswordError::InvalidCharacter => BizErrorKind::UserPasswordInvalidCharacter,
        }
    }
}

impl From<EmailError> for BizError {
    fn from(value: EmailError) -> Self {
        match value {
            EmailError::InvalidFormat => BizErrorKind::UserEmailInvalidFormat,
        }
    }
}
