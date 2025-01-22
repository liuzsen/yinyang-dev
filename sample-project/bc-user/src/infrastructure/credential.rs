use actix_identity::Identity;
use actix_web::FromRequest;
use bagua::http::HttpCredential;

use crate::{adapters::api_http::UserCredential, domain::user::UserId};

pub struct CredentialExtractor {
    id: UserId,
}

impl FromRequest for CredentialExtractor {
    type Error = actix_web::Error;

    type Future = std::future::Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        payload: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let id = Identity::from_request(req, payload).into_inner();
        let id = match id {
            Ok(id) => id,
            Err(err) => return std::future::ready(Err(err)),
        };

        let id = id.id();
        let id = match id {
            Ok(id) => id,
            Err(err) => return std::future::ready(Err(err.into())),
        };
        let id = match UserId::from_str(&id) {
            Ok(id) => id,
            Err(err) => {
                return std::future::ready(Err(actix_web::error::InternalError::new(
                    err,
                    actix_web::http::StatusCode::INTERNAL_SERVER_ERROR,
                )
                .into()))
            }
        };

        std::future::ready(Ok(CredentialExtractor { id }))
    }
}

impl HttpCredential<UserCredential> for CredentialExtractor {
    fn credential(&self) -> UserCredential {
        UserCredential { id: self.id }
    }
}
