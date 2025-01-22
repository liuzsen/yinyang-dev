pub mod create {
    use crate::adapters::api_http::BizErrorKind;
    use crate::domain::user::{Email, Password, Username};
    use bagua::http::HttpAdapter;

    use bagua::Provider;
    use bagua::{
        http::{biz_err::BizError, HttpJsonBody},
        usecase::UseCase,
    };
    use serde::Deserialize;

    pub type UCInput = crate::application::user::create::Input;
    pub type UCOutput = crate::application::user::create::Output;
    pub type UCError = crate::application::user::create::Error;

    #[derive(Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Request {
        pub email: String,
        pub email_code: String,
        pub name: String,
        pub password: String,
    }

    pub type Response = UCOutput;

    #[derive(Default, Provider)]
    pub struct AdapterImpl {}

    impl<R, U> HttpAdapter<R, U> for AdapterImpl
    where
        U: UseCase<Input = UCInput, Output = UCOutput, Error = UCError>,
        R: HttpJsonBody<Request>,
    {
        type RequestBody = Request;

        type ResponseBody = Response;

        fn convert_request(
            &self,
            req: R,
        ) -> Result<<U as bagua::usecase::UseCase>::Input, BizError> {
            let req = req.get_body();
            let Request {
                email,
                email_code,
                name,
                password,
            } = req;
            let input = crate::application::user::create::Input {
                email: Email::try_from(email)?,
                email_code,
                name: Username::try_from(name)?,
                password: Password::try_from(password)?,
            };
            Ok(input)
        }

        fn convert_response(
            &self,
            output: <U as bagua::usecase::UseCase>::Output,
        ) -> Self::ResponseBody {
            output
        }

        fn convert_error(&self, err: <U as bagua::usecase::UseCase>::Error) -> BizError {
            match err {
                crate::application::user::create::Error::UserAlreadyExists => {
                    BizErrorKind::UserAlreadyExists
                }
                crate::application::user::create::Error::EmailCodeInvalid => {
                    BizErrorKind::UserEmailInvalidFormat
                }
            }
        }
    }
}

pub mod update {
    use bagua::http::{HttpAdapter, HttpCredential};
    use bagua::{
        http::{biz_err::BizError, HttpJsonBody},
        usecase::UseCase,
    };

    use crate::adapters::api_http::{BizErrorKind, UserCredential};
    use crate::domain::user::{UserUpdater, Username};
    pub type UCInput = crate::application::user::update::Input;
    pub type UCOutput = crate::application::user::update::Output;
    pub type UCError = crate::application::user::update::Error;

    pub struct Request {
        name: Option<String>,
    }

    pub type Response = UCOutput;

    #[derive(Default)]
    pub struct AdapterImpl {}
    impl<R, U> HttpAdapter<R, U> for AdapterImpl
    where
        U: UseCase<Input = UCInput, Output = UCOutput, Error = UCError>,
        R: HttpJsonBody<Request>,
        R: HttpCredential<UserCredential>,
    {
        type RequestBody = Request;
        type ResponseBody = Response;

        fn convert_request(
            &self,
            req: R,
        ) -> Result<<U as bagua::usecase::UseCase>::Input, BizError> {
            let credential = req.credential();
            let Request { name } = req.get_body();
            let name = name.map(|name| Username::try_from(name)).transpose()?;
            Ok(UCInput {
                id: credential.id,
                updater: UserUpdater { name },
            })
        }

        fn convert_response(
            &self,
            output: <U as bagua::usecase::UseCase>::Output,
        ) -> Self::ResponseBody {
            output
        }

        fn convert_error(&self, err: <U as bagua::usecase::UseCase>::Error) -> BizError {
            match err {
                crate::application::user::update::Error::UserNotFound => BizErrorKind::UserNotFound,
            }
        }
    }
}
