use bagua::Provider;

use crate::application::traits::EmailService;

#[derive(Provider)]
pub struct EmailServiceImpl {}

impl EmailService for EmailServiceImpl {
    async fn send_email_code(
        &mut self,
        email: &crate::domain::user::Email,
    ) -> anyhow::Result<String> {
        println!("send email code to {}", email.as_str());

        Ok("123456".to_string())
    }

    async fn verify_email_code(
        &mut self,
        email: &crate::domain::user::Email,
        code: &str,
    ) -> anyhow::Result<bool> {
        println!("verify email code {} for {}", code, email.as_str());

        Ok(true)
    }
}
