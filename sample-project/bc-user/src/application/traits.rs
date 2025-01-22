use anyhow::Result;

use crate::domain::user::Email;

pub trait EmailService {
    async fn send_email_code(&mut self, email: &Email) -> Result<String>;

    async fn verify_email_code(&mut self, email: &Email, code: &str) -> Result<bool>;
}
