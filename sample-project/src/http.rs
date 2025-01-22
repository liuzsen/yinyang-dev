use actix_web::{web, HttpServer};
use bagua::actix_route;
use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct HttpCfg {
    pub host: String,
    pub port: u16,
}

pub async fn build_server(cfg: &HttpCfg) -> anyhow::Result<actix_web::dev::Server> {
    let settings = cfg.clone();
    let server = HttpServer::new(move || actix_web::App::new().configure(route));

    let server = server.bind(format!("{}:{}", settings.host, settings.port))?;
    let server = server.run();

    Ok(server)
}

pub fn route(cfg: &mut web::ServiceConfig) {
    use bc_user::infrastructure::router as user_api;

    actix_route!(
        router = cfg;
        {
            "/ping" GET => ping,
        }

        "/api/user" {
            "/register" POST => user_api::user::create,
        }
    );
}

async fn ping() -> &'static str {
    "pong"
}
