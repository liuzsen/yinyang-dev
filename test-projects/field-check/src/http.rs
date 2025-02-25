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
    actix_route!(
        router = cfg;
        {
            "/ping" GET => ping,
        }

        "/api/health" {
            "/ping" GET => ping,
        }
    );
}

async fn ping() -> &'static str {
    "pong"
}
