use std::{path::Path, sync::OnceLock};

use anyhow::Context;
use config::Config;
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Settings {
    pub common: common::settings::Settings,
    pub bc_user: bc_user::settings::Settings,
    pub http: crate::http::HttpCfg,
}

static SETTINGS: OnceLock<Settings> = OnceLock::new();

impl Settings {
    pub fn init(this: Self) {
        SETTINGS.get_or_init(|| this);
    }

    pub fn get() -> &'static Settings {
        SETTINGS
            .get()
            .expect("you must call `Settings::init()` before using the settings")
    }
}

pub struct ConfigResource<'a> {
    path: &'a Path,
    required: bool,
}

pub fn load_settings(resources: Vec<ConfigResource>) -> anyhow::Result<&'static Settings> {
    if let Some(settings) = SETTINGS.get() {
        return Ok(settings);
    }

    let mut builder = Config::builder();
    for resource in resources {
        if !resource.path.exists() {
            println!("Config file not found: {:?}", resource.path);
        }
        builder = builder.add_source(config::File::from(resource.path).required(resource.required));
    }

    let mut config: Settings = builder
        .build()
        .context("error building configuration")?
        .try_deserialize()
        .context("error deserializing configuration")?;
    config.bc_user.db_url = config.common.postgres.url.clone();

    Settings::init(From::from(config));
    Ok(Settings::get())
}

pub fn load_from_convention_files() -> anyhow::Result<&'static Settings> {
    let mut resources: Vec<ConfigResource> = vec![];

    resources.push(ConfigResource {
        path: Path::new("configs/default.toml"),
        required: false,
    });

    if let Ok(env) = std::env::var("RUN_ENV") {
        match env.as_str() {
            "beta" => {
                resources.push(ConfigResource {
                    path: Path::new("configs/beta.toml"),
                    required: false,
                });
            }
            "prod" => {
                resources.push(ConfigResource {
                    path: Path::new("configs/prod.toml"),
                    required: false,
                });
            }

            _ => {
                anyhow::bail!("unknown RUN_ENV: {}. expect `beta` | `prod`", env);
            }
        }
    }

    load_settings(resources)
}
