use std::io::IsTerminal;

use anyhow::{Context, Result};
use bagua::InitFunction;
use serde::Deserialize;
use tracing_subscriber::{
    fmt::{self, format::Writer, time::FormatTime},
    prelude::__tracing_subscriber_SubscriberExt,
    EnvFilter, Layer,
};

fn yes() -> bool {
    true
}

fn no() -> bool {
    false
}

#[derive(Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct TracingCfg {
    pub level: String,
    #[serde(default)]
    pub directives: Vec<String>,

    #[serde(default = "yes")]
    pub with_target: bool,
    #[serde(default = "no")]
    pub with_file: bool,
    #[serde(default = "yes")]
    pub with_color: bool,
    #[serde(default = "no")]
    pub with_line_number: bool,
}

struct LocalTimer;
impl FormatTime for LocalTimer {
    fn format_time(&self, w: &mut Writer<'_>) -> std::fmt::Result {
        write!(
            w,
            "{}",
            chrono::Local::now().format("%Y-%m-%dT%H:%M:%S%.3f")
        )
    }
}

static ADDITION_DIRECTIVE: &[&str] = &["hyper=warn", "actix_server::worker=warn"];

pub fn init(config: &TracingCfg) -> Result<()> {
    let std_out = {
        let mut filter = EnvFilter::from_default_env().add_directive(config.level.parse()?);
        for d in ADDITION_DIRECTIVE {
            filter = filter.add_directive(d.parse().unwrap());
        }
        for d in &config.directives {
            let d = d
                .parse()
                .with_context(|| format!("failed to parse directive: {}", d))?;
            filter = filter.add_directive(d);
        }
        let is_tty = std::io::stdout().is_terminal();
        let use_ansi = is_tty && config.with_color;
        fmt::Layer::new()
            .with_timer(LocalTimer)
            .with_ansi(use_ansi)
            .with_target(config.with_target)
            .with_file(config.with_file)
            .with_line_number(config.with_line_number)
            .with_writer(std::io::stdout)
            .with_filter(filter)
    };

    let collector_std = tracing_subscriber::registry().with(std_out);
    tracing::subscriber::set_global_default(collector_std).expect("failed to init logger");
    Ok(())
}

#[InitFunction(priority = 0)]
pub async fn init_fn(settings: &crate::settings::Settings) -> anyhow::Result<()> {
    init(&settings.tracing)
        .with_context(|| format!("Failed to init logger. Config = {:#?}", settings.tracing))?;

    Ok(())
}
