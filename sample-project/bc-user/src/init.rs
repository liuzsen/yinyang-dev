use linkme::distributed_slice;

use crate::settings::Settings;

type BoxedInitFn = for<'a> fn(
    &'a Settings,
) -> std::pin::Pin<
    Box<dyn std::future::Future<Output = anyhow::Result<()>> + 'a>,
>;

#[distributed_slice]
pub static BC_USER_INIT_FUNCTIONS: [InitFunction];

pub async fn run_all(s: &Settings) -> anyhow::Result<()> {
    let mut functions = BC_USER_INIT_FUNCTIONS.iter().collect::<Vec<_>>();
    functions.sort_by_key(|f| f.priority);

    for init_fn in functions {
        init_fn.call(s).await?;
    }
    Ok(())
}

pub struct InitFunction {
    pub function: BoxedInitFn,
    pub priority: u8,
}

impl InitFunction {
    async fn call(&self, s: &Settings) -> anyhow::Result<()> {
        (self.function)(s).await
    }
}

pub struct InitFunctionBuilder {
    function: BoxedInitFn,
    priority: Option<u8>,
}

impl InitFunctionBuilder {
    pub const fn new(function: BoxedInitFn) -> Self {
        Self {
            function,
            priority: None,
        }
    }

    pub const fn priority(mut self, priority: u8) -> Self {
        self.priority = Some(priority);
        self
    }

    pub const fn build(self) -> InitFunction {
        let priority = match self.priority {
            Some(priority) => priority,
            None => u8::MAX / 2,
        };

        InitFunction {
            function: self.function,
            priority,
        }
    }
}
