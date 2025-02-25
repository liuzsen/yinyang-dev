use bagua::Provider;

pub mod dao;

#[allow(dead_code)]
#[derive(Provider)]
pub struct BasicRepo<T> {
    adapter: T,
}
