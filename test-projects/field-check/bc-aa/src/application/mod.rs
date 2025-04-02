pub mod field_access;
// pub mod find;
// pub mod mini;
pub mod traits;

pub use aa::identity;
pub mod aa {

    pub fn identity<T>(t: T) -> T {
        t
    }
}

#[macro_export]
macro_rules! same {
    ($($tt:tt)*) => {
        $($tt)*
    };
}

same! {
trait This: Sized {
    fn this(self) -> Self {
        self
    }

    fn this_ref(&self) -> &Self {
        self
    }
}

impl<T> This for T {}
}

trait ThisField<T> {
    fn this_field(&self) -> &Self;
}

impl<T> ThisField<T> for bagua::entity::field::Field<T> {
    fn this_field(&self) -> &Self {
        self
    }
}
