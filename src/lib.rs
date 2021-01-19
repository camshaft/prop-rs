//! Provides generic ways to get and set values on an object
//!
//! ## Example
//!
//! ```rust
//! use prop::{props, prop, Prop};
//!
//! #[derive(Prop)]
//! struct Cheese {
//!     #[prop(get, set = "Self::set_age")]
//!     age: u32,
//! }
//!
//! impl Cheese {
//!     fn set_age(&mut self, age: u32) {
//!         if age > 15 {
//!             panic!("Don't eat it!");
//!         }
//!
//!         self.age = age;
//!     }
//! }
//!
//! #[derive(Prop)]
//! struct Wine {
//!     // public fields are automatically derived
//!     pub age: u32,
//! }
//!
//! #[props]
//! fn replace_age<T: Prop<u32, "age">>(object: &mut T, new_age: u32) -> u32 {
//!     let prev = *prop!(object.age);
//!     prop!(object.age = new_age);
//!     prev
//! }
//!
//! # fn main() {
//! let mut wensleydale = Cheese {
//!     age: 2,
//! };
//! let mut cabernet = Wine {
//!     age: 16,
//! };
//!
//! assert_eq!(replace_age(&mut wensleydale, 4), 2);
//! assert_eq!(*prop!(wensleydale.age), 4);
//!
//! assert_eq!(replace_age(&mut cabernet, 8), 16);
//! assert_eq!(*prop!(cabernet.age), 8);
//! # }
//! ```

pub use prop_macros::*;

/// An identifier for a particular prop
///
/// Using this type alias will ensure that when this library switches to use `&'static str`,
/// consumers will continue to work.
pub type Id = u128;

/// Convenience methods for types that implement [`Get`], [`GetMut`], and [`Set`].
pub trait Object {
    /// Sets the field with the given value
    fn set<Value, const ID: Id>(&mut self, value: Value) -> &mut Self
    where
        Self: Set<Value, ID>,
    {
        <Self as Set<Value, ID>>::set_value(self, value);
        self
    }

    /// Gets the field's value
    fn get<Value, const ID: Id>(&self) -> &Value
    where
        Self: Get<Value, ID>,
    {
        <Self as Get<Value, ID>>::get_value(self)
    }

    /// Gets a mutable reference for the field's value
    fn get_mut<Value, const ID: Id>(&mut self) -> &mut Value
    where
        Self: GetMut<Value, ID>,
    {
        <Self as GetMut<Value, ID>>::get_value_mut(self)
    }
}

/// Automatically implement the convenience for all types
impl<T> Object for T {}

/// Sets the field with the given value
pub trait Set<Value, const ID: Id> {
    fn set_value(&mut self, value: Value);
}

/// Tries to set the field with the given value
pub trait TrySet<Value, Error, const ID: Id> {
    fn try_set_value(&mut self, value: Value) -> Result<(), Error>;
}

/// Gets the field's value
pub trait Get<Value, const ID: Id> {
    fn get_value(&self) -> &Value;
}

/// Tries to get the field's value
pub trait TryGet<Value, Error, const ID: Id> {
    fn try_get_value(&self) -> Result<&Value, Error>;
}

/// Gets a mutable reference for the field's value
pub trait GetMut<Value, const ID: Id> {
    fn get_value_mut(&mut self) -> &mut Value;
}

/// Automatically deref `&mut` to `&`
impl<T: Get<Value, ID>, Value, const ID: Id> Get<Value, ID> for &mut T {
    fn get_value(&self) -> &Value {
        <T as Get<Value, ID>>::get_value(self)
    }
}

/// Convenience trait for types that implement both [`Set`] and [`Get`]
pub trait Prop<Value, const ID: Id>: Set<Value, ID> + Get<Value, ID> {}

/// Automatically implement `Prop` for all types that implement [`Set`] and [`Get`]
impl<T, Value, const ID: Id> Prop<Value, ID> for T where T: Set<Value, ID> + Get<Value, ID> {}

/// Convenience trait for types that implement both [`TrySet`] and [`TryGet`]
pub trait TryProp<Value, Error, const ID: Id>:
    TrySet<Value, Error, ID> + TryGet<Value, Error, ID>
{
}

/// Automatically implement `TryProp` for all types that implement [`TrySet`] and [`TryGet`]
impl<T, Value, Error, const ID: Id> TryProp<Value, Error, ID> for T where
    T: TrySet<Value, Error, ID> + TryGet<Value, Error, ID>
{
}

/// Gets or sets a value on a generic object
///
/// ## Examples
///
/// ```rust
/// use prop::{props, prop, Get};
///
/// struct Person {
///    age: u32,
/// }
///
/// #[props]
/// impl Get<u32, "age"> for Person {
///    fn get_value(&self) -> &u32 {
///        &self.age
///    }
/// }
///
/// # fn main() {
/// let mut person = Person {
///    age: 32,
/// };
///
/// assert_eq!(*prop!(person.age), 32);
/// # }
/// ```
#[macro_export]
macro_rules! prop {
    ($receiver:ident.$name:ident) => {{
        use $crate::{id, Object};
        $receiver.get::<_, { id!($name) }>()
    }};
    (($receiver:expr).$name:ident) => {{
        use $crate::{id, Object};
        $receiver.get::<_, { id!($name) }>()
    }};
    ($receiver:ident.$name:ident = $value:expr) => {{
        use $crate::{id, Object};
        $receiver.set::<_, { id!($name) }>($value)
    }};
    (($receiver:expr).$name:ident = $value:expr) => {{
        use $crate::{id, Object};
        $receiver.set::<_, { id!($name) }>($value)
    }};
}

#[test]
fn get_set() {
    struct Thing(u32);

    #[props]
    impl Set<u32, "foo"> for Thing {
        fn set_value(&mut self, value: u32) {
            self.0 = value;
        }
    }

    #[props]
    impl Get<u32, "foo"> for Thing {
        fn get_value(&self) -> &u32 {
            &self.0
        }
    }

    #[props]
    fn generic_method<T: Prop<u32, "foo">>(thing: &mut T) -> u32 {
        prop!(thing.foo = 123);
        *prop!(thing.foo)
    }

    let mut t = Thing(0);

    generic_method(&mut t);

    prop!(t.foo = 123);
    let v = *prop!(t.foo);
    assert_eq!(v, 123u32);
}
