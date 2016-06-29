//! Defines transfer and propagate traits used for interprocess analysis.

use frontend::containers::{RModule, RFunction};

pub trait Transfer<T: RModule> {
    fn transfer(&mut T, &T::FnRef);
}

pub trait Propagate<T: RModule> {
    fn propagate(&mut T, &T::FnRef);
}
