#![feature(specialization)]

use std::fmt::Debug;

pub trait Loggable {
    fn autolog_fmt(&self) -> String;
}

impl<T> Loggable for T {
    default fn autolog_fmt(&self) -> String {
        "<..>".into()
    }
}

impl<T: Debug> Loggable for T {
    fn autolog_fmt(&self) -> String {
        format!("{:?}", self)
    }
}
