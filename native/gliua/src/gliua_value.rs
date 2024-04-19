use std::ops::Deref;

use rustler::{Env, NifMap, NifStruct, NifTaggedEnum, ResourceArc, Term};
use uiua::{Uiua, UiuaError, Value};

use crate::uiua_env::ExUiua;

pub(crate) struct ValueRef(pub Value);


#[derive(NifStruct)]
#[module = "uiua_value"]
pub(crate) struct ExValue {
    pub resource: ResourceArc<ValueRef>,
}

impl ExValue {
    pub fn new(value: Value) -> Self {
        Self {
            resource: ResourceArc::new(ValueRef::new(value)),
        }
    }
}

impl ValueRef {
    pub fn new(value: Value) -> Self {
        Self(value)
    }
}

impl Deref for ExValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.resource.0
    }
}

unsafe impl Send for ValueRef {}
unsafe impl Sync for ValueRef {}

#[rustler::nif(schedule = "DirtyCpu")]
pub(crate) fn to_string(value: ExValue) -> String {
    value.to_string()
}