use rustler::{Env, NifStruct, ResourceArc, Term};
use uiua::Uiua;
use std::ops::Deref;

pub struct UiuaRef(pub Uiua);

#[derive(NifStruct)]
#[module = "Uiua.Runtime"]
pub struct ExUiua {
    pub resource: ResourceArc<UiuaRef>,
}

impl ExUiua {
    pub fn new(runtime: Uiua) -> Self {
        Self {
            resource: ResourceArc::new(UiuaRef::new(runtime)),
        }
    }
}

impl UiuaRef {
    pub fn new(runtime: Uiua) -> Self {
        Self(runtime)
    }
}

impl Deref for ExUiua {
    type Target = Uiua;

    fn deref(&self) -> &Self::Target {
        &self.resource.0
    }
}

unsafe impl Send for UiuaRef {}
unsafe impl Sync for UiuaRef {}

#[rustler::nif(schedule = "DirtyCpu")]
pub fn new_runtime() -> ExUiua {
    ExUiua::new(Uiua::with_safe_sys())
}