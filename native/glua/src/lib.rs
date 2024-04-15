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

fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource!(UiuaRef, env);
    true
}

#[rustler::nif(schedule = "DirtyCpu")]
fn init_runtime() -> Result<ExUiua, ()> {
    let runtime = Uiua::with_safe_sys();
    Ok(ExUiua::new(runtime))
}

rustler::init!(
    "Elixir.Glua",
    [init_runtime],
    load = on_load
);