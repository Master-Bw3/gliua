mod gliua_value;
mod instruction;
mod uiua_env;

use instruction::Op;
use rustler::{Env, Term};

use crate::{gliua_value::{ValueRef}, uiua_env::{ExUiua, UiuaRef}};

#[rustler::nif(schedule = "DirtyCpu")]
fn evaluate(instruction_stack: Vec<Op>) -> Result<ExUiua, String> {
    let mut runtime = uiua::Uiua::with_safe_sys();

    for instruction in instruction_stack.into_iter().rev() {
        instruction
            .apply(&mut runtime)
            .map_err(|err| err.to_string())?
    }

    Ok(ExUiua::new(runtime))
}

fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource!(ValueRef, env);
    rustler::resource!(UiuaRef, env);
    true
}

rustler::init!(
    "gliua_rs",
    [
        evaluate,
        uiua_env::new_runtime,
        gliua_value::to_string,
    ],
    load = on_load
);
