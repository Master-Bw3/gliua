mod gliua_value;
mod instruction;
mod uiua_env;

use gliua_value::ExValue;
use instruction::Op;
use rustler::{Encoder, Env, Term};

use crate::{
    gliua_value::{DecodeError, ValueRef},
    uiua_env::{ExUiua, UiuaRef},
};

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

#[rustler::nif]
fn stack(runtime: ExUiua) -> Vec<ExValue> {
    runtime
        .stack()
        .iter()
        .map(|value| ExValue::new(value.clone()))
        .collect()
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
        stack,
        uiua_env::new_runtime,
        gliua_value::to_string,
        gliua_value::as_int,
        gliua_value::as_float,
        gliua_value::as_bool,
        gliua_value::as_char,
        gliua_value::as_complex,
    ],
    load = on_load
);
