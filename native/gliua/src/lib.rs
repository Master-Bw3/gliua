mod gliua_value;
mod instruction;
mod uiua_env;

use instruction::Op;
use rustler::{Env, Term};

use crate::{gliua_value::{ExValue, ValueRef}, uiua_env::UiuaRef};

#[rustler::nif]
fn evaluate(instruction_stack: Vec<Op>) -> Result<Vec<ExValue>, String> {
    let mut runtime = uiua::Uiua::with_safe_sys();

    for instruction in instruction_stack.into_iter().rev() {
        instruction
            .apply(&mut runtime)
            .map_err(|err| err.to_string())?
    }

    Ok(runtime
        .take_stack()
        .into_iter()
        .map(|value| ExValue::new(value))
        .collect())
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
