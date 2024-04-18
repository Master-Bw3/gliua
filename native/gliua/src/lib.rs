mod gliua_value;
mod instruction;

use instruction::Op;
use rustler::{Env, Term};

use crate::gliua_value::{ExValue, ValueRef};

#[rustler::nif]
fn evaluate(instruction_stack: Vec<Op>) -> Result<Vec<ExValue>, String> {
    let mut runtime = uiua::Uiua::with_safe_sys();

    for instruction in instruction_stack.iter().rev() {
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
    true
}

rustler::init!(
    "gliua_rs",
    [
        evaluate,
        gliua_value::to_string,
        gliua_value::join,
        gliua_value::couple,
        gliua_value::uncouple
    ],
    load = on_load
);
