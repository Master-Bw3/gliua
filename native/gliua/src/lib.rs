mod instruction;

use instruction::Op;
use rustler::{Decoder, Encoder, Env, Term};
use uiua::Uiua;



#[rustler::nif]
fn evaluate(env: Env, instruction_stack: Vec<Op>) -> Result<Vec<Term>, String> {
    let mut runtime = uiua::Uiua::with_safe_sys();


    for instruction in instruction_stack.iter().rev() {
        instruction.apply(&mut runtime).map_err(|err| err.to_string())?
    }

    Ok(runtime
        .take_stack()
        .iter()
        .map(|value| uiua_value_to_erlang_term(env.clone(), &runtime, value))
        .collect())
}

fn uiua_value_to_erlang_term<'a>(
    elixir_env: Env<'a>,
    uiua_env: &Uiua,
    value: &uiua::Value,
) -> Term<'a> {
    match value {
        uiua::Value::Byte(_) => uiua::Value::as_bytes(value, uiua_env, "")
            .unwrap()
            .encode(elixir_env),

        uiua::Value::Num(_) => uiua::Value::as_num(value, uiua_env, "")
            .map(|x| x.encode(elixir_env))
            .or_else(|_| uiua::Value::as_nums(value, uiua_env, "").map(|x| x.encode(elixir_env)))
            .unwrap(),

        uiua::Value::Complex(_) => todo!(),
        uiua::Value::Char(_) => todo!(),
        uiua::Value::Box(_) => todo!(),
    }
}

rustler::init!("gliua_rs", [evaluate]);
