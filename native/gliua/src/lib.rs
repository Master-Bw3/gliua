mod instruction;
use instruction::Op;
use rustler::{Decoder, Encoder, Env, Term};
use uiua::{Uiua, Value};

use crate::instruction::Instruction;

#[rustler::nif]
fn evaluate(env: Env, instruction_stack: Vec<Instruction>) -> Vec<Term> {
    let mut runtime = uiua::Uiua::with_safe_sys();

    instruction_stack
        .iter()
        .rev()
        .for_each(|intstruction| intstruction.apply(&mut runtime));

    runtime
        .take_stack()
        .iter()
        .map(|value| uiua_value_to_elixir_term(env.clone(), &runtime, value))
        .collect()
}

fn uiua_value_to_elixir_term<'a>(elixir_env: Env<'a>, uiua_env: &Uiua, value: &Value) -> Term<'a> {
    match value {
        Value::Byte(_) => Value::as_bytes(value, uiua_env, "")
            .unwrap()
            .encode(elixir_env),

        Value::Num(_) => Value::as_num(value, uiua_env, "")
            .map(|x| x.encode(elixir_env))
            .or_else(|_| Value::as_nums(value, uiua_env, "").map(|x| x.encode(elixir_env)))
            .unwrap(),

        Value::Complex(_) => todo!(),
        Value::Char(_) => todo!(),
        Value::Box(_) => todo!(),
    }
}

rustler::init!("gliua_rs", [evaluate]);
