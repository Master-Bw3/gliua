mod op;
use op::Op;
use rustler::{Encoder, Env, NifMap, NifStruct, NifUnitEnum, ResourceArc, Term};
use uiua::{Primitive, Uiua, Value};

#[derive(NifMap, Clone)]
struct Instruction {
    pub op: Op,
    pub value: Option<i32>,
}

impl Instruction {
    pub(crate) fn apply(&self, uiua: &mut Uiua) {
        match self.op {
            Op::Push => uiua.push(self.value.unwrap()),
            _ => {
                let _ = Primitive::from(self.op.clone()).run(uiua);
            }
        }
    }
}

#[rustler::nif]
fn empty_stack() -> Vec<Instruction> {
    return vec![];
}

#[rustler::nif]
fn push(instruction_stack: Vec<Instruction>, constant: i32) -> Vec<Instruction> {
    let mut new_instruction_stack = instruction_stack.clone();
    new_instruction_stack.push(Instruction {
        op: Op::Push,
        value: Some(constant),
    });
    return new_instruction_stack;
}

#[rustler::nif]
fn add(instruction_stack: Vec<Instruction>) -> Vec<Instruction> {
    let mut new_instruction_stack = instruction_stack.clone();
    new_instruction_stack.push(Instruction {
        op: Op::Add,
        value: None,
    });
    return new_instruction_stack;
}

#[rustler::nif]
fn take_stack(env: Env, instruction_stack: Vec<Instruction>) -> Vec<Term> {
    let mut runtime = uiua::Uiua::with_safe_sys();
    
    instruction_stack.iter().for_each(|intstruction| intstruction.apply(&mut runtime));

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

rustler::init!("Elixir.Gliua", [empty_stack, push, add, take_stack]);
