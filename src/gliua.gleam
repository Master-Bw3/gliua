import gleam/dynamic.{type Dynamic}
import gleam/io
import gliua/instruction.{type Instruction}

pub type Value

pub fn main() {
  todo
}

@external(erlang, "gliua_rs", "evaluate")
pub fn take_stack(instructions: List(Instruction)) -> List(Dynamic)
