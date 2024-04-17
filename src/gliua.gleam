import gleam/dynamic.{type Dynamic}
import gleam/io
import gliua/instruction.{type Instruction}

pub fn main() {
  // empty_stack()
  // |> push(1)
  // |> push(2)
  // |> add()
  // |> io.debug()
  // |> take_stack()
  // |> io.debug()

  push_op(instruction.Push(2), [])
  push_op(instruction.Dup, [])
}

@external(erlang, "gliua_rs", "empty_stack")
pub fn empty_stack() -> List(Instruction)

@external(erlang, "gliua_rs", "push")
pub fn push(instructions: List(Instruction), constant: Int) -> List(Instruction)

@external(erlang, "gliua_rs", "push_op")
pub fn push_op(
  constant: Instruction,
  instructions: List(Instruction),
) -> List(Instruction)

@external(erlang, "gliua_rs", "add")
pub fn add(instructions: List(Instruction)) -> List(Instruction)

@external(erlang, "gliua_rs", "take_stack")
pub fn take_stack(instructions: List(Instruction)) -> List(Dynamic)

@external(erlang, "gliua_rs", "testing")
pub fn testing(instruction: Instruction) -> Nil
