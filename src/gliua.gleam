import gleam/io
import gliua/instruction.{type Instruction}

pub fn main() {
  empty_stack()
  |> io.debug()
}

@external(erlang, "gliua_rs", "empty_stack")
pub fn empty_stack() -> List(Instruction)
// @external(erlang, "ffi", "empty_stack")
// pub fn empty_stack() -> List(Instruction)
// @external(erlang, "Elixir.Gliua", "push")
// pub fn push(instructions: List(Instruction), constant: Int) -> List(Instruction)

// @external(erlang, "Elixir.Gliua", "add")
// pub fn add(instructions: List(Instruction)) -> List(Instruction)

// @external(erlang, "Elixir.Gliua", "take_stack")
// pub fn take_stack(instructions: List(Instruction)) -> List(Dynamic)

// @external(erlang, "Elixir.Gliua", "testing")
// pub fn testing(instruction: Instruction) -> Nil
