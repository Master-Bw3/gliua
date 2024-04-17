import gleam/dynamic.{type Dynamic}
import gleam/io
import gliua/instruction.{type Instruction}

pub fn main() {
  []
  |> push(1)
  |> push(2)
  |> add()
  |> io.debug()
  |> take_stack()
  |> io.debug()
}

pub fn push(instructions: List(Instruction), constant: Int) -> List(Instruction) {
  [instruction.Push(constant), ..instructions]
}

pub fn add(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Add, ..instructions]
}

@external(erlang, "gliua_rs", "evaluate")
pub fn take_stack(instructions: List(Instruction)) -> List(Dynamic)
