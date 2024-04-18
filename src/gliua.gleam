import gleam/io
import gleam/list
import gliua/builder
import gliua/instruction.{type Instruction}
import gliua/value.{type Value}

pub fn main() {
  let assert Ok(values) =
    []
    |> builder.push_int_list([1, 2, 54])
    |> builder.couple()
    |> builder.take_stack()

  let assert Ok(value) = list.first(values)

  value.to_string(value)
  |> io.debug()
}
