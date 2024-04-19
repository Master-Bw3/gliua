import gleam/dynamic
import gleam/io
import gleam/list
import gleeunit
import gleeunit/should
import gliua
import gliua/builder
import gliua/instruction.{type Instruction}

pub fn main() {
  gleeunit.main()
}

pub fn push_int_test() {
  let result =
    []
    |> builder.push_int(5)
    |> builder.take_stack()

  should.be_ok(result)
  let assert Ok(stack) = result
}
