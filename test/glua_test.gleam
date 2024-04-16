import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/list
import gleeunit
import gleeunit/should
import gliua

pub fn main() {
  gleeunit.main()
}

pub fn ffi_test() {
  let result =
    gliua.empty_stack()
    |> gliua.push(1)
    |> gliua.push(2)
    |> gliua.add()
    |> gliua.take_stack()

  let decoded =
    result
    |> list.map(dynamic.float)

  should.equal([Ok(3.0)], decoded)
}
