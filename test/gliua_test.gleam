import gleam/io
import gleam/result
import gleeunit
import gleeunit/should
import gliua/builder
import gliua/decode
import gliua/runtime

pub fn main() {
  gleeunit.main()
}

pub fn push_int_test() {
  let eval_result =
    []
    |> builder.push_int(5)
    |> builder.evaluate()

  let result =
    result.map(eval_result, fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(runtime, decode.int)
    })

  should.equal(result, Ok(Ok(5)))
}

pub fn push_float_test() {
  let eval_result =
    []
    |> builder.push_float(5.5)
    |> builder.evaluate()

  let result =
    result.map(eval_result, fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(runtime, decode.float)
    })

  should.equal(result, Ok(Ok(5.5)))
}
