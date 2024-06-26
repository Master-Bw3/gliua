import gleam/result
import gleeunit
import gleeunit/should
import gliua/builder
import gliua/character
import gliua/decode
import gliua/runtime

pub fn main() {
  gleeunit.main()
}

pub fn eval_test() {
  let eval_result =
    []
    |> builder.push_int(5)
    |> builder.evaluate()

  should.be_ok(eval_result)
}

pub fn fail_eval_test() {
  let eval_result =
    []
    |> builder.add()
    |> builder.evaluate()

  should.be_error(eval_result)
}

pub fn char() {
  let assert Ok(char) = character.from_string("a")

  let eval_result =
    []
    |> builder.push_char(char)
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.char)
      |> result.map(character.to_string)
    })

  should.equal(eval_result, Ok(Ok("a")))
}

pub fn int_test() {
  let eval_result =
    []
    |> builder.push_int(5)
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.int)
    })

  should.equal(eval_result, Ok(Ok(5)))
}

pub fn float_test() {
  let eval_result =
    []
    |> builder.push_float(5.5)
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.float)
    })

  should.equal(eval_result, Ok(Ok(5.5)))
}

pub fn complex_number_test() {
  let eval_result =
    []
    |> builder.push_complex(5.5, 4.0)
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.complex)
    })

  should.equal(eval_result, Ok(Ok(#(5.5, 4.0))))
}

pub fn string_test() {
  let eval_result =
    []
    |> builder.push_string("hello")
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.string)
    })

  should.equal(eval_result, Ok(Ok("hello")))
}

pub fn int_list_test() {
  let eval_result =
    []
    |> builder.push_int(2)
    |> builder.push_int(1)
    |> builder.join()
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.rows(decode.int))
    })

  should.equal(eval_result, Ok(Ok([1, 2])))
}

pub fn int_matrix_test() {
  let eval_result =
    []
    |> builder.push_int(2)
    |> builder.push_int(1)
    |> builder.join()
    |> builder.duplicate()
    |> builder.couple()
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.rows(decode.rows(decode.int)))
    })

  should.equal(eval_result, Ok(Ok([[1, 2], [1, 2]])))
}

pub fn run_str_test() {
  let eval_result =
    []
    |> builder.run_str("⊟. [1 2]")
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.rows(decode.rows(decode.int)))
    })

  should.equal(eval_result, Ok(Ok([[1, 2], [1, 2]])))
}

pub fn run_str_2_test() {
  let eval_result =
    []
    |> builder.push_int(2)
    |> builder.push_int(1)
    |> builder.join()
    |> builder.run_str("⊟.")
    |> builder.evaluate()

  let assert Ok(runtime) = eval_result

  let stack =
    runtime.stack(runtime)
    |> decode.stack_1(decode.rows(decode.rows(decode.int)))

  should.equal(stack, Ok([[1, 2], [1, 2]]))
}

pub fn run_file_test() {
  let eval_result =
    []
    |> builder.run_file("./test/test.ua")
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.rows(decode.rows(decode.int)))
    })

  should.equal(eval_result, Ok(Ok([[1, 2], [1, 2]])))
}

pub fn run_file_2_test() {
  let eval_result =
    []
    |> builder.push_int(2)
    |> builder.push_int(1)
    |> builder.join()
    |> builder.run_file("./test/test2.ua")
    |> builder.evaluate()
    |> result.map(fn(runtime) {
      runtime.stack(runtime)
      |> decode.stack_1(decode.rows(decode.rows(decode.int)))
    })

  should.equal(eval_result, Ok(Ok([[1, 2], [1, 2]])))
}
