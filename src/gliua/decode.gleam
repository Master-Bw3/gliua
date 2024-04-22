import gleam/list
import gliua/runtime.{type Runtime}
import gliua/value.{type Value}

pub type DecodeErrors =
  List(DecodeError)

pub type DecodeError {
  DecodeError(expected: String, found: String)
}

pub type Decoder(t) =
  fn(Value, Runtime) -> Result(t, DecodeErrors)

pub fn stack_1(
  runtime runtime: Runtime,
  first decoder: Decoder(a),
) -> fn(List(Value)) -> Result(a, DecodeErrors) {
  fn(stack) {
    case stack {
      [a, ..] -> decoder(a, runtime)
      _ ->
        Error([
          DecodeError(
            expected: "at least 2 elements on the stack",
            found: "less than 2 elements on the stacks",
          ),
        ])
    }
  }
}

pub fn stack_2(
  runtime runtime: Runtime,
  first decoder1: Decoder(a),
  second decoder2: Decoder(b),
) -> fn(List(Value)) -> Result(#(a, b), DecodeErrors) {
  fn(stack) {
    case stack {
      [a, b, ..] ->
        case decoder1(a, runtime), decoder2(b, runtime) {
          Ok(a), Ok(b) -> Ok(#(a, b))
          a, b -> Error(list.concat([all_errors(a), all_errors(b)]))
        }
      _ ->
        Error([
          DecodeError(
            expected: "at least 2 elements on the stack",
            found: "less than 2 elements on the stacks",
          ),
        ])
    }
  }
}

pub fn stack_3(
  runtime runtime: Runtime,
  first decoder1: Decoder(a),
  second decoder2: Decoder(b),
  third decoder3: Decoder(c),
) -> fn(List(Value)) -> Result(#(a, b, c), DecodeErrors) {
  fn(stack) {
    case stack {
      [a, b, c, ..] ->
        case decoder1(a, runtime), decoder2(b, runtime), decoder3(c, runtime) {
          Ok(a), Ok(b), Ok(c) -> Ok(#(a, b, c))
          a, b, c ->
            Error(list.concat([all_errors(a), all_errors(b), all_errors(c)]))
        }
      _ ->
        Error([
          DecodeError(
            expected: "at least 3 elements on the stack",
            found: "less than 3 elements on the stacks",
          ),
        ])
    }
  }
}

fn all_errors(result: Result(a, List(DecodeError))) -> List(DecodeError) {
  case result {
    Ok(_) -> []
    Error(errors) -> errors
  }
}

@external(erlang, "gliua_rs", "as_int")
pub fn int(value: Value, runtime: Runtime) -> Result(Int, DecodeErrors)

@external(erlang, "gliua_rs", "as_float")
pub fn float(value: Value, runtime: Runtime) -> Result(Float, DecodeErrors)
