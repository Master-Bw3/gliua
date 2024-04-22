import gleam/list
import gleam/result
import gliua/runtime.{type Runtime}
import gliua/value.{type Value}

pub type DecodeErrors =
  List(DecodeError)

pub type DecodeError {
  DecodeError(expected: String, found: String)
}

pub type Decoder(t) =
  fn(Value, Runtime) -> Result(t, DecodeErrors)

pub fn stack_2(
  first decoder1: Decoder(a),
  second decoder2: Decoder(b),
) -> fn(List(Value), Runtime) -> Result(#(a, b), DecodeErrors) {
  fn(stack, runtime) {
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
  first decoder1: Decoder(a),
  second decoder2: Decoder(b),
  third decoder3: Decoder(c),
) -> fn(List(Value), Runtime) -> Result(#(a, b, c), DecodeErrors) {
  fn(stack, runtime) {
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

@external(erlang, "gliua_rs", "to_int")
pub fn int(value: Value, runtime: Runtime) -> Result(Int, DecodeErrors)
