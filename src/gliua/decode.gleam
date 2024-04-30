import gleam/function
import gleam/list
import gleam/result
import gleam/string
import gliua/character
import gliua/value.{type Value}

pub type DecodeErrors =
  List(DecodeError)

pub type DecodeError {
  DecodeError(expected: String, found: String)
}

pub type Decoder(t) =
  fn(Value) -> Result(t, DecodeErrors)

pub fn stack_1(
  first decoder: Decoder(a),
) -> fn(List(Value)) -> Result(a, DecodeErrors) {
  fn(stack) {
    case stack {
      [a, ..] -> decoder(a)
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
  first decoder1: Decoder(a),
  second decoder2: Decoder(b),
) -> fn(List(Value)) -> Result(#(a, b), DecodeErrors) {
  fn(stack) {
    case stack {
      [a, b, ..] ->
        case decoder1(a), decoder2(b) {
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
) -> fn(List(Value)) -> Result(#(a, b, c), DecodeErrors) {
  fn(stack) {
    case stack {
      [a, b, c, ..] ->
        case decoder1(a), decoder2(b), decoder3(c) {
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

pub fn rows(of decoder_type: Decoder(inner)) -> Decoder(List(inner)) {
  fn(value) {
    value.rows(value)
    |> list.try_map(decoder_type)
  }
}

pub fn string(value: Value) -> Result(String, DecodeErrors) {
  rows(char)
  |> function.apply1(value)
  |> result.map(fn(x) { string.concat(list.map(x, character.to_string)) })
}

@external(erlang, "gliua_rs", "as_int")
pub fn int(value: Value) -> Result(Int, DecodeErrors)

@external(erlang, "gliua_rs", "as_float")
pub fn float(value: Value) -> Result(Float, DecodeErrors)

@external(erlang, "gliua_rs", "as_complex")
pub fn complex(value: Value) -> Result(#(Float, Float), DecodeErrors)

@external(erlang, "gliua_rs", "as_char")
pub fn char(value: Value) -> Result(character.Character, DecodeErrors)
