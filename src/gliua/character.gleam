import gleam/result
import gleam/string

pub opaque type Character {
  Character(String)
}

pub fn from_string(string: String) -> Result(Character, Nil) {
  case string.length(string) {
    1 -> result.map(string.first(string), Character)

    _ -> Error(Nil)
  }
}
