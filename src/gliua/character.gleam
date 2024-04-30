import gleam/result
import gleam/string

// Represents a Uiua character.
pub opaque type Character {
  Character(String)
}

/// Create a Uiua character from a string. If the string is not a length of 1, 
/// `Error(Nil)` will be returned.
pub fn from_string(string: String) -> Result(Character, Nil) {
  case string.length(string) {
    1 -> result.map(string.first(string), Character)

    _ -> Error(Nil)
  }
}
