import gleam/result
import gleam/string

/// Represents a Uiua Character.
pub opaque type Character {
  Character(String)
}

/// Creates a Character from a String. If the string is not a length of 1, 
/// `Error(Nil)` will be returned.
pub fn from_string(string: String) -> Result(Character, Nil) {
  case string.length(string) {
    1 -> result.map(string.first(string), Character)

    _ -> Error(Nil)
  }
}

/// Converts a Character into a String.
pub fn to_string(character: Character) -> String {
  case character {
    Character(str) -> str
  }
}
