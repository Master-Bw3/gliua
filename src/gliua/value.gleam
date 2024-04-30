/// A Uiua value.
pub type Value

/// Returns the string representation of a `Value`.
@external(erlang, "gliua_rs", "to_string")
pub fn to_string(value: Value) -> String

/// Returns a list of the rows of a `Value`.
@external(erlang, "gliua_rs", "rows")
pub fn rows(value: Value) -> List(Value)
