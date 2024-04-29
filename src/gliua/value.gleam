pub type Value

@external(erlang, "gliua_rs", "to_string")
pub fn to_string(value: Value) -> String

@external(erlang, "gliua_rs", "rows")
pub fn rows(value: Value) -> List(Value)
