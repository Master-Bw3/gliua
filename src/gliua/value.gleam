pub type Value

@external(erlang, "gliua_rs", "to_string")
pub fn to_string(value: Value) -> String

@external(erlang, "gliua_rs", "uncouple")
pub fn uncouple(value: Value) -> #(Value, Value)

@external(erlang, "gliua_rs", "join")
pub fn join(this: Value, other: Value) -> Value

@external(erlang, "gliua_rs", "couple")
pub fn couple(this: Value, other: Value) -> Value
