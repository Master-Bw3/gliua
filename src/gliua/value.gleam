import gliua/runtime.{type Runtime}

pub type Value

@external(erlang, "gliua_rs", "to_string")
pub fn to_string(value: Value) -> String
