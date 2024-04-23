import gliua/value.{type Value}

pub type Runtime

@external(erlang, "gliua_rs", "stack")
pub fn stack(runtime: Runtime) -> List(Value)
