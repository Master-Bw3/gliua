import gliua/value.{type Value}

/// A Uiua runtime
pub type Runtime

/// Retrieves the stack from the provided runtime.
@external(erlang, "gliua_rs", "stack")
pub fn stack(runtime: Runtime) -> List(Value)
