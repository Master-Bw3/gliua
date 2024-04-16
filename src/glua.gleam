import gleam/dynamic.{type Dynamic}
import gleam/io

@external(erlang, "Elixir.Gliua", "empty_stack")
pub fn empty_stack() -> List(Dynamic)

@external(erlang, "Elixir.Gliua", "push")
pub fn push(instructions: List(Dynamic), constant: Int) -> List(Dynamic)

@external(erlang, "Elixir.Gliua", "add")
pub fn add(instructions: List(Dynamic)) -> List(Dynamic)

@external(erlang, "Elixir.Gliua", "take_stack")
pub fn take_stack(instructions: List(Dynamic)) -> List(Dynamic)
