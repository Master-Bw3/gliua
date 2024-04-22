import gleeunit
import gliua/decode

pub fn main() {
  testing(decode.DecodeError("hi", "bye"))
  gleeunit.main()
}

@external(erlang, "gliua_rs", "testing")
pub fn testing(value: a) -> Nil
