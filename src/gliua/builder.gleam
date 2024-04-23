import gleam/int
import gleam/list
import gliua/instruction.{type Instruction}
import gliua/runtime.{type Runtime}
import gliua/value.{type Value}

pub fn push_value(
  instructions: List(Instruction),
  value: Value,
) -> List(Instruction) {
  [instruction.PushValue(value), ..instructions]
}

pub fn push_int(
  instructions: List(Instruction),
  value: Int,
) -> List(Instruction) {
  [instruction.PushNum(int.to_float(value)), ..instructions]
}

pub fn push_float(
  instructions: List(Instruction),
  value: Float,
) -> List(Instruction) {
  [instruction.PushNum(value), ..instructions]
}

pub fn push_complex(
  instructions: List(Instruction),
  real: Float,
  imaginary: Float,
) -> List(Instruction) {
  [instruction.PushComplex(real, imaginary), ..instructions]
}

pub fn push_int_list(
  instructions: List(Instruction),
  value: List(Int),
) -> List(Instruction) {
  [instruction.PushNumList(list.map(value, int.to_float)), ..instructions]
}

pub fn push_float_list(
  instructions: List(Instruction),
  value: List(Float),
) -> List(Instruction) {
  [instruction.PushNumList(value), ..instructions]
}

pub fn push_string(
  instructions: List(Instruction),
  value: String,
) -> List(Instruction) {
  [instruction.PushString(value), ..instructions]
}

pub fn push_complex_list(
  instructions: List(Instruction),
  value: List(#(Float, Float)),
) -> List(Instruction) {
  [instruction.PushComplexList(value), ..instructions]
}

pub fn push_byte_array(
  instructions: List(Instruction),
  value: List(Int),
) -> List(Instruction) {
  [instruction.PushByteArray(value), ..instructions]
}

pub fn add(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Add, ..instructions]
}

pub fn stack(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Stack, ..instructions]
}

pub fn repr(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Repr, ..instructions]
}

pub fn pop(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Pop, ..instructions]
}

pub fn couple(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Couple, ..instructions]
}

@external(erlang, "gliua_rs", "evaluate")
pub fn evaluate(instructions: List(Instruction)) -> Result(Runtime, String)
