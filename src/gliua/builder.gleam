import gleam/int
import gliua/instruction.{type Instruction}

pub fn push_int(
  instructions: List(Instruction),
  constant: Int,
) -> List(Instruction) {
  [instruction.PushNum(int.to_float(constant)), ..instructions]
}

pub fn push_float(
  instructions: List(Instruction),
  constant: Float,
) -> List(Instruction) {
  [instruction.PushNum(constant), ..instructions]
}

pub fn push_complex(
  instructions: List(Instruction),
  real: Float,
  imaginary: Float,
) -> List(Instruction) {
  [instruction.PushComplex(real, imaginary), ..instructions]
}

pub fn push_number_list(
  instructions: List(Instruction),
  constant: List(Float),
) -> List(Instruction) {
  [instruction.PushNumList(constant), ..instructions]
}

pub fn push_string(
  instructions: List(Instruction),
  constant: String,
) -> List(Instruction) {
  [instruction.PushString(constant), ..instructions]
}

pub fn push_complex_list(
  instructions: List(Instruction),
  constant: List(#(Float, Float)),
) -> List(Instruction) {
  [instruction.PushComplexList(constant), ..instructions]
}

pub fn push_byte_array(
  instructions: List(Instruction),
  constant: List(Int),
) -> List(Instruction) {
  [instruction.PushByteArray(constant), ..instructions]
}

pub fn add(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Add, ..instructions]
}
