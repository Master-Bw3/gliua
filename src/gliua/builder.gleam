//// Helper functions for building a list of instructions to be evaluated.
//// 
//// Use `run_str` to add an instruction to run uiua source code.
//// Use `run_file` to add an instruction to run a uiua source file.
//// Use `evaluate` to evaluate a list of instructions.

import gleam/int
import gleam/list
import gliua/character
import gliua/instruction
import gliua/runtime.{type Runtime}
import gliua/value.{type Value}

/// add an instruction to run uiua source code
pub fn run_str(
  instructions: List(Instruction),
  str: String,
) -> List(Instruction) {
  [instruction.RunStr(str), ..instructions]
}

/// Adds an instruction to run a uiua source file.
pub fn run_file(
  instructions instructions: List(Instruction),
  path str: String,
) -> List(Instruction) {
  [instruction.RunFile(str), ..instructions]
}

/// Adds an instruction to push a uiua value to the stack.
pub fn push_value(
  instructions: List(Instruction),
  value: Value,
) -> List(Instruction) {
  [instruction.PushValue(value), ..instructions]
}

/// Adds an instruction to push an integer to the stack.
pub fn push_int(
  instructions: List(Instruction),
  value: Int,
) -> List(Instruction) {
  [instruction.PushNum(int.to_float(value)), ..instructions]
}

/// Adds an instruction to push a character to the stack.
pub fn push_char(
  instructions: List(Instruction),
  value: character.Character,
) -> List(Instruction) {
  [instruction.PushChar(value), ..instructions]
}

/// Adds an instruction to push a float to the stack.
pub fn push_float(
  instructions: List(Instruction),
  value: Float,
) -> List(Instruction) {
  [instruction.PushNum(value), ..instructions]
}

/// Adds an instruction to push a complex number to the stack.
pub fn push_complex(
  instructions: List(Instruction),
  real: Float,
  imaginary: Float,
) -> List(Instruction) {
  [instruction.PushComplex(real, imaginary), ..instructions]
}

/// Adds an instruction to push a list of integers to the stack.
pub fn push_int_list(
  instructions: List(Instruction),
  value: List(Int),
) -> List(Instruction) {
  [instruction.PushNumList(list.map(value, int.to_float)), ..instructions]
}

/// Adds an instruction to push a list of floats to the stack.
pub fn push_float_list(
  instructions: List(Instruction),
  value: List(Float),
) -> List(Instruction) {
  [instruction.PushNumList(value), ..instructions]
}

/// Adds an instruction to push a string to the stack.
pub fn push_string(
  instructions: List(Instruction),
  value: String,
) -> List(Instruction) {
  [instruction.PushString(value), ..instructions]
}

/// Adds an instruction to push a list of complex numbers to the stack.
pub fn push_complex_list(
  instructions: List(Instruction),
  value: List(#(Float, Float)),
) -> List(Instruction) {
  [instruction.PushComplexList(value), ..instructions]
}

/// Adds an instruction to push a byte array to the stack.
pub fn push_byte_array(
  instructions: List(Instruction),
  value: List(Int),
) -> List(Instruction) {
  [instruction.PushByteArray(value), ..instructions]
}

pub fn eta(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Eta, ..instructions]
}

pub fn pi(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Pi, ..instructions]
}

pub fn tau(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Tau, ..instructions]
}

pub fn infinity(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Infinity, ..instructions]
}

pub fn not(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Not, ..instructions]
}

pub fn negate(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Neg, ..instructions]
}

pub fn abs(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Abs, ..instructions]
}

pub fn sign(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Sign, ..instructions]
}

pub fn sqrt(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Sqrt, ..instructions]
}

pub fn sin(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Sin, ..instructions]
}

pub fn floor(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Floor, ..instructions]
}

pub fn ceil(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Ceil, ..instructions]
}

pub fn round(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Round, ..instructions]
}

pub fn equal_to(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Eq, ..instructions]
}

pub fn not_equal_to(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Ne, ..instructions]
}

pub fn less_than(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Lt, ..instructions]
}

pub fn less_than_or_equal_to(
  instructions: List(Instruction),
) -> List(Instruction) {
  [instruction.Le, ..instructions]
}

pub fn greater_than(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Gt, ..instructions]
}

pub fn greater_than_or_equal_to(
  instructions: List(Instruction),
) -> List(Instruction) {
  [instruction.Ge, ..instructions]
}

pub fn add(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Add, ..instructions]
}

pub fn subtract(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Sub, ..instructions]
}

pub fn multiply(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Mul, ..instructions]
}

pub fn divide(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Div, ..instructions]
}

pub fn modulo(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Mod, ..instructions]
}

pub fn pow(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Pow, ..instructions]
}

pub fn log(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Log, ..instructions]
}

pub fn min(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Min, ..instructions]
}

pub fn max(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Max, ..instructions]
}

pub fn atan(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Atan, ..instructions]
}

pub fn complex(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Complex, ..instructions]
}

pub fn match(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Match, ..instructions]
}

pub fn join(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Join, ..instructions]
}

pub fn transpose(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Transpose, ..instructions]
}

pub fn keep(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Keep, ..instructions]
}

pub fn take(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Take, ..instructions]
}

pub fn drop(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Drop, ..instructions]
}

pub fn rotate(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Rotate, ..instructions]
}

pub fn couple(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Couple, ..instructions]
}

pub fn rise(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Rise, ..instructions]
}

pub fn fall(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Fall, ..instructions]
}

pub fn select(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Select, ..instructions]
}

pub fn windows(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Windows, ..instructions]
}

pub fn where(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Where, ..instructions]
}

pub fn classify(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Classify, ..instructions]
}

pub fn deduplicate(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Deduplicate, ..instructions]
}

pub fn unique(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Unique, ..instructions]
}

pub fn member(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Member, ..instructions]
}

pub fn find(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Find, ..instructions]
}

pub fn mask(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Mask, ..instructions]
}

pub fn index_of(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.IndexOf, ..instructions]
}

pub fn coordinate(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Coordinate, ..instructions]
}

pub fn box(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Box, ..instructions]
}

pub fn representation(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Repr, ..instructions]
}

pub fn parse(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Parse, ..instructions]
}

pub fn utf(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Utf, ..instructions]
}

pub fn range(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Range, ..instructions]
}

pub fn reverse(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Reverse, ..instructions]
}

pub fn deshape(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Deshape, ..instructions]
}

pub fn fix(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Fix, ..instructions]
}

pub fn first(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.First, ..instructions]
}

pub fn length(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Len, ..instructions]
}

pub fn shape(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Shape, ..instructions]
}

pub fn bits(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Bits, ..instructions]
}

pub fn reduce(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Reduce, ..instructions]
}

pub fn scan(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Scan, ..instructions]
}

pub fn fold(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Fold, ..instructions]
}

pub fn each(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Each, ..instructions]
}

pub fn rows(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Rows, ..instructions]
}

pub fn table(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Table, ..instructions]
}

pub fn inventory(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Inventory, ..instructions]
}

pub fn repeat(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Repeat, ..instructions]
}

pub fn do(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Do, ..instructions]
}

pub fn group(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Group, ..instructions]
}

pub fn partition(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Partition, ..instructions]
}

pub fn reshape(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Reshape, ..instructions]
}

pub fn rerank(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Rerank, ..instructions]
}

pub fn duplicate(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Dup, ..instructions]
}

pub fn flip(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Flip, ..instructions]
}

pub fn over(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Over, ..instructions]
}

pub fn pop(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Pop, ..instructions]
}

pub fn all(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.All, ..instructions]
}

pub fn fill(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Fill, ..instructions]
}

pub fn this(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.This, ..instructions]
}

pub fn recurse(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Recur, ..instructions]
}

pub fn try(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Try, ..instructions]
}

pub fn assert_(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Assert, ..instructions]
}

pub fn random(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Rand, ..instructions]
}

pub fn generate(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Gen, ..instructions]
}

pub fn deal(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Deal, ..instructions]
}

pub fn tag(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Tag, ..instructions]
}

pub fn type_(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Type, ..instructions]
}

pub fn memoize(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Memo, ..instructions]
}

pub fn spawn(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Spawn, ..instructions]
}

pub fn pool(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Pool, ..instructions]
}

pub fn wait(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Wait, ..instructions]
}

pub fn send(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Send, ..instructions]
}

pub fn recieve(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Recv, ..instructions]
}

pub fn try_recieve(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.TryRecv, ..instructions]
}

pub fn now(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Now, ..instructions]
}

pub fn set_inverse(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.SetInverse, ..instructions]
}

pub fn set_under(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.SetUnder, ..instructions]
}

pub fn insert(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Insert, ..instructions]
}

pub fn has(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Has, ..instructions]
}

pub fn get(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Get, ..instructions]
}

pub fn remove(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Remove, ..instructions]
}

pub fn map(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Map, ..instructions]
}

pub fn shapes(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Shapes, ..instructions]
}

pub fn types(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Types, ..instructions]
}

pub fn trace(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Trace, ..instructions]
}

pub fn stack(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Stack, ..instructions]
}

pub fn dump(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Dump, ..instructions]
}

pub fn regex(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Regex, ..instructions]
}

pub fn csv(instructions: List(Instruction)) -> List(Instruction) {
  [instruction.Csv, ..instructions]
}

/// Evaluates a list of instructions and return the resulting runtime or an error string as a `Result`.
@external(erlang, "gliua_rs", "evaluate")
pub fn evaluate(instructions: List(Instruction)) -> Result(Runtime, String)
