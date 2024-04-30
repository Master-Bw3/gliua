//// Helper functions for building a list of instructions to be evaluated.
//// 
//// Use `run_str` to add an instruction to run uiua source code.
//// Use `run_file` to add an instruction to run a uiua source file.
//// Use `evaluate` to evaluate a list of instructions.

import gleam/int
import gleam/list
import gliua/runtime.{type Runtime}
import gliua/value.{type Value}

/// add an instruction to run uiua source code
pub fn run_str(
  instructions: List(Instruction),
  str: String,
) -> List(Instruction) {
  [RunStr(str), ..instructions]
}

/// Adds an instruction to run a uiua source file.
pub fn run_file(
  instructions instructions: List(Instruction),
  path str: String,
) -> List(Instruction) {
  [RunFile(str), ..instructions]
}

/// Adds an instruction to push a uiua value to the stack.
pub fn push_value(
  instructions: List(Instruction),
  value: Value,
) -> List(Instruction) {
  [PushValue(value), ..instructions]
}

/// Adds an instruction to push an integer to the stack.
pub fn push_int(
  instructions: List(Instruction),
  value: Int,
) -> List(Instruction) {
  [PushNum(int.to_float(value)), ..instructions]
}

/// Adds an instruction to push a float to the stack.
pub fn push_float(
  instructions: List(Instruction),
  value: Float,
) -> List(Instruction) {
  [PushNum(value), ..instructions]
}

/// Adds an instruction to push a complex number to the stack.
pub fn push_complex(
  instructions: List(Instruction),
  real: Float,
  imaginary: Float,
) -> List(Instruction) {
  [PushComplex(real, imaginary), ..instructions]
}

/// Adds an instruction to push a list of integers to the stack.
pub fn push_int_list(
  instructions: List(Instruction),
  value: List(Int),
) -> List(Instruction) {
  [PushNumList(list.map(value, int.to_float)), ..instructions]
}

/// Adds an instruction to push a list of floats to the stack.
pub fn push_float_list(
  instructions: List(Instruction),
  value: List(Float),
) -> List(Instruction) {
  [PushNumList(value), ..instructions]
}

/// Adds an instruction to push a string to the stack.
pub fn push_string(
  instructions: List(Instruction),
  value: String,
) -> List(Instruction) {
  [PushString(value), ..instructions]
}

/// Adds an instruction to push a list of complex numbers to the stack.
pub fn push_complex_list(
  instructions: List(Instruction),
  value: List(#(Float, Float)),
) -> List(Instruction) {
  [PushComplexList(value), ..instructions]
}

/// Adds an instruction to push a byte array to the stack.
pub fn push_byte_array(
  instructions: List(Instruction),
  value: List(Int),
) -> List(Instruction) {
  [PushByteArray(value), ..instructions]
}

pub fn eta(instructions: List(Instruction)) -> List(Instruction) {
  [Eta, ..instructions]
}

pub fn pi(instructions: List(Instruction)) -> List(Instruction) {
  [Pi, ..instructions]
}

pub fn tau(instructions: List(Instruction)) -> List(Instruction) {
  [Tau, ..instructions]
}

pub fn infinity(instructions: List(Instruction)) -> List(Instruction) {
  [Infinity, ..instructions]
}

pub fn not(instructions: List(Instruction)) -> List(Instruction) {
  [Not, ..instructions]
}

pub fn negate(instructions: List(Instruction)) -> List(Instruction) {
  [Neg, ..instructions]
}

pub fn abs(instructions: List(Instruction)) -> List(Instruction) {
  [Abs, ..instructions]
}

pub fn sign(instructions: List(Instruction)) -> List(Instruction) {
  [Sign, ..instructions]
}

pub fn sqrt(instructions: List(Instruction)) -> List(Instruction) {
  [Sqrt, ..instructions]
}

pub fn sin(instructions: List(Instruction)) -> List(Instruction) {
  [Sin, ..instructions]
}

pub fn floor(instructions: List(Instruction)) -> List(Instruction) {
  [Floor, ..instructions]
}

pub fn ceil(instructions: List(Instruction)) -> List(Instruction) {
  [Ceil, ..instructions]
}

pub fn round(instructions: List(Instruction)) -> List(Instruction) {
  [Round, ..instructions]
}

pub fn equal_to(instructions: List(Instruction)) -> List(Instruction) {
  [Eq, ..instructions]
}

pub fn not_equal_to(instructions: List(Instruction)) -> List(Instruction) {
  [Ne, ..instructions]
}

pub fn less_than(instructions: List(Instruction)) -> List(Instruction) {
  [Lt, ..instructions]
}

pub fn less_than_or_equal_to(
  instructions: List(Instruction),
) -> List(Instruction) {
  [Le, ..instructions]
}

pub fn greater_than(instructions: List(Instruction)) -> List(Instruction) {
  [Gt, ..instructions]
}

pub fn greater_than_or_equal_to(
  instructions: List(Instruction),
) -> List(Instruction) {
  [Ge, ..instructions]
}

pub fn add(instructions: List(Instruction)) -> List(Instruction) {
  [Add, ..instructions]
}

pub fn subtract(instructions: List(Instruction)) -> List(Instruction) {
  [Sub, ..instructions]
}

pub fn multiply(instructions: List(Instruction)) -> List(Instruction) {
  [Mul, ..instructions]
}

pub fn divide(instructions: List(Instruction)) -> List(Instruction) {
  [Div, ..instructions]
}

pub fn modulo(instructions: List(Instruction)) -> List(Instruction) {
  [Mod, ..instructions]
}

pub fn pow(instructions: List(Instruction)) -> List(Instruction) {
  [Pow, ..instructions]
}

pub fn log(instructions: List(Instruction)) -> List(Instruction) {
  [Log, ..instructions]
}

pub fn min(instructions: List(Instruction)) -> List(Instruction) {
  [Min, ..instructions]
}

pub fn max(instructions: List(Instruction)) -> List(Instruction) {
  [Max, ..instructions]
}

pub fn atan(instructions: List(Instruction)) -> List(Instruction) {
  [Atan, ..instructions]
}

pub fn complex(instructions: List(Instruction)) -> List(Instruction) {
  [Complex, ..instructions]
}

pub fn match(instructions: List(Instruction)) -> List(Instruction) {
  [Match, ..instructions]
}

pub fn join(instructions: List(Instruction)) -> List(Instruction) {
  [Join, ..instructions]
}

pub fn transpose(instructions: List(Instruction)) -> List(Instruction) {
  [Transpose, ..instructions]
}

pub fn keep(instructions: List(Instruction)) -> List(Instruction) {
  [Keep, ..instructions]
}

pub fn take(instructions: List(Instruction)) -> List(Instruction) {
  [Take, ..instructions]
}

pub fn drop(instructions: List(Instruction)) -> List(Instruction) {
  [Drop, ..instructions]
}

pub fn rotate(instructions: List(Instruction)) -> List(Instruction) {
  [Rotate, ..instructions]
}

pub fn couple(instructions: List(Instruction)) -> List(Instruction) {
  [Couple, ..instructions]
}

pub fn rise(instructions: List(Instruction)) -> List(Instruction) {
  [Rise, ..instructions]
}

pub fn fall(instructions: List(Instruction)) -> List(Instruction) {
  [Fall, ..instructions]
}

pub fn select(instructions: List(Instruction)) -> List(Instruction) {
  [Select, ..instructions]
}

pub fn windows(instructions: List(Instruction)) -> List(Instruction) {
  [Windows, ..instructions]
}

pub fn where(instructions: List(Instruction)) -> List(Instruction) {
  [Where, ..instructions]
}

pub fn classify(instructions: List(Instruction)) -> List(Instruction) {
  [Classify, ..instructions]
}

pub fn deduplicate(instructions: List(Instruction)) -> List(Instruction) {
  [Deduplicate, ..instructions]
}

pub fn unique(instructions: List(Instruction)) -> List(Instruction) {
  [Unique, ..instructions]
}

pub fn member(instructions: List(Instruction)) -> List(Instruction) {
  [Member, ..instructions]
}

pub fn find(instructions: List(Instruction)) -> List(Instruction) {
  [Find, ..instructions]
}

pub fn mask(instructions: List(Instruction)) -> List(Instruction) {
  [Mask, ..instructions]
}

pub fn index_of(instructions: List(Instruction)) -> List(Instruction) {
  [IndexOf, ..instructions]
}

pub fn coordinate(instructions: List(Instruction)) -> List(Instruction) {
  [Coordinate, ..instructions]
}

pub fn box(instructions: List(Instruction)) -> List(Instruction) {
  [Box, ..instructions]
}

pub fn representation(instructions: List(Instruction)) -> List(Instruction) {
  [Repr, ..instructions]
}

pub fn parse(instructions: List(Instruction)) -> List(Instruction) {
  [Parse, ..instructions]
}

pub fn utf(instructions: List(Instruction)) -> List(Instruction) {
  [Utf, ..instructions]
}

pub fn range(instructions: List(Instruction)) -> List(Instruction) {
  [Range, ..instructions]
}

pub fn reverse(instructions: List(Instruction)) -> List(Instruction) {
  [Reverse, ..instructions]
}

pub fn deshape(instructions: List(Instruction)) -> List(Instruction) {
  [Deshape, ..instructions]
}

pub fn fix(instructions: List(Instruction)) -> List(Instruction) {
  [Fix, ..instructions]
}

pub fn first(instructions: List(Instruction)) -> List(Instruction) {
  [First, ..instructions]
}

pub fn length(instructions: List(Instruction)) -> List(Instruction) {
  [Len, ..instructions]
}

pub fn shape(instructions: List(Instruction)) -> List(Instruction) {
  [Shape, ..instructions]
}

pub fn bits(instructions: List(Instruction)) -> List(Instruction) {
  [Bits, ..instructions]
}

pub fn reduce(instructions: List(Instruction)) -> List(Instruction) {
  [Reduce, ..instructions]
}

pub fn scan(instructions: List(Instruction)) -> List(Instruction) {
  [Scan, ..instructions]
}

pub fn fold(instructions: List(Instruction)) -> List(Instruction) {
  [Fold, ..instructions]
}

pub fn each(instructions: List(Instruction)) -> List(Instruction) {
  [Each, ..instructions]
}

pub fn rows(instructions: List(Instruction)) -> List(Instruction) {
  [Rows, ..instructions]
}

pub fn table(instructions: List(Instruction)) -> List(Instruction) {
  [Table, ..instructions]
}

pub fn inventory(instructions: List(Instruction)) -> List(Instruction) {
  [Inventory, ..instructions]
}

pub fn repeat(instructions: List(Instruction)) -> List(Instruction) {
  [Repeat, ..instructions]
}

pub fn do(instructions: List(Instruction)) -> List(Instruction) {
  [Do, ..instructions]
}

pub fn group(instructions: List(Instruction)) -> List(Instruction) {
  [Group, ..instructions]
}

pub fn partition(instructions: List(Instruction)) -> List(Instruction) {
  [Partition, ..instructions]
}

pub fn reshape(instructions: List(Instruction)) -> List(Instruction) {
  [Reshape, ..instructions]
}

pub fn rerank(instructions: List(Instruction)) -> List(Instruction) {
  [Rerank, ..instructions]
}

pub fn duplicate(instructions: List(Instruction)) -> List(Instruction) {
  [Dup, ..instructions]
}

pub fn flip(instructions: List(Instruction)) -> List(Instruction) {
  [Flip, ..instructions]
}

pub fn over(instructions: List(Instruction)) -> List(Instruction) {
  [Over, ..instructions]
}

pub fn pop(instructions: List(Instruction)) -> List(Instruction) {
  [Pop, ..instructions]
}

pub fn all(instructions: List(Instruction)) -> List(Instruction) {
  [All, ..instructions]
}

pub fn fill(instructions: List(Instruction)) -> List(Instruction) {
  [Fill, ..instructions]
}

pub fn this(instructions: List(Instruction)) -> List(Instruction) {
  [This, ..instructions]
}

pub fn recurse(instructions: List(Instruction)) -> List(Instruction) {
  [Recur, ..instructions]
}

pub fn try(instructions: List(Instruction)) -> List(Instruction) {
  [Try, ..instructions]
}

pub fn assert_(instructions: List(Instruction)) -> List(Instruction) {
  [Assert, ..instructions]
}

pub fn random(instructions: List(Instruction)) -> List(Instruction) {
  [Rand, ..instructions]
}

pub fn generate(instructions: List(Instruction)) -> List(Instruction) {
  [Gen, ..instructions]
}

pub fn deal(instructions: List(Instruction)) -> List(Instruction) {
  [Deal, ..instructions]
}

pub fn tag(instructions: List(Instruction)) -> List(Instruction) {
  [Tag, ..instructions]
}

pub fn type_(instructions: List(Instruction)) -> List(Instruction) {
  [Type, ..instructions]
}

pub fn memoize(instructions: List(Instruction)) -> List(Instruction) {
  [Memo, ..instructions]
}

pub fn spawn(instructions: List(Instruction)) -> List(Instruction) {
  [Spawn, ..instructions]
}

pub fn pool(instructions: List(Instruction)) -> List(Instruction) {
  [Pool, ..instructions]
}

pub fn wait(instructions: List(Instruction)) -> List(Instruction) {
  [Wait, ..instructions]
}

pub fn send(instructions: List(Instruction)) -> List(Instruction) {
  [Send, ..instructions]
}

pub fn recieve(instructions: List(Instruction)) -> List(Instruction) {
  [Recv, ..instructions]
}

pub fn try_recieve(instructions: List(Instruction)) -> List(Instruction) {
  [TryRecv, ..instructions]
}

pub fn now(instructions: List(Instruction)) -> List(Instruction) {
  [Now, ..instructions]
}

pub fn set_inverse(instructions: List(Instruction)) -> List(Instruction) {
  [SetInverse, ..instructions]
}

pub fn set_under(instructions: List(Instruction)) -> List(Instruction) {
  [SetUnder, ..instructions]
}

pub fn insert(instructions: List(Instruction)) -> List(Instruction) {
  [Insert, ..instructions]
}

pub fn has(instructions: List(Instruction)) -> List(Instruction) {
  [Has, ..instructions]
}

pub fn get(instructions: List(Instruction)) -> List(Instruction) {
  [Get, ..instructions]
}

pub fn remove(instructions: List(Instruction)) -> List(Instruction) {
  [Remove, ..instructions]
}

pub fn map(instructions: List(Instruction)) -> List(Instruction) {
  [Map, ..instructions]
}

pub fn shapes(instructions: List(Instruction)) -> List(Instruction) {
  [Shapes, ..instructions]
}

pub fn types(instructions: List(Instruction)) -> List(Instruction) {
  [Types, ..instructions]
}

pub fn trace(instructions: List(Instruction)) -> List(Instruction) {
  [Trace, ..instructions]
}

pub fn stack(instructions: List(Instruction)) -> List(Instruction) {
  [Stack, ..instructions]
}

pub fn dump(instructions: List(Instruction)) -> List(Instruction) {
  [Dump, ..instructions]
}

pub fn regex(instructions: List(Instruction)) -> List(Instruction) {
  [Regex, ..instructions]
}

pub fn csv(instructions: List(Instruction)) -> List(Instruction) {
  [Csv, ..instructions]
}

/// Evaluates a list of instructions and return the resulting runtime or an error string as a `Result`.
@external(erlang, "gliua_rs", "evaluate")
pub fn evaluate(instructions: List(Instruction)) -> Result(Runtime, String)

@internal
pub type Instruction {
  RunStr(String)
  RunFile(String)
  PushValue(Value)
  PushNum(Float)
  PushChar(String)
  PushComplex(Float, Float)
  PushNumList(List(Float))
  PushString(String)
  PushComplexList(List(#(Float, Float)))
  PushByteArray(List(Int))
  Dup
  Over
  Flip
  Pop
  Identity
  Not
  Sign
  Neg
  Abs
  Sqrt
  Sin
  Floor
  Ceil
  Round
  Eq
  Ne
  Lt
  Le
  Gt
  Ge
  Add
  Sub
  Mul
  Div
  Mod
  Pow
  Log
  Min
  Max
  Atan
  Complex
  Len
  Shape
  Range
  First
  Reverse
  Deshape
  Fix
  Bits
  Transpose
  Rise
  Fall
  Where
  Classify
  Deduplicate
  Unique
  Box
  Parse
  Match
  Couple
  Join
  Select
  Pick
  Reshape
  Rerank
  Take
  Drop
  Rotate
  Windows
  Keep
  Find
  Mask
  Member
  IndexOf
  Coordinate
  Reduce
  Fold
  Scan
  Each
  Rows
  Table
  Inventory
  Repeat
  Group
  Partition
  Content
  Gap
  Dip
  On
  By
  Both
  Bind
  Un
  SetInverse
  SetUnder
  Under
  Fork
  Cascade
  Bracket
  All
  Do
  Fill
  Try
  Assert
  This
  Recur
  Rand
  Memo
  Comptime
  Spawn
  Pool
  Wait
  Send
  Recv
  TryRecv
  Gen
  Deal
  Regex
  Utf
  Tag
  Type
  Now
  Eta
  Pi
  Tau
  Infinity
  Map
  Insert
  Has
  Get
  Remove
  Shapes
  Types
  Stack
  Trace
  Dump
  Stringify
  Quote
  Sig
  Csv
  Repr
}
