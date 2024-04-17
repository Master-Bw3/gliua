use rustler::{Encoder, Env, NifUnitEnum, Term};
use uiua::Uiua;

#[derive(NifUnitEnum, Clone, Copy, Debug)]
pub(crate) enum Op {
    Push,
    Dup,
    Over,
    Flip,
    Pop,
    Identity,
    Not,
    Sign,
    Neg,
    Abs,
    Sqrt,
    Sin,
    Floor,
    Ceil,
    Round,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Log,
    Min,
    Max,
    Atan,
    Complex,
    Len,
    Shape,
    Range,
    First,
    Reverse,
    Deshape,
    Fix,
    Bits,
    Transpose,
    Rise,
    Fall,
    Where,
    Classify,
    Deduplicate,
    Unique,
    Box,
    Parse,
    Match,
    Couple,
    Join,
    Select,
    Pick,
    Reshape,
    Rerank,
    Take,
    Drop,
    Rotate,
    Windows,
    Keep,
    Find,
    Mask,
    Member,
    IndexOf,
    Coordinate,
    Reduce,
    Fold,
    Scan,
    Each,
    Rows,
    Table,
    Inventory,
    Repeat,
    Group,
    Partition,
    Content,
    Gap,
    Dip,
    On,
    By,
    Both,
    Bind,
    Un,
    SetInverse,
    SetUnder,
    Under,
    Fork,
    Cascade,
    Bracket,
    All,
    Do,
    Fill,
    Try,
    Assert,
    This,
    Recur,
    Rand,
    Memo,
    Comptime,
    Spawn,
    Pool,
    Wait,
    Send,
    Recv,
    TryRecv,
    Gen,
    Deal,
    Regex,
    Utf,
    Tag,
    Type,
    Now,
    Eta,
    Pi,
    Tau,
    Infinity,
    Map,
    Insert,
    Has,
    Get,
    Remove,
    Shapes,
    Types,
    Stack,
    Trace,
    Dump,
    Stringify,
    Quote,
    Sig,
    Csv,
    Repr
}

impl From<uiua::Primitive> for Op {
    fn from(value: uiua::Primitive) -> Self {
        match value {
            uiua::Primitive::Dup => Op::Dup,
            uiua::Primitive::Over => Op::Over,
            uiua::Primitive::Flip => Op::Flip,
            uiua::Primitive::Pop => Op::Pop,
            uiua::Primitive::Identity => Op::Identity,
            uiua::Primitive::Not => Op::Not,
            uiua::Primitive::Sign => Op::Sign,
            uiua::Primitive::Neg => Op::Neg,
            uiua::Primitive::Abs => Op::Abs,
            uiua::Primitive::Sqrt => Op::Sqrt,
            uiua::Primitive::Sin => Op::Sin,
            uiua::Primitive::Floor => Op::Floor,
            uiua::Primitive::Ceil => Op::Ceil,
            uiua::Primitive::Round => Op::Round,
            uiua::Primitive::Eq => Op::Eq,
            uiua::Primitive::Ne => Op::Ne,
            uiua::Primitive::Lt => Op::Lt,
            uiua::Primitive::Le => Op::Le,
            uiua::Primitive::Gt => Op::Gt,
            uiua::Primitive::Ge => Op::Ge,
            uiua::Primitive::Add => Op::Add,
            uiua::Primitive::Sub => Op::Sub,
            uiua::Primitive::Mul => Op::Mul,
            uiua::Primitive::Div => Op::Div,
            uiua::Primitive::Mod => Op::Mod,
            uiua::Primitive::Pow => Op::Pow,
            uiua::Primitive::Log => Op::Log,
            uiua::Primitive::Min => Op::Min,
            uiua::Primitive::Max => Op::Max,
            uiua::Primitive::Atan => Op::Atan,
            uiua::Primitive::Complex => Op::Complex,
            uiua::Primitive::Len => Op::Len,
            uiua::Primitive::Shape => Op::Shape,
            uiua::Primitive::Range => Op::Range,
            uiua::Primitive::First => Op::First,
            uiua::Primitive::Reverse => Op::Reverse,
            uiua::Primitive::Deshape => Op::Deshape,
            uiua::Primitive::Fix => Op::Fix,
            uiua::Primitive::Bits => Op::Bits,
            uiua::Primitive::Transpose => Op::Transpose,
            uiua::Primitive::Rise => Op::Rise,
            uiua::Primitive::Fall => Op::Fall,
            uiua::Primitive::Where => Op::Where,
            uiua::Primitive::Classify => Op::Classify,
            uiua::Primitive::Deduplicate => Op::Deduplicate,
            uiua::Primitive::Unique => Op::Unique,
            uiua::Primitive::Box => Op::Box,
            uiua::Primitive::Parse => Op::Parse,
            uiua::Primitive::Match => Op::Match,
            uiua::Primitive::Couple => Op::Couple,
            uiua::Primitive::Join => Op::Join,
            uiua::Primitive::Select => Op::Select,
            uiua::Primitive::Pick => Op::Pick,
            uiua::Primitive::Reshape => Op::Reshape,
            uiua::Primitive::Rerank => Op::Rerank,
            uiua::Primitive::Take => Op::Take,
            uiua::Primitive::Drop => Op::Drop,
            uiua::Primitive::Rotate => Op::Rotate,
            uiua::Primitive::Windows => Op::Windows,
            uiua::Primitive::Keep => Op::Keep,
            uiua::Primitive::Find => Op::Find,
            uiua::Primitive::Mask => Op::Mask,
            uiua::Primitive::Member => Op::Member,
            uiua::Primitive::IndexOf => Op::IndexOf,
            uiua::Primitive::Coordinate => Op::Coordinate,
            uiua::Primitive::Reduce => Op::Reduce,
            uiua::Primitive::Fold => Op::Fold,
            uiua::Primitive::Scan => Op::Scan,
            uiua::Primitive::Each => Op::Each,
            uiua::Primitive::Rows => Op::Rows,
            uiua::Primitive::Table => Op::Table,
            uiua::Primitive::Inventory => Op::Inventory,
            uiua::Primitive::Repeat => Op::Repeat,
            uiua::Primitive::Group => Op::Group,
            uiua::Primitive::Partition => Op::Partition,
            uiua::Primitive::Content => Op::Content,
            uiua::Primitive::Gap => Op::Gap,
            uiua::Primitive::Dip => Op::Dip,
            uiua::Primitive::On => Op::On,
            uiua::Primitive::By => Op::By,
            uiua::Primitive::Both => Op::Both,
            uiua::Primitive::Bind => Op::Bind,
            uiua::Primitive::Un => Op::Un,
            uiua::Primitive::SetInverse => Op::SetInverse,
            uiua::Primitive::SetUnder => Op::SetUnder,
            uiua::Primitive::Under => Op::Under,
            uiua::Primitive::Fork => Op::Fork,
            uiua::Primitive::Cascade => Op::Cascade,
            uiua::Primitive::Bracket => Op::Bracket,
            uiua::Primitive::All => Op::All,
            uiua::Primitive::Do => Op::Do,
            uiua::Primitive::Fill => Op::Fill,
            uiua::Primitive::Try => Op::Try,
            uiua::Primitive::Assert => Op::Assert,
            uiua::Primitive::This => Op::This,
            uiua::Primitive::Recur => Op::Recur,
            uiua::Primitive::Rand => Op::Rand,
            uiua::Primitive::Memo => Op::Memo,
            uiua::Primitive::Comptime => Op::Comptime,
            uiua::Primitive::Spawn => Op::Spawn,
            uiua::Primitive::Pool => Op::Pool,
            uiua::Primitive::Wait => Op::Wait,
            uiua::Primitive::Send => Op::Send,
            uiua::Primitive::Recv => Op::Recv,
            uiua::Primitive::TryRecv => Op::TryRecv,
            uiua::Primitive::Gen => Op::Gen,
            uiua::Primitive::Deal => Op::Deal,
            uiua::Primitive::Regex => Op::Regex,
            uiua::Primitive::Utf => Op::Utf,
            uiua::Primitive::Tag => Op::Tag,
            uiua::Primitive::Type => Op::Type,
            uiua::Primitive::Now => Op::Now,
            uiua::Primitive::Eta => Op::Eta,
            uiua::Primitive::Pi => Op::Pi,
            uiua::Primitive::Tau => Op::Tau,
            uiua::Primitive::Infinity => Op::Infinity,
            uiua::Primitive::Map => Op::Map,
            uiua::Primitive::Insert => Op::Insert,
            uiua::Primitive::Has => Op::Has,
            uiua::Primitive::Get => Op::Get,
            uiua::Primitive::Remove => Op::Remove,
            uiua::Primitive::Shapes => Op::Shapes,
            uiua::Primitive::Types => Op::Types,
            uiua::Primitive::Stack => Op::Stack,
            uiua::Primitive::Trace => Op::Trace,
            uiua::Primitive::Dump => Op::Dump,
            uiua::Primitive::Stringify => Op::Stringify,
            uiua::Primitive::Quote => Op::Quote,
            uiua::Primitive::Sig => Op::Sig,
            uiua::Primitive::Csv => Op::Csv,
            uiua::Primitive::Repr => Op::Repr,
            uiua::Primitive::Sys(_) => unimplemented!(),
        }
    }
}


impl From<Op> for uiua::Primitive {
    fn from(value: Op) -> Self {
        match value {
            Op::Push => unimplemented!(),
            Op::Dup => uiua::Primitive::Dup,
            Op::Over => uiua::Primitive::Over,
            Op::Flip => uiua::Primitive::Flip,
            Op::Pop => uiua::Primitive::Pop,
            Op::Identity => uiua::Primitive::Identity,
            Op::Not => uiua::Primitive::Not,
            Op::Sign => uiua::Primitive::Sign,
            Op::Neg => uiua::Primitive::Neg,
            Op::Abs => uiua::Primitive::Abs,
            Op::Sqrt => uiua::Primitive::Sqrt,
            Op::Sin => uiua::Primitive::Sin,
            Op::Floor => uiua::Primitive::Floor,
            Op::Ceil => uiua::Primitive::Ceil,
            Op::Round => uiua::Primitive::Round,
            Op::Eq => uiua::Primitive::Eq,
            Op::Ne => uiua::Primitive::Ne,
            Op::Lt => uiua::Primitive::Lt,
            Op::Le => uiua::Primitive::Le,
            Op::Gt => uiua::Primitive::Gt,
            Op::Ge => uiua::Primitive::Ge,
            Op::Add => uiua::Primitive::Add,
            Op::Sub => uiua::Primitive::Sub,
            Op::Mul => uiua::Primitive::Mul,
            Op::Div => uiua::Primitive::Div,
            Op::Mod => uiua::Primitive::Mod,
            Op::Pow => uiua::Primitive::Pow,
            Op::Log => uiua::Primitive::Log,
            Op::Min => uiua::Primitive::Min,
            Op::Max => uiua::Primitive::Max,
            Op::Atan => uiua::Primitive::Atan,
            Op::Complex => uiua::Primitive::Complex,
            Op::Len => uiua::Primitive::Len,
            Op::Shape => uiua::Primitive::Shape,
            Op::Range => uiua::Primitive::Range,
            Op::First => uiua::Primitive::First,
            Op::Reverse => uiua::Primitive::Reverse,
            Op::Deshape => uiua::Primitive::Deshape,
            Op::Fix => uiua::Primitive::Fix,
            Op::Bits => uiua::Primitive::Bits,
            Op::Transpose => uiua::Primitive::Transpose,
            Op::Rise => uiua::Primitive::Rise,
            Op::Fall => uiua::Primitive::Fall,
            Op::Where => uiua::Primitive::Where,
            Op::Classify => uiua::Primitive::Classify,
            Op::Deduplicate => uiua::Primitive::Deduplicate,
            Op::Unique => uiua::Primitive::Unique,
            Op::Box => uiua::Primitive::Box,
            Op::Parse => uiua::Primitive::Parse,
            Op::Match => uiua::Primitive::Match,
            Op::Couple => uiua::Primitive::Couple,
            Op::Join => uiua::Primitive::Join,
            Op::Select => uiua::Primitive::Select,
            Op::Pick => uiua::Primitive::Pick,
            Op::Reshape => uiua::Primitive::Reshape,
            Op::Rerank => uiua::Primitive::Rerank,
            Op::Take => uiua::Primitive::Take,
            Op::Drop => uiua::Primitive::Drop,
            Op::Rotate => uiua::Primitive::Rotate,
            Op::Windows => uiua::Primitive::Windows,
            Op::Keep => uiua::Primitive::Keep,
            Op::Find => uiua::Primitive::Find,
            Op::Mask => uiua::Primitive::Mask,
            Op::Member => uiua::Primitive::Member,
            Op::IndexOf => uiua::Primitive::IndexOf,
            Op::Coordinate => uiua::Primitive::Coordinate,
            Op::Reduce => uiua::Primitive::Reduce,
            Op::Fold => uiua::Primitive::Fold,
            Op::Scan => uiua::Primitive::Scan,
            Op::Each => uiua::Primitive::Each,
            Op::Rows => uiua::Primitive::Rows,
            Op::Table => uiua::Primitive::Table,
            Op::Inventory => uiua::Primitive::Inventory,
            Op::Repeat => uiua::Primitive::Repeat,
            Op::Group => uiua::Primitive::Group,
            Op::Partition => uiua::Primitive::Partition,
            Op::Content => uiua::Primitive::Content,
            Op::Gap => uiua::Primitive::Gap,
            Op::Dip => uiua::Primitive::Dip,
            Op::On => uiua::Primitive::On,
            Op::By => uiua::Primitive::By,
            Op::Both => uiua::Primitive::Both,
            Op::Bind => uiua::Primitive::Bind,
            Op::Un => uiua::Primitive::Un,
            Op::SetInverse => uiua::Primitive::SetInverse,
            Op::SetUnder => uiua::Primitive::SetUnder,
            Op::Under => uiua::Primitive::Under,
            Op::Fork => uiua::Primitive::Fork,
            Op::Cascade => uiua::Primitive::Cascade,
            Op::Bracket => uiua::Primitive::Bracket,
            Op::All => uiua::Primitive::All,
            Op::Do => uiua::Primitive::Do,
            Op::Fill => uiua::Primitive::Fill,
            Op::Try => uiua::Primitive::Try,
            Op::Assert => uiua::Primitive::Assert,
            Op::This => uiua::Primitive::This,
            Op::Recur => uiua::Primitive::Recur,
            Op::Rand => uiua::Primitive::Rand,
            Op::Memo => uiua::Primitive::Memo,
            Op::Comptime => uiua::Primitive::Comptime,
            Op::Spawn => uiua::Primitive::Spawn,
            Op::Pool => uiua::Primitive::Pool,
            Op::Wait => uiua::Primitive::Wait,
            Op::Send => uiua::Primitive::Send,
            Op::Recv => uiua::Primitive::Recv,
            Op::TryRecv => uiua::Primitive::TryRecv,
            Op::Gen => uiua::Primitive::Gen,
            Op::Deal => uiua::Primitive::Deal,
            Op::Regex => uiua::Primitive::Regex,
            Op::Utf => uiua::Primitive::Utf,
            Op::Tag => uiua::Primitive::Tag,
            Op::Type => uiua::Primitive::Type,
            Op::Now => uiua::Primitive::Now,
            Op::Eta => uiua::Primitive::Eta,
            Op::Pi => uiua::Primitive::Pi,
            Op::Tau => uiua::Primitive::Tau,
            Op::Infinity => uiua::Primitive::Infinity,
            Op::Map => uiua::Primitive::Map,
            Op::Insert => uiua::Primitive::Insert,
            Op::Has => uiua::Primitive::Has,
            Op::Get => uiua::Primitive::Get,
            Op::Remove => uiua::Primitive::Remove,
            Op::Shapes => uiua::Primitive::Shapes,
            Op::Types => uiua::Primitive::Types,
            Op::Stack => uiua::Primitive::Stack,
            Op::Trace => uiua::Primitive::Trace,
            Op::Dump => uiua::Primitive::Dump,
            Op::Stringify => uiua::Primitive::Stringify,
            Op::Quote => uiua::Primitive::Quote,
            Op::Sig => uiua::Primitive::Sig,
            Op::Csv => uiua::Primitive::Csv,
            Op::Repr => uiua::Primitive::Repr,
        }
    }
}

