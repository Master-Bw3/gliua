use std::ops::Deref;

use rustler::{Env, NifMap, NifStruct, NifTaggedEnum, ResourceArc, Term};
use uiua::{Uiua, UiuaError, Value};

use crate::uiua_env::ExUiua;

pub(crate) struct ValueRef(pub Value);

#[derive(NifStruct)]
#[module = "uiua_value"]
pub(crate) struct ExValue {
    pub resource: ResourceArc<ValueRef>,
}

impl ExValue {
    pub fn new(value: Value) -> Self {
        Self {
            resource: ResourceArc::new(ValueRef::new(value)),
        }
    }

    pub fn value(&self) -> &Value {
        &self.resource.0
    }
}

impl ValueRef {
    pub fn new(value: Value) -> Self {
        Self(value)
    }
}

impl Deref for ExValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.resource.0
    }
}

unsafe impl Send for ValueRef {}
unsafe impl Sync for ValueRef {}

#[rustler::nif]
pub(crate) fn to_string(value: ExValue) -> String {
    value.to_string()
}

#[rustler::nif]
pub(crate) fn rows(value: ExValue) -> Vec<ExValue> {
    value.rows().map(ExValue::new).collect()
}

#[derive(NifTaggedEnum, Debug)]
pub(crate) enum DecodeError {
    DecodeError(String, String),
}

#[rustler::nif]
pub(crate) fn as_int(value: ExValue) -> Result<isize, DecodeError> {
    let expected = String::from("int");
    match value.value() {
        Value::Num(nums) => {
            if nums.rank() > 0 {
                return Err(DecodeError::DecodeError(
                    expected,
                    String::from("number array with rank > 0"),
                ));
            }
            let num = nums.as_scalar().unwrap();
            if num.is_infinite() {
                Err(DecodeError::DecodeError(expected, String::from("infinity")))
            } else if num.is_nan() {
                Err(DecodeError::DecodeError(expected, String::from("NaN")))
            } else if num.fract() != 0.0 {
                Err(DecodeError::DecodeError(expected, String::from("float")))
            } else {
                Ok(*num as isize)
            }
        }
        Value::Byte(bytes) => {
            if bytes.rank() > 0 {
                return Err(DecodeError::DecodeError(
                    expected,
                    String::from("Byte array with rank > 0"),
                ));
            } else {
                Ok(*bytes.as_scalar().unwrap() as isize)
            }
        }
        value => Err(DecodeError::DecodeError(
            expected,
            format!("{} array", value.type_name()),
        )),
    }
}

#[rustler::nif]
pub(crate) fn as_float(value: ExValue) -> Result<f64, DecodeError> {
    let expected = String::from("float");
    match value.value() {
        Value::Num(nums) => {
            if nums.rank() > 0 {
                Err(DecodeError::DecodeError(
                    expected,
                    String::from("number array with rank > 0"),
                ))
            } else {
                Ok(*nums.as_scalar().unwrap())
            }
        }

        Value::Byte(bytes) => {
            if bytes.rank() > 0 {
                Err(DecodeError::DecodeError(
                    expected,
                    String::from("byte array with rank > 0"),
                ))
            } else {
                Ok(*bytes.as_scalar().unwrap() as f64)
            }
        }
        value => Err(DecodeError::DecodeError(
            expected,
            format!("{} array", value.type_name()),
        )),
    }
}

#[rustler::nif]
pub(crate) fn as_bool(value: ExValue) -> Result<bool, DecodeError> {
    let expected = String::from("bool");
    match value.value() {
        Value::Num(nums) => {
            if nums.rank() > 0 {
                return Err(DecodeError::DecodeError(
                    expected,
                    String::from("number array with rank > 0"),
                ));
            }
            let num = *nums.as_scalar().unwrap();
            if num == 0.0 {
                Ok(false)
            } else if num == 1.0 {
                Ok(true)
            } else {
                Err(DecodeError::DecodeError(expected, String::from("number")))
            }
        }
        Value::Byte(bytes) => {
            if bytes.rank() > 0 {
                return Err(DecodeError::DecodeError(
                    expected,
                    String::from("byte array with rank > 0"),
                ));
            }
            let num = *bytes.as_scalar().unwrap();
            if num == 0 {
                Ok(false)
            } else if num == 1 {
                Ok(true)
            } else {
                Err(DecodeError::DecodeError(expected, String::from("number")))
            }
        }
        value => Err(DecodeError::DecodeError(
            expected,
            format!("{} array", value.type_name()),
        )),
    }
}

#[rustler::nif]
pub(crate) fn as_char(value: ExValue) -> Result<Character, DecodeError> {
    let expected = String::from("character");
    match value.value() {
        Value::Char(chars) => {
            if chars.rank() > 0 {
                Err(DecodeError::DecodeError(
                    expected,
                    String::from("character array with rank > 0"),
                ))
            } else {
                Ok(Character::Character(chars.as_scalar().unwrap().to_string()))
            }
        }
        value => Err(DecodeError::DecodeError(
            expected,
            format!("{} array", value.type_name()),
        )),
    }
}

#[rustler::nif]
pub(crate) fn as_complex(value: ExValue) -> Result<(f64, f64), DecodeError> {
    let expected = String::from("complex");
    match value.value() {
        Value::Complex(complex) => {
            if complex.rank() > 0 {
                Err(DecodeError::DecodeError(
                    expected,
                    String::from("complex array with rank > 0"),
                ))
            } else {
                let complex = complex.as_scalar().unwrap();
                Ok((complex.re, complex.im))
            }
        }
        value => Err(DecodeError::DecodeError(
            expected,
            format!("{} array", value.type_name()),
        )),
    }
}

#[derive(NifTaggedEnum)]
pub(crate) enum Character {
    Character(String)
}