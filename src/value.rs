use ustr::Ustr;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(Ustr),
    Boolean(bool),
    #[default]
    Undefined,
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        if value.fract() < f64::EPSILON {
            Self::Int(value as i64)
        } else {
            Self::Float(value)
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<Ustr> for Value {
    fn from(value: Ustr) -> Self {
        Self::String(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::String(value) => !value.is_empty(),
            Value::Int(value) => *value != 0,
            Value::Float(value) => value.is_normal(),
            Value::Boolean(value) => *value,
            Value::Undefined => false,
        }
    }

    pub fn as_boolean(self) -> Self {
        Self::Boolean(self.is_truthy())
    }

    pub fn as_string(self) -> Self {
        Self::String(match self {
            Self::String(value) => value,
            Self::Int(value) => format!("{value}").into(),
            Self::Float(value) => format!("{value}").into(),
            Self::Boolean(value) => format!("{value}").into(),
            Self::Undefined => "undefined".to_string().into(),
        })
    }

    // Number may be NaN
    pub fn as_number(self) -> Self {
        match self {
            Self::String(value) => value.parse::<f64>().unwrap_or(f64::NAN).into(),
            Self::Int(value) => value.into(),
            Self::Float(value) => value.into(),
            Self::Boolean(true) => 1.into(),
            Self::Boolean(false) => 0.into(),
            Self::Undefined => 0.into(),
        }
    }
}
