use crate::ty::{Use, Value};

/// The general strategy is to create a `Value` type for each expression and then a `Use` type for
/// each expression operand and then unify these. We create a constraint between the `Use` and the
/// `Value` and say that the `Value` flows to it's `Use`
pub struct TyCkCore {}

impl TyCkCore {
    pub fn var(&mut self) -> (Value, Use) {
        todo!()
    }

    pub fn bool(&mut self) -> Value {
        todo!()
    }
    pub fn bool_use(&mut self) -> Use {
        todo!()
    }

    pub fn int(&mut self) -> Value {
        todo!()
    }
    pub fn int_use(&mut self) -> Use {
        todo!()
    }

    pub fn float(&mut self) -> Value {
        todo!()
    }
    pub fn float_use(&mut self) -> Use {
        todo!()
    }

    /// The definition of a function essentially
    pub fn func(&mut self, args: Vec<Use>, ret: Value) -> Value {
        todo!()
    }
    /// Applications of a function since the return type is a Use so you can think of it being like
    /// used in an expression f(x) + 1.
    pub fn func_use(&mut self, args: Vec<Value>, ret: Use) -> Use {
        todo!()
    }

    /// Creates a flow relationship betwen a `Value` and a `Use` which essentially means that
    /// `Value` flows to the `Use` so it should be able to be used like a `Use`
    pub fn flow(&mut self, v: Value, u: Use) {
        todo!()
    }
}
