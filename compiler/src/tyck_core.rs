/// A type of a program value
struct Value;
/// A type that represents a value that is _read_ from context when the value is used.
struct Use;

/// The general strategy is to create a `Value` type for each expression and then a `Use` type for
/// each expression operand and then unify these
struct TyCkCore {}

impl TyCkCore {
    pub fn var(&mut self) -> (Value, Use) {
        todo!()
    }
}
