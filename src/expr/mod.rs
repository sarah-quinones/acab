use crate::pointer::*;

#[repr(C)]
pub enum Expr<'a, P: Pointer> {
	Unary(P::Boxed<'a, Self>),
	Binary(P::Boxed<'a, Self>),
}

as_ref!(Expr, ExprBox, ExprRef, ExprDyn);
derive_enum!(Expr, Unary, Binary);

pub mod literal;
