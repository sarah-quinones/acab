use crate::delimited::*;
use crate::ident::*;
use crate::lifetime::*;
use crate::pointer::*;
use crate::sep::*;
use crate::ty::*;

#[repr(u8)]
pub enum ExprRepr<'a, P: Pointer> {
	Lit(literal::LiteralExpr<'a, P>),
}
as_ref!(ExprRepr, ExprReprBox, ExprReprRef, ExprReprDyn);
derive_enum!(ExprRepr, Lit);

#[repr(C)]
pub struct Expr<'a, P: Pointer> {
	pub repr: P::Boxed<'a, ExprRepr<'a, P>>,
}
as_ref!(Expr, ExprBox, ExprRef, ExprDyn);
derive_struct!(Expr, repr);

pub mod literal;
pub mod path;
