use crate::delimited::*;
use crate::ident::*;
use crate::lifetime::*;
use crate::pointer::*;
use crate::sep::*;

#[repr(u8)]
pub enum TypeRepr<'a, P: Pointer> {
	Placeholder(P::Boxed<'a, [crate::Span]>),
}

#[repr(C)]
pub struct Type<'a, P: Pointer> {
	pub repr: TypeRepr<'a, P>,
}

as_ref!(Type, TypeBox, TypeRef, TypeDyn);
as_ref!(TypeRepr, TypeReprBox, TypeReprRef, TypeReprDyn);

derive_enum!(TypeRepr, Placeholder);
derive_struct!(Type, repr);
