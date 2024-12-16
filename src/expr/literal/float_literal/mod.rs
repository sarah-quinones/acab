use super::*;
use int_literal::DecLiteral;

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum E {
		Lowercase,
		Uppercase,
	}
);
reborrow_copy!(E);

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum Sign {
		Plus,
		Minus,
	}
);
reborrow_copy!(Sign);

#[repr(C)]
pub struct Exponent<'a, P: Pointer> {
	pub e: E,
	pub sign: Option<Sign>,
	pub leading_underscores: usize,
	pub digits: DecLiteral<'a, P>,
}
as_ref!(Exponent, ExponentBox, ExponentRef, ExponentDyn);
derive_struct!(Exponent, e, sign, leading_underscores, digits);

#[repr(C)]
pub struct FloatNoExp<'a, P: Pointer> {
	pub int: DecLiteral<'a, P>,
	pub frac: DecLiteral<'a, P>,
	pub suffix: Option<SuffixNoExp<'a, P>>,
}
as_ref!(FloatNoExp, FloatNoExpBox, FloatNoExpRef, FloatNoExpDyn);
derive_struct!(FloatNoExp, int, frac, suffix);

#[repr(C)]
pub struct FloatExp<'a, P: Pointer> {
	pub int: DecLiteral<'a, P>,
	pub frac: Option<DecLiteral<'a, P>>,
	pub exp: Exponent<'a, P>,
	pub suffix: Option<Suffix<'a, P>>,
}
as_ref!(FloatExp, FloatExpBox, FloatExpRef, FloatExpDyn);
derive_struct!(FloatExp, int, frac, exp, suffix);

#[repr(u8)]
pub enum FloatLiteralRepr<'a, P: Pointer> {
	Int(DecLiteral<'a, P>),
	NoExp(FloatNoExp<'a, P>),
	Exp(FloatExp<'a, P>),
}
as_ref!(FloatLiteralRepr, FloatLiteralReprBox, FloatLiteralReprRef, FloatLiteralReprDyn);
derive_enum!(FloatLiteralRepr, Int, NoExp, Exp);

#[repr(C)]
pub struct FloatLiteral<'a, P: Pointer> {
	pub repr: FloatLiteralRepr<'a, P>,
	pub span: Span,
}
as_ref!(FloatLiteral, FloatLiteralBox, FloatLiteralRef, FloatLiteralDyn);
derive_struct!(FloatLiteral, repr, span);

impl Spanned for FloatLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
