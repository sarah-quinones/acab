use super::*;
use int_literal::DecLiteral;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum E {
	Lowercase,
	Uppercase,
}
reborrow_copy!(E);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Sign {
	Plus,
	Minus,
}
reborrow_copy!(Sign);

pub struct Exponent<'a, P: Pointer> {
	pub e: E,
	pub sign: Option<Sign>,
	pub leading_underscores: usize,
	pub digits: P::Boxed<'a, [DecDigit]>,
}
as_ref!(Exponent, ExponentBox, ExponentRef, ExponentDyn);
derive_struct!(Exponent, e, sign, leading_underscores, digits);

pub struct FloatNoExp<'a, P: Pointer> {
	pub int: DecLiteral<'a, P>,
	pub frac: DecLiteral<'a, P>,
	pub suffix: SuffixNoExp<'a, P>,
}
as_ref!(FloatNoExp, FloatNoExpBox, FloatNoExpRef, FloatNoExpDyn);
derive_struct!(FloatNoExp, int, frac, suffix);

pub struct FloatExp<'a, P: Pointer> {
	pub int: DecLiteral<'a, P>,
	pub frac: Option<DecLiteral<'a, P>>,
	pub exp: Exponent<'a, P>,
	pub suffix: Suffix<'a, P>,
}
as_ref!(FloatExp, FloatExpBox, FloatExpRef, FloatExpDyn);
derive_struct!(FloatExp, int, frac, exp, suffix);

pub enum FloatLiteral<'a, P: Pointer> {
	Int(DecLiteral<'a, P>),
	NoExp(FloatNoExp<'a, P>),
	Exp(FloatExp<'a, P>),
}
as_ref!(FloatLiteral, FloatLiteralBox, FloatLiteralRef, FloatLiteralDyn);
derive_enum!(FloatLiteral, Int, NoExp, Exp);
