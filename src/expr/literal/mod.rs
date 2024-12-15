use crate::Span;
use crate::ident::IdentOrKeyword;
use crate::pointer::*;

pub mod char_literal;
pub mod float_literal;
pub mod int_literal;
pub mod str_literal;

use char_literal::uint;

#[repr(C)]
pub struct Suffix<'a, P: Pointer> {
	pub ident: IdentOrKeyword<'a, P>,
}
as_ref!(Suffix, SuffixBox, SuffixRef, SuffixDyn);
derive_struct!(Suffix, ident);

#[repr(C)]
pub struct SuffixNoExp<'a, P: Pointer> {
	repr: Suffix<'a, P>,
}
as_ref!(SuffixNoExp, SuffixNoExpBox, SuffixNoExpRef, SuffixNoExpDyn);
derive_struct!(SuffixNoExp, repr);

impl SuffixNoExpDyn<'_> {
	pub const fn repr(&self) -> SuffixRef<'_> {
		self.rb().repr
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct BinDigit {
	c: uint<u8, 1>,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct OctDigit {
	c: uint<u8, 3>,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct DecDigit {
	c: u8,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct HexDigit {
	// top bit is 1 if uppercase
	c: uint<u8, 5>,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct AsciiDigit {
	c: uint<u8, 7>,
}
reborrow_copy!(BinDigit);
reborrow_copy!(DecDigit);
reborrow_copy!(OctDigit);
reborrow_copy!(HexDigit);
reborrow_copy!(AsciiDigit);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct NonZero<T: Copy> {
	inner: T,
}
reborrow_copy!(NonZero<T> where T: Copy);

impl OctDigit {
	pub const fn new(c: u8) -> Option<Self> {
		match uint::new(c) {
			Some(c) => Some(Self { c }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0o10`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		Self {
			c: unsafe { uint::new_unchecked(c) },
		}
	}

	pub const fn as_byte(self) -> u8 {
		self.c.get()
	}

	pub const fn non_zero(self) -> Option<NonZero<Self>> {
		if self.c.get() == 0 { None } else { Some(NonZero { inner: self }) }
	}
}

impl BinDigit {
	pub const fn new(c: u8) -> Option<Self> {
		match uint::new(c) {
			Some(c) => Some(Self { c }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0b10`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		Self {
			c: unsafe { uint::new_unchecked(c) },
		}
	}

	pub const fn as_byte(self) -> u8 {
		self.c.get()
	}

	pub const fn non_zero(self) -> Option<NonZero<Self>> {
		if self.c.get() == 0 { None } else { Some(NonZero { inner: self }) }
	}
}
impl DecDigit {
	pub const fn new(c: u8) -> Option<Self> {
		if c < 10 { Some(Self { c }) } else { None }
	}

	/// # Safety
	/// `c < 10`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		debug_assert!(c < 10);
		Self { c }
	}

	pub const fn as_byte(self) -> u8 {
		self.c
	}

	pub const fn non_zero(self) -> Option<NonZero<Self>> {
		if self.c == 0 { None } else { Some(NonZero { inner: self }) }
	}
}

impl AsciiDigit {
	pub const fn new(c: u8) -> Option<Self> {
		match uint::new(c) {
			Some(c) => Some(Self { c }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0x7F`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		Self {
			c: unsafe { uint::new_unchecked(c) },
		}
	}

	pub const fn as_byte(self) -> u8 {
		self.c.get()
	}

	pub const fn non_zero(self) -> Option<NonZero<Self>> {
		if self.c.get() == 0 { None } else { Some(NonZero { inner: self }) }
	}
}

impl HexDigit {
	pub const fn new_lowercase(c: u8) -> Option<Self> {
		match uint::<u8, 4>::new(c) {
			Some(_) => Some(unsafe { Self::new_lowercase_unchecked(c) }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0x10`
	pub const unsafe fn new_lowercase_unchecked(c: u8) -> Self {
		unsafe {
			// for debug asserts
			_ = uint::<u8, 4>::new_unchecked(c);

			Self { c: uint::new_unchecked(c) }
		}
	}

	pub const fn new_uppercase(c: u8) -> Option<Self> {
		match uint::<u8, 4>::new(c) {
			Some(_) => Some(unsafe { Self::new_uppercase_unchecked(c) }),
			None => None,
		}
	}

	pub const fn from_byte_literal(byte: u8) -> Option<Self> {
		match byte {
			b'0'..=b'9' => Some(unsafe {
				Self {
					c: uint::new_unchecked(byte - b'0'),
				}
			}),
			b'a'..=b'f' => Some(unsafe {
				Self {
					c: uint::new_unchecked(byte - b'a' + 0xA),
				}
			}),
			b'A'..=b'F' => Some(unsafe {
				Self {
					c: uint::new_unchecked((byte - b'A' + 0xA) | (1u8 << 4)),
				}
			}),
			_ => None,
		}
	}

	/// # Safety
	/// `c < 0x10`
	pub const unsafe fn new_uppercase_unchecked(c: u8) -> Self {
		unsafe {
			// for debug asserts
			_ = uint::<u8, 4>::new_unchecked(c);

			if c < 0xA {
				Self { c: uint::new_unchecked(c) }
			} else {
				Self {
					c: uint::new_unchecked(c | (1u8 << 4)),
				}
			}
		}
	}

	pub const fn as_byte(self) -> u8 {
		self.c.get() & 0xF
	}

	pub const fn is_uppercase(self) -> bool {
		(self.c.get() >> 4) == 1
	}

	pub const fn non_zero(self) -> Option<NonZero<Self>> {
		if self.c.get() == 0 { None } else { Some(NonZero { inner: self }) }
	}
}

pub trait ExcludeList: Copy + Clone {
	type Type: 'static + Copy;
	const EXCLUDED_VALUES: &'static [Self::Type];
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Exclude<E: ExcludeList<Type = T>, T: Copy = <E as ExcludeList>::Type> {
	inner: E::Type,
}
reborrow_copy!(Exclude<E> where E: ExcludeList);

impl<E: ExcludeList<Type: core::fmt::Debug>> core::fmt::Debug for Exclude<E> {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		self.inner.fmt(f)
	}
}

impl<E: ExcludeList> Exclude<E> {
	/// # Safety
	/// [`Exclude::EXCLUDED_VALUES`] must not contain `c`.
	pub const unsafe fn new_unchecked(c: E::Type) -> Self {
		Self { inner: c }
	}

	pub const fn get(self) -> E::Type {
		self.inner
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct True {
	pub span: Span,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct False {
	pub span: Span,
}
reborrow_copy!(True);
reborrow_copy!(False);

impl True {
	pub const fn span(&self) -> Span {
		self.span
	}
}
impl False {
	pub const fn span(&self) -> Span {
		self.span
	}
}

#[non_exhaustive]
pub enum Literal<'a, P: Pointer> {
	Char(char_literal::CharLiteral<'a, P>),
	Byte(char_literal::ByteLiteral<'a, P>),

	Str(str_literal::StrLiteral<'a, P>),
	RawStr(str_literal::RawStrLiteral<'a, P>),
	ByteStr(str_literal::ByteStrLiteral<'a, P>),
	RawByteStr(str_literal::RawByteStrLiteral<'a, P>),
	CStr(str_literal::CStrLiteral<'a, P>),
	RawCStr(str_literal::RawCStrLiteral<'a, P>),

	Int(int_literal::IntLiteral<'a, P>),
	Float(float_literal::FloatLiteral<'a, P>),

	True(True),
	False(False),
}
as_ref!(Literal, LiteralBox, LiteralRef, LiteralDyn);
derive_enum!(
	Literal, Char, Byte, Str, RawStr, ByteStr, RawByteStr, CStr, RawCStr, Int, Float, True, False,
);

impl Spanned for LiteralDyn<'_> {
	fn span(&self) -> Span {
		match self.rb() {
			Literal::Char(lit) => lit.span(),
			Literal::Byte(lit) => lit.span(),
			Literal::Str(lit) => lit.span(),
			Literal::RawStr(lit) => lit.span(),
			Literal::ByteStr(lit) => lit.span(),
			Literal::RawByteStr(lit) => lit.span(),
			Literal::CStr(lit) => lit.span(),
			Literal::RawCStr(lit) => lit.span(),
			Literal::Int(lit) => lit.span(),
			Literal::Float(lit) => lit.span(),
			Literal::True(lit) => lit.span(),
			Literal::False(lit) => lit.span(),
		}
	}
}

#[cfg(feature = "alloc")]
mod alloc {
	use crate::Edition;

	use super::*;
	use ::alloc::string::ToString;
	use char_literal::{AsciiDigits, AsciiEscape, Char, CharLiteral, CharVerbatim, QuoteEscape, UnicodeEscape};

	impl LiteralBox {
		pub fn from_token(edition: Edition, token: &proc::Literal) -> Self {
			let span = token.span().into();
			let mut repr = &*token.to_string();

			if repr.starts_with('\'') {
				// char literal
				repr = &repr[1..];
				let Some(c) = repr.chars().next() else { panic!() };
				repr = &repr[c.len_utf8()..];
				let c = if c != '\\' {
					Char::Verbatim(CharVerbatim::new(c).unwrap())
				} else {
					let c = repr.chars().next().unwrap();
					repr = &repr[c.len_utf8()..];
					match c {
						'\'' => Char::Quote(QuoteEscape::Single),
						'\"' => Char::Quote(QuoteEscape::Double),
						'n' => Char::Ascii(AsciiEscape::LineFeed),
						'r' => Char::Ascii(AsciiEscape::CarriageReturn),
						't' => Char::Ascii(AsciiEscape::HorizontalTabulation),
						'\\' => Char::Ascii(AsciiEscape::Backslash),
						'\0' => Char::Ascii(AsciiEscape::Null),
						'x' => {
							let msb = repr.as_bytes()[0];
							let lsb = repr.as_bytes()[1];
							repr = &repr[2..];

							let msb = OctDigit::new(msb - b'0').unwrap();
							let lsb = HexDigit::from_byte_literal(lsb).unwrap();
							Char::Ascii(AsciiEscape::Digits(AsciiDigits { msb, lsb }))
						},
						'u' => {
							repr = &repr[1..];
							let mut bytes = repr.as_bytes();

							let mut chars = StackVec::new([(HexDigit::from_byte_literal(bytes[0]).unwrap(), 0)]);
							bytes = &bytes[1..];
							repr = &repr[1..];

							while bytes[0] != b'}' {
								let b = bytes[0];
								bytes = &bytes[1..];
								repr = &repr[1..];

								if b == b'_' {
									chars.last_mut().unwrap().1 += 1;
								} else {
									chars.push((HexDigit::from_byte_literal(b).unwrap(), 0));
								}
							}
							repr = &repr[1..];

							Char::Unicode(UnicodeEscape { chars })
						},
						_ => panic!(),
					}
				};
				repr = &repr[1..];

				return Self::Char(CharLiteral {
					char: c,
					suffix: if repr.is_empty() {
						None
					} else {
						Some(Suffix {
							ident: IdentOrKeyword::new(edition, repr.to_string().into_boxed_str()).unwrap(),
						})
					},
					span,
				});
			}
			if repr.starts_with("b'") {
				// byte literal
			}

			todo!()
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::Edition;
	use char_literal::*;
	use proc::TokenTree;

	#[test]
	fn test_char_literal() {
		fn imp() -> Option<()> {
			let edition = Edition::Edition2021;

			let TokenTree::Literal(c) = quote!('c').into_iter().next()? else {
				panic!()
			};
			assert_eq!(
				*Literal::from_token(edition, &c).rb(),
				Literal::Char(CharLiteral {
					char: Char::Verbatim(CharVerbatim::new('c')?),
					suffix: None,
					span: Span::call_site()
				})
			);

			let TokenTree::Literal(c) = quote!('c'abc).into_iter().next()? else {
				panic!()
			};
			assert_eq!(
				*Literal::from_token(edition, &c).rb(),
				Literal::Char(CharLiteral {
					char: Char::Verbatim(CharVerbatim::new('c')?),
					suffix: Some(Suffix {
						ident: IdentOrKeyword::new(edition, "abc").ok()?
					}),
					span: Span::call_site()
				})
			);

			let TokenTree::Literal(c) = quote!('\u{0__0a_1B}').into_iter().next()? else {
				panic!()
			};

			assert_eq!(
				*Literal::from_token(edition, &c).rb(),
				Literal::Char(CharLiteral {
					char: Char::Unicode(UnicodeEscape {
						chars: {
							let mut v = StackVec::new([(HexDigit::new_lowercase(0x0)?, 2)]);
							v.push((HexDigit::new_lowercase(0x0)?, 0));
							v.push((HexDigit::new_lowercase(0xA)?, 1));
							v.push((HexDigit::new_lowercase(0x1)?, 0));
							v.push((HexDigit::new_uppercase(0xB)?, 0));
							v
						}
					}),
					suffix: None,
					span: Span::call_site()
				})
			);
			Some(())
		}
		imp().unwrap();
	}
}
