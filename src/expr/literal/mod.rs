use core::fmt::{self, Write};

use crate::Span;
use crate::ident::IdentOrKeyword;
use crate::pointer::*;

pub mod char_literal;
pub mod float_literal;
pub mod int_literal;
pub mod str_literal;

use char_literal::uint;
use float_literal::E;

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
	pub const fn ident(&self) -> SuffixRef<'_> {
		self.rb().repr
	}
}
impl<'a, P: Pointer> SuffixNoExp<'a, P> {
	pub const fn new(ident: IdentOrKeyword<'a, P>) -> Result<Self, (IdentOrKeyword<'a, P>, E)> {
		let first = ident.as_dyn().as_str().as_bytes().first().copied();
		if matches!(first, Some(b'e')) {
			return Err((ident, E::Lowercase));
		}
		if matches!(first, Some(b'E')) {
			return Err((ident, E::Uppercase));
		}
		Ok(Self { repr: Suffix { ident } })
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct BinDigit {
	c: uint<u8, 1>,
}
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct OctDigit {
	c: uint<u8, 3>,
}
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct DecDigit {
	c: u8,
}
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct HexDigit {
	// top bit is 1 if uppercase
	c: uint<u8, 5>,
}
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct AsciiDigit {
	c: uint<u8, 7>,
}
reborrow_copy!(BinDigit);
reborrow_copy!(DecDigit);
reborrow_copy!(OctDigit);
reborrow_copy!(HexDigit);
reborrow_copy!(AsciiDigit);

impl fmt::Debug for HexDigit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let c = if self.is_uppercase() {
			self.as_u8() - 0xA + b'A'
		} else if self.c.get() <= 9 {
			self.as_u8() + b'0'
		} else {
			self.as_u8() - 0xA + b'a'
		};
		f.write_str("0x")?;
		f.write_char(char::from_u32(c as u32).unwrap())
	}
}

impl fmt::Debug for OctDigit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let c = self.c.get() + b'0';
		f.write_str("0o")?;
		f.write_char(char::from_u32(c as u32).unwrap())
	}
}
impl fmt::Debug for BinDigit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let c = self.c.get() + b'0';
		f.write_str("0b")?;
		f.write_char(char::from_u32(c as u32).unwrap())
	}
}
impl fmt::Debug for DecDigit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let c = self.c + b'0';
		f.write_char(char::from_u32(c as u32).unwrap())
	}
}
impl fmt::Debug for AsciiDigit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		char::from_u32((self.c.get()) as u32).unwrap().fmt(f)
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct NonZero<T: Copy> {
	inner: T,
}
reborrow_copy!(NonZero<T> where T: Copy);

impl OctDigit {
	pub const OCT_0: Self = unsafe { Self::new_unchecked(0x0) };
	pub const OCT_1: Self = unsafe { Self::new_unchecked(0x1) };
	pub const OCT_2: Self = unsafe { Self::new_unchecked(0x2) };
	pub const OCT_3: Self = unsafe { Self::new_unchecked(0x3) };
	pub const OCT_4: Self = unsafe { Self::new_unchecked(0x4) };
	pub const OCT_5: Self = unsafe { Self::new_unchecked(0x5) };
	pub const OCT_6: Self = unsafe { Self::new_unchecked(0x6) };
	pub const OCT_7: Self = unsafe { Self::new_unchecked(0x7) };

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

	pub const fn from_ascii(byte: u8) -> Option<Self> {
		match byte {
			b'0'..=b'7' => Some(unsafe { OctDigit::new_unchecked(byte - b'0') }),
			_ => None,
		}
	}

	pub const fn as_u8(self) -> u8 {
		self.c.get()
	}

	pub const fn non_zero(self) -> Option<NonZero<Self>> {
		if self.c.get() == 0 { None } else { Some(NonZero { inner: self }) }
	}
}

impl BinDigit {
	pub const BIN_0: Self = unsafe { Self::new_unchecked(0x0) };
	pub const BIN_1: Self = unsafe { Self::new_unchecked(0x1) };

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

	pub const fn from_ascii(byte: u8) -> Option<Self> {
		match byte {
			b'0'..=b'1' => Some(unsafe { BinDigit::new_unchecked(byte - b'0') }),
			_ => None,
		}
	}

	pub const fn as_u8(self) -> u8 {
		self.c.get()
	}

	pub const fn non_zero(self) -> Option<NonZero<Self>> {
		if self.c.get() == 0 { None } else { Some(NonZero { inner: self }) }
	}
}
impl DecDigit {
	pub const DEC_0: Self = unsafe { Self::new_unchecked(0x0) };
	pub const DEC_1: Self = unsafe { Self::new_unchecked(0x1) };
	pub const DEC_2: Self = unsafe { Self::new_unchecked(0x2) };
	pub const DEC_3: Self = unsafe { Self::new_unchecked(0x3) };
	pub const DEC_4: Self = unsafe { Self::new_unchecked(0x4) };
	pub const DEC_5: Self = unsafe { Self::new_unchecked(0x5) };
	pub const DEC_6: Self = unsafe { Self::new_unchecked(0x6) };
	pub const DEC_7: Self = unsafe { Self::new_unchecked(0x7) };
	pub const DEC_8: Self = unsafe { Self::new_unchecked(0x8) };
	pub const DEC_9: Self = unsafe { Self::new_unchecked(0x9) };

	pub const fn new(c: u8) -> Option<Self> {
		if c < 10 { Some(Self { c }) } else { None }
	}

	/// # Safety
	/// `c < 10`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		debug_assert!(c < 10);
		Self { c }
	}

	pub const fn from_ascii(byte: u8) -> Option<Self> {
		match byte {
			b'0'..=b'9' => Some(unsafe { DecDigit::new_unchecked(byte - b'0') }),
			_ => None,
		}
	}

	pub const fn as_u8(self) -> u8 {
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

	pub const fn from_ascii(byte: u8) -> Option<Self> {
		match byte {
			0..128 => Some(unsafe { AsciiDigit::new_unchecked(byte) }),
			_ => None,
		}
	}

	pub const fn as_u8(self) -> u8 {
		self.c.get()
	}

	pub const fn non_zero(self) -> Option<NonZero<Self>> {
		if self.c.get() == 0 { None } else { Some(NonZero { inner: self }) }
	}
}

impl HexDigit {
	pub const HEX_0: Self = unsafe { Self::new_lowercase_unchecked(0x0) };
	pub const HEX_1: Self = unsafe { Self::new_lowercase_unchecked(0x1) };
	pub const HEX_2: Self = unsafe { Self::new_lowercase_unchecked(0x2) };
	pub const HEX_3: Self = unsafe { Self::new_lowercase_unchecked(0x3) };
	pub const HEX_4: Self = unsafe { Self::new_lowercase_unchecked(0x4) };
	pub const HEX_5: Self = unsafe { Self::new_lowercase_unchecked(0x5) };
	pub const HEX_6: Self = unsafe { Self::new_lowercase_unchecked(0x6) };
	pub const HEX_7: Self = unsafe { Self::new_lowercase_unchecked(0x7) };
	pub const HEX_8: Self = unsafe { Self::new_lowercase_unchecked(0x8) };
	pub const HEX_9: Self = unsafe { Self::new_lowercase_unchecked(0x9) };
	pub const HEX_A_LOWERCASE: Self = unsafe { Self::new_lowercase_unchecked(0xA) };
	pub const HEX_A_UPPERCASE: Self = unsafe { Self::new_uppercase_unchecked(0xA) };
	pub const HEX_B_LOWERCASE: Self = unsafe { Self::new_lowercase_unchecked(0xB) };
	pub const HEX_B_UPPERCASE: Self = unsafe { Self::new_uppercase_unchecked(0xB) };
	pub const HEX_C_LOWERCASE: Self = unsafe { Self::new_lowercase_unchecked(0xC) };
	pub const HEX_C_UPPERCASE: Self = unsafe { Self::new_uppercase_unchecked(0xC) };
	pub const HEX_D_LOWERCASE: Self = unsafe { Self::new_lowercase_unchecked(0xD) };
	pub const HEX_D_UPPERCASE: Self = unsafe { Self::new_uppercase_unchecked(0xD) };
	pub const HEX_E_LOWERCASE: Self = unsafe { Self::new_lowercase_unchecked(0xE) };
	pub const HEX_E_UPPERCASE: Self = unsafe { Self::new_uppercase_unchecked(0xE) };
	pub const HEX_F_LOWERCASE: Self = unsafe { Self::new_lowercase_unchecked(0xF) };
	pub const HEX_F_UPPERCASE: Self = unsafe { Self::new_uppercase_unchecked(0xF) };

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

	pub const fn from_ascii(byte: u8) -> Option<Self> {
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

	pub const fn as_u8(self) -> u8 {
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
pub enum LiteralExpr<'a, P: Pointer> {
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
as_ref!(LiteralExpr, LiteralExprBox, LiteralExprRef, LiteralExprDyn);
derive_enum!(
	LiteralExpr,
	Char,
	Byte,
	Str,
	RawStr,
	ByteStr,
	RawByteStr,
	CStr,
	RawCStr,
	Int,
	Float,
	True,
	False,
);

impl Spanned for LiteralExprDyn<'_> {
	fn span(&self) -> Span {
		match self.rb() {
			LiteralExpr::Char(lit) => lit.span(),
			LiteralExpr::Byte(lit) => lit.span(),
			LiteralExpr::Str(lit) => lit.span(),
			LiteralExpr::RawStr(lit) => lit.span(),
			LiteralExpr::ByteStr(lit) => lit.span(),
			LiteralExpr::RawByteStr(lit) => lit.span(),
			LiteralExpr::CStr(lit) => lit.span(),
			LiteralExpr::RawCStr(lit) => lit.span(),
			LiteralExpr::Int(lit) => lit.span(),
			LiteralExpr::Float(lit) => lit.span(),
			LiteralExpr::True(lit) => lit.span(),
			LiteralExpr::False(lit) => lit.span(),
		}
	}
}

#[cfg(feature = "alloc")]
mod alloc {
	use super::*;
	use crate::Edition;
	use crate::alloc::string::ToString;

	use char_literal::*;
	use float_literal::*;
	use int_literal::*;
	use str_literal::*;

	impl LiteralExprBox {
		pub(crate) fn from_raw(edition: Edition, token: &proc::Literal) -> Self {
			let span = Span::from_repr(token.span());
			let repr = &mut &*token.to_string();

			if repr.starts_with('\'') {
				let c = parse_char(repr);

				return Self::Char(CharLiteral {
					char: c,
					suffix: parse_maybe_suffix(edition, repr),
					span,
				});
			}
			if repr.starts_with("b'") {
				let c = parse_byte(repr);

				return Self::Byte(ByteLiteral {
					byte: c,
					suffix: parse_maybe_suffix(edition, repr),
					span,
				});
			}
			if repr.starts_with('\"') {
				let contents = parse_str_contents(repr);
				*repr = &repr[1..];
				return Self::Str(StrLiteral {
					contents,
					suffix: parse_maybe_suffix(edition, repr),
					span,
				});
			}
			if repr.starts_with("b\"") {
				let contents = parse_byte_str_contents(repr);
				*repr = &repr[1..];
				return Self::ByteStr(ByteStrLiteral {
					contents,
					suffix: parse_maybe_suffix(edition, repr),
					span,
				});
			}
			if repr.starts_with("r\"") || repr.starts_with("r#\"") || repr.starts_with("r##") {
				return Self::RawStr(parse_raw_str(edition, repr, span));
			}
			if repr.starts_with("br\"") || repr.starts_with("br#\"") || repr.starts_with("br##") {
				return Self::RawByteStr(parse_raw_byte_str(edition, repr, span));
			}
			if repr.starts_with("cr\"") || repr.starts_with("cr#\"") || repr.starts_with("cr##") {
				return Self::RawCStr(parse_raw_cstr(edition, repr, span));
			}
			if repr.starts_with("0b") {
				return Self::Int(IntLiteral {
					repr: IntLiteralRepr::Bin(parse_bin_literal(repr)),
					suffix: parse_maybe_suffix_no_exp(edition, repr),
					span,
				});
			}
			if repr.starts_with("0o") {
				return Self::Int(IntLiteral {
					repr: IntLiteralRepr::Oct(parse_oct_literal(repr)),
					suffix: parse_maybe_suffix_no_exp(edition, repr),
					span,
				});
			}
			if repr.starts_with("0x") {
				return Self::Int(IntLiteral {
					repr: IntLiteralRepr::Hex(parse_hex_literal(repr)),
					suffix: parse_maybe_suffix_no_exp(edition, repr),
					span,
				});
			}

			let dec = parse_dec_literal(repr);

			if repr.is_empty() || !(repr.starts_with('.') || repr.starts_with('e') || repr.starts_with('E')) {
				return Self::Int(IntLiteral {
					repr: IntLiteralRepr::Dec(dec),
					suffix: parse_maybe_suffix_no_exp(edition, repr),
					span,
				});
			}

			let repr = if *repr == "." {
				FloatLiteralRepr::Int(dec)
			} else {
				let frac;
				if repr.starts_with('.') {
					*repr = &repr[1..];
					frac = Some(parse_dec_literal(repr));
				} else {
					frac = None;
				}

				let e = repr.as_bytes()[0];
				if e.to_ascii_lowercase() != b'e' {
					FloatLiteralRepr::NoExp(FloatNoExp {
						int: dec,
						frac: frac.unwrap(),
						suffix: parse_maybe_suffix_no_exp(edition, repr),
					})
				} else {
					let e = if e == b'e' { E::Lowercase } else { E::Uppercase };
					*repr = &repr[1..];

					let sign = repr.as_bytes()[0];
					let sign = if sign == b'+' {
						*repr = &repr[1..];
						Some(Sign::Plus)
					} else if sign == b'-' {
						*repr = &repr[1..];
						Some(Sign::Minus)
					} else {
						None
					};

					let mut leading_underscores = 0;
					while repr.as_bytes()[leading_underscores] == b'_' {
						leading_underscores += 1;
					}
					*repr = &repr[leading_underscores..];

					FloatLiteralRepr::Exp(FloatExp {
						int: dec,
						frac,
						exp: Exponent {
							e,
							sign,
							leading_underscores,
							digits: parse_dec_literal(repr),
						},
						suffix: parse_maybe_suffix(edition, repr),
					})
				}
			};

			return Self::Float(FloatLiteral { repr, span });
		}
	}

	fn parse_maybe_suffix(edition: Edition, repr: &mut &str) -> Option<SuffixBox> {
		if repr.is_empty() {
			None
		} else {
			Some(Suffix {
				ident: IdentOrKeyword::new(edition, repr.to_string().into_boxed_str()).unwrap(),
			})
		}
	}
	fn parse_maybe_suffix_no_exp(edition: Edition, repr: &mut &str) -> Option<SuffixNoExpBox> {
		if repr.is_empty() {
			None
		} else {
			Some(SuffixNoExp::new(IdentOrKeyword::new(edition, repr.to_string().into_boxed_str()).unwrap()).unwrap())
		}
	}

	fn parse_unicode_escape_between_braces(from_brace: &mut &str) -> UnicodeEscape {
		let repr = from_brace;

		*repr = &repr[1..];
		let mut bytes = repr.as_bytes();

		let mut chars = StackVec::new([TrailingUnderscores {
			digit: HexDigit::from_ascii(bytes[0]).unwrap(),
			underscore_count: 0,
		}]);
		bytes = &bytes[1..];
		*repr = &repr[1..];

		while bytes[0] != b'}' {
			let b = bytes[0];
			bytes = &bytes[1..];
			*repr = &repr[1..];

			if b == b'_' {
				chars.last_mut().unwrap().underscore_count += 1;
			} else {
				chars.push(TrailingUnderscores {
					digit: HexDigit::from_ascii(b).unwrap(),
					underscore_count: 0,
				});
			}
		}
		*repr = &repr[1..];

		UnicodeEscape { chars }
	}

	fn parse_ascii_digits(repr: &mut &str) -> AsciiDigits {
		let msb = repr.as_bytes()[0];
		let lsb = repr.as_bytes()[1];

		*repr = &repr[2..];
		let msb = OctDigit::from_ascii(msb).unwrap();
		let lsb = HexDigit::from_ascii(lsb).unwrap();

		AsciiDigits { msb, lsb }
	}

	fn parse_byte_digits(repr: &mut &str) -> ByteDigits {
		let msb = repr.as_bytes()[0];
		let lsb = repr.as_bytes()[1];

		*repr = &repr[2..];
		let msb = HexDigit::from_ascii(msb).unwrap();
		let lsb = HexDigit::from_ascii(lsb).unwrap();

		ByteDigits { msb, lsb }
	}

	// https://doc.rust-lang.org/reference/tokens.html#character-literals
	fn parse_char(repr: &mut &str) -> Char {
		// char literal
		*repr = &repr[1..];
		let c = repr.chars().next().unwrap();
		*repr = &repr[c.len_utf8()..];
		let c = if c != '\\' {
			Char::Verbatim(CharVerbatim::new(c).unwrap())
		} else {
			let c = repr.as_bytes()[0];
			*repr = &repr[1..];
			match c {
				b'\'' => Char::Quote(QuoteEscape::Single),
				b'\"' => Char::Quote(QuoteEscape::Double),
				b'n' => Char::Ascii(AsciiEscape::LineFeed),
				b'r' => Char::Ascii(AsciiEscape::CarriageReturn),
				b't' => Char::Ascii(AsciiEscape::HorizontalTabulation),
				b'\\' => Char::Ascii(AsciiEscape::Backslash),
				b'0' => Char::Ascii(AsciiEscape::Null),
				b'x' => Char::Ascii(AsciiEscape::Digits(parse_ascii_digits(repr))),
				b'u' => Char::Unicode(parse_unicode_escape_between_braces(repr)),
				_ => panic!(),
			}
		};
		*repr = &repr[1..];
		c
	}

	// https://doc.rust-lang.org/reference/tokens.html#byte-literals
	fn parse_byte(repr: &mut &str) -> Byte {
		// byte literal
		*repr = &repr[2..];

		let c = repr.as_bytes()[0];
		*repr = &repr[1..];

		let c = if c != b'\\' {
			Byte::Verbatim(AsciiVerbatim::new(AsciiDigit::new(c).unwrap()).unwrap())
		} else {
			let c = repr.as_bytes()[0];
			*repr = &repr[1..];
			match c {
				b'x' => Byte::Byte(ByteEscape::Digits(parse_byte_digits(repr))),
				b'n' => Byte::Byte(ByteEscape::LineFeed),
				b'r' => Byte::Byte(ByteEscape::CarriageReturn),
				b't' => Byte::Byte(ByteEscape::HorizontalTabulation),
				b'\\' => Byte::Byte(ByteEscape::Backslash),
				b'0' => Byte::Byte(ByteEscape::Null),
				b'\'' => Byte::Byte(ByteEscape::Quote(QuoteEscape::Single)),
				b'\"' => Byte::Byte(ByteEscape::Quote(QuoteEscape::Double)),
				_ => panic!(),
			}
		};
		*repr = &repr[1..];
		c
	}

	fn parse_str_continue(repr: &mut &[u8]) -> Box<[StrContinueWhitespace]> {
		let mut chars = repr.iter();

		let mut last_whitespace = chars.clone();
		let mut str_cont = Vec::new();

		loop {
			let c = match chars.next().copied() {
				Some(x) if x as u32 == '\u{0009}' as u32 => {
					last_whitespace = chars.clone();
					StrContinueWhitespace::HorizontalTabulation
				},
				Some(x) if x as u32 == '\u{000A}' as u32 => {
					last_whitespace = chars.clone();
					StrContinueWhitespace::LineFeed
				},
				Some(x) if x as u32 == '\u{000D}' as u32 => {
					last_whitespace = chars.clone();
					StrContinueWhitespace::CarriageReturn
				},
				Some(x) if x as u32 == '\u{0020}' as u32 => {
					last_whitespace = chars.clone();
					StrContinueWhitespace::Space
				},
				_ => break,
			};
			str_cont.push(c);
		}
		*repr = last_whitespace.as_slice();

		str_cont.into_boxed_slice()
	}

	// https://doc.rust-lang.org/reference/tokens.html#string-literals
	fn parse_str_contents(repr: &mut &str) -> StrContentsBox {
		*repr = &repr[1..];

		struct CharsForStr<'a> {
			inner: core::str::Chars<'a>,
		}

		impl Iterator for CharsForStr<'_> {
			type Item = CharForStrBox;

			fn size_hint(&self) -> (usize, Option<usize>) {
				(0, self.inner.size_hint().1)
			}

			fn next(&mut self) -> Option<Self::Item> {
				if self.inner.as_str().starts_with('\"') {
					None
				} else {
					let next = self.inner.next();
					match next {
						None => None,
						Some('\r') => panic!(),
						Some('\\') => Some(match self.inner.next() {
							None => panic!(),
							Some('\'') => CharForStr::Quote(QuoteEscape::Single),
							Some('\"') => CharForStr::Quote(QuoteEscape::Double),
							Some('0') => CharForStr::Ascii(AsciiEscape::Null),
							Some('\\') => CharForStr::Ascii(AsciiEscape::Backslash),
							Some('n') => CharForStr::Ascii(AsciiEscape::LineFeed),
							Some('r') => CharForStr::Ascii(AsciiEscape::CarriageReturn),
							Some('t') => CharForStr::Ascii(AsciiEscape::HorizontalTabulation),
							Some('x') => CharForStr::Ascii(AsciiEscape::Digits(AsciiDigits {
								msb: OctDigit::from_ascii(self.inner.next().unwrap() as u8).unwrap(),
								lsb: HexDigit::from_ascii(self.inner.next().unwrap() as u8).unwrap(),
							})),
							Some('u') => {
								let mut str = self.inner.as_str();
								let out = CharForStr::Unicode(parse_unicode_escape_between_braces(&mut str));
								self.inner = str.chars();
								out
							},
							Some('\n') => {
								let mut str = self.inner.as_str().as_bytes();
								let out = parse_str_continue(&mut str);
								self.inner = unsafe { core::str::from_utf8_unchecked(str).chars() };
								CharForStr::Continue(out)
							},
							_ => panic!(),
						}),
						Some(c) => Some(CharForStr::Verbatim(CharForStrVerbatim::new(c).unwrap())),
					}
				}
			}
		}

		let mut chars = CharsForStr { inner: repr.chars() };
		let contents = StrContents::from_iter(&mut chars);
		*repr = chars.inner.as_str();
		contents
	}

	// https://doc.rust-lang.org/reference/tokens.html#byte-literals
	fn parse_byte_str_contents(repr: &mut &str) -> ByteStrContentsBox {
		*repr = &repr[2..];

		struct BytesForStr<'a> {
			inner: core::slice::Iter<'a, u8>,
		}

		impl Iterator for BytesForStr<'_> {
			type Item = AsciiForStrBox;

			fn size_hint(&self) -> (usize, Option<usize>) {
				(0, self.inner.size_hint().1)
			}

			fn next(&mut self) -> Option<Self::Item> {
				if self.inner.as_slice().first() == Some(&b'\"') {
					None
				} else {
					let next = self.inner.next().copied();
					match next {
						None => None,
						Some(b'\r') => panic!(),
						Some(b'\\') => Some(match self.inner.next().copied() {
							None => panic!(),
							Some(b'\'') => AsciiForStr::Quote(QuoteEscape::Single),
							Some(b'\"') => AsciiForStr::Quote(QuoteEscape::Double),
							Some(b'0') => AsciiForStr::Byte(ByteEscape::Null),
							Some(b'\\') => AsciiForStr::Byte(ByteEscape::Backslash),
							Some(b'n') => AsciiForStr::Byte(ByteEscape::LineFeed),
							Some(b'r') => AsciiForStr::Byte(ByteEscape::CarriageReturn),
							Some(b't') => AsciiForStr::Byte(ByteEscape::HorizontalTabulation),
							Some(b'x') => AsciiForStr::Byte(ByteEscape::Digits(ByteDigits {
								msb: HexDigit::from_ascii(*self.inner.next().unwrap()).unwrap(),
								lsb: HexDigit::from_ascii(*self.inner.next().unwrap()).unwrap(),
							})),
							Some(b'\n') => {
								let mut str = self.inner.as_slice();
								let out = parse_str_continue(&mut str);
								self.inner = str.iter();
								AsciiForStr::Continue(out)
							},
							_ => panic!(),
						}),
						Some(c) => Some(AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(c).unwrap()).unwrap())),
					}
				}
			}
		}

		let mut chars = BytesForStr {
			inner: repr.as_bytes().iter(),
		};
		let contents = ByteStrContents::from_iter(&mut chars);
		*repr = unsafe { core::str::from_utf8_unchecked(chars.inner.as_slice()) };
		contents
	}

	// https://doc.rust-lang.org/reference/tokens.html#c-string-literals
	fn parse_cstr_contents(repr: &mut &str) -> CStrContentsBox {
		*repr = &repr[1..];

		struct CharsForCStr<'a> {
			inner: core::str::Chars<'a>,
		}

		impl Iterator for CharsForCStr<'_> {
			type Item = CharForCStrBox;

			fn size_hint(&self) -> (usize, Option<usize>) {
				(0, self.inner.size_hint().1)
			}

			fn next(&mut self) -> Option<Self::Item> {
				if self.inner.as_str().starts_with('\"') {
					None
				} else {
					let next = self.inner.next();
					match next {
						None => None,
						Some('\r') => panic!(),
						Some('\\') => Some(match self.inner.next() {
							None => panic!(),
							Some('0') => panic!(),
							Some('\'') => CharForCStr::Byte(ByteEscapeForCStr::new(ByteEscape::Quote(QuoteEscape::Single)).unwrap()),
							Some('\"') => CharForCStr::Byte(ByteEscapeForCStr::new(ByteEscape::Quote(QuoteEscape::Double)).unwrap()),
							Some('\\') => CharForCStr::Byte(ByteEscapeForCStr::new(ByteEscape::Backslash).unwrap()),
							Some('n') => CharForCStr::Byte(ByteEscapeForCStr::new(ByteEscape::LineFeed).unwrap()),
							Some('r') => CharForCStr::Byte(ByteEscapeForCStr::new(ByteEscape::CarriageReturn).unwrap()),
							Some('t') => CharForCStr::Byte(ByteEscapeForCStr::new(ByteEscape::HorizontalTabulation).unwrap()),
							Some('x') => CharForCStr::Byte(
								ByteEscapeForCStr::new(ByteEscape::Digits(ByteDigits {
									msb: HexDigit::from_ascii(self.inner.next().unwrap() as u8).unwrap(),
									lsb: HexDigit::from_ascii(self.inner.next().unwrap() as u8).unwrap(),
								}))
								.unwrap(),
							),
							Some('u') => {
								let mut str = self.inner.as_str();
								let unicode = parse_unicode_escape_between_braces(&mut str);
								for &u in &*unicode.chars {
									assert!(u.digit.as_u8() != 0);
								}
								let out = CharForCStr::Unicode(unsafe {
									NonZeroUnicodeEscape {
										chars: core::mem::transmute(unicode.chars),
									}
								});
								self.inner = str.chars();
								out
							},
							Some('\n') => {
								let mut str = self.inner.as_str().as_bytes();
								let out = parse_str_continue(&mut str);
								self.inner = unsafe { core::str::from_utf8_unchecked(str).chars() };
								CharForCStr::Continue(out)
							},
							_ => panic!(),
						}),
						Some(c) => Some(CharForCStr::Verbatim(CharForCStrVerbatim::new(c).unwrap())),
					}
				}
			}
		}

		let mut chars = CharsForCStr { inner: repr.chars() };
		let contents = CStrContents::from_iter(&mut chars);
		*repr = chars.inner.as_str();
		contents
	}

	// https://doc.rust-lang.org/reference/tokens.html#raw-string-literals
	fn parse_raw_str(edition: Edition, repr: &mut &str, span: Span) -> RawStrLiteralBox {
		*repr = &repr[1..];
		let mut hash_count = 0u8;
		{
			let bytes = repr.as_bytes();
			while bytes.get(hash_count as usize).copied() == Some(b'#') {
				hash_count += 1;
			}
		}
		*repr = &repr[hash_count as usize + 1..];

		let mut contents = Vec::new();

		let mut chars = repr.chars();
		while let Some(c) = chars.next() {
			if c == '\"' {
				let bytes = chars.as_str().as_bytes();
				let mut count = 0u8;
				while bytes.get(count as usize).copied() == Some(b'#') {
					count += 1;
				}
				if count == hash_count {
					*repr = &chars.as_str()[hash_count as usize..];
					break;
				}
				contents.push(CharForRawStrVerbatim::new(c).unwrap())
			} else {
				contents.push(CharForRawStrVerbatim::new(c).unwrap())
			}
		}

		RawStrLiteral {
			hash_count,
			contents: contents.into_boxed_slice(),
			suffix: parse_maybe_suffix(edition, repr),
			span,
		}
	}

	// https://doc.rust-lang.org/reference/tokens.html#raw-byte-string-literals
	fn parse_raw_byte_str(edition: Edition, repr: &mut &str, span: Span) -> RawByteStrLiteralBox {
		let repr_ = repr;
		let mut repr = repr_.as_bytes();
		repr = &repr[2..];

		let mut hash_count = 0u8;
		{
			let bytes = repr;
			while bytes.get(hash_count as usize).copied() == Some(b'#') {
				hash_count += 1;
			}
		}
		repr = &repr[hash_count as usize + 1..];

		let mut contents = Vec::new();

		let mut chars = repr.iter();
		while let Some(&c) = chars.next() {
			if c == b'\"' {
				let bytes = chars.as_slice();
				let mut count = 0u8;
				while bytes.get(count as usize).copied() == Some(b'#') {
					count += 1;
				}
				if count == hash_count {
					repr = &chars.as_slice()[hash_count as usize..];
					break;
				}
				contents.push(AsciiForRawByteStrVerbatim::new(AsciiDigit::new(c).unwrap()).unwrap())
			} else {
				contents.push(AsciiForRawByteStrVerbatim::new(AsciiDigit::new(c).unwrap()).unwrap())
			}
		}

		*repr_ = unsafe { core::str::from_utf8_unchecked(repr) };
		let repr = repr_;

		RawByteStrLiteral {
			hash_count,
			contents: contents.into_boxed_slice(),
			suffix: parse_maybe_suffix(edition, repr),
			span,
		}
	}

	// https://doc.rust-lang.org/reference/tokens.html#raw-c-string-literals
	fn parse_raw_cstr(edition: Edition, repr: &mut &str, span: Span) -> RawCStrLiteralBox {
		*repr = &repr[2..];
		let mut hash_count = 0u8;
		{
			let bytes = repr.as_bytes();
			while bytes.get(hash_count as usize).copied() == Some(b'#') {
				hash_count += 1;
			}
		}
		*repr = &repr[hash_count as usize + 1..];

		let mut contents = Vec::new();

		let mut chars = repr.chars();
		while let Some(c) = chars.next() {
			if c == '\"' {
				let bytes = chars.as_str().as_bytes();
				let mut count = 0u8;
				while bytes.get(count as usize).copied() == Some(b'#') {
					count += 1;
				}
				if count == hash_count {
					*repr = &chars.as_str()[hash_count as usize..];
					break;
				}
				contents.push(CharForRawCStrVerbatim::new(c).unwrap())
			} else {
				contents.push(CharForRawCStrVerbatim::new(c).unwrap())
			}
		}

		RawCStrLiteral {
			hash_count,
			contents: contents.into_boxed_slice(),
			suffix: parse_maybe_suffix(edition, repr),
			span,
		}
	}

	// https://doc.rust-lang.org/reference/tokens.html#integer-literals
	// https://doc.rust-lang.org/reference/tokens.html#floating-point-literals

	fn parse_bin_literal(repr: &mut &str) -> BinLiteralBox {
		*repr = &repr[2..];
		struct Digits<'a> {
			inner: &'a [u8],
		}
		impl Iterator for Digits<'_> {
			type Item = MaybeUnderscore<BinDigit>;

			fn next(&mut self) -> Option<Self::Item> {
				match self.inner.split_first() {
					None => None,
					Some((&b, next)) => {
						if b == b'_' {
							self.inner = next;
							Some(MaybeUnderscore::Underscore)
						} else if let Some(b) = BinDigit::from_ascii(b) {
							self.inner = next;
							Some(MaybeUnderscore::Digit(b))
						} else {
							None
						}
					},
				}
			}
		}

		let mut digits = Digits { inner: repr.as_bytes() };
		let out = BinLiteral::from_iter(&mut digits);
		*repr = unsafe { core::str::from_utf8_unchecked(digits.inner) };
		out
	}
	fn parse_hex_literal(repr: &mut &str) -> HexLiteralBox {
		*repr = &repr[2..];
		struct Digits<'a> {
			inner: &'a [u8],
		}
		impl Iterator for Digits<'_> {
			type Item = MaybeUnderscore<HexDigit>;

			fn next(&mut self) -> Option<Self::Item> {
				match self.inner.split_first() {
					None => None,
					Some((&b, next)) => {
						if b == b'_' {
							self.inner = next;
							Some(MaybeUnderscore::Underscore)
						} else if let Some(b) = HexDigit::from_ascii(b) {
							self.inner = next;
							Some(MaybeUnderscore::Digit(b))
						} else {
							None
						}
					},
				}
			}
		}

		let mut digits = Digits { inner: repr.as_bytes() };
		let out = HexLiteral::from_iter(&mut digits);
		*repr = unsafe { core::str::from_utf8_unchecked(digits.inner) };
		out
	}
	fn parse_oct_literal(repr: &mut &str) -> OctLiteralBox {
		*repr = &repr[2..];
		struct Digits<'a> {
			inner: &'a [u8],
		}
		impl Iterator for Digits<'_> {
			type Item = MaybeUnderscore<OctDigit>;

			fn next(&mut self) -> Option<Self::Item> {
				match self.inner.split_first() {
					None => None,
					Some((&b, next)) => {
						if b == b'_' {
							self.inner = next;
							Some(MaybeUnderscore::Underscore)
						} else if let Some(b) = OctDigit::from_ascii(b) {
							self.inner = next;
							Some(MaybeUnderscore::Digit(b))
						} else {
							None
						}
					},
				}
			}
		}

		let mut digits = Digits { inner: repr.as_bytes() };
		let out = OctLiteral::from_iter(&mut digits);
		*repr = unsafe { core::str::from_utf8_unchecked(digits.inner) };
		out
	}

	fn parse_dec_literal(repr: &mut &str) -> DecLiteralBox {
		struct Digits<'a> {
			inner: &'a [u8],
		}
		impl Iterator for Digits<'_> {
			type Item = MaybeUnderscore<DecDigit>;

			fn next(&mut self) -> Option<Self::Item> {
				match self.inner.split_first() {
					None => None,
					Some((&b, next)) => {
						if b == b'_' {
							self.inner = next;
							Some(MaybeUnderscore::Underscore)
						} else if let Some(b) = DecDigit::from_ascii(b) {
							self.inner = next;
							Some(MaybeUnderscore::Digit(b))
						} else {
							None
						}
					},
				}
			}
		}

		let mut digits = Digits { inner: repr.as_bytes() };
		let out = DecLiteral::from_iter(&mut digits);
		*repr = unsafe { core::str::from_utf8_unchecked(digits.inner) };
		out
	}

	fn parse_float(edition: Edition, repr: &mut &str, span: Span) -> FloatLiteralBox {
		todo!()
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::Edition;
	use proc::TokenTree;

	use char_literal::*;
	use float_literal::*;
	use int_literal::*;
	use str_literal::*;

	macro_rules! unwrap {
		($result: expr $(,)?) => {
			match $result {
				::core::result::Result::Ok(__ok) => __ok,
				::core::result::Result::Err(_) => ::core::panic!(),
			}
		};
	}

	#[test]
	fn test_char_literal() {
		const EDITION: Edition = Edition::Edition2021;

		let TokenTree::Literal(c) = quote!('c').into_iter().next().unwrap() else {
			panic!()
		};
		assert_eq!(
			LiteralExpr::from_raw(EDITION, &c).rb(),
			&const {
				LiteralExpr::Char(CharLiteral {
					char: Char::Verbatim(CharVerbatim::new('c').unwrap()),
					suffix: None,
					span: Span::call_site(),
				})
			}
		);

		let TokenTree::Literal(c) = quote!('c'abc).into_iter().next().unwrap() else {
			panic!()
		};
		assert_eq!(
			LiteralExpr::from_raw(EDITION, &c).rb(),
			&const {
				LiteralExpr::Char(CharLiteral {
					char: Char::Verbatim(CharVerbatim::new('c').unwrap()),
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "abc")),
					}),
					span: Span::call_site(),
				})
			}
		);

		let TokenTree::Literal(c) = quote!('\u{0__0a_1B}'invalid_suffix).into_iter().next().unwrap() else {
			panic!()
		};
		assert_eq!(
			LiteralExpr::from_raw(EDITION, &c).rb(),
			&const {
				LiteralExpr::Char(CharLiteral {
					char: Char::Unicode(UnicodeEscape {
						chars: {
							StackVec::from_array([
								TrailingUnderscores::new(HexDigit::HEX_0, 2),
								TrailingUnderscores::new(HexDigit::HEX_0, 0),
								TrailingUnderscores::new(HexDigit::HEX_A_LOWERCASE, 1),
								TrailingUnderscores::new(HexDigit::HEX_1, 0),
								TrailingUnderscores::new(HexDigit::HEX_B_UPPERCASE, 0),
							])
						},
					}),
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "invalid_suffix")),
					}),
					span: Span::call_site(),
				})
			}
		);
	}

	#[test]
	fn test_byte_literal() {
		const EDITION: Edition = Edition::Edition2021;

		let TokenTree::Literal(c) = quote!(b'c').into_iter().next().unwrap() else {
			panic!()
		};
		assert_eq!(
			LiteralExpr::from_raw(EDITION, &c).rb(),
			&const {
				LiteralExpr::Byte(ByteLiteral {
					byte: Byte::Verbatim(AsciiVerbatim::new(AsciiDigit::new(b'c').unwrap()).unwrap()),
					suffix: None,
					span: Span::call_site(),
				})
			}
		);

		let TokenTree::Literal(c) = quote!(b'c'abc).into_iter().next().unwrap() else {
			panic!()
		};
		assert_eq!(
			LiteralExpr::from_raw(EDITION, &c).rb(),
			&const {
				LiteralExpr::Byte(ByteLiteral {
					byte: Byte::Verbatim(AsciiVerbatim::new(AsciiDigit::new(b'c').unwrap()).unwrap()),
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "abc")),
					}),
					span: Span::call_site(),
				})
			}
		);

		let TokenTree::Literal(c) = quote!(b'\xaF'invalid_suffix).into_iter().next().unwrap() else {
			panic!()
		};
		assert_eq!(
			LiteralExpr::from_raw(EDITION, &c).rb(),
			&const {
				LiteralExpr::Byte(ByteLiteral {
					byte: Byte::Byte(ByteEscape::Digits(ByteDigits {
						msb: HexDigit::HEX_A_LOWERCASE,
						lsb: HexDigit::HEX_F_UPPERCASE,
					})),
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "invalid_suffix")),
					}),
					span: Span::call_site(),
				})
			}
		);
	}

	#[test]
	fn test_str_literal() {
		const EDITION: Edition = Edition::Edition2021;

		#[rustfmt::skip]
		let TokenTree::Literal(s) = quote!("a \x20 \' \" \u{00__Fa}bc\
			  def"__my_suffix__).into_iter().next().unwrap() else { panic!() };

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &s).rb(),
			&const {
				LiteralExpr::Str(StrLiteral {
					contents: StrContents::new(
						const {
							&[
								CharForStr::Verbatim(CharForStrVerbatim::new('a').unwrap()),
								CharForStr::Verbatim(CharForStrVerbatim::new(' ').unwrap()),
								CharForStr::Ascii(AsciiEscape::Digits(AsciiDigits {
									msb: OctDigit::OCT_2,
									lsb: HexDigit::HEX_0,
								})),
								CharForStr::Verbatim(CharForStrVerbatim::new(' ').unwrap()),
								CharForStr::Quote(QuoteEscape::Single),
								CharForStr::Verbatim(CharForStrVerbatim::new(' ').unwrap()),
								CharForStr::Quote(QuoteEscape::Double),
								CharForStr::Verbatim(CharForStrVerbatim::new(' ').unwrap()),
								CharForStr::Unicode(UnicodeEscape {
									chars: StackVec::from_array([
										TrailingUnderscores::new(HexDigit::HEX_0, 0),
										TrailingUnderscores::new(HexDigit::HEX_0, 2),
										TrailingUnderscores::new(HexDigit::HEX_F_UPPERCASE, 0),
										TrailingUnderscores::new(HexDigit::HEX_A_LOWERCASE, 0),
									]),
								}),
								CharForStr::Verbatim(CharForStrVerbatim::new('b').unwrap()),
								CharForStr::Verbatim(CharForStrVerbatim::new('c').unwrap()),
								CharForStr::Continue(&[
									StrContinueWhitespace::HorizontalTabulation,
									StrContinueWhitespace::HorizontalTabulation,
									StrContinueWhitespace::HorizontalTabulation,
									StrContinueWhitespace::Space,
									StrContinueWhitespace::Space,
								] as &[_]),
								CharForStr::Verbatim(CharForStrVerbatim::new('d').unwrap()),
								CharForStr::Verbatim(CharForStrVerbatim::new('e').unwrap()),
								CharForStr::Verbatim(CharForStrVerbatim::new('f').unwrap()),
							] as &[_]
						},
					),
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "__my_suffix__")),
					}),
					span: Span::call_site(),
				})
			}
		);
	}

	#[test]
	fn test_byte_str_literal() {
		const EDITION: Edition = Edition::Edition2021;

		#[rustfmt::skip]
		let TokenTree::Literal(s) = quote!(b"a \x20 \' \" bc\
			  def"__my_suffix__).into_iter().next().unwrap() else { panic!() };

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &s).rb(),
			&const {
				LiteralExpr::ByteStr(ByteStrLiteral {
					contents: ByteStrContents::new(
						const {
							&[
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b'a').unwrap()).unwrap()),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap()),
								AsciiForStr::Byte(ByteEscape::Digits(ByteDigits {
									msb: HexDigit::HEX_2,
									lsb: HexDigit::HEX_0,
								})),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap()),
								AsciiForStr::Quote(QuoteEscape::Single),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap()),
								AsciiForStr::Quote(QuoteEscape::Double),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap()),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b'b').unwrap()).unwrap()),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b'c').unwrap()).unwrap()),
								AsciiForStr::Continue(&[
									StrContinueWhitespace::HorizontalTabulation,
									StrContinueWhitespace::HorizontalTabulation,
									StrContinueWhitespace::HorizontalTabulation,
									StrContinueWhitespace::Space,
									StrContinueWhitespace::Space,
								] as &[_]),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b'd').unwrap()).unwrap()),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b'e').unwrap()).unwrap()),
								AsciiForStr::Verbatim(AsciiForStrVerbatim::new(AsciiDigit::new(b'f').unwrap()).unwrap()),
							] as &[_]
						},
					),
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "__my_suffix__")),
					}),
					span: Span::call_site(),
				})
			}
		);
	}

	#[test]
	fn test_raw_byte_str_literal() {
		const EDITION: Edition = Edition::Edition2021;

		#[rustfmt::skip]
		let TokenTree::Literal(s) = quote!(br###"a \x20 \' \" bc\
			  de"##f"###__my_suffix__).into_iter().next().unwrap() else { panic!() };

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &s).rb(),
			&const {
				LiteralExpr::RawByteStr(RawByteStrLiteral {
					hash_count: 3,
					contents: const {
						&[
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'a').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\\').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'x').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'2').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'0').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\\').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\'').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\\').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\"').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'b').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'c').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\\').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\n').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\t').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\t').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\t').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b' ').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'd').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'e').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'\"').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'#').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'#').unwrap()).unwrap(),
							AsciiForRawByteStrVerbatim::new(AsciiDigit::new(b'f').unwrap()).unwrap(),
						]
					} as &[_],
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "__my_suffix__")),
					}),
					span: Span::call_site(),
				})
			}
		);
	}

	#[test]
	fn test_raw_str_literal() {
		const EDITION: Edition = Edition::Edition2021;

		#[rustfmt::skip]
		let TokenTree::Literal(s) = quote!(r###"a \x20 \' \" bc\
			  de"##f"###__my_suffix__).into_iter().next().unwrap() else { panic!() };

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &s).rb(),
			&const {
				LiteralExpr::RawStr(RawStrLiteral {
					hash_count: 3,
					contents: const {
						&[
							CharForRawStrVerbatim::new('a').unwrap(),
							CharForRawStrVerbatim::new(' ').unwrap(),
							CharForRawStrVerbatim::new('\\').unwrap(),
							CharForRawStrVerbatim::new('x').unwrap(),
							CharForRawStrVerbatim::new('2').unwrap(),
							CharForRawStrVerbatim::new('0').unwrap(),
							CharForRawStrVerbatim::new(' ').unwrap(),
							CharForRawStrVerbatim::new('\\').unwrap(),
							CharForRawStrVerbatim::new('\'').unwrap(),
							CharForRawStrVerbatim::new(' ').unwrap(),
							CharForRawStrVerbatim::new('\\').unwrap(),
							CharForRawStrVerbatim::new('\"').unwrap(),
							CharForRawStrVerbatim::new(' ').unwrap(),
							CharForRawStrVerbatim::new('b').unwrap(),
							CharForRawStrVerbatim::new('c').unwrap(),
							CharForRawStrVerbatim::new('\\').unwrap(),
							CharForRawStrVerbatim::new('\n').unwrap(),
							CharForRawStrVerbatim::new('\t').unwrap(),
							CharForRawStrVerbatim::new('\t').unwrap(),
							CharForRawStrVerbatim::new('\t').unwrap(),
							CharForRawStrVerbatim::new(' ').unwrap(),
							CharForRawStrVerbatim::new(' ').unwrap(),
							CharForRawStrVerbatim::new('d').unwrap(),
							CharForRawStrVerbatim::new('e').unwrap(),
							CharForRawStrVerbatim::new('\"').unwrap(),
							CharForRawStrVerbatim::new('#').unwrap(),
							CharForRawStrVerbatim::new('#').unwrap(),
							CharForRawStrVerbatim::new('f').unwrap(),
						]
					} as &[_],
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "__my_suffix__")),
					}),
					span: Span::call_site(),
				})
			}
		);
	}

	#[test]
	fn test_raw_cstr_literal() {
		const EDITION: Edition = Edition::Edition2021;

		#[rustfmt::skip]
		let TokenTree::Literal(s) = quote!(cr###"a \x20 \' \" bc\
			  de"##f"###__my_suffix__).into_iter().next().unwrap() else { panic!() };

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &s).rb(),
			&const {
				LiteralExpr::RawCStr(RawCStrLiteral {
					hash_count: 3,
					contents: const {
						&[
							CharForRawCStrVerbatim::new('a').unwrap(),
							CharForRawCStrVerbatim::new(' ').unwrap(),
							CharForRawCStrVerbatim::new('\\').unwrap(),
							CharForRawCStrVerbatim::new('x').unwrap(),
							CharForRawCStrVerbatim::new('2').unwrap(),
							CharForRawCStrVerbatim::new('0').unwrap(),
							CharForRawCStrVerbatim::new(' ').unwrap(),
							CharForRawCStrVerbatim::new('\\').unwrap(),
							CharForRawCStrVerbatim::new('\'').unwrap(),
							CharForRawCStrVerbatim::new(' ').unwrap(),
							CharForRawCStrVerbatim::new('\\').unwrap(),
							CharForRawCStrVerbatim::new('\"').unwrap(),
							CharForRawCStrVerbatim::new(' ').unwrap(),
							CharForRawCStrVerbatim::new('b').unwrap(),
							CharForRawCStrVerbatim::new('c').unwrap(),
							CharForRawCStrVerbatim::new('\\').unwrap(),
							CharForRawCStrVerbatim::new('\n').unwrap(),
							CharForRawCStrVerbatim::new('\t').unwrap(),
							CharForRawCStrVerbatim::new('\t').unwrap(),
							CharForRawCStrVerbatim::new('\t').unwrap(),
							CharForRawCStrVerbatim::new(' ').unwrap(),
							CharForRawCStrVerbatim::new(' ').unwrap(),
							CharForRawCStrVerbatim::new('d').unwrap(),
							CharForRawCStrVerbatim::new('e').unwrap(),
							CharForRawCStrVerbatim::new('\"').unwrap(),
							CharForRawCStrVerbatim::new('#').unwrap(),
							CharForRawCStrVerbatim::new('#').unwrap(),
							CharForRawCStrVerbatim::new('f').unwrap(),
						]
					} as &[_],
					suffix: Some(Suffix {
						ident: unwrap!(IdentOrKeyword::new(EDITION, "__my_suffix__")),
					}),
					span: Span::call_site(),
				})
			}
		);
	}

	#[test]
	fn test_int_literal() {
		const EDITION: Edition = Edition::Edition2021;
		let TokenTree::Literal(i) = quote!(0b0101010__101_01suffix).into_iter().next().unwrap() else {
			panic!()
		};

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &i).rb(),
			&const {
				LiteralExpr::Int(IntLiteral {
					repr: IntLiteralRepr::Bin(unwrap!(BinLiteral::new(&[
						MaybeUnderscore::Digit(BinDigit::BIN_0),
						MaybeUnderscore::Digit(BinDigit::BIN_1),
						MaybeUnderscore::Digit(BinDigit::BIN_0),
						MaybeUnderscore::Digit(BinDigit::BIN_1),
						MaybeUnderscore::Digit(BinDigit::BIN_0),
						MaybeUnderscore::Digit(BinDigit::BIN_1),
						MaybeUnderscore::Digit(BinDigit::BIN_0),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(BinDigit::BIN_1),
						MaybeUnderscore::Digit(BinDigit::BIN_0),
						MaybeUnderscore::Digit(BinDigit::BIN_1),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(BinDigit::BIN_0),
						MaybeUnderscore::Digit(BinDigit::BIN_1),
					] as &[_]))),
					suffix: Some(unwrap!(SuffixNoExp::new(unwrap!(IdentOrKeyword::new(EDITION, "suffix"))))),
					span: Span::call_site(),
				})
			},
		);

		let TokenTree::Literal(i) = quote!(0o07211_4__101_01suffix).into_iter().next().unwrap() else {
			panic!()
		};

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &i).rb(),
			&const {
				LiteralExpr::Int(IntLiteral {
					repr: IntLiteralRepr::Oct(unwrap!(OctLiteral::new(&[
						MaybeUnderscore::Digit(OctDigit::OCT_0),
						MaybeUnderscore::Digit(OctDigit::OCT_7),
						MaybeUnderscore::Digit(OctDigit::OCT_2),
						MaybeUnderscore::Digit(OctDigit::OCT_1),
						MaybeUnderscore::Digit(OctDigit::OCT_1),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(OctDigit::OCT_4),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(OctDigit::OCT_1),
						MaybeUnderscore::Digit(OctDigit::OCT_0),
						MaybeUnderscore::Digit(OctDigit::OCT_1),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(OctDigit::OCT_0),
						MaybeUnderscore::Digit(OctDigit::OCT_1),
					] as &[_]))),
					suffix: Some(unwrap!(SuffixNoExp::new(unwrap!(IdentOrKeyword::new(EDITION, "suffix"))))),
					span: Span::call_site(),
				})
			},
		);

		#[rustfmt::skip]
		let TokenTree::Literal(i) = quote!(0x01aA0f0__F01_01suffix).into_iter().next().unwrap() else {
			panic!()
		};

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &i).rb(),
			&const {
				LiteralExpr::Int(IntLiteral {
					repr: IntLiteralRepr::Hex(unwrap!(HexLiteral::new(&[
						MaybeUnderscore::Digit(HexDigit::HEX_0),
						MaybeUnderscore::Digit(HexDigit::HEX_1),
						MaybeUnderscore::Digit(HexDigit::HEX_A_LOWERCASE),
						MaybeUnderscore::Digit(HexDigit::HEX_A_UPPERCASE),
						MaybeUnderscore::Digit(HexDigit::HEX_0),
						MaybeUnderscore::Digit(HexDigit::HEX_F_LOWERCASE),
						MaybeUnderscore::Digit(HexDigit::HEX_0),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(HexDigit::HEX_F_UPPERCASE),
						MaybeUnderscore::Digit(HexDigit::HEX_0),
						MaybeUnderscore::Digit(HexDigit::HEX_1),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(HexDigit::HEX_0),
						MaybeUnderscore::Digit(HexDigit::HEX_1),
					] as &[_]))),
					suffix: Some(unwrap!(SuffixNoExp::new(unwrap!(IdentOrKeyword::new(EDITION, "suffix"))))),
					span: Span::call_site(),
				})
			},
		);

		let TokenTree::Literal(i) = quote!(007211_4__101_01suffix).into_iter().next().unwrap() else {
			panic!()
		};

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &i).rb(),
			&const {
				LiteralExpr::Int(IntLiteral {
					repr: IntLiteralRepr::Dec(unwrap!(DecLiteral::new(&[
						MaybeUnderscore::Digit(DecDigit::DEC_0),
						MaybeUnderscore::Digit(DecDigit::DEC_0),
						MaybeUnderscore::Digit(DecDigit::DEC_7),
						MaybeUnderscore::Digit(DecDigit::DEC_2),
						MaybeUnderscore::Digit(DecDigit::DEC_1),
						MaybeUnderscore::Digit(DecDigit::DEC_1),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(DecDigit::DEC_4),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(DecDigit::DEC_1),
						MaybeUnderscore::Digit(DecDigit::DEC_0),
						MaybeUnderscore::Digit(DecDigit::DEC_1),
						MaybeUnderscore::Underscore,
						MaybeUnderscore::Digit(DecDigit::DEC_0),
						MaybeUnderscore::Digit(DecDigit::DEC_1),
					] as &[_]))),
					suffix: Some(unwrap!(SuffixNoExp::new(unwrap!(IdentOrKeyword::new(EDITION, "suffix"))))),
					span: Span::call_site(),
				})
			},
		);
	}

	#[test]
	fn test_float_literal() {
		const EDITION: Edition = Edition::Edition2021;
		let TokenTree::Literal(i) = quote!(123.456suffix).into_iter().next().unwrap() else {
			panic!()
		};

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &i).rb(),
			&const {
				LiteralExpr::Float(FloatLiteral {
					repr: FloatLiteralRepr::NoExp(FloatNoExp {
						int: unwrap!(DecLiteral::new(&[
							MaybeUnderscore::Digit(DecDigit::DEC_1),
							MaybeUnderscore::Digit(DecDigit::DEC_2),
							MaybeUnderscore::Digit(DecDigit::DEC_3),
						] as &[_])),
						frac: unwrap!(DecLiteral::new(&[
							MaybeUnderscore::Digit(DecDigit::DEC_4),
							MaybeUnderscore::Digit(DecDigit::DEC_5),
							MaybeUnderscore::Digit(DecDigit::DEC_6),
						] as &[_])),
						suffix: Some(unwrap!(SuffixNoExp::new(unwrap!(IdentOrKeyword::new(EDITION, "suffix"))))),
					}),
					span: Span::call_site(),
				})
			},
		);

		let TokenTree::Literal(i) = quote!(123.456e1suffix).into_iter().next().unwrap() else {
			panic!()
		};

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &i).rb(),
			&const {
				LiteralExpr::Float(FloatLiteral {
					repr: FloatLiteralRepr::Exp(FloatExp {
						int: unwrap!(DecLiteral::new(&[
							MaybeUnderscore::Digit(DecDigit::DEC_1),
							MaybeUnderscore::Digit(DecDigit::DEC_2),
							MaybeUnderscore::Digit(DecDigit::DEC_3),
						] as &[_])),
						frac: Some(unwrap!(DecLiteral::new(&[
							MaybeUnderscore::Digit(DecDigit::DEC_4),
							MaybeUnderscore::Digit(DecDigit::DEC_5),
							MaybeUnderscore::Digit(DecDigit::DEC_6),
						] as &[_]))),
						exp: Exponent {
							e: E::Lowercase,
							sign: None,
							leading_underscores: 0,
							digits: unwrap!(DecLiteral::new(const { &[MaybeUnderscore::Digit(DecDigit::new(1).unwrap())] as &[_] })),
						},
						suffix: Some(Suffix {
							ident: unwrap!(IdentOrKeyword::new(EDITION, "suffix")),
						}),
					}),
					span: Span::call_site(),
				})
			},
		);

		let TokenTree::Literal(i) = quote!(123.).into_iter().next().unwrap() else {
			panic!()
		};

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &i).rb(),
			&const {
				LiteralExpr::Float(FloatLiteral {
					repr: FloatLiteralRepr::Int(unwrap!(DecLiteral::new(&[
						MaybeUnderscore::Digit(DecDigit::DEC_1),
						MaybeUnderscore::Digit(DecDigit::DEC_2),
						MaybeUnderscore::Digit(DecDigit::DEC_3),
					] as &[_]))),
					span: Span::call_site(),
				})
			},
		);

		let TokenTree::Literal(i) = quote!(123.456e____5___suffix).into_iter().next().unwrap() else {
			panic!()
		};

		assert_eq!(
			LiteralExpr::from_raw(EDITION, &i).rb(),
			&const {
				LiteralExpr::Float(FloatLiteral {
					repr: FloatLiteralRepr::Exp(FloatExp {
						int: unwrap!(DecLiteral::new(&[
							MaybeUnderscore::Digit(DecDigit::DEC_1),
							MaybeUnderscore::Digit(DecDigit::DEC_2),
							MaybeUnderscore::Digit(DecDigit::DEC_3),
						] as &[_])),
						frac: Some(unwrap!(DecLiteral::new(&[
							MaybeUnderscore::Digit(DecDigit::DEC_4),
							MaybeUnderscore::Digit(DecDigit::DEC_5),
							MaybeUnderscore::Digit(DecDigit::DEC_6),
						] as &[_]))),
						exp: Exponent {
							e: E::Lowercase,
							sign: None,
							leading_underscores: 4,
							digits: unwrap!(DecLiteral::new(
								const {
									&[
										MaybeUnderscore::Digit(DecDigit::new(5).unwrap()),
										MaybeUnderscore::Underscore,
										MaybeUnderscore::Underscore,
										MaybeUnderscore::Underscore,
									] as &[_]
								}
							)),
						},
						suffix: Some(Suffix {
							ident: unwrap!(IdentOrKeyword::new(EDITION, "suffix")),
						}),
					}),
					span: Span::call_site(),
				})
			},
		);
	}
}
