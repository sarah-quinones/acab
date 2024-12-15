use super::*;

#[inline]
const fn contains_char(needle: char, haystack: &[char]) -> bool {
	let mut haystack = haystack;
	while let Some((&hay, next)) = haystack.split_first() {
		if needle == hay {
			return true;
		}
		haystack = next;
	}
	false
}

#[inline]
const fn contains_byte_escape(needle: ByteEscape, haystack: &[ByteEscape]) -> bool {
	let mut haystack = haystack;
	while let Some((&hay, next)) = haystack.split_first() {
		if match (needle, hay) {
			(ByteEscape::Digits(left), ByteEscape::Digits(right)) => {
				left.msb.as_byte() == right.msb.as_byte() && left.lsb.as_byte() == right.lsb.as_byte()
			},
			(ByteEscape::Null, ByteEscape::Null) => true,
			(ByteEscape::Backslash, ByteEscape::Backslash) => true,
			(ByteEscape::LineFeed, ByteEscape::LineFeed) => true,
			(ByteEscape::CarriageReturn, ByteEscape::CarriageReturn) => true,
			(ByteEscape::HorizontalTabulation, ByteEscape::HorizontalTabulation) => true,
			(ByteEscape::SingleQuote, ByteEscape::SingleQuote) => true,
			(ByteEscape::DoubleQuote, ByteEscape::DoubleQuote) => true,
			_ => false,
		} {
			return true;
		}
		haystack = next;
	}
	false
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum QuoteEscape {
	Single,
	Double,
}
reborrow_copy!(QuoteEscape);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(transparent)]
pub(super) struct uint<U: Copy, const BITS: usize> {
	byte: U,
}

impl<U: Copy, const BITS: usize> uint<U, BITS> {
	pub const fn get(self) -> U {
		self.byte
	}
}

impl<const BITS: usize> uint<u8, BITS> {
	pub const MAX: u8 = {
		assert!(BITS <= 8);

		if BITS == 8 { u8::MAX } else { (1u8 << BITS) - 1 }
	};

	/// # Safety
	/// `byte <= Self::MAX`
	pub const unsafe fn new_unchecked(byte: u8) -> Self {
		debug_assert!(byte <= const { Self::MAX });
		Self { byte }
	}

	pub const fn new(byte: u8) -> Option<Self> {
		if byte <= Self::MAX { Some(Self { byte }) } else { None }
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AsciiDigits {
	pub msb: OctDigit,
	pub lsb: HexDigit,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ByteDigits {
	pub msb: HexDigit,
	pub lsb: HexDigit,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AsciiEscape {
	Digits(AsciiDigits),
	Null,
	Backslash,
	LineFeed,
	CarriageReturn,
	HorizontalTabulation,
}
reborrow_copy!(AsciiEscape);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ByteEscape {
	Digits(ByteDigits),
	Null,
	Backslash,
	LineFeed,
	CarriageReturn,
	HorizontalTabulation,
	SingleQuote,
	DoubleQuote,
}
reborrow_copy!(ByteEscape);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct UnicodeEscape {
	pub chars: StackVec<(HexDigit, usize), 1, 6>,
}
reborrow_copy!(UnicodeEscape);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct NonZeroUnicodeEscape {
	pub chars: StackVec<(NonZero<HexDigit>, usize), 1, 6>,
}
reborrow_copy!(NonZeroUnicodeEscape);

impl<E: ExcludeList<Type = char>> Exclude<E, char> {
	pub const fn new(c: char) -> Option<Self> {
		if contains_char(c, E::EXCLUDED_VALUES) {
			None
		} else {
			Some(Self { inner: c })
		}
	}
}

impl<E: ExcludeList<Type = ByteEscape>> Exclude<E, ByteEscape> {
	pub const fn new(byte: ByteEscape) -> Option<Self> {
		if contains_byte_escape(byte, E::EXCLUDED_VALUES) {
			None
		} else {
			Some(Self { inner: byte })
		}
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AsciiForbidList;
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CharForbidList;

impl ExcludeList for CharForbidList {
	type Type = char;

	const EXCLUDED_VALUES: &'static [Self::Type] = &['\'', '\\', '\n', '\r', '\t'];
}
pub type CharVerbatim = Exclude<CharForbidList>;

impl ExcludeList for AsciiForbidList {
	type Type = AsciiDigit;

	const EXCLUDED_VALUES: &'static [Self::Type] = &[
		AsciiDigit::new(b'\'').unwrap(),
		AsciiDigit::new(b'\\').unwrap(),
		AsciiDigit::new(b'\n').unwrap(),
		AsciiDigit::new(b'\r').unwrap(),
		AsciiDigit::new(b'\t').unwrap(),
	];
}
pub type AsciiVerbatim = Exclude<AsciiForbidList>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Char {
	Verbatim(CharVerbatim),
	Quote(QuoteEscape),
	Ascii(AsciiEscape),
	Unicode(UnicodeEscape),
}
reborrow_copy!(Char);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Byte {
	Verbatim(AsciiVerbatim),
	Byte(ByteEscape),
}
reborrow_copy!(Byte);

#[repr(C)]
pub struct CharLiteral<'a, P: Pointer> {
	pub char: Char,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(CharLiteral, CharLiteralBox, CharLiteralRef, CharLiteralDyn);
derive_struct!(CharLiteral, char, suffix, span);

#[repr(C)]
pub struct ByteLiteral<'a, P: Pointer> {
	pub byte: Byte,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(ByteLiteral, ByteLiteralBox, ByteLiteralRef, ByteLiteralDyn);
derive_struct!(ByteLiteral, byte, suffix, span);

impl Spanned for CharLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
impl Spanned for ByteLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
