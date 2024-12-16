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
const fn contains_ascii_digit(needle: AsciiDigit, haystack: &[AsciiDigit]) -> bool {
	let mut haystack = haystack;
	while let Some((&hay, next)) = haystack.split_first() {
		if needle.as_u8() == hay.as_u8() {
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
			(ByteEscape::Digits(left), ByteEscape::Digits(right)) => left.msb.as_u8() == right.msb.as_u8() && left.lsb.as_u8() == right.lsb.as_u8(),
			(ByteEscape::Null, ByteEscape::Null) => true,
			(ByteEscape::Backslash, ByteEscape::Backslash) => true,
			(ByteEscape::LineFeed, ByteEscape::LineFeed) => true,
			(ByteEscape::CarriageReturn, ByteEscape::CarriageReturn) => true,
			(ByteEscape::HorizontalTabulation, ByteEscape::HorizontalTabulation) => true,
			(ByteEscape::Quote(QuoteEscape::Single), ByteEscape::Quote(QuoteEscape::Single)) => true,
			(ByteEscape::Quote(QuoteEscape::Double), ByteEscape::Quote(QuoteEscape::Double)) => true,
			_ => false,
		} {
			return true;
		}
		haystack = next;
	}
	false
}

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum QuoteEscape {
		Single,
		Double,
	}
);
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct AsciiDigits {
	pub msb: OctDigit,
	pub lsb: HexDigit,
}

fn write_hex(x: HexDigit, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	if x.is_uppercase() {
		f.write_char(char::from_u32((x.as_u8() - 0xA + b'A') as u32).unwrap())
	} else {
		match x.as_u8() {
			0..=9 => f.write_char(char::from_u32((x.as_u8() + b'0') as u32).unwrap()),
			0xA..=0xF => f.write_char(char::from_u32((x.as_u8() - 0xA + b'A') as u32).unwrap()),
			_ => panic!(),
		}
	}
}

impl fmt::Debug for AsciiDigits {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("b\'\\x")?;
		f.write_char(char::from_u32((self.msb.as_u8() + b'0') as u32).unwrap())?;
		write_hex(self.lsb, f)?;
		f.write_char('\'')
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ByteDigits {
	pub msb: HexDigit,
	pub lsb: HexDigit,
}

impl fmt::Debug for ByteDigits {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("b\'\\x")?;

		write_hex(self.msb, f)?;
		write_hex(self.lsb, f)?;
		f.write_char('\'')
	}
}

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum AsciiEscape {
		Digits(AsciiDigits),
		Null,
		Backslash,
		LineFeed,
		CarriageReturn,
		HorizontalTabulation,
	}
);
reborrow_copy!(AsciiEscape);

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum ByteEscape {
		Digits(ByteDigits),
		Null,
		Backslash,
		LineFeed,
		CarriageReturn,
		HorizontalTabulation,
		Quote(QuoteEscape),
	}
);
reborrow_copy!(ByteEscape);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TrailingUnderscores<Digit: Copy> {
	pub digit: Digit,
	pub underscore_count: usize,
}
reborrow_copy!(TrailingUnderscores<Digit> where Digit: Copy);

impl<Digit: Copy> TrailingUnderscores<Digit> {
	pub const fn new(digit: Digit, underscore_count: usize) -> Self {
		Self { digit, underscore_count }
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct UnicodeEscape {
	pub chars: StackVec<TrailingUnderscores<HexDigit>, 1, 6>,
}
reborrow_copy!(UnicodeEscape);
impl fmt::Debug for UnicodeEscape {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("\'\\u{")?;
		for &c in &*self.chars {
			write_hex(c.digit, f)?;
			for _ in 0..c.underscore_count {
				f.write_char('_')?;
			}
		}
		f.write_str("}\'")
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct NonZeroUnicodeEscape {
	pub chars: StackVec<TrailingUnderscores<NonZero<HexDigit>>, 1, 6>,
}
reborrow_copy!(NonZeroUnicodeEscape);

impl fmt::Debug for NonZeroUnicodeEscape {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		unsafe { core::mem::transmute::<&NonZeroUnicodeEscape, &UnicodeEscape>(self).fmt(f) }
	}
}

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

impl<E: ExcludeList<Type = AsciiDigit>> Exclude<E, AsciiDigit> {
	pub const fn new(digit: AsciiDigit) -> Option<Self> {
		if contains_ascii_digit(digit, E::EXCLUDED_VALUES) {
			None
		} else {
			Some(Self { inner: digit })
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

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum Char {
		Verbatim(CharVerbatim),
		Quote(QuoteEscape),
		Ascii(AsciiEscape),
		Unicode(UnicodeEscape),
	}
);
reborrow_copy!(Char);

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum Byte {
		Verbatim(AsciiVerbatim),
		Byte(ByteEscape),
	}
);
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
