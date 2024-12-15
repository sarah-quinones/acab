use char_literal::NonZeroUnicodeEscape;

use super::char_literal::{AsciiEscape, ByteDigits, ByteEscape, QuoteEscape, UnicodeEscape};
use super::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AsciiForStrForbidList;
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AsciiForRawStrForbidList;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CharForStrForbidList;
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CharForRawStrForbidList;
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CharForCStrForbidList;
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CharForRawCStrForbidList;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ByteEscapeForCStrForbidList;
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct UnicodeEscapeForCStrForbidList;

impl ExcludeList for CharForStrForbidList {
	type Type = char;

	const EXCLUDED_VALUES: &'static [Self::Type] = &['"', '\\', '\r'];
}
pub type CharForStrVerbatim = Exclude<CharForStrForbidList>;

impl ExcludeList for CharForRawStrForbidList {
	type Type = char;

	const EXCLUDED_VALUES: &'static [Self::Type] = &['\r'];
}
pub type CharForRawStrVerbatim = Exclude<CharForRawStrForbidList>;

impl ExcludeList for AsciiForStrForbidList {
	type Type = AsciiDigit;

	const EXCLUDED_VALUES: &'static [Self::Type] = &[
		AsciiDigit::new(b'"').unwrap(),
		AsciiDigit::new(b'\\').unwrap(),
		AsciiDigit::new(b'\r').unwrap(),
	];
}
pub type AsciiForStrVerbatim = Exclude<AsciiForStrForbidList>;

impl ExcludeList for AsciiForRawStrForbidList {
	type Type = AsciiDigit;

	const EXCLUDED_VALUES: &'static [Self::Type] = &[AsciiDigit::new(b'\r').unwrap()];
}
pub type AsciiForRawStrVerbatim = Exclude<AsciiForRawStrForbidList>;

impl ExcludeList for CharForCStrForbidList {
	type Type = char;

	const EXCLUDED_VALUES: &'static [Self::Type] = &['"', '\\', '\r', '\0'];
}
pub type CharForCStrVerbatim = Exclude<CharForCStrForbidList>;

impl ExcludeList for CharForRawCStrForbidList {
	type Type = char;

	const EXCLUDED_VALUES: &'static [Self::Type] = &['\r', '\0'];
}
pub type CharForRawCStrVerbatim = Exclude<CharForRawCStrForbidList>;

impl ExcludeList for ByteEscapeForCStrForbidList {
	type Type = ByteEscape;

	const EXCLUDED_VALUES: &'static [Self::Type] = &[
		ByteEscape::Null,
		ByteEscape::Digits(ByteDigits {
			msb: HexDigit::new_lowercase(0).unwrap(),
			lsb: HexDigit::new_lowercase(0).unwrap(),
		}),
	];
}
pub type ByteEscapeForCStr = Exclude<ByteEscapeForCStrForbidList>;

impl ExcludeList for UnicodeEscapeForCStrForbidList {
	type Type = UnicodeEscape;

	const EXCLUDED_VALUES: &'static [Self::Type] = &const {
		let null = HexDigit::new_lowercase(0).unwrap();

		let mut esc = [UnicodeEscape {
			chars: StackVec::<_, 1, 6>::new([(null, 0)]),
		}; 6];

		let mut i = 1;
		while i < 6 {
			esc[i] = esc[i - 1];
			esc[i].chars.push((null, 0));

			i += 1;
		}

		esc
	};
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum StrContinueWhitespace {
	HorizontalTabulation,
	LineFeed,
	CarriageReturn,
	Space,
}
reborrow_copy!(StrContinueWhitespace);

#[repr(u8)]
pub enum CharForStr<'a, P: Pointer> {
	Verbatim(CharForStrVerbatim),
	Quote(QuoteEscape),
	Ascii(AsciiEscape),
	Unicode(UnicodeEscape),
	Continue(P::Boxed<'a, [StrContinueWhitespace]>),
}
as_ref!(CharForStr, CharForStrBox, CharForStrRef, CharForStrDyn);
derive_enum!(CharForStr, Verbatim, Quote, Ascii, Unicode, Continue);

#[repr(u8)]
pub enum CharForCStr<'a, P: Pointer> {
	Verbatim(CharForCStrVerbatim),
	Byte(ByteEscapeForCStr),
	Unicode(NonZeroUnicodeEscape),
	Continue(P::Boxed<'a, [StrContinueWhitespace]>),
}
as_ref!(CharForCStr, CharForCStrBox, CharForCStrRef, CharForCStrDyn);
derive_enum!(CharForCStr, Verbatim, Byte, Unicode, Continue);

#[repr(u8)]
pub enum AsciiForStr<'a, P: Pointer> {
	Verbatim(AsciiForStrVerbatim),
	Quote(QuoteEscape),
	Byte(ByteEscape),
	Continue(P::Boxed<'a, [StrContinueWhitespace]>),
}
as_ref!(AsciiForStr, AsciiForStrBox, AsciiForStrRef, AsciiForStrDyn);
derive_enum!(AsciiForStr, Verbatim, Quote, Byte, Continue);

#[repr(C)]
pub struct StrLiteral<'a, P: Pointer> {
	char_contents: P::Boxed<'a, [u32]>,
	other_contents: P::Boxed<'a, [CharForStr<'a, P>]>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(StrLiteral, StrLiteralBox, StrLiteralRef, StrLiteralDyn);
derive_struct!(StrLiteral, char_contents, other_contents, suffix, span);

#[repr(C)]
pub struct RawStrLiteral<'a, P: Pointer> {
	pub hash_count: u8,
	pub contents: P::Boxed<'a, [CharForRawStrVerbatim]>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(RawStrLiteral, RawStrLiteralBox, RawStrLiteralRef, RawStrLiteralDyn);
derive_struct!(RawStrLiteral, hash_count, contents, suffix, span);

#[repr(C)]
pub struct ByteStrLiteral<'a, P: Pointer> {
	ascii_contents: P::Boxed<'a, [u8]>,
	other_contents: P::Boxed<'a, [AsciiForStr<'a, P>]>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(ByteStrLiteral, ByteStrLiteralBox, ByteStrLiteralRef, ByteStrLiteralDyn);
derive_struct!(ByteStrLiteral, ascii_contents, other_contents, suffix, span);

#[repr(C)]
pub struct RawByteStrLiteral<'a, P: Pointer> {
	pub hash_count: u8,
	pub contents: P::Boxed<'a, [AsciiForRawStrVerbatim]>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}

as_ref!(RawByteStrLiteral, RawByteStrLiteralBox, RawByteStrLiteralRef, RawByteStrLiteralDyn);
derive_struct!(RawByteStrLiteral, hash_count, contents, suffix, span);

#[repr(C)]
pub struct CStrLiteral<'a, P: Pointer> {
	char_contents: P::Boxed<'a, [u32]>,
	other_contents: P::Boxed<'a, [CharForCStr<'a, P>]>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(CStrLiteral, CStrLiteralBox, CStrLiteralRef, CStrLiteralDyn);
derive_struct!(CStrLiteral, char_contents, other_contents, suffix, span);

#[repr(C)]
pub struct RawCStrLiteral<'a, P: Pointer> {
	pub hash_count: u8,
	pub contents: P::Boxed<'a, [CharForRawCStrVerbatim]>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(RawCStrLiteral, RawCStrLiteralBox, RawCStrLiteralRef, RawCStrLiteralDyn);
derive_struct!(RawCStrLiteral, hash_count, contents, suffix, span);

impl StrLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().char_contents.len()
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> CharForStrRef<'_> {
		let this = *self.rb();
		let c = this.char_contents[idx];
		if c >> (u32::BITS - 1) == 0 {
			CharForStrRef::Verbatim(unsafe { CharForStrVerbatim::new_unchecked(char::from_u32_unchecked(c)) })
		} else {
			this.other_contents[(c & (u32::MAX >> 1)) as usize]
		}
	}
}

impl RawStrLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().contents.len()
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> CharForRawStrVerbatim {
		self.rb().contents[idx]
	}
}

impl ByteStrLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().ascii_contents.len()
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> AsciiForStrRef<'_> {
		let this = *self.rb();
		let c = this.ascii_contents[idx];
		if c >> (u8::BITS - 1) == 0 {
			unsafe { AsciiForStrRef::Verbatim(AsciiForStrVerbatim::new_unchecked(AsciiDigit::new_unchecked(c))) }
		} else {
			this.other_contents[(c & (u8::MAX >> 1)) as usize]
		}
	}
}

impl RawByteStrLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().contents.len()
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> AsciiForRawStrVerbatim {
		self.rb().contents[idx]
	}
}

impl CStrLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().char_contents.len()
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> CharForCStrRef<'_> {
		let this = *self.rb();
		let c = this.char_contents[idx];
		if c >> (u32::BITS - 1) == 0 {
			CharForCStrRef::Verbatim(unsafe { CharForCStrVerbatim::new_unchecked(char::from_u32_unchecked(c)) })
		} else {
			this.other_contents[(c & (u32::MAX >> 1)) as usize]
		}
	}
}

impl RawCStrLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().contents.len()
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> CharForRawCStrVerbatim {
		self.rb().contents[idx]
	}
}

impl Spanned for StrLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
impl Spanned for RawStrLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
impl Spanned for ByteStrLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
impl Spanned for RawByteStrLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
impl Spanned for CStrLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
impl Spanned for RawCStrLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
