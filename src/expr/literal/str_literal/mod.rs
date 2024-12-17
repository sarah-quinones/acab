use char_literal::{NonZeroUnicodeEscape, TrailingUnderscores};

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
pub type AsciiForRawByteStrVerbatim = Exclude<AsciiForRawStrForbidList>;

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
			chars: StackVec::<_, 1, 6>::new([TrailingUnderscores::new(null, 0)]),
		}; 6];

		let mut i = 1;
		while i < 6 {
			esc[i] = esc[i - 1];
			esc[i].chars.push(TrailingUnderscores::new(null, 0));

			i += 1;
		}

		esc
	};
}

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum StrContinueWhitespace {
		HorizontalTabulation,
		LineFeed,
		CarriageReturn,
		Space,
	}
);
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
pub struct StrContents<'a, P: Pointer> {
	char_contents: Option<P::Boxed<'a, [u32]>>,
	other_contents: P::Boxed<'a, [CharForStr<'a, P>]>,
}
as_ref!(StrContents, StrContentsBox, StrContentsRef, StrContentsDyn);
derive_struct_copy_clone!(StrContents, char_contents, other_contents);

impl PartialEq for StrContentsRef<'_> {
	fn eq(&self, other: &Self) -> bool {
		match (self.char_contents, other.char_contents) {
			(None, None) => self.other_contents == other.other_contents,
			(Some(left), Some(right)) => left == right && self.other_contents == other.other_contents,
			_ => {
				if self.len() != other.len() {
					return false;
				}
				for i in 0..self.len() {
					if self.get(i) != other.get(i) {
						return false;
					}
				}
				true
			},
		}
	}
}

impl core::fmt::Debug for StrContentsRef<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_list().entries((0..self.len()).map(|i| self.get(i))).finish()
	}
}

#[repr(C)]
pub struct CStrContents<'a, P: Pointer> {
	char_contents: Option<P::Boxed<'a, [u32]>>,
	other_contents: P::Boxed<'a, [CharForCStr<'a, P>]>,
}
as_ref!(CStrContents, CStrContentsBox, CStrContentsRef, CStrContentsDyn);
derive_struct_copy_clone!(CStrContents, char_contents, other_contents);

impl PartialEq for CStrContentsRef<'_> {
	fn eq(&self, other: &Self) -> bool {
		match (self.char_contents, other.char_contents) {
			(None, None) => self.other_contents == other.other_contents,
			(Some(left), Some(right)) => left == right && self.other_contents == other.other_contents,
			_ => {
				if self.len() != other.len() {
					return false;
				}
				for i in 0..self.len() {
					if self.get(i) != other.get(i) {
						return false;
					}
				}
				true
			},
		}
	}
}

impl core::fmt::Debug for CStrContentsRef<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_list().entries((0..self.len()).map(|i| self.get(i))).finish()
	}
}

#[repr(C)]
pub struct ByteStrContents<'a, P: Pointer> {
	ascii_contents: Option<P::Boxed<'a, [u8]>>,
	other_contents: P::Boxed<'a, [AsciiForStr<'a, P>]>,
}
as_ref!(ByteStrContents, ByteStrContentsBox, ByteStrContentsRef, ByteStrContentsDyn);
derive_struct_copy_clone!(ByteStrContents, ascii_contents, other_contents);

impl PartialEq for ByteStrContentsRef<'_> {
	fn eq(&self, other: &Self) -> bool {
		match (self.ascii_contents, other.ascii_contents) {
			(None, None) => self.other_contents == other.other_contents,
			(Some(left), Some(right)) => left == right && self.other_contents == other.other_contents,
			_ => {
				if self.len() != other.len() {
					return false;
				}
				for i in 0..self.len() {
					if self.get(i) != other.get(i) {
						return false;
					}
				}
				true
			},
		}
	}
}
impl core::fmt::Debug for ByteStrContentsRef<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_list().entries((0..self.len()).map(|i| self.get(i))).finish()
	}
}

impl<'a, P: Pointer> StrContents<'a, P> {
	pub const fn new(contents: impl PointerTo<'a, [CharForStr<'a, P>], Pointer = P>) -> Self {
		StrContents {
			char_contents: None,
			other_contents: into_boxed(contents),
		}
	}
}

impl<'a, P: Pointer> ByteStrContents<'a, P> {
	pub const fn new(contents: impl PointerTo<'a, [AsciiForStr<'a, P>], Pointer = P>) -> Self {
		ByteStrContents {
			ascii_contents: None,
			other_contents: into_boxed(contents),
		}
	}
}

impl<'a, P: Pointer> CStrContents<'a, P> {
	pub const fn new(contents: impl PointerTo<'a, [CharForCStr<'a, P>], Pointer = P>) -> Self {
		CStrContents {
			char_contents: None,
			other_contents: into_boxed(contents),
		}
	}
}

#[repr(C)]
pub struct StrLiteral<'a, P: Pointer> {
	pub contents: StrContents<'a, P>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(StrLiteral, StrLiteralBox, StrLiteralRef, StrLiteralDyn);
derive_struct!(StrLiteral, contents, suffix, span);

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
	pub contents: ByteStrContents<'a, P>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(ByteStrLiteral, ByteStrLiteralBox, ByteStrLiteralRef, ByteStrLiteralDyn);
derive_struct!(ByteStrLiteral, contents, suffix, span);

#[repr(C)]
pub struct RawByteStrLiteral<'a, P: Pointer> {
	pub hash_count: u8,
	pub contents: P::Boxed<'a, [AsciiForRawByteStrVerbatim]>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}

as_ref!(RawByteStrLiteral, RawByteStrLiteralBox, RawByteStrLiteralRef, RawByteStrLiteralDyn);
derive_struct!(RawByteStrLiteral, hash_count, contents, suffix, span);

#[repr(C)]
pub struct CStrLiteral<'a, P: Pointer> {
	pub contents: CStrContents<'a, P>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(CStrLiteral, CStrLiteralBox, CStrLiteralRef, CStrLiteralDyn);
derive_struct!(CStrLiteral, contents, suffix, span);

#[repr(C)]
pub struct RawCStrLiteral<'a, P: Pointer> {
	pub hash_count: u8,
	pub contents: P::Boxed<'a, [CharForRawCStrVerbatim]>,
	pub suffix: Option<Suffix<'a, P>>,
	pub span: Span,
}
as_ref!(RawCStrLiteral, RawCStrLiteralBox, RawCStrLiteralRef, RawCStrLiteralDyn);
derive_struct!(RawCStrLiteral, hash_count, contents, suffix, span);

impl StrContentsDyn<'_> {
	pub const fn len(&self) -> usize {
		let this = self.rb();
		match this.char_contents {
			Some(char_contents) => char_contents.len(),
			None => this.other_contents.len(),
		}
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> CharForStrRef<'_> {
		let this = self.rb();
		match this.char_contents {
			Some(char_contents) => {
				let c = char_contents[idx];
				if c >> (u32::BITS - 1) == 0 {
					CharForStrRef::Verbatim(unsafe { CharForStrVerbatim::new_unchecked(char::from_u32_unchecked(c)) })
				} else {
					this.other_contents[(c & (u32::MAX >> 1)) as usize]
				}
			},
			None => this.other_contents[idx],
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

impl ByteStrContentsDyn<'_> {
	pub const fn len(&self) -> usize {
		let this = self.rb();
		match this.ascii_contents {
			Some(char_contents) => char_contents.len(),
			None => this.other_contents.len(),
		}
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> AsciiForStrRef<'_> {
		let this = self.rb();
		match this.ascii_contents {
			Some(ascii_contents) => {
				let c = ascii_contents[idx];
				if c >> (u8::BITS - 1) == 0 {
					unsafe { AsciiForStrRef::Verbatim(AsciiForStrVerbatim::new_unchecked(AsciiDigit::new_unchecked(c))) }
				} else {
					this.other_contents[(c & (u8::MAX >> 1)) as usize]
				}
			},
			None => this.other_contents[idx],
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

	pub const fn get(&self, idx: usize) -> AsciiForRawByteStrVerbatim {
		self.rb().contents[idx]
	}
}

impl CStrContentsDyn<'_> {
	pub const fn len(&self) -> usize {
		let this = self.rb();
		match this.char_contents {
			Some(char_contents) => char_contents.len(),
			None => this.other_contents.len(),
		}
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn get(&self, idx: usize) -> CharForCStrRef<'_> {
		let this = self.rb();
		match this.char_contents {
			Some(char_contents) => {
				let c = char_contents[idx];
				if c >> (u32::BITS - 1) == 0 {
					CharForCStrRef::Verbatim(unsafe { CharForCStrVerbatim::new_unchecked(char::from_u32_unchecked(c)) })
				} else {
					this.other_contents[(c & (u32::MAX >> 1)) as usize]
				}
			},
			None => this.other_contents[idx],
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

#[cfg(feature = "alloc")]
mod alloc {
	use super::*;

	impl FromIterator<CharForStrBox> for StrContentsBox {
		fn from_iter<T: IntoIterator<Item = CharForStrBox>>(iter: T) -> Self {
			let iter = iter.into_iter();

			let mut chars = Vec::new();
			let mut other = Vec::new();

			iter.for_each(|item| match item {
				CharForStr::Verbatim(char) => chars.push(char.get() as u32),
				char => {
					chars.push((1 << (u32::BITS - 1)) | other.len() as u32);
					other.push(char);
				},
			});

			Self {
				char_contents: Some(chars.into_boxed_slice()),
				other_contents: other.into_boxed_slice(),
			}
		}
	}

	impl FromIterator<CharForCStrBox> for CStrContentsBox {
		fn from_iter<T: IntoIterator<Item = CharForCStrBox>>(iter: T) -> Self {
			let iter = iter.into_iter();

			let mut chars = Vec::new();
			let mut other = Vec::new();

			match iter.size_hint().1 {
				Some(hi) if hi <= u32::MAX as usize => {
					iter.for_each(|item| match item {
						CharForCStr::Verbatim(char) => chars.push(char.get() as u32),
						char => {
							chars.push((1 << (u32::BITS - 1)) | other.len() as u32);
							other.push(char);
						},
					});
					Self {
						char_contents: Some(chars.into_boxed_slice()),
						other_contents: other.into_boxed_slice(),
					}
				},
				_ => {
					other.extend(iter);
					Self {
						char_contents: None,
						other_contents: other.into_boxed_slice(),
					}
				},
			}
		}
	}

	impl FromIterator<AsciiForStrBox> for ByteStrContentsBox {
		fn from_iter<T: IntoIterator<Item = AsciiForStrBox>>(iter: T) -> Self {
			let iter = iter.into_iter();

			let mut chars = Vec::new();
			let mut other = Vec::new();

			match iter.size_hint().1 {
				Some(hi) if hi <= u8::MAX as usize => {
					iter.for_each(|item| match item {
						AsciiForStrBox::Verbatim(char) => chars.push(char.get().as_u8()),
						char => {
							chars.push((1 << (u8::BITS - 1)) | other.len() as u8);
							other.push(char);
						},
					});
					Self {
						ascii_contents: Some(chars.into_boxed_slice()),
						other_contents: other.into_boxed_slice(),
					}
				},
				_ => {
					other.extend(iter);
					Self {
						ascii_contents: None,
						other_contents: other.into_boxed_slice(),
					}
				},
			}
		}
	}
}
