use crate::pointer::*;
use crate::*;
use unicode_xid::UnicodeXID;

#[repr(C)]
pub struct IdentOrKeyword<'a, P: Pointer> {
	edition: Edition,
	source: P::Boxed<'a, str>,
}
as_ref!(IdentOrKeyword, IdentOrKeywordBox, IdentOrKeywordRef, IdentOrKeywordDyn);
derive_struct!(IdentOrKeyword, edition, source);

#[repr(C)]
pub struct NonKeywordIdent<'a, P: Pointer> {
	edition: Edition,
	source: P::Boxed<'a, str>,
}
as_ref!(NonKeywordIdent, NonKeywordIdentBox, NonKeywordIdentRef, NonKeywordIdentDyn);
derive_struct!(NonKeywordIdent, edition, source);

#[repr(C)]
pub struct RawIdent<'a, P: Pointer> {
	edition: Edition,
	source: P::Boxed<'a, str>,
}
as_ref!(RawIdent, RawIdentBox, RawIdentRef, RawIdentDyn);
derive_struct!(RawIdent, edition, source);

#[repr(C)]
pub enum IdentRepr<'a, P: Pointer> {
	Normal(NonKeywordIdent<'a, P>),
	Raw(RawIdent<'a, P>),
}
as_ref!(IdentRepr, IdentReprBox, IdentReprRef, IdentReprDyn);
derive_enum!(IdentRepr, Normal, Raw);

#[repr(C)]
pub struct Ident<'a, P: Pointer> {
	pub ident: IdentRepr<'a, P>,
	pub span: Span,
}
as_ref!(Ident, IdentBox, IdentRef, IdentDyn);
derive_struct!(Ident, ident, span);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IdentOrKeywordParseError {
	Empty,
	Underscore,
	InvalidLeadingChar { c: char },
	InvalidTrailingChar { index: usize, c: char },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IdentParseError {
	Empty,
	Underscore,
	InvalidLeadingChar { c: char },
	InvalidTrailingChar { index: usize, c: char },
	Reserved,
}

impl<'a, P: Pointer> IdentOrKeyword<'a, P> {
	/// # Safety
	/// - `source != "_"`
	/// - leading character of `source` must be `_` or [`XID_Start`](http://unicode.org/cldr/utility/list-unicodeset.jsp?a=%5B%3AXID_Start%3A%5D&abb=on&g=&i=)
	/// - non leading characters of `source` must be [`XID_Continue`](http://unicode.org/cldr/utility/list-unicodeset.jsp?a=%5B%3AXID_Continue%3A%5D&abb=on&g=&i=)
	pub const unsafe fn new_unchecked(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Self {
		Self {
			edition,
			source: as_boxed(source),
		}
	}

	pub fn new(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Result<Self, IdentOrKeywordParseError> {
		fn check_valid(edition: Edition, source: &str) -> Result<(), IdentOrKeywordParseError> {
			_ = edition;
			if source == "_" {
				return Err(IdentOrKeywordParseError::Underscore);
			}

			let mut chars = source.char_indices();
			let Some((_, leading)) = chars.next() else {
				return Err(IdentOrKeywordParseError::Empty);
			};

			if leading != '_' && !leading.is_xid_start() {
				return Err(IdentOrKeywordParseError::InvalidLeadingChar { c: leading });
			}

			for (i, c) in &mut chars {
				if !c.is_xid_continue() {
					return Err(IdentOrKeywordParseError::InvalidTrailingChar { index: i, c });
				}
			}
			Ok(())
		}

		check_valid(edition, &source)?;
		// SAFETY: we just performed a validity check
		Ok(unsafe { Self::new_unchecked(edition, source) })
	}
}

impl<'a, P: Pointer> RawIdent<'a, P> {
	/// # Safety
	/// - must be a valid [`IdentOrKeyword`], with the exception of `["crate", "self", "super",
	///   "Self"]`
	pub const unsafe fn new_unchecked(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Self {
		Self {
			edition,
			source: as_boxed(source),
		}
	}

	pub fn new(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Result<Self, IdentParseError> {
		fn check_valid(edition: Edition, source: &str) -> Result<(), IdentParseError> {
			if ["crate", "self", "super", "Self"].contains(&source) {
				return Err(IdentParseError::Reserved);
			}
			match IdentOrKeyword::new(edition, source) {
				Ok(_) => Ok(()),
				Err(IdentOrKeywordParseError::Underscore) => Err(IdentParseError::Underscore),
				Err(IdentOrKeywordParseError::Empty) => Err(IdentParseError::Empty),
				Err(IdentOrKeywordParseError::InvalidLeadingChar { c }) => Err(IdentParseError::InvalidLeadingChar { c }),
				Err(IdentOrKeywordParseError::InvalidTrailingChar { index, c }) => Err(IdentParseError::InvalidTrailingChar { index, c }),
			}
		}

		check_valid(edition, &source)?;
		Ok(unsafe { Self::new_unchecked(edition, source) })
	}
}

impl<'a, P: Pointer> NonKeywordIdent<'a, P> {
	/// # Safety
	/// - must be a valid [`IdentOrKeyword`], with the exception of `["as", "break", "const",
	///   "continue", "crate", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in",
	///   "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self",
	///   "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where", "while",
	///   "abstract", "become", "box", "do", "final", "macro", "override", "priv", "typeof",
	///   "unsized", "virtual", "yield"]`
	/// - if `edition >= Edition::Edition2018`, must not be one of `["async", "await", "dyn",
	///   "try"]`
	pub const unsafe fn new_unchecked(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Self {
		Self {
			edition,
			source: as_boxed(source),
		}
	}

	pub fn new(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Result<Self, IdentParseError> {
		fn check_valid(edition: Edition, source: &str) -> Result<(), IdentParseError> {
			if [
				"as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop",
				"match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
				"unsafe", "use", "where", "while", "abstract", "become", "box", "do", "final", "macro", "override", "priv", "typeof", "unsized",
				"virtual", "yield",
			]
			.contains(&source)
			{
				return Err(IdentParseError::Reserved);
			}

			if edition >= Edition::Edition2018 && ["async", "await", "dyn", "try"].contains(&source) {
				return Err(IdentParseError::Reserved);
			}
			match IdentOrKeywordRef::new(edition, source) {
				Ok(_) => Ok(()),
				Err(IdentOrKeywordParseError::Underscore) => Err(IdentParseError::Underscore),
				Err(IdentOrKeywordParseError::Empty) => Err(IdentParseError::Empty),
				Err(IdentOrKeywordParseError::InvalidLeadingChar { c }) => Err(IdentParseError::InvalidLeadingChar { c }),
				Err(IdentOrKeywordParseError::InvalidTrailingChar { index, c }) => Err(IdentParseError::InvalidTrailingChar { index, c }),
			}
		}
		check_valid(edition, &source)?;
		Ok(unsafe { Self::new_unchecked(edition, source) })
	}
}

impl<'a, P: Pointer> Ident<'a, P> {
	pub fn new(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>, span: Span) -> Result<Self, IdentParseError> {
		match NonKeywordIdent::new(edition, source) {
			Ok(ident) => Ok(Self {
				span,
				ident: IdentRepr::Normal(ident),
			}),
			Err(e) => Err(e),
		}
	}

	pub fn raw(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>, span: Span) -> Result<Self, IdentParseError> {
		match RawIdent::new(edition, source) {
			Ok(ident) => Ok(Self {
				span,
				ident: IdentRepr::Raw(ident),
			}),
			Err(e) => Err(e),
		}
	}
}

impl IdentDyn<'_> {
	pub const fn as_str(&self) -> &str {
		match self.rb().ident {
			IdentRepr::Normal(ident) => ident.source,
			IdentRepr::Raw(ident) => ident.source,
		}
	}
}
