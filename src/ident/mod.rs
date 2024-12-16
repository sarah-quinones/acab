use crate::pointer::*;
use crate::*;

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
	pub repr: IdentRepr<'a, P>,
	pub span: Span,
}
as_ref!(Ident, IdentBox, IdentRef, IdentDyn);
derive_struct!(Ident, repr, span);

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

const fn contains_str(needle: &str, haystack: &[&str]) -> bool {
	let mut haystack = haystack;
	while let Some((&hay, next)) = haystack.split_first() {
		if konst::eq_str(needle, hay) {
			return true;
		}
		haystack = next;
	}
	false
}

impl<'a, P: Pointer> IdentOrKeyword<'a, P> {
	/// # Safety
	/// - `source != "_"`
	/// - leading character of `source` must be `_` or [`XID_Start`](http://unicode.org/cldr/utility/list-unicodeset.jsp?a=%5B%3AXID_Start%3A%5D&abb=on&g=&i=)
	/// - non leading characters of `source` must be [`XID_Continue`](http://unicode.org/cldr/utility/list-unicodeset.jsp?a=%5B%3AXID_Continue%3A%5D&abb=on&g=&i=)
	pub const unsafe fn new_unchecked(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Self {
		Self {
			edition,
			source: into_boxed(source),
		}
	}

	pub const fn new(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Result<Self, (P::Boxed<'a, str>, IdentOrKeywordParseError)> {
		const fn check_valid(edition: Edition, source: &str) -> Result<(), IdentOrKeywordParseError> {
			_ = edition;
			if konst::eq_str(source, "_") {
				return Err(IdentOrKeywordParseError::Underscore);
			}

			let chars = konst::string::char_indices(source);
			let Some(((_, leading), mut chars)) = chars.next() else {
				return Err(IdentOrKeywordParseError::Empty);
			};

			if leading != '_' && !crate::unicode::xid::is_xid_start(leading) {
				return Err(IdentOrKeywordParseError::InvalidLeadingChar { c: leading });
			}

			while let Some(((i, c), next)) = chars.next() {
				if !crate::unicode::xid::is_xid_continue(c) {
					return Err(IdentOrKeywordParseError::InvalidTrailingChar { index: i, c });
				}
				chars = next;
			}
			Ok(())
		}

		let source = into_boxed(source);
		match check_valid(edition, as_ref(&source)) {
			Ok(()) => {},
			Err(e) => return Err((source, e)),
		};
		// SAFETY: we just performed a validity check
		Ok(Self { edition, source })
	}
}

impl<'a, P: Pointer> RawIdent<'a, P> {
	/// # Safety
	/// - must be a valid [`IdentOrKeyword`], with the exception of `["crate", "self", "super",
	///   "Self"]`
	pub const unsafe fn new_unchecked(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Self {
		Self {
			edition,
			source: into_boxed(source),
		}
	}

	pub const fn new(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Result<Self, (P::Boxed<'a, str>, IdentParseError)> {
		const fn check_valid(edition: Edition, source: &str) -> Result<(), IdentParseError> {
			if contains_str(source, &["crate", "self", "super", "Self"]) {
				return Err(IdentParseError::Reserved);
			}
			match IdentOrKeyword::new(edition, source) {
				Ok(_) => Ok(()),
				Err((_, IdentOrKeywordParseError::Underscore)) => Err(IdentParseError::Underscore),
				Err((_, IdentOrKeywordParseError::Empty)) => Err(IdentParseError::Empty),
				Err((_, IdentOrKeywordParseError::InvalidLeadingChar { c })) => Err(IdentParseError::InvalidLeadingChar { c }),
				Err((_, IdentOrKeywordParseError::InvalidTrailingChar { index, c })) => Err(IdentParseError::InvalidTrailingChar { index, c }),
			}
		}

		let source = into_boxed(source);
		match check_valid(edition, as_ref(&source)) {
			Ok(()) => {},
			Err(e) => return Err((source, e)),
		}
		Ok(Self { edition, source })
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
			source: into_boxed(source),
		}
	}

	pub const fn new(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>) -> Result<Self, (P::Boxed<'a, str>, IdentParseError)> {
		const fn check_valid(edition: Edition, source: &str) -> Result<(), IdentParseError> {
			if contains_str(source, &[
				"as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop",
				"match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
				"unsafe", "use", "where", "while", "abstract", "become", "box", "do", "final", "macro", "override", "priv", "typeof", "unsized",
				"virtual", "yield",
			]) {
				return Err(IdentParseError::Reserved);
			}

			if !matches!(edition, Edition::Edition2015) && contains_str(source, &["async", "await", "dyn", "try"]) {
				return Err(IdentParseError::Reserved);
			}
			match IdentOrKeywordRef::new(edition, source) {
				Ok(_) => Ok(()),
				Err((_, IdentOrKeywordParseError::Underscore)) => Err(IdentParseError::Underscore),
				Err((_, IdentOrKeywordParseError::Empty)) => Err(IdentParseError::Empty),
				Err((_, IdentOrKeywordParseError::InvalidLeadingChar { c })) => Err(IdentParseError::InvalidLeadingChar { c }),
				Err((_, IdentOrKeywordParseError::InvalidTrailingChar { index, c })) => Err(IdentParseError::InvalidTrailingChar { index, c }),
			}
		}
		let source = into_boxed(source);
		match check_valid(edition, as_ref(&source)) {
			Ok(()) => {},
			Err(e) => return Err((source, e)),
		}
		Ok(Self { edition, source })
	}
}

impl<'a, P: Pointer> Ident<'a, P> {
	pub const fn new(
		edition: Edition,
		source: impl PointerTo<'a, str, Pointer = P>,
		span: Span,
	) -> Result<Self, (P::Boxed<'a, str>, IdentParseError)> {
		match NonKeywordIdent::new(edition, as_ref(&source)) {
			Ok(_) => Ok(unsafe { Self::new_unchecked(edition, source, span) }),
			Err((_, e)) => Err((into_boxed(source), e)),
		}
	}

	pub const fn raw(
		edition: Edition,
		source: impl PointerTo<'a, str, Pointer = P>,
		span: Span,
	) -> Result<Self, (P::Boxed<'a, str>, IdentParseError)> {
		match RawIdent::new(edition, as_ref(&source)) {
			Ok(_) => Ok(unsafe { Self::raw_unchecked(edition, source, span) }),
			Err((_, e)) => Err((into_boxed(source), e)),
		}
	}

	/// # Safety
	/// see [`NonKeywordIdent::new_unchecked`]
	pub const unsafe fn new_unchecked(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>, span: Span) -> Self {
		unsafe {
			Self {
				span,
				repr: IdentRepr::Normal(NonKeywordIdent::new_unchecked(edition, source)),
			}
		}
	}

	/// # Safety
	/// see [`RawIdent::new_unchecked`]
	pub const unsafe fn raw_unchecked(edition: Edition, source: impl PointerTo<'a, str, Pointer = P>, span: Span) -> Self {
		unsafe {
			Self {
				span,
				repr: IdentRepr::Raw(RawIdent::new_unchecked(edition, source)),
			}
		}
	}
}

impl IdentDyn<'_> {
	pub const fn as_str(&self) -> &str {
		match self.rb().repr {
			IdentRepr::Normal(ident) => ident.source,
			IdentRepr::Raw(ident) => ident.source,
		}
	}
}

impl IdentOrKeywordDyn<'_> {
	pub const fn as_str(&self) -> &str {
		self.rb().source
	}
}
