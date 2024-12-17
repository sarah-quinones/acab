use crate::Span;
use core::fmt;

pub type Spacing = bool;

pub const ALONE: Spacing = false;
pub const JOINT: Spacing = true;

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Punct<const P: char, const SPACING: Spacing> {
	pub span: Span,
}
reborrow_copy!(Punct<P, SPACING> where const P: char, const SPACING: Spacing);

impl<const P: char, const SPACING: Spacing> fmt::Debug for Punct<P, SPACING> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		core::fmt::Debug::fmt(&P, f)
	}
}
