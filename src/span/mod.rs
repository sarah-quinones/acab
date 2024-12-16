#[derive(Copy, Clone, Debug)]
enum SpanRepr {
	CallSite,
	MixedSite,
	Internal(proc::Span),
}

#[derive(Copy, Clone, Debug)]
pub struct Span {
	repr: SpanRepr,
}

impl PartialEq for Span {
	fn eq(&self, _: &Self) -> bool {
		true
	}
}
impl Eq for Span {}

impl Span {
	pub(crate) fn from_repr(span: proc::Span) -> Self {
		Self {
			repr: SpanRepr::Internal(span),
		}
	}

	pub(crate) fn repr(&self) -> proc::Span {
		match self.repr {
			SpanRepr::CallSite => proc::Span::call_site(),
			SpanRepr::MixedSite => proc::Span::mixed_site(),
			SpanRepr::Internal(span) => span,
		}
	}

	pub fn located_at(&self, other: Self) -> Self {
		Self::from_repr(self.repr().located_at(other.repr()))
	}

	pub fn resolved_at(&self, other: Self) -> Self {
		Self::from_repr(self.repr().resolved_at(other.repr()))
	}

	pub const fn call_site() -> Self {
		Span { repr: SpanRepr::CallSite }
	}

	pub const fn mixed_site() -> Self {
		Span { repr: SpanRepr::MixedSite }
	}
}
