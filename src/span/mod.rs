#[derive(Copy, Clone, Debug)]
pub struct Span {
	pub repr: proc::Span,
}

impl From<proc::Span> for Span {
	fn from(repr: proc::Span) -> Self {
		Self { repr }
	}
}

impl PartialEq for Span {
	fn eq(&self, _: &Self) -> bool {
		true
	}
}
impl Eq for Span {}

impl Span {
	pub fn located_at(&self, other: Self) -> Self {
		self.repr.located_at(other.repr).into()
	}

	pub fn resolved_at(&self, other: Self) -> Self {
		self.repr.resolved_at(other.repr).into()
	}

	pub fn call_site() -> Self {
		proc::Span::call_site().into()
	}

	pub fn mixed_site() -> Self {
		proc::Span::mixed_site().into()
	}
}
