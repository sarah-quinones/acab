use crate::punct::*;
use crate::{Reborrow, ReborrowTarget};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Lifetime<Name> {
	pub tick: Punct<'\'', JOINT>,
	pub name: Name,
}

impl<'a, Name: ReborrowTarget<'a>> ReborrowTarget<'a> for Lifetime<Name> {
	type Ref = Lifetime<Name::Ref>;
}

unsafe impl<Name: Reborrow> Reborrow for Lifetime<Name> {
	#[cfg(feature = "alloc")]
	type Box = Lifetime<Name::Box>;
	type Target = Lifetime<Name::Target>;

	fn __rb(&self) -> <Self::Target as ReborrowTarget<'_>>::Ref {
		Lifetime {
			tick: self.tick,
			name: self.name.__rb(),
		}
	}

	#[cfg(feature = "alloc")]
	fn to_box(&self) -> Self::Box {
		Lifetime {
			tick: self.tick,
			name: self.name.to_box(),
		}
	}
}
