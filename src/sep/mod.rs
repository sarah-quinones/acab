use crate::punct::*;
use crate::{Reborrow, ReborrowTarget};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PathSep(Punct<':', JOINT>, Punct<':', ALONE>);
pub type CommaSep = Punct<',', ALONE>;

reborrow_copy!(PathSep);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Separated<Sep, Arg> {
	pub sep: Sep,
	pub arg: Arg,
}

impl<'a, Sep: ReborrowTarget<'a>, Arg: ReborrowTarget<'a>> ReborrowTarget<'a> for Separated<Sep, Arg> {
	type Ref = Separated<Sep::Ref, Arg::Ref>;
}

unsafe impl<Sep: Reborrow, Arg: Reborrow> Reborrow for Separated<Sep, Arg> {
	#[cfg(feature = "alloc")]
	type Box = Separated<Sep::Box, Arg::Box>;
	type Target = Separated<Sep::Target, Arg::Target>;

	fn __rb(&self) -> <Self::Target as ReborrowTarget<'_>>::Ref {
		Separated {
			sep: self.sep.__rb(),
			arg: self.arg.__rb(),
		}
	}

	#[cfg(feature = "alloc")]
	fn to_box(&self) -> Self::Box {
		Separated {
			sep: self.sep.to_box(),
			arg: self.arg.to_box(),
		}
	}
}
