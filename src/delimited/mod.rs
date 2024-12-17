use core::marker::PhantomData;

use crate::pointer::*;
use crate::sep::*;
use crate::{ReborrowTarget, Span};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct DelimitedRepr<Sep: Copy, Arg, Rest, const OPEN: char, const CLOSE: char> {
	pub span_open: Span,
	pub span_close: Span,
	pub span: Span,
	pub contents: Option<DelimitedContentsRepr<Sep, Arg, Rest>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct DelimitedContentsRepr<Sep: Copy, Arg, Rest> {
	pub leading: Arg,
	pub rest: Rest,
	pub trailing: Option<Sep>,
}

pub type Delimited<'a, P, Sep, Arg, const OPEN: char, const CLOSE: char> =
	DelimitedRepr<Sep, Arg, <P as Pointer>::Boxed<'a, [Separated<Sep, Arg>]>, OPEN, CLOSE>;

impl<'a, Sep: Copy, Arg: ?Sized + ReborrowTarget<'a>, Rest: ?Sized + ReborrowTarget<'a>> ReborrowTarget<'a>
	for DelimitedContentsRepr<Sep, PhantomData<Arg>, PhantomData<Rest>>
{
	type Ref = DelimitedContentsRepr<Sep, Arg::Ref, Rest::Ref>;
}
impl<'a, Sep: Copy, Arg: ?Sized + ReborrowTarget<'a>, Rest: ?Sized + ReborrowTarget<'a>, const OPEN: char, const CLOSE: char> ReborrowTarget<'a>
	for DelimitedRepr<Sep, PhantomData<Arg>, PhantomData<Rest>, OPEN, CLOSE>
{
	type Ref = DelimitedRepr<Sep, Arg::Ref, Rest::Ref, OPEN, CLOSE>;
}

unsafe impl<Sep: Copy, Arg: Reborrow, Rest: Reborrow> Reborrow for DelimitedContentsRepr<Sep, Arg, Rest> {
	#[cfg(feature = "alloc")]
	type Box = DelimitedContentsRepr<Sep, Arg::Box, Rest::Box>;
	type Target = DelimitedContentsRepr<Sep, PhantomData<Arg::Target>, PhantomData<Rest::Target>>;

	fn __rb(&self) -> <Self::Target as ReborrowTarget<'_>>::Ref {
		DelimitedContentsRepr {
			leading: self.leading.__rb(),
			rest: self.rest.__rb(),
			trailing: self.trailing,
		}
	}

	#[cfg(feature = "alloc")]
	fn to_box(&self) -> Self::Box {
		DelimitedContentsRepr {
			leading: self.leading.to_box(),
			rest: self.rest.to_box(),
			trailing: self.trailing,
		}
	}
}

unsafe impl<Sep: Copy, Arg: Reborrow, Rest: Reborrow, const OPEN: char, const CLOSE: char> Reborrow for DelimitedRepr<Sep, Arg, Rest, OPEN, CLOSE> {
	#[cfg(feature = "alloc")]
	type Box = DelimitedRepr<Sep, Arg::Box, Rest::Box, OPEN, CLOSE>;
	type Target = DelimitedRepr<Sep, PhantomData<Arg::Target>, PhantomData<Rest::Target>, OPEN, CLOSE>;

	fn __rb(&self) -> <Self::Target as ReborrowTarget<'_>>::Ref {
		DelimitedRepr {
			span_open: self.span_open,
			span_close: self.span_close,
			span: self.span,
			contents: self.contents.__rb(),
		}
	}

	#[cfg(feature = "alloc")]
	fn to_box(&self) -> Self::Box {
		DelimitedRepr {
			span_open: self.span_open,
			span_close: self.span_close,
			span: self.span,
			contents: self.contents.to_box(),
		}
	}
}
