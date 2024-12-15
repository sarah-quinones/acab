use crate::Span;
use crate::ident::IdentOrKeyword;
use crate::pointer::*;

pub mod char_literal;
pub mod float_literal;
pub mod int_literal;
pub mod str_literal;

use char_literal::uint;

#[repr(C)]
pub struct Suffix<'a, P: Pointer> {
	pub ident: IdentOrKeyword<'a, P>,
}
as_ref!(Suffix, SuffixBox, SuffixRef, SuffixDyn);
derive_struct!(Suffix, ident);

#[repr(C)]
pub struct SuffixNoExp<'a, P: Pointer> {
	repr: Suffix<'a, P>,
}
as_ref!(SuffixNoExp, SuffixNoExpBox, SuffixNoExpRef, SuffixNoExpDyn);
derive_struct!(SuffixNoExp, repr);

impl SuffixNoExpDyn<'_> {
	pub fn repr(&self) -> SuffixRef<'_> {
		self.rb().repr
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct BinDigit {
	c: uint<u8, 1>,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct OctDigit {
	c: uint<u8, 3>,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct DecDigit {
	c: u8,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct HexDigit {
	// top bit is 1 if uppercase
	c: uint<u8, 5>,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct AsciiDigit {
	c: uint<u8, 7>,
}
reborrow_copy!(BinDigit);
reborrow_copy!(DecDigit);
reborrow_copy!(OctDigit);
reborrow_copy!(HexDigit);
reborrow_copy!(AsciiDigit);

impl OctDigit {
	pub const fn new(c: u8) -> Option<Self> {
		match uint::new(c) {
			Some(c) => Some(Self { c }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0o10`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		Self {
			c: unsafe { uint::new_unchecked(c) },
		}
	}

	pub const fn as_byte(self) -> u8 {
		self.c.get()
	}
}

impl BinDigit {
	pub const fn new(c: u8) -> Option<Self> {
		match uint::new(c) {
			Some(c) => Some(Self { c }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0b10`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		Self {
			c: unsafe { uint::new_unchecked(c) },
		}
	}

	pub const fn as_byte(self) -> u8 {
		self.c.get()
	}
}
impl DecDigit {
	pub const fn new(c: u8) -> Option<Self> {
		if c < 10 { Some(Self { c }) } else { None }
	}

	/// # Safety
	/// `c < 10`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		debug_assert!(c < 10);
		Self { c }
	}

	pub const fn as_byte(self) -> u8 {
		self.c
	}
}

impl AsciiDigit {
	pub const fn new(c: u8) -> Option<Self> {
		match uint::new(c) {
			Some(c) => Some(Self { c }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0x7F`
	pub const unsafe fn new_unchecked(c: u8) -> Self {
		Self {
			c: unsafe { uint::new_unchecked(c) },
		}
	}

	pub const fn as_byte(self) -> u8 {
		self.c.get()
	}
}

impl HexDigit {
	pub const fn new_lowercase(c: u8) -> Option<Self> {
		match uint::<u8, 4>::new(c) {
			Some(_) => Some(unsafe { Self::new_lowercase_unchecked(c) }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0x10`
	pub const unsafe fn new_lowercase_unchecked(c: u8) -> Self {
		unsafe {
			// for debug asserts
			_ = uint::<u8, 4>::new_unchecked(c);

			Self { c: uint::new_unchecked(c) }
		}
	}

	pub const fn new_uppercase(c: u8) -> Option<Self> {
		match uint::<u8, 4>::new(c) {
			Some(_) => Some(unsafe { Self::new_uppercase_unchecked(c) }),
			None => None,
		}
	}

	/// # Safety
	/// `c < 0x10`
	pub const unsafe fn new_uppercase_unchecked(c: u8) -> Self {
		unsafe {
			// for debug asserts
			_ = uint::<u8, 4>::new_unchecked(c);

			if c < 0xA {
				Self { c: uint::new_unchecked(c) }
			} else {
				Self {
					c: uint::new_unchecked(c | (1u8 << 4)),
				}
			}
		}
	}

	pub const fn as_byte(self) -> u8 {
		self.c.get() & 0xF
	}

	pub const fn is_uppercase(self) -> bool {
		(self.c.get() >> 4) == 1
	}
}

pub trait ExcludeList: Copy + Clone {
	type Type: 'static + Copy;
	const EXCLUDED_VALUES: &'static [Self::Type];
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Exclude<E: ExcludeList<Type = T>, T: Copy = <E as ExcludeList>::Type> {
	inner: E::Type,
}
reborrow_copy!(Exclude<E> where E: ExcludeList);

impl<E: ExcludeList<Type: core::fmt::Debug>> core::fmt::Debug for Exclude<E> {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		self.inner.fmt(f)
	}
}

impl<E: ExcludeList> Exclude<E> {
	/// # Safety
	/// [`Exclude::EXCLUDED_VALUES`] must not contain `c`.
	pub const unsafe fn new_unchecked(c: E::Type) -> Self {
		Self { inner: c }
	}

	pub const fn get(self) -> E::Type {
		self.inner
	}
}
