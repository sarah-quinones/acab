#![deny(elided_lifetimes_in_associated_constant, elided_lifetimes_in_paths, clippy::transmute_undefined_repr)]
#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(feature = "alloc")]
use alloc::boxed::Box;

extern crate proc_macro2 as proc;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Edition {
	Edition2015,
	Edition2018,
	Edition2021,
	Edition2024,
}

macro_rules! reborrow_copy {
	($ty: ty $( where $($def: tt)*)? ) => {
		impl<'a $(,$($def)*)?> $crate::ReborrowTarget<'a> for $ty {
			type Ref = &'a Self;
		}

		impl<$($($def)*)?> $crate::Reborrow for $ty {
			#[cfg(feature = "alloc")]
			type Box = Self;
			type Target = Self;

			fn __rb(&self) -> <Self::Target as $crate::ReborrowTarget<'_>>::Ref {
				self
			}

			#[cfg(feature = "alloc")]
			fn to_box(&self) -> Self::Box {
				*self
			}
		}
	};
}

reborrow_copy!(Edition);
reborrow_copy!(Span);
reborrow_copy!(u8);
reborrow_copy!(u16);
reborrow_copy!(u32);
reborrow_copy!(u64);
reborrow_copy!(usize);

impl<'a> ReborrowTarget<'a> for str {
	type Ref = &'a Self;
}
impl<'a, T> ReborrowTarget<'a> for [T] {
	type Ref = &'a Self;
}

impl Reborrow for &str {
	#[cfg(feature = "alloc")]
	type Box = Box<str>;
	type Target = str;

	fn __rb(&self) -> <Self::Target as ReborrowTarget<'_>>::Ref {
		self
	}

	#[cfg(feature = "alloc")]
	fn to_box(&self) -> Self::Box {
		(*self).into()
	}
}

impl<T: Reborrow> Reborrow for &[T] {
	#[cfg(feature = "alloc")]
	type Box = Box<[T::Box]>;
	type Target = [T];

	fn __rb(&self) -> <Self::Target as ReborrowTarget<'_>>::Ref {
		self
	}

	#[cfg(feature = "alloc")]
	fn to_box(&self) -> Self::Box {
		Box::from_iter(self.iter().map(|i| T::to_box(i)))
	}
}

// as_ref!(X, XBox, XRef, XDyn);
macro_rules! as_ref {
	($ty: ident, $boxed: ident, $reference: ident, $dyn: ident) => {
		#[cfg(feature = "alloc")]
		pub type $boxed = $ty<'static, $crate::pointer::Boxed>;
		pub type $reference<'a> = $ty<'a, $crate::pointer::Ref>;
		#[cfg(feature = "alloc")]
		pub type $dyn<'a> = dyn 'a + $crate::Reborrow<Target = $reference<'static>, Box = $boxed>;
		#[cfg(not(feature = "alloc"))]
		pub type $dyn<'a> = dyn 'a + $crate::Reborrow<Target = $reference<'static>>;

		#[cfg(feature = "alloc")]
		const _: () = {
			fn __check_transmute_undefined_repr__(ptr: &$boxed) -> &$ty<'_, $crate::pointer::Ref> {
				unsafe { core::mem::transmute(ptr) }
			}
		};

		impl<'a, P: $crate::pointer::Pointer> ::core::ops::Deref for $ty<'a, P> {
			type Target = $dyn<'a>;

			fn deref(&self) -> &Self::Target {
				self
			}
		}

		impl<'a, P: $crate::pointer::Pointer> $ty<'a, P> {
			pub const fn rb(&self) -> &'_ $ty<'_, $crate::pointer::Ref> {
				unsafe { &*(self as *const _ as *const _) }
			}

			pub const fn as_dyn(&self) -> &$dyn<'a> {
				self
			}
		}
		impl $dyn<'_> {
			pub const fn rb(&self) -> &'_ $ty<'_, $crate::pointer::Ref> {
				unsafe { &*(self as *const _ as *const _) }
			}
		}

		impl<'a> $crate::ReborrowTarget<'a> for $reference<'static> {
			type Ref = &'a $ty<'a, $crate::pointer::Ref>;
		}
		impl<'a, P: $crate::pointer::Pointer> $crate::Reborrow for $ty<'a, P> {
			#[cfg(feature = "alloc")]
			type Box = $boxed;
			type Target = $reference<'static>;

			fn __rb(&self) -> <Self::Target as $crate::ReborrowTarget<'_>>::Ref {
				// SAFETY: same layout
				unsafe { &*(self as *const _ as *const _) }
			}

			#[cfg(feature = "alloc")]
			fn to_box(&self) -> <Self as $crate::Reborrow>::Box {
				<Self as $crate::Reborrow>::__rb(&self).__to_box()
			}
		}
	};
}

macro_rules! derive_struct {
	($ty: ident, $($field: ident),* $(,)? ) => {
		impl<'a> ::core::marker::Copy for $ty<'a, $crate::pointer::Ref> {}
		impl<'a> ::core::clone::Clone for $ty<'a, $crate::pointer::Ref> { fn clone(&self) -> Self { *self } }
		impl<'a> ::core::fmt::Debug for $ty<'a, $crate::pointer::Ref> {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
				f.debug_struct(::core::stringify!($ty))$(.field( ::core::stringify!($field), &self.$field ))*.finish()
			}
		}

		#[cfg(feature = "alloc")]
		impl<'a> ::core::clone::Clone for $ty<'a, $crate::pointer::Boxed> {
			fn clone(&self) -> Self {
				Self {
					$($field: ::core::clone::Clone::clone(&self.$field) ,)*
				}
			}
		}
		#[cfg(feature = "alloc")]
		impl<'a> ::core::fmt::Debug for $ty<'a, $crate::pointer::Boxed> {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
				::core::fmt::Debug::fmt($crate::Reborrow::__rb(self), f)
			}
		}

		#[cfg(feature = "alloc")]
		impl<'a> $ty<'a, $crate::pointer::Ref> {
			fn __to_box(self) -> $ty<'static, $crate::pointer::Boxed> {
				$ty::<$crate::pointer::Boxed> {
					$($field: $crate::Reborrow::to_box(&self.$field)),*
				}
			}
		}
	};
}

macro_rules! derive_enum {
	($ty: ident, $($field: ident),* $(,)? ) => {
		impl<'a> ::core::marker::Copy for $ty<'a, $crate::pointer::Ref> {}
		impl<'a> ::core::clone::Clone for $ty<'a, $crate::pointer::Ref> { fn clone(&self) -> Self { *self } }
		impl<'a> ::core::fmt::Debug for $ty<'a, $crate::pointer::Ref> {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
				match self {
					$(Self::$field(this) => { f.debug_tuple(::core::concat!(::core::stringify!($ty), "::", ::core::stringify!($field))).field(this).finish() },)*
				}
			}
		}
		#[cfg(feature = "alloc")]
		impl<'a> ::core::clone::Clone for $ty<'a, $crate::pointer::Boxed> {
			fn clone(&self) -> Self {
				match self {
					$(Self::$field(this) => { Self::$field(::core::clone::Clone::clone(this)) },)*
				}
			}
		}
		#[cfg(feature = "alloc")]
		impl<'a> ::core::fmt::Debug for $ty<'a, $crate::pointer::Boxed> {
			fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
				::core::fmt::Debug::fmt($crate::Reborrow::__rb(self), f)
			}
		}

		#[cfg(feature = "alloc")]
		impl<'a> $ty<'a, $crate::pointer::Ref> {
			fn __to_box(&self) -> $ty<'static, $crate::pointer::Boxed> {
				match self {
					$(Self::$field(this) => { $ty::$field($crate::Reborrow::to_box(this).into()) },)*
				}
			}
		}
	};
}

pub mod pointer {
	#[cfg(feature = "alloc")]
	use crate::Box;
	use core::mem::MaybeUninit;
	use core::ops::{Deref, DerefMut};

	pub const fn as_boxed<'a, T: 'a + ?Sized, P: PointerTo<'a, T>>(ptr: P) -> <P::Pointer as Pointer>::Boxed<'a, T> {
		ptr
	}

	pub trait Pointer: 'static + Clone + core::fmt::Debug + PartialEq + Eq {
		type Boxed<'a, T: ?Sized + 'a>: PointerTo<'a, T, Pointer = Self>;
	}

	/// # Safety
	/// must have the same layout as `&T`.
	pub unsafe trait PointerTo<'a, T: 'a + ?Sized>: Sized + 'a + core::ops::Deref<Target = T> {
		type Pointer: Pointer<Boxed<'a, T> = Self>;
	}

	// SAFETY: &T is always the same layout as &T
	unsafe impl<'a, T: ?Sized> PointerTo<'a, T> for &'a T {
		type Pointer = Ref;
	}

	// SAFETY: see comments in `__BoxLayoutAssert`
	#[cfg(feature = "alloc")]
	unsafe impl<'a, T: ?Sized> PointerTo<'a, T> for Box<T>
	where
		T: 'a,
	{
		type Pointer = Boxed;
	}

	#[cfg(feature = "alloc")]
	const _: () = {
		const fn __box_layout_assert<T>() {
			use core::{mem, ptr, slice};

			trait Any {}
			impl<T: ?Sized> Any for T {}

			trait __BoxLayoutAssert: Sized {
				const __PTR__: *mut [Self; 0] = ptr::NonNull::<[Self; 0]>::dangling().as_ptr();
				const __DYN_PTR__: *mut dyn Any = ptr::NonNull::<()>::dangling().as_ptr() as *mut dyn Any;

				// SAFETY: we rely on UB being caught in const eval to justify expecting `Box<T: ?Sized>` to have
				// the same layout as `*const T`. don't do this at home
				#[allow(clippy::borrowed_box)]
				const __DYN_LAYOUT__: &'static Box<dyn Any> = unsafe { mem::transmute(&Self::__DYN_PTR__) };
				// SAFETY: see above
				const __SLICE_LAYOUT__: Box<[Self]> = unsafe { mem::transmute(slice::from_raw_parts_mut(Self::__PTR__, 0)) };
				// SAFETY: see above
				const __LAYOUT__: Box<[Self; 0]> = unsafe { mem::transmute(Self::__PTR__) };
			}
			impl<T> __BoxLayoutAssert for T {}

			_ = const { &<T as __BoxLayoutAssert>::__LAYOUT__ };
			_ = const { &<T as __BoxLayoutAssert>::__SLICE_LAYOUT__ };
			_ = const { &<T as __BoxLayoutAssert>::__DYN_LAYOUT__ };
		}

		__box_layout_assert::<i32>();
	};

	#[derive(Clone, Debug, PartialEq, Eq)]
	#[cfg(feature = "alloc")]
	pub struct Boxed;
	#[derive(Copy, Clone, Debug, PartialEq, Eq)]
	pub struct Ref;
	#[cfg(feature = "alloc")]
	impl Pointer for Boxed {
		type Boxed<'a, T: ?Sized + 'a> = Box<T>;
	}
	impl Pointer for Ref {
		type Boxed<'a, T: ?Sized + 'a> = &'a T;
	}

	#[derive(Copy, Clone)]
	#[repr(C)]
	pub struct StackVec<T: Copy, const MIN: usize, const MAX: usize> {
		// SAFETY INVARIANT: slice[..len] is initialized
		slice: [MaybeUninit<T>; MAX],
		// SAFETY INVARIANT: MIN <= len <= MAX
		len: u8,
	}

	impl<T: Copy + PartialEq, const MIN: usize, const MAX: usize> PartialEq for StackVec<T, MIN, MAX> {
		fn eq(&self, other: &Self) -> bool {
			self.as_slice() == other.as_slice()
		}
	}
	impl<T: Copy + Eq, const MIN: usize, const MAX: usize> Eq for StackVec<T, MIN, MAX> {}

	impl<T: Copy, const MIN: usize, const MAX: usize> StackVec<T, MIN, MAX> {
		const __ASSERT__: () = {
			assert!(MIN <= MAX);
			assert!(MAX < 256);
		};

		pub const fn new(values: [T; MIN]) -> Self {
			const { Self::__ASSERT__ };

			let mut slice = [const { core::mem::MaybeUninit::uninit() }; MAX];
			// SAFETY: `MIN <= MAX`, `T: Copy` so there's no drop code running here.
			unsafe { *((&raw mut slice) as *mut [T; MIN]) = values };

			Self { slice, len: MIN as u8 }
		}

		/// # Safety
		/// inputs must have been acquired from a previous call to [`Self::into_raw_parts`]
		pub const unsafe fn from_raw_parts(slice: [MaybeUninit<T>; MAX], len: usize) -> Self {
			debug_assert!(len >= MIN);
			debug_assert!(len <= MAX);

			Self { slice, len: len as u8 }
		}

		pub const fn into_raw_parts(self) -> ([MaybeUninit<T>; MAX], usize) {
			let Self { slice, len } = self;
			(slice, len as usize)
		}

		pub const fn len(&self) -> usize {
			self.len as usize
		}

		pub const fn is_empty(&self) -> bool {
			self.len == 0
		}

		/// # Safety
		/// - `MIN <= len <= MAX`
		/// - elements between `self.as_ptr()` as `self.as_ptr().add(len)` must be initialized.
		pub const unsafe fn set_len(&mut self, len: usize) {
			debug_assert!(len >= MIN);
			debug_assert!(len <= MAX);

			self.len = len as u8;
		}

		pub const fn as_ptr(&self) -> *const T {
			self.slice.as_ptr() as *const T
		}

		pub const fn as_mut_ptr(&mut self) -> *mut T {
			self.slice.as_mut_ptr() as *mut T
		}

		pub const fn push(&mut self, value: T) {
			assert!(self.len() < MAX);

			let len = self.len();
			self.slice[len].write(value);

			self.len += 1;
		}

		pub const fn as_slice(&self) -> &[T] {
			// SAFETY: see struct safety invariant
			unsafe { core::slice::from_raw_parts(self.slice.as_ptr() as *const T, self.len as usize) }
		}

		pub const fn as_mut_slice(&mut self) -> &mut [T] {
			// SAFETY: see struct safety invariant
			unsafe { core::slice::from_raw_parts_mut(self.slice.as_mut_ptr() as *mut T, self.len as usize) }
		}

		pub const fn pop(&mut self) -> Option<T> {
			if self.len() == MIN {
				None
			} else {
				self.len -= 1;
				let len = self.len();
				// SAFETY: see struct safety invariant
				let val = unsafe { self.slice[len].assume_init() };
				// so miri can catch ub access
				self.slice[len] = MaybeUninit::uninit();

				Some(val)
			}
		}
	}

	impl<T: Copy, const MIN: usize, const MAX: usize> Deref for StackVec<T, MIN, MAX> {
		type Target = [T];

		fn deref(&self) -> &Self::Target {
			self.as_slice()
		}
	}

	impl<T: Copy, const MIN: usize, const MAX: usize> DerefMut for StackVec<T, MIN, MAX> {
		fn deref_mut(&mut self) -> &mut Self::Target {
			self.as_mut_slice()
		}
	}

	impl<T: Copy + core::fmt::Debug, const MIN: usize, const MAX: usize> core::fmt::Debug for StackVec<T, MIN, MAX> {
		fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
			self.as_slice().fmt(f)
		}
	}
}

pub mod expr;
pub mod ident;
pub mod keyword;

pub use expr::Expr;
pub use ident::Ident;
pub use proc::Span;

pub trait ReborrowTarget<'a, Outlives = &'a Self> {
	type Ref;
}

pub trait Reborrow {
	#[cfg(feature = "alloc")]
	type Box;
	type Target: ?Sized + for<'a> ReborrowTarget<'a>;

	fn __rb(&self) -> <Self::Target as ReborrowTarget<'_>>::Ref;

	#[cfg(feature = "alloc")]
	fn to_box(&self) -> Self::Box;
}

impl<T: ?Sized + Reborrow> Reborrow for &T {
	#[cfg(feature = "alloc")]
	type Box = T::Box;
	type Target = T::Target;

	fn __rb(&self) -> <Self::Target as ReborrowTarget<'_>>::Ref {
		(**self).__rb()
	}

	#[cfg(feature = "alloc")]
	fn to_box(&self) -> Self::Box {
		(**self).to_box()
	}
}

pub mod prelude {
	pub use crate::Reborrow;
}
