use super::*;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MaybeUnderscore<Digit: Copy> {
	Underscore,
	Digit(Digit),
}
reborrow_copy!(MaybeUnderscore<Digit> where Digit: Copy);

impl<Digit: fmt::Debug + Copy> fmt::Debug for MaybeUnderscore<Digit> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			MaybeUnderscore::Underscore => f.write_char('_'),
			MaybeUnderscore::Digit(this) => this.fmt(f),
		}
	}
}

mod repr {
	use super::*;

	#[repr(C)]
	pub struct Compressed<'a, P: Pointer> {
		pub contents: P::Boxed<'a, [u8]>,
		pub len: usize,
	}
	as_ref!(Compressed, CompressedBox, CompressedRef, CompressedDyn);
	derive_struct!(Compressed, contents, len);

	#[repr(u8)]
	pub enum BinLiteralRepr<'a, P: Pointer> {
		// in chunks of u2
		Compressed(Compressed<'a, P>),
		Uncompressed(P::Boxed<'a, [MaybeUnderscore<BinDigit>]>),
	}
	as_ref!(BinLiteralRepr, BinLiteralReprBox, BinLiteralReprRef, BinLiteralReprDyn);
	derive_enum_copy_clone!(BinLiteralRepr, Compressed, Uncompressed);

	#[repr(u8)]
	pub enum DecLiteralRepr<'a, P: Pointer> {
		// each chunk of u8 stores two values in base 11
		Compressed(Compressed<'a, P>),
		Uncompressed(P::Boxed<'a, [MaybeUnderscore<DecDigit>]>),
	}
	as_ref!(DecLiteralRepr, DecLiteralReprBox, DecLiteralReprRef, DecLiteralReprDyn);
	derive_enum_copy_clone!(DecLiteralRepr, Compressed, Uncompressed);

	#[repr(u8)]
	pub enum OctLiteralRepr<'a, P: Pointer> {
		// in chunks of u4
		Compressed(Compressed<'a, P>),
		Uncompressed(P::Boxed<'a, [MaybeUnderscore<OctDigit>]>),
	}
	as_ref!(OctLiteralRepr, OctLiteralReprBox, OctLiteralReprRef, OctLiteralReprDyn);
	derive_enum_copy_clone!(OctLiteralRepr, Compressed, Uncompressed);

	#[repr(u8)]
	pub enum HexLiteralRepr<'a, P: Pointer> {
		Compressed(Compressed<'a, P>),
		Uncompressed(P::Boxed<'a, [MaybeUnderscore<HexDigit>]>),
	}
	as_ref!(HexLiteralRepr, HexLiteralReprBox, HexLiteralReprRef, HexLiteralReprDyn);
	derive_enum_copy_clone!(HexLiteralRepr, Compressed, Uncompressed);

	impl fmt::Debug for BinLiteralReprRef<'_> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			let this = unsafe { core::mem::transmute::<&BinLiteralReprRef<'_>, &BinLiteralRef<'_>>(self) };
			f.debug_list().entries((0..this.len()).map(|i| this.get(i))).finish()
		}
	}
	impl PartialEq for BinLiteralReprRef<'_> {
		fn eq(&self, other: &Self) -> bool {
			let left = unsafe { core::mem::transmute::<&BinLiteralReprRef<'_>, &BinLiteralRef<'_>>(self) };
			let right = unsafe { core::mem::transmute::<&BinLiteralReprRef<'_>, &BinLiteralRef<'_>>(other) };

			if left.len() != right.len() {
				return false;
			}
			for i in 0..left.len() {
				if left.get(i) != right.get(i) {
					return false;
				}
			}
			true
		}
	}

	impl fmt::Debug for OctLiteralReprRef<'_> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			let this = unsafe { core::mem::transmute::<&OctLiteralReprRef<'_>, &OctLiteralRef<'_>>(self) };
			f.debug_list().entries((0..this.len()).map(|i| this.get(i))).finish()
		}
	}
	impl PartialEq for OctLiteralReprRef<'_> {
		fn eq(&self, other: &Self) -> bool {
			let left = unsafe { core::mem::transmute::<&OctLiteralReprRef<'_>, &OctLiteralRef<'_>>(self) };
			let right = unsafe { core::mem::transmute::<&OctLiteralReprRef<'_>, &OctLiteralRef<'_>>(other) };

			if left.len() != right.len() {
				return false;
			}
			for i in 0..left.len() {
				if left.get(i) != right.get(i) {
					return false;
				}
			}
			true
		}
	}

	impl fmt::Debug for DecLiteralReprRef<'_> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			let this = unsafe { core::mem::transmute::<&DecLiteralReprRef<'_>, &DecLiteralRef<'_>>(self) };
			f.debug_list().entries((0..this.len()).map(|i| this.get(i))).finish()
		}
	}
	impl PartialEq for DecLiteralReprRef<'_> {
		fn eq(&self, other: &Self) -> bool {
			let left = unsafe { core::mem::transmute::<&DecLiteralReprRef<'_>, &DecLiteralRef<'_>>(self) };
			let right = unsafe { core::mem::transmute::<&DecLiteralReprRef<'_>, &DecLiteralRef<'_>>(other) };

			if left.len() != right.len() {
				return false;
			}
			for i in 0..left.len() {
				if left.get(i) != right.get(i) {
					return false;
				}
			}
			true
		}
	}

	impl fmt::Debug for HexLiteralReprRef<'_> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			let this = unsafe { core::mem::transmute::<&HexLiteralReprRef<'_>, &HexLiteralRef<'_>>(self) };
			f.debug_list().entries((0..this.len()).map(|i| this.get(i))).finish()
		}
	}
	impl PartialEq for HexLiteralReprRef<'_> {
		fn eq(&self, other: &Self) -> bool {
			let left = unsafe { core::mem::transmute::<&HexLiteralReprRef<'_>, &HexLiteralRef<'_>>(self) };
			let right = unsafe { core::mem::transmute::<&HexLiteralReprRef<'_>, &HexLiteralRef<'_>>(other) };

			if left.len() != right.len() {
				return false;
			}
			for i in 0..left.len() {
				if left.get(i) != right.get(i) {
					return false;
				}
			}
			true
		}
	}
}

#[repr(C)]
pub struct BinLiteral<'a, P: Pointer> {
	contents: repr::BinLiteralRepr<'a, P>,
}
as_ref!(BinLiteral, BinLiteralBox, BinLiteralRef, BinLiteralDyn);
derive_struct!(BinLiteral, contents);

#[repr(C)]
pub struct DecLiteral<'a, P: Pointer> {
	contents: repr::DecLiteralRepr<'a, P>,
}
as_ref!(DecLiteral, DecLiteralBox, DecLiteralRef, DecLiteralDyn);
derive_struct!(DecLiteral, contents);

#[repr(C)]
pub struct OctLiteral<'a, P: Pointer> {
	contents: repr::OctLiteralRepr<'a, P>,
}
as_ref!(OctLiteral, OctLiteralBox, OctLiteralRef, OctLiteralDyn);
derive_struct!(OctLiteral, contents);

#[repr(C)]
pub struct HexLiteral<'a, P: Pointer> {
	contents: repr::HexLiteralRepr<'a, P>,
}
as_ref!(HexLiteral, HexLiteralBox, HexLiteralRef, HexLiteralDyn);
derive_struct!(HexLiteral, contents);

derive_enum_debug!(
	#[derive(Copy, Clone, PartialEq, Eq)]
	pub enum ParseIntLiteralError {
		Empty,
		LeadingUnderscore,
	}
);

impl<'a, P: Pointer> BinLiteral<'a, P> {
	pub const fn new(
		contents: impl PointerTo<'a, [MaybeUnderscore<BinDigit>], Pointer = P>,
	) -> Result<Self, (P::Boxed<'a, [MaybeUnderscore<BinDigit>]>, ParseIntLiteralError)> {
		match as_ref(&contents).first() {
			None => Err((into_boxed(contents), ParseIntLiteralError::Empty)),
			Some(MaybeUnderscore::Underscore) => Err((into_boxed(contents), ParseIntLiteralError::LeadingUnderscore)),
			_ => Ok(Self {
				contents: repr::BinLiteralRepr::Uncompressed(into_boxed(contents)),
			}),
		}
	}
}
impl<'a, P: Pointer> HexLiteral<'a, P> {
	pub const fn new(
		contents: impl PointerTo<'a, [MaybeUnderscore<HexDigit>], Pointer = P>,
	) -> Result<Self, (P::Boxed<'a, [MaybeUnderscore<HexDigit>]>, ParseIntLiteralError)> {
		match as_ref(&contents).first() {
			None => Err((into_boxed(contents), ParseIntLiteralError::Empty)),
			Some(MaybeUnderscore::Underscore) => Err((into_boxed(contents), ParseIntLiteralError::LeadingUnderscore)),
			_ => Ok(Self {
				contents: repr::HexLiteralRepr::Uncompressed(into_boxed(contents)),
			}),
		}
	}
}
impl<'a, P: Pointer> OctLiteral<'a, P> {
	pub const fn new(
		contents: impl PointerTo<'a, [MaybeUnderscore<OctDigit>], Pointer = P>,
	) -> Result<Self, (P::Boxed<'a, [MaybeUnderscore<OctDigit>]>, ParseIntLiteralError)> {
		match as_ref(&contents).first() {
			None => Err((into_boxed(contents), ParseIntLiteralError::Empty)),
			Some(MaybeUnderscore::Underscore) => Err((into_boxed(contents), ParseIntLiteralError::LeadingUnderscore)),
			_ => Ok(Self {
				contents: repr::OctLiteralRepr::Uncompressed(into_boxed(contents)),
			}),
		}
	}
}
impl<'a, P: Pointer> DecLiteral<'a, P> {
	pub const fn new(
		contents: impl PointerTo<'a, [MaybeUnderscore<DecDigit>], Pointer = P>,
	) -> Result<Self, (P::Boxed<'a, [MaybeUnderscore<DecDigit>]>, ParseIntLiteralError)> {
		match as_ref(&contents).first() {
			None => Err((into_boxed(contents), ParseIntLiteralError::Empty)),
			Some(MaybeUnderscore::Underscore) => Err((into_boxed(contents), ParseIntLiteralError::LeadingUnderscore)),
			_ => Ok(Self {
				contents: repr::DecLiteralRepr::Uncompressed(into_boxed(contents)),
			}),
		}
	}
}
impl BinLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		match self.rb().contents {
			repr::BinLiteralRepr::Compressed(comp) => comp.len,
			repr::BinLiteralRepr::Uncompressed(uncomp) => uncomp.len(),
		}
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn lsb(&self) -> BinDigit {
		let MaybeUnderscore::Digit(digit) = self.get(0) else { panic!() };
		digit
	}

	pub const fn get(&self, idx: usize) -> MaybeUnderscore<BinDigit> {
		assert!(idx < self.len());
		let this = *self.rb();
		match this.contents {
			repr::BinLiteralRepr::Compressed(this) => {
				let bits = (this.contents[idx / 4] >> (2 * (idx % 4))) & 0b11;
				if bits >> 1 == 1 {
					MaybeUnderscore::Underscore
				} else {
					MaybeUnderscore::Digit(unsafe { core::mem::transmute(bits) })
				}
			},
			repr::BinLiteralRepr::Uncompressed(this) => this[idx],
		}
	}
}

impl OctLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		match self.rb().contents {
			repr::OctLiteralRepr::Compressed(comp) => comp.len,
			repr::OctLiteralRepr::Uncompressed(uncomp) => uncomp.len(),
		}
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn lsb(&self) -> OctDigit {
		let MaybeUnderscore::Digit(digit) = self.get(0) else { panic!() };
		digit
	}

	pub const fn get(&self, idx: usize) -> MaybeUnderscore<OctDigit> {
		assert!(idx < self.len());
		let this = *self.rb();
		match this.contents {
			repr::OctLiteralRepr::Compressed(this) => {
				let bits = (this.contents[idx / 2] >> (4 * (idx % 2))) & 0b1111;
				if bits >> 3 == 1 {
					MaybeUnderscore::Underscore
				} else {
					MaybeUnderscore::Digit(unsafe { core::mem::transmute(bits) })
				}
			},
			repr::OctLiteralRepr::Uncompressed(this) => this[idx],
		}
	}
}

impl HexLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		match self.rb().contents {
			repr::HexLiteralRepr::Compressed(comp) => comp.len,
			repr::HexLiteralRepr::Uncompressed(uncomp) => uncomp.len(),
		}
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn lsb(&self) -> HexDigit {
		let MaybeUnderscore::Digit(digit) = self.get(0) else { panic!() };
		digit
	}

	pub const fn get(&self, idx: usize) -> MaybeUnderscore<HexDigit> {
		assert!(idx < self.len());
		let this = *self.rb();
		match this.contents {
			repr::HexLiteralRepr::Compressed(this) => {
				let bits = this.contents[idx];
				if bits >> 7 == 1 {
					MaybeUnderscore::Underscore
				} else {
					MaybeUnderscore::Digit(unsafe { core::mem::transmute(bits) })
				}
			},
			repr::HexLiteralRepr::Uncompressed(this) => this[idx],
		}
	}
}

impl DecLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		match self.rb().contents {
			repr::DecLiteralRepr::Compressed(comp) => comp.len,
			repr::DecLiteralRepr::Uncompressed(uncomp) => uncomp.len(),
		}
	}

	pub const fn is_empty(&self) -> bool {
		self.len() == 0
	}

	pub const fn lsb(&self) -> DecDigit {
		let MaybeUnderscore::Digit(digit) = self.get(0) else { panic!() };
		digit
	}

	pub const fn get(&self, idx: usize) -> MaybeUnderscore<DecDigit> {
		assert!(idx < self.len());
		let this = *self.rb();

		match this.contents {
			repr::DecLiteralRepr::Compressed(this) => {
				let bits = this.contents[idx / 2];
				let bits = if idx % 2 == 1 { bits / 11 } else { bits % 11 };

				if bits == 10 {
					MaybeUnderscore::Underscore
				} else {
					MaybeUnderscore::Digit(unsafe { core::mem::transmute(bits) })
				}
			},
			repr::DecLiteralRepr::Uncompressed(this) => this[idx],
		}
	}
}

#[repr(C)]
pub enum IntLiteralRepr<'a, P: Pointer> {
	Dec(DecLiteral<'a, P>),
	Bin(BinLiteral<'a, P>),
	Oct(OctLiteral<'a, P>),
	Hex(HexLiteral<'a, P>),
}
as_ref!(IntLiteralRepr, IntLiteralReprBox, IntLiteralReprRef, IntLiteralReprDyn);
derive_enum!(IntLiteralRepr, Dec, Bin, Oct, Hex);

#[repr(C)]
pub struct IntLiteral<'a, P: Pointer> {
	pub repr: IntLiteralRepr<'a, P>,
	pub suffix: Option<SuffixNoExp<'a, P>>,
	pub span: Span,
}
as_ref!(IntLiteral, IntLiteralBox, IntLiteralRef, IntLiteralDyn);
derive_struct!(IntLiteral, repr, suffix, span);

impl Spanned for IntLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}

#[cfg(feature = "alloc")]
mod alloc {
	use super::*;

	impl FromIterator<MaybeUnderscore<BinDigit>> for BinLiteralBox {
		fn from_iter<T: IntoIterator<Item = MaybeUnderscore<BinDigit>>>(iter: T) -> Self {
			let mut iter = iter.into_iter();

			let mut digits = Vec::new();
			let mut len = 0;

			let to_bits = |b: MaybeUnderscore<BinDigit>| -> u8 {
				match b {
					MaybeUnderscore::Underscore => 0b10,
					MaybeUnderscore::Digit(b) => b.as_u8(),
				}
			};

			loop {
				let i0 = iter.next();
				let i1 = iter.next();
				let i2 = iter.next();
				let i3 = iter.next();

				match (i0, i1, i2, i3) {
					(Some(i0), Some(i1), Some(i2), Some(i3)) => {
						digits.push((to_bits(i0) << 0) | (to_bits(i1) << 2) | (to_bits(i2) << 4) | (to_bits(i3) << 6));
						len += 4;
					},
					(Some(i0), Some(i1), Some(i2), _) => {
						digits.push((to_bits(i0) << 0) | (to_bits(i1) << 2) | (to_bits(i2) << 4));
						len += 3;
					},
					(Some(i0), Some(i1), _, _) => {
						digits.push((to_bits(i0) << 0) | (to_bits(i1) << 2));
						len += 2;
					},
					(Some(i0), _, _, _) => {
						digits.push(to_bits(i0) << 0);
						len += 1;
					},
					_ => break,
				}
			}
			BinLiteral {
				contents: repr::BinLiteralRepr::Compressed(repr::Compressed {
					contents: digits.into_boxed_slice(),
					len,
				}),
			}
		}
	}

	impl FromIterator<MaybeUnderscore<OctDigit>> for OctLiteralBox {
		fn from_iter<T: IntoIterator<Item = MaybeUnderscore<OctDigit>>>(iter: T) -> Self {
			let mut iter = iter.into_iter();

			let mut digits = Vec::new();
			let mut len = 0;

			let to_bits = |b: MaybeUnderscore<OctDigit>| -> u8 {
				match b {
					MaybeUnderscore::Underscore => 0b1000,
					MaybeUnderscore::Digit(b) => b.as_u8(),
				}
			};

			loop {
				let i0 = iter.next();
				let i1 = iter.next();

				match (i0, i1) {
					(Some(i0), Some(i1)) => {
						digits.push((to_bits(i0) << 0) | (to_bits(i1) << 4));
						len += 2;
					},
					(Some(i0), _) => {
						digits.push(to_bits(i0) << 0);
						len += 1;
					},
					_ => break,
				}
			}
			OctLiteral {
				contents: repr::OctLiteralRepr::Compressed(repr::Compressed {
					contents: digits.into_boxed_slice(),
					len,
				}),
			}
		}
	}

	impl FromIterator<MaybeUnderscore<DecDigit>> for DecLiteralBox {
		fn from_iter<T: IntoIterator<Item = MaybeUnderscore<DecDigit>>>(iter: T) -> Self {
			let mut iter = iter.into_iter();

			let mut digits = Vec::new();
			let mut len = 0;

			let to_bits = |b: MaybeUnderscore<DecDigit>| -> u8 {
				match b {
					MaybeUnderscore::Underscore => 10,
					MaybeUnderscore::Digit(b) => b.as_u8(),
				}
			};

			loop {
				let i0 = iter.next();
				let i1 = iter.next();
				extern crate std;

				match (i0, i1) {
					(Some(i0), Some(i1)) => {
						digits.push(to_bits(i0) + 11 * to_bits(i1));
						len += 2;
					},
					(Some(i0), _) => {
						digits.push(to_bits(i0));
						len += 1;
					},
					_ => break,
				}
			}
			DecLiteral {
				contents: repr::DecLiteralRepr::Compressed(repr::Compressed {
					contents: digits.into_boxed_slice(),
					len,
				}),
			}
		}
	}

	impl FromIterator<MaybeUnderscore<HexDigit>> for HexLiteralBox {
		fn from_iter<T: IntoIterator<Item = MaybeUnderscore<HexDigit>>>(iter: T) -> Self {
			let mut iter = iter.into_iter();

			let mut digits = Vec::new();
			let mut len = 0;

			let to_bits = |b: MaybeUnderscore<HexDigit>| -> u8 {
				match b {
					MaybeUnderscore::Underscore => 0b1000_0000,
					MaybeUnderscore::Digit(b) => unsafe { core::mem::transmute(b) },
				}
			};

			loop {
				let i0 = iter.next();

				match i0 {
					Some(i0) => {
						digits.push(to_bits(i0));
						len += 1;
					},
					_ => break,
				}
			}
			extern crate std;
			HexLiteral {
				contents: repr::HexLiteralRepr::Compressed(repr::Compressed {
					contents: digits.into_boxed_slice(),
					len,
				}),
			}
		}
	}
}
