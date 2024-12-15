use super::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MaybeUnderscore<Digit> {
	Underscore,
	Digit(Digit),
}

#[repr(C)]
pub struct BinLiteral<'a, P: Pointer> {
	// in chunks of u2
	contents: P::Boxed<'a, [u8]>,
	len: usize,
}
as_ref!(BinLiteral, BinLiteralBox, BinLiteralRef, BinLiteralDyn);
derive_struct!(BinLiteral, contents, len);

#[repr(C)]
pub struct DecLiteral<'a, P: Pointer> {
	// each chunk of u8 stores two values in base 11
	contents: P::Boxed<'a, [u8]>,
	len: usize,
}
as_ref!(DecLiteral, DecLiteralBox, DecLiteralRef, DecLiteralDyn);
derive_struct!(DecLiteral, contents, len);

#[repr(C)]
pub struct OctLiteral<'a, P: Pointer> {
	// in chunks of u4
	contents: P::Boxed<'a, [u8]>,
	len: usize,
}
as_ref!(OctLiteral, OctLiteralBox, OctLiteralRef, OctLiteralDyn);
derive_struct!(OctLiteral, contents, len);

#[repr(C)]
pub struct HexLiteral<'a, P: Pointer> {
	contents: P::Boxed<'a, [u8]>,
}
as_ref!(HexLiteral, HexLiteralBox, HexLiteralRef, HexLiteralDyn);
derive_struct!(HexLiteral, contents);

impl BinLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().len
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
		let bits = this.contents[idx / 4] >> (2 * (idx % 4));
		if bits >> 1 == 1 {
			MaybeUnderscore::Underscore
		} else {
			MaybeUnderscore::Digit(unsafe { core::mem::transmute(bits) })
		}
	}
}

impl OctLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().len
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
		let bits = this.contents[idx / 2] >> (4 * (idx % 2));
		if bits >> 3 == 1 {
			MaybeUnderscore::Underscore
		} else {
			MaybeUnderscore::Digit(unsafe { core::mem::transmute(bits) })
		}
	}
}

impl HexLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().contents.len()
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
		let bits = this.contents[idx];
		if bits >> 7 == 1 {
			MaybeUnderscore::Underscore
		} else {
			MaybeUnderscore::Digit(unsafe { core::mem::transmute(bits) })
		}
	}
}

impl DecLiteralDyn<'_> {
	pub const fn len(&self) -> usize {
		self.rb().len
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

		let bits = this.contents[idx / 2];
		let bits = if idx % 2 == 1 { bits / 11 } else { bits };

		if bits == 10 {
			MaybeUnderscore::Underscore
		} else {
			MaybeUnderscore::Digit(unsafe { core::mem::transmute(bits) })
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
	pub suffix: SuffixNoExp<'a, P>,
	pub span: Span,
}
as_ref!(IntLiteral, IntLiteralBox, IntLiteralRef, IntLiteralDyn);
derive_struct!(IntLiteral, repr, suffix, span);

impl Spanned for IntLiteralDyn<'_> {
	fn span(&self) -> Span {
		self.rb().span
	}
}
