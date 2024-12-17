use super::*;

#[repr(u8)]
pub enum PathIdentSegment<'a, P: Pointer> {
	Ident(Ident<'a, P>),
	Super(Keyword![super]),
	SelfValue(Keyword![self]),
	SelfType(Keyword![Self]),
	Crate(Keyword![crate]),
	MacroCrate(Keyword![crate]),
}
as_ref!(PathIdentSegment, PathIdentSegmentBox, PathIdentSegmentRef, PathIdentSegmentDyn);
derive_enum!(PathIdentSegment, Ident, Super, SelfValue, SelfType, Crate, MacroCrate);

#[repr(u8)]
pub enum GenericLifetime<'a, P: Pointer> {
	Named(Lifetime<NonKeywordIdent<'a, P>>),
	Infer(Lifetime<Keyword![_]>),
	Static(Lifetime<Keyword![static]>),
}
as_ref!(GenericLifetime, GenericLifetimeBox, GenericLifetimeRef, GenericLifetimeDyn);
derive_enum!(GenericLifetime, Named, Infer, Static);

#[repr(u8)]
pub enum GenericArg<'a, P: Pointer> {
	Lifetime(GenericLifetime<'a, P>),
	Type(Type<'a, P>),
}
as_ref!(GenericArg, GenericArgBox, GenericArgRef, GenericArgDyn);
derive_enum!(GenericArg, Lifetime, Type);

#[repr(C)]
pub struct GenericArgs<'a, P: Pointer> {
	pub args: Delimited<'a, P, CommaSep, GenericArg<'a, P>, '<', '>'>,
}
as_ref!(GenericArgs, GenericArgsBox, GenericArgsRef, GenericArgsDyn);
derive_struct!(GenericArgs, args);

#[repr(C)]
pub struct PathExprSegment<'a, P: Pointer> {
	pub ident: PathIdentSegment<'a, P>,
	pub args: Option<Separated<PathSep, GenericArgs<'a, P>>>,
}
as_ref!(PathExprSegment, PathExprSegmentBox, PathExprSegmentRef, PathExprSegmentDyn);
derive_struct!(PathExprSegment, ident, args);

#[repr(C)]
pub struct PathInExpression<'a, P: Pointer> {
	pub leading: Separated<Option<PathSep>, PathExprSegment<'a, P>>,
	pub trailing: P::Boxed<'a, [Separated<PathSep, PathExprSegment<'a, P>>]>,
}
as_ref!(PathInExpression, PathInExpressionBox, PathInExpressionRef, PathInExpressionDyn);
derive_struct!(PathInExpression, leading, trailing);

#[repr(C)]
pub struct QualifiedPathInExpression<'a, P: Pointer> {
	pub qualified_type: Type<'a, P>,
	pub leading: Separated<PathSep, PathExprSegment<'a, P>>,
	pub trailing: P::Boxed<'a, [Separated<PathSep, PathExprSegment<'a, P>>]>,
}
as_ref!(
	QualifiedPathInExpression,
	QualifiedPathInExpressionBox,
	QualifiedPathInExpressionRef,
	QualifiedPathInExpressionDyn
);
derive_struct!(QualifiedPathInExpression, qualified_type, leading, trailing);

#[repr(u8)]
pub enum PathExpr<'a, P: Pointer> {
	Unqual(PathInExpression<'a, P>),
	Qual(QualifiedPathInExpression<'a, P>),
}
as_ref!(PathExpr, PathExprBox, PathExprRef, PathExprDyn);
derive_enum!(PathExpr, Unqual, Qual);
