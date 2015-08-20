// derived from the C syntax definition

use std::rc::Rc;
use std::marker::PhantomData;

pub enum BinaryOperator {
	LogicalOr,
	LogicalAnd,
	InclusiveOr,
	ExclusiveOr,
	And,
	Eq,
	Ne,
	Lt,
	Gt,
	Le,
	Ge,
	Left,
	Right,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
}

pub type TranslationUnit = Vec<ExternalDecl>;

pub type Exp = Box<Exp_>;
pub type Stat = Box<Stat_>;
pub type Id = Rc<String>;

pub enum ExternalDecl {
	FunctionDefinition(DeclSpecs),
	Decl(Decl)
}

pub struct FunctionDefinition(DeclSpecs, Declarator, /* No K&R decls, */ CompoundStat);

pub struct Decl(DeclSpecs, Vec<InitDeclarator>);
pub struct StructDecl(SpecQualifiers, Vec<StructDeclarator>);

pub struct InitDeclarator(Declarator, Option<Initializer>);
pub struct StructDeclarator(Declarator, Option<Exp>);

pub struct DeclSpecs {
	scs: StorageClassSpecs,
	sq: SpecQualifiers
}

pub struct SpecQualifiers {
	tq: TypeQualifiers,
	ts: TypeSpecs,
}

pub type TypeSpecs = Vec<TypeSpec>;

pub enum TypeSpec {
	Void,
	Char,
	Short,
	Int,
	Long,
	Float,
	Double,
	Signed,
	Unsigned,
	Struct(Id),
	Union(Id),
	Enum(Id),
	Typedef(Id),
}

pub enum TypeQualifier {
	Const,
	Volatile,
}

pub enum StorageClassSpec {
	Auto,
	Register,
	Static,
	Extern,
	Typedef,
}

pub struct Bitfield<T, U>(T, PhantomData<U>);

pub type StorageClassSpecs = Bitfield<u8, StorageClassSpec>;
pub type TypeQualifiers    = Bitfield<u8, TypeQualifier>;

pub struct StructOrUnionSpec {
	sut: StructOrUnion,
	id: Option<Id>,
	fields: Vec<StructDecl>
}

pub enum StructOrUnion {
	Struct,
	Union,
}

pub struct EnumSpec(Id, Option<EnumeratorList>);
pub type EnumeratorList = Vec<Enumerator>;
pub struct Enumerator(Id, Option<Exp>);

pub type Declarator = Rc<Declarator_>;
pub enum Declarator_ {
	Abstract,
	Id(Id),
	Pointer(Pointer, Declarator),
	Array(Declarator, Option<Exp>),
	Function(Declarator, ParamTypes),
}

pub struct Pointer(TypeQualifiers);

pub type Pointers = Vec<Pointer>;

pub struct ParamTypes {
	params: Vec<ParamDecl>,
	vararg: bool
}

pub struct ParamDecl(DeclSpecs, Declarator);

pub enum Initializer {
	Exp(Exp),
	IList(Vec<Initializer>),
	/*IFields*/
}

pub struct TypeRef(SpecQualifiers, Declarator);

pub struct Statement {
	labels: Vec<Label>
}

pub enum Label {
	Id(Id),
	Case(Exp),
	Default,
}

pub struct Stat_ {
	labels: Vec<Label>,
	body: Stat2,
}

pub enum Stat2 {
	ExpStat(Exp),
	CompoundStat(CompoundStat),

	If(Exp, Stat, Stat),
	Switch(Exp, Stat),

	While(Exp, Stat),
	DoWhile(Exp, Stat),
	For(Option<Exp>, Option<Exp>, Option<Exp>, Stat),

	GotoStat(String),
	Continue,
	Break,
	Return(Exp),
	ReturnVoid,
}

pub struct CompoundStat(Vec<Decl>, Vec<Stat>);

/*
assignment_operator     : '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<='
						| '>>=' | '&=' | '^=' | '|='
						;
*/
pub enum Exp_ {
	CommaExp(Exp, Exp),
	AssignmentExp(BinaryOperator, Exp, Exp),
	ConditionalExp(Exp, Exp, Exp),
	BinaryExp(BinaryOperator, Exp, Exp),
	Cast(TypeRef, Exp),
	UnaryExp(UnaryOperator),
	SizeofExp(Exp),
	SizeofTy(TypeRef),
	IndexExp(Exp, Exp),
	CallExp(Exp, Vec<Exp>),
	DotExp(Exp, Id),
	ArrowExp(Exp, Id),
	PostIncrement(Exp),
	PostDecrement(Exp),
	PrimaryExp(PrimaryExp),
}

pub enum UnaryOperator {
	AddrOf,
	Deref,
	Plus,
	Minus,
	Not,
	LogicalNot,
	Increment,
	Decrement,
}

pub enum PrimaryExp {
	Id(Id),
	Const(Const),
	String(String),
}

pub enum Const {
	UInt(u64),
	Int(i64),
	Float(f64),
	EnumerationConst(Id),
}
