package volpts.compiler
package ast

sealed trait Node

case class CompilationUnit(decls: Decl) extends Node

sealed trait Type

case class TypeGeneric(name: String) extends Type

case class TypeApp(id: QualId, args: List[Type]) extends Type

case class TypeFun(params: List[Type], ret: Type) extends Type

sealed trait TypeDef

sealed trait VariantPart

case class ADTPart(id: String, ty: Type) extends VariantPart

case class GADTPart(id: String, fun: TypeFun) extends VariantPart

case class Variant(parts: List[VariantPart]) extends TypeDef

case class RecordPart(id: String, ty: Type)

case class Record(parts: List[RecordPart]) extends TypeDef

case class TypeAlias(ty: Type) extends TypeDef

case class TypeDecl(id: String, param: List[TypeGeneric], typedef: TypeDef) extends Decl

case class ValDecl(id: String, ty: Type, expr: Expr) extends Decl

case class ImportDecl(id: QualId, alias: Option[String]) extends Decl

sealed trait Decl extends Node

case class QualId(ids: List[String])

sealed trait Expr extends Node

case class IntegerLiteral(x: Int) extends Expr

