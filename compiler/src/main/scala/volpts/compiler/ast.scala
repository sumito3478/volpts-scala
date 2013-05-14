package volpts.compiler

package object ast {
  sealed trait Expr

  case class Identifier(name: String) extends Expr

  case class QualifiedId(ids: List[Identifier]) extends Expr

  case class Paren(exp: Expr) extends Expr

  case class App(fun: Expr, arg: Expr) extends Expr

  case class Lambda(pats: List[Pat], expr: Expr) extends Expr

  case class Tuple(exprs: List[Expr]) extends Expr

  sealed trait Literal extends Expr

  case class StringLiteral(value: String) extends Literal

  case class Int32Literal(value: Int) extends Literal

  case class Int64Literal(value: Long) extends Literal

  case class DoubleLiteral(value: Double) extends Literal

  case class BooleanLiteral(value: Boolean) extends Literal

  case class Binding(pats: List[Pat], expr: Expr)

  case class Let(binding: Binding, expr: Expr) extends Expr

  case class LetRec(binding: Binding, expr: Expr) extends Expr

  sealed trait Pat

  case class TuplePattern(pats: List[Pat]) extends Pat

  case class AsPattern(id: QualifiedId, pat: Pat) extends Pat

  case class VarPattern(id: QualifiedId) extends Pat

  case class LiteralPattern(literal: Literal) extends Pat
}
