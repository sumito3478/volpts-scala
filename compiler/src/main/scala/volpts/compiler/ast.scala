package volpts.compiler

package object ast {
  sealed trait Expr

  sealed trait Identifier extends Expr

  case class Paren(exp: Expr) extends Expr

  case class App(fun: Expr, arg: Expr) extends Expr

  case class Lambda(pats: List[Pat], expr: Expr) extends Expr

  case class Tuple(exprs: List[Expr])

  sealed trait Literal extends Expr

  case class StringLiteral(value: String) extends Literal

  case class Int32Literal(value: Int) extends Literal

  case class Int64Literal(value: Long) extends Literal

  case class DoubleLiteral(value: Double) extends Literal

  case class BooleanLiteral(value: Boolean) extends Literal

  sealed trait Pat

  case class PTuple(pats: List[Pat]) extends Pat
}
