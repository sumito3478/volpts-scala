package volpts.compiler

package object ast {
  sealed trait Expr

  sealed trait Identifier extends Expr

  case class Paren(exp: Expr) extends Expr

  case class App(fun: Expr, arg: Expr) extends Expr

  case class Lambda(pats: List[Pat], expr: Expr) extends Expr

  case class Tuple(exprs: List[Expr])

  sealed trait Pat

  case class PTuple(pats: List[Pat]) extends Pat
}
