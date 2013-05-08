package volpts

package object meta {

  import annotation.tailrec
  import language.experimental.macros
  import reflect.macros._

  private[this] implicit class ContextW(val underlined: Context) extends AnyVal {
    def toCompilerContext = underlined.asInstanceOf[runtime.Context]
  }

  trait ValWithName[A] extends (String => A) {
    def apply(name: String): A
  }

  def valDefWithName[A: c.WeakTypeTag](c: Context)(valWithName: c.Expr[ValWithName[A]]): c.Expr[A] = {
    import c.universe._
    val method = c.macroApplication.symbol.name
    @tailrec
    def enclosingVal(trees: List[c.Tree]): String = trees match {
      case ValDef(_, name, _, _) :: _ => name.decoded.trim
      case (_: Apply | _: Select | _: TypeApply) :: xs => enclosingVal(xs)
      case Block(_, _) :: DefDef(mods, name, _, _, _, _) :: xs if mods.hasFlag(Flag.LAZY) => name.decoded.trim
      case _ => {
        c.error(c.enclosingPosition, s"expected: val someName = ${method.decoded}[SomeType](someTypeInstance)")
        "<error>"
      }
    }
    val name = c.Expr[String](Literal(Constant(enclosingVal(c.toCompilerContext.callsiteTyper.context.enclosingContextChain.map(_.tree.asInstanceOf[c.Tree])))))
    c.universe.reify(valWithName.splice(name.splice))
  }
}