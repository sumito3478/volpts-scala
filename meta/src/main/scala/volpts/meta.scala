package volpts

package object meta {

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
    val name = c.Expr[String](Literal(Constant((for {
      treeTail <-  (for {
        chain <- c.toCompilerContext.callsiteTyper.context.enclosingContextChain
        tree = chain.tree.asInstanceOf[c.Tree]
      } yield(tree)).tails
      name <- treeTail match {
        case ValDef(_, name, _, _) :: _ => Some(name.decoded.trim)
        case (_: Apply | _: Select | _: TypeApply) :: _ => None
        case Block(_, _) :: DefDef(mods, name, _, _, _, _) :: _ if mods.hasFlag(Flag.LAZY) => Some(name.decoded.trim)
        case DefDef(mods, name, _, _, _, _) :: _ if mods.hasFlag(Flag.LAZY) => Some(name.decoded.trim)
        case _ => {
          c.error(c.enclosingPosition, s"expected: [lazy] val someName = ${c.macroApplication.symbol.name.decoded}[SomeType](someTypeInstance)")
          Some("<error>")
        }
      }
    } yield(name)).next)))
    reify(valWithName.splice(name.splice))
  }
}