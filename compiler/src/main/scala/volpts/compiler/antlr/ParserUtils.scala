package volpts.compiler.antlr

import VolptsLexer._
import org.antlr.runtime._

class ParserUtils {
  val nonOffendingTokens = List(SEMICOLON, NEW_LINE, EOF)

  def promoteNEW_LINE(scope: ParserRuleReturnScope, input: TokenStream) = {
    val lt = input.LT(1)
    val la = lt.getType
    if (!nonOffendingTokens.contains(la)) {
      def loop(idx: Int): Unit = if (idx > 0) {
        val lt = input.get(idx)
        if (lt.getChannel == Token.DEFAULT_CHANNEL) ()
        else if (lt.getType == NEW_LINE) {
          lt.setChannel(Token.DEFAULT_CHANNEL)
          input.seek(lt.getTokenIndex)
          if (scope != null) scope.start = lt
        } else loop(idx - 1)
      }
      loop(lt.getTokenIndex - 1)
    }
  }
}
