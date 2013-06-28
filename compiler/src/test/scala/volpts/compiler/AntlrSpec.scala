package volpts.compiler

import org.scalatest._

import org.antlr.runtime._
import org.antlr.runtime.tree._
import scalax.io._
import volpts.compiler.antlr._

class AntlrSpec extends WordSpec {
  "antlr.VolptsParser" should {
    "parse test.volpts" in {
      val code = Resource.fromURL(getClass.getResource("test.volpts")).string(Codec.UTF8)
      val input = new ANTLRStringStream(code)
      println("starting parser")
      val lex = new VolptsLexer(input)
      val tokens = new CommonTokenStream(lex)
      val parser = new VolptsParser(tokens)
      val ret = parser.compilation_unit
      println(ret.getTree.asInstanceOf[Tree].toStringTree)
    }
  }
}
