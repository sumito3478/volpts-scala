package volpts.spec.compiler.syntax

import org.scalatest.WordSpec
import volpts.parsing.{InputData, StringInputData}
import volpts.collection.StringView

class Spec extends WordSpec {
  "VolptsTokenizer#integerLiteral" should {
    "parse 100" in {
      import volpts.compiler.syntax.VolptsTokenizer
      import volpts.parsing.StringInputData
      val tokenizer = new VolptsTokenizer {
        val inputOps: StringInputData = StringInputData("100")
      }
      import tokenizer.inputOps._
      println(tokenizer.integerLiteral(slice(0, wholeSize)))
    }
    "parse 100L" in {
      import volpts.compiler.syntax.VolptsTokenizer
      import volpts.parsing.StringInputData
      val tokenizer = new VolptsTokenizer {
        val inputOps: StringInputData = StringInputData("100L")
      }
      import tokenizer.inputOps._
      println(tokenizer.integerLiteral(slice(0, wholeSize)))
    }
    "parse -100" in {
      import volpts.compiler.syntax.VolptsTokenizer
      import volpts.parsing.StringInputData
      val tokenizer = new VolptsTokenizer {
        val inputOps: StringInputData = StringInputData("-100")
      }
      import tokenizer.inputOps._
      println(tokenizer.integerLiteral(slice(0, wholeSize)))
    }
  }
}
