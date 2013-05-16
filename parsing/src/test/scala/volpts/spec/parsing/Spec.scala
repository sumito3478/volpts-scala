package volpts.spec.parsing

import org.scalatest.WordSpec

class Spec extends WordSpec {
  "parsing.Parser#Result" should {
    "be able to be used in for comprehension" in {
      import volpts.parsing._
      val parser = new StringParser {
        def inputOps = new StringInputData("")
      }
      import parser._

    }
  }
}
