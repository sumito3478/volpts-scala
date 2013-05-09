package volpts.spec.meta

import org.scalatest._

class Spec extends WordSpec {
  "valDefWithName" should {
    // exmaple case class
    case class Something(x: String, y: Int)
    "calls the given ValWithName function with the val name currently trying to bind and binds the result to that val" in {
      import volpts.meta._
      import language.experimental.macros
      implicit def somethingToValWithName(something: Something) = new ValWithName[Something] {
        def apply(name: String) = Something(name, something.y)
      }
      def declareSomething(valWithName: ValWithName[Something]) = macro valDefWithName[Something]
      val someVal = declareSomething(Something("origName", 1))
      assert(someVal === Something("someVal", 1))
    }
  }
}
