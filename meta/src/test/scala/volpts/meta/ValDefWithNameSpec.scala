package volpts
package meta

import org.scalatest.FunSpec
import language.experimental.macros

class ValDefWithNameSpec extends FunSpec {
  describe("volpts.meta.ValDefWithName") {
    it("should call ValWithName with the val name and assign the result to that val.") {
      case class Something(name: Option[String] = None) {
        def withName(newName: String) = Something(Some(newName))
      }
      implicit def someThingToValWithName(something: Something) = new ValWithName[Something] {
        def apply(name: String) = something.withName(name)
      }
      def something(valWithName: ValWithName[Something]): Something = macro valDefWithName[Something]
      val testingSomething = something(Something())
      assert(testingSomething === Something(Some("testingSomething")))
    }
  }
}