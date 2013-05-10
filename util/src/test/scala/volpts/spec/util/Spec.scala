package volpts.spec.util
import org.scalatest._

class Spec extends WordSpec {
  "IteratorW" when {
    "nextOption" should {
      "has a next element and returns it" in {
        import volpts.util._
        assert(Iterator(1).nextOption.isDefined === true)
      }

      "has no next element and returns None" in {
        import volpts.util._
        val it = Iterator(1)
        it.next // make it empty
        assert(it.nextOption.isEmpty === true)
      }
    }
  }

  "TraversableLikeW" when {
    // example GADT trait and case classes
    sealed trait A[A] {
      def x: A
    }
    case class B(x: Int) extends A[Int]
    case class C(x: String) extends A[String]
    case class D(x: A[Int]) extends A[A[Int]]
    "collectFirstWithTails" should {
      "successfully collects" in {
        import volpts.util._
        val input = List[A[_]](B(1), D(B(2)), C("3"), B(4), B(5), D(B(6)), B(7))
        // let's find the first occurence of D(x), B(x) and return (Int, Int). if not found, return (8, 9).
        val ret = input.collectFirstWithTails(_ => (8, 9)) {
          case D(B(x)) :: B(y) :: _ => (x, y)
        }
        assert(ret === (6, 7))
      }

      "fails to collect and fallbacks to the default" in {
        import volpts.util._
        val input = List[A[_]](B(1), D(B(2)), C("3"), B(4), B(5), D(B(6)), B(7))
        // let's find the first occurence of B(x), B(x), C(x) and return (Int, Int, String). if not found, return (8, 9, "10")
        val ret = input.collectFirstWithTails(_ => (8, 9, "10")) {
          case B(x) :: B(y) :: C(z) :: _ => (x, y, z)
        }
        assert(ret === (8, 9, "10"))
      }
    }
  }
}