package volpts

package object util {
  import scala.collection.TraversableLike

  type -->[-A, +B] = PartialFunction[A, B]

  implicit class IteratorW[A](val self: Iterator[A]) extends AnyVal {
    def nextOption = if(self.hasNext) Some(self.next) else None
  }

  implicit class TraversableLikeW[A, Repr](val self: TraversableLike[A, Repr]) extends AnyVal {
    def collectFirstWithTails[B](default: Repr => B)(p: Repr --> B): B = (for {
      tail <- self.tails
      collected <- p.lift(tail)
    } yield(collected)).nextOption.getOrElse(default(self.repr))
  }
}
