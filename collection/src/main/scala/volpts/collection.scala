package volpts

package object collection {
  import scala.collection._
  import scala.collection.mutable
  import scala.reflect.ClassTag

  implicit class IteratorW[A](val self: Iterator[A]) {
    def concatenated[B](implicit ev: A =:= Iterator[B]): Iterator[B] = self.foldLeft[Iterator[B]](Iterator.empty)((acc, i) => acc ++ i)

    def firstSome[B](implicit ev: A =:= Option[B]): Option[B] = self.find(_.isDefined).map(_.get)

    def somes[B](implicit ev: A =:= Option[B]): Iterator[B] = self.filter(_.isDefined).map(_.get)

    def last = self.foldLeft[Option[A]](None)((acc, i) => Some(i))
  }

  implicit class TraversableLikeW[A, Repr](val self: TraversableLike[A, Repr]) {
    def collectFirst[B](default: A => B)(pf: A --> B) = self.collectFirst(pf).getOrElse(default)

    def collectWithTails[B](pf: Repr --> B) = self.tails.collect(pf)

    def collectFirstWithTails[B](pf: Repr --> B) = self.tails.collectFirst(pf)

    def collectFirstWithTails[B](default: Repr => B)(pf: Repr --> B) = self.tails.collectFirst(pf).getOrElse(default)
  }

  trait MutableStackedMap[A, B] extends mutable.Map[A, B] with mutable.MapLike[A, B, MutableStackedMap[A, B]] {
    self =>
    protected[this] def newMutableMap: mutable.Map[A, B]

    private[this] case class Frame(name: String, data: mutable.Map[A, B])

    private[this] val stack = new mutable.Stack[Frame]

    enter

    def enter(name: String): Unit = stack.push(Frame(name, newMutableMap))

    def enter: Unit = enter("<anon>")

    def leave: Unit = stack.pop

    def frame[A](name: String)(f: this.type => A): A = {
      enter(name)
      try f(this)
      finally leave
    }

    def frame[A](f: this.type => A): A = frame("<anon>")(f)

    def iterator = stack.iterator.map(_.data.iterator).concatenated[(A, B)]

    def get(key: A) = stack.iterator.map(_.data.get(key)).firstSome[B]

    def +=(kv: (A, B)) = {
      stack.top.data += kv
      this
    }

    def -=(key: A) = {
      stack.top.data -= key
      this
    }

    override def empty = new MutableStackedMap[A, B] {
      def newMutableMap = self.newMutableMap
    }
  }

  object MutableStackedMap {
    def apply[A, B](elems: (A, B)*) = empty[A, B] ++= elems

    def empty[A, B] = new MutableStackedMap[A, B] {
      protected[this] def newMutableMap = mutable.OpenHashMap.empty[A, B]
    }
  }

  class MutableArrayMap[A: ClassTag](override val size: Int) extends mutable.Map[Int, A] with mutable.MapLike[Int, A, MutableArrayMap[A]] {
    private[this] val buffer = implicitly[ClassTag[A]].newArray(size)

    def iterator = buffer.iterator.zipWithIndex.map(_.swap)

    def get(key: Int) = Option(buffer(key))

    def +=(kv: (Int, A)) = {
      buffer(kv._1) = kv._2
      this
    }

    def -=(key: Int) = {
      buffer(key) = _ : A
      this
    }

    override def empty = new MutableArrayMap[A](size)
  }

  case class StringView(src: String, from: Int, until: Int) extends IndexedSeq[Char] with IndexedSeqOptimized[Char, StringView] {
    def apply(idx: Int) = src.charAt(from + idx)

    def length: Int = until - from

    override def slice(from: Int, until: Int) = StringView(src, this.from + from , this.from + until)
  }
}
