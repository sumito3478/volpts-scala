package object volpts {
  type -->[-A, +B] = PartialFunction[A, B]

  class __

  class ---[A, B]

  sealed trait Dummy0

  implicit object dummy0 extends Dummy0

  sealed trait Dummy1

  implicit object dummy1 extends Dummy1

  sealed trait Dummy2

  implicit object dummy2 extends Dummy2

  sealed trait Dummy3

  implicit object dummy3 extends Dummy3

  implicit class AnyW[A](val self: A) {
    def ###[B](that: B): A ### B = volpts.###.apply(self, that)
  }
}
