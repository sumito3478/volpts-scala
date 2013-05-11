package object volpts {
  type -->[-A, +B] = PartialFunction[A, B]

  class __

  class ---[A, B]

  implicit object Dummy0 extends (__ --- __)

  implicit object Dummy1 extends (__ --- __ --- __)

  implicit object Dummy2 extends (__ --- __ --- __ --- __)

  implicit object Dummy3 extends (__ --- __ --- __ --- __ --- __)
}
