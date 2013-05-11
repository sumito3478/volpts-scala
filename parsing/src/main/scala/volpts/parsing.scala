package volpts

import volpts.collection.MutableArrayMap

package object parsing {
  import scala.collection.mutable
  import scala.collection.immutable
  import annotation.tailrec
  import util._
  import collection._

  trait InputData[Input, Source] {
    def slice(from: Int, until: Int): Input

    def slice(self: Input, from: Int, until: Int): Input

    def size(self: Input)(implicit __ : Dummy0.type): Int

    def size(self: Source)(implicit __ : Dummy1.type): Int

    def wholeSize: Int

    def pos(self: Input): Int

    def until(self: Input): Int

    def startsWith(self: Input, x: Input)(implicit __ : Dummy0.type): Boolean

    def startsWith(self: Input, x: Source)(implicit __ : Dummy1.type): Boolean
  }

  trait Parser[Input, Source] {
    parser =>

    val inputOp: InputData[Input, Source]

    import inputOp._

    sealed trait Result[+A]

    case class Success[+A](x: A, in: Input) extends Result[A]

    case class Error(e: String, stackFrame: immutable.Stack[Frame])

    case class Failure(es: List[Error]) extends Result[Nothing] {
      def ++(that: Failure) = Failure(this.es ++ that.es)
    }

    case class Frame(name: String, in: Input)

    private[this] val stack = new mutable.Stack[Frame]

    def frame[A](frame: Frame)(f: => A): A = {
      stack.push(frame)
      try f
      finally stack.pop
    }

    def frames: immutable.Stack[Frame] = immutable.Stack(stack: _*)

    trait RuleLike[+A] extends (Input => Result[A]) {
      protected val f: Input => Result[A]

      val name: String

      type Repr[+A] <: RuleLike[A]

      private[this] lazy val cache = new MutableArrayMap[Result[A]](wholeSize)

      def apply(in: Input): Result[A] = frame(Frame(name, in)){
        cache.getOrElseUpdate(pos(in), f(in))
      }

      protected[this] def Repr[B](f: Input => Result[B]): Repr[B]

      def /[B <: C, C >: A](that: => Repr[B]): Repr[C] = Repr(in => (this(in): Result[C]) match {
        case s: Success[A] => s: Result[C]
        case f: Failure => that(in) match {
          case s: Success[B] => s: Result[C]
          case f2: Failure => f ++ f2
        }
      })

      def ~[B](that: => Repr[B]): Repr[A *** B] = Repr(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(x1 *** x2, in2)
          case f: Failure => f
        }
        case f: Failure => f
      })

      def &[B](that: => Repr[B]): Repr[B] = Repr(in => this(in) match {
        case Success(_, _) => that(in)
        case f: Failure => f
      })

      def ^^[B](f: A => B): Repr[B] = Repr(in => this(in) match {
        case Success(x1, in1) => Success(f(x1), in1)
        case f: Failure => f
      })

      def ? : NonStoppableRule[Option[A]] = NonStoppableRule(in => this(in) match {
        case Success(x1, in1) => Success(Some(x1), in1)
        case _: Failure => Success(None, in)
      })

      def * : NonStoppableRule[List[A]] = NonStoppableRule(in => {
        @tailrec
        def loop(xs: List[A], in: Input): (List[A], Input) = this(in) match {
          case Success(x2, in2) => {
            loop(xs ::: List(x2), in2)
          }
          case f: Failure => (xs, in)
        }
        val (xs, in2) = loop(Nil, in)
        Success(xs, in2)
      })

      def repeat(implicit ev: A <:< Input): NonStoppableRule[Input] = NonStoppableRule(in => {
        @tailrec
        def loop(x: Input, in: Input): (Input, Input) = this(in) match {
          case Success(x2, in2) => loop(slice(pos(x), until(x2)), in2)
          case f: Failure => (x, in)
        }
        val (xs, in2) = loop(in, in)
        Success(xs, in2)
      })

      def + : Repr[List[A]] = Repr(in => {
        @tailrec
        def loop(xs: List[A], in: Input): (List[A], Input, Failure) = this(in) match {
          case Success(x2, in2) => {
            loop(xs ::: List(x2), in2)
          }
          case f: Failure => (xs, in, f)
        }
        val (xs, in2, f) = loop(Nil, in)
        if (xs.length > 0) Success(xs, in2)
        else f
      })

      def repeat1(implicit ev: A <:< Input): Repr[Input] = Repr(in => {
        @tailrec
        def loop(view: Input, in: Input): (Input, Input, Result[Input]) = this(in) match {
          case Success(x2, in2) => loop(slice(pos(view), until(x2)), in2)
          case f: Failure => (view, in, f)
        }
        val (xs, in2, f) = loop(in, in)
        if (size(xs) > 0) Success(xs, in2)
        else f
      })

      def repeat(n: Int)(implicit ev: A <:< Input): Repr[Input] = Repr(in => {
        @tailrec
        def loop(view: Input, in: Input, rest: Int): (Input, Input, Result[Input]) = this(in) match {
          case Success(x2, in2) => if(rest > 1) loop(slice(pos(view), until(x2)), in2, rest - 1)
          else (view, in, Success(x2, in2))
          case f: Failure => (view, in, f)
        }
        val (xs, in2, f) = loop(in, in, n)
        if (size(xs) == n) Success(xs, in2)
        else f
      })
    }

    final case class NonStoppableRule[+A](protected val f: Input => Result[A], name: String = "") extends RuleLike[A] {
      type Repr[+A] = NonStoppableRule[A]

      def Repr[B](f: Input => Result[B]) = NonStoppableRule(f)

      def /[B <: C, C >: A](that: => Rule[B]): NonStoppableRule[C] = NonStoppableRule(in => (this(in): Result[C]) match {
        case s: Success[A] => s: Result[C]
        case f: Failure => that(in) match {
          case s: Success[B] => s: Result[C]
          case f2: Failure => f ++ f2
        }
      })

      def &[B](that: => Rule[B]): Rule[B] = Rule(in => this(in) match {
        case Success(_, _) => that(in)
        case f: Failure => f
      })

      def ~[B](that: => Rule[B]): Rule[A *** B] = Rule(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(x1 *** x2, in2)
          case f: Failure => f
        }
        case f: Failure => f
      })
    }

    final case class Rule[+A](protected val f: Input => Result[A], name: String = "") extends RuleLike[A] {
      type Repr[+A] = Rule[A]

      def Repr[B](f: Input => Result[B]) = Rule(f)

      def /[B <: C, C >: A](that: => NonStoppableRule[B]): NonStoppableRule[C] = NonStoppableRule(in => (this(in): Result[C]) match {
        case s: Success[A] => s: Result[C]
        case f: Failure => that(in) match {
          case s: Success[B] => s: Result[C]
          case f2: Failure => f ++ f2
        }
      })

      def ~[B](that: => NonStoppableRule[B]): Rule[A *** B] = Rule(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(x1 *** x2, in2)
          case f: Failure => f
        }
        case f: Failure => f
      })

      //
      //      def ![B](that: => Rule[B]): Rule[A --> B] = Rule(in => this(in) match {
      //        case Success(x1, in1) => {
      //          cache(in)
      //          cut(in) // invalidate backtracking to in
      //          val Success(x2, in2) = that(in1)
      //          Success(-->(x1, x2), in2)
      //        }
      //        case f: Failure => f
      //      })

      def unary_! : NonStoppableRule[Unit] = NonStoppableRule(in => this(in) match {
        case _: Success[A] => {
          val f = frames
          Failure(Error(s"while parsing ${f.top.name}: unary_! failure", f) :: Nil)
        }
        case _: Failure => Success((), in)
      })
    }

    import meta._
    import language.experimental.macros

    implicit def RuleValWithName[A](rule: Rule[A]): ValWithName[Rule[A]] = new ValWithName[Rule[A]] {
      def apply(name: String) = Rule(in => rule(in), name)
    }

    def rule[A](valWithName: ValWithName[Rule[A]]): Rule[A] = macro valDefWithName[Rule[A]]

    def inputRule(param: Source): Rule[Input] = Rule(in =>
      if(startsWith(in, param)) Success(slice(in, 0, size(param)), slice(in, size(param), size(in)))
      else Failure(Error(s"expected: $param actual: ${slice(in, 0, 10)}", frames) :: Nil))
  }
}
