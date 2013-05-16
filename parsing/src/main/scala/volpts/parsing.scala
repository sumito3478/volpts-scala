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

    def size(self: Input)(implicit dummy0 : Dummy0): Int

    def size(self: Source)(implicit dummy0 : Dummy1): Int

    def wholeSize: Int

    def pos(self: Input): Int

    def until(self: Input): Int

    def startsWith(self: Input, x: Input)(implicit dummy0 : Dummy0): Boolean

    def startsWith(self: Input, x: Source)(implicit dummy1 : Dummy1): Boolean

    def concatenate(self: Input, that: Input): Input = {
      require(until(self) == pos(that))
      slice(pos(self), until(that))
    }
  }

  trait Parser[Input, Source] {
    parser =>

    val inputOps: InputData[Input, Source]

    import inputOps._

    sealed trait Result[+A] {
      self =>

      def map[B](f: (A, Input) => (B, Input)): Result[B] = this match {
        case Success(x, in) => (Success.apply[B] _).tupled(f(x, in))
        case f: Failure => f
      }

      def flatMap[B](f: (A, Input) => Result[B]) = this match {
        case Success(x, in) => f(x, in)
        case f: Failure => f
      }

      def filter(p: A => Boolean): Result[A] = this match {
        case Success(x, in) if(p(x)) => this
        case Success(x, in) => Failure(Error("result filtered", frames) :: Nil)
        case f: Failure => f
      }

      def foreach[U](f: A => U): Unit = this match {
        case Success(x, in) => f(x)
        case _: Failure => ()
      }

      class WithFilter(p: A => Boolean) {
        def map[B](f: (A, Input) => (B, Input)) = self filter p map f
        def flatMap[B](f: (A, Input) => Result[B]) = self filter p flatMap f
        def foreach[U](f: A => U) = self filter p foreach f
        def withFilter(q: A => Boolean) = new WithFilter(x => p(x) && q(x))
      }

      def withFilter(p: A => Boolean) = new WithFilter(p)
    }

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

      private[this] lazy val cache = new MutableArrayMap[Result[A]](wholeSize + 1)

      private[this] val recursion = new mutable.Stack[Int]

      private[this] def ruleFrame(in: Input)(f: => Result[A]): Result[A] = if(recursion.contains(pos(in))) Failure(Error(s"recursion of $name", frames) :: Nil)
        else {
          recursion.push(pos(in))
          try f
          finally recursion.pop
        }

      def apply(in: Input): Result[A] = frame(Frame(name, in)){
        ruleFrame(in) {
          cache.getOrElseUpdate(pos(in), f(in))
        }
      }

      protected[this] def Repr[B](f: Input => Result[B]): Repr[B]

      def /[B <: C, C >: A](that: => Repr[B]): Repr[C] = Repr(in => (this(in): Result[C]) match {
        case s: Success[A] => s: Result[C]
        case f: Failure => that(in) match {
          case s: Success[B] => s: Result[C]
          case f2: Failure => f ++ f2
        }
      })

      def ~[B](that: => Repr[B]): Repr[A ### B] = Repr(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(x1 ### x2, in2)
          case f: Failure => f
        }
        case f: Failure => f
      })

      def ++(that: => Repr[Input])(implicit ev: A <:< Input): Repr[Input] = Repr(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(concatenate(x1, x2), in2)
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

      def opt(implicit ev: A <:< Input) : NonStoppableRule[Input] = NonStoppableRule(in => this(in) match {
        case Success(x1, in1) => Success(x1, in1)
        case _: Failure => Success(slice(pos(in), pos(in)), in)
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

      def followed[B](that: Repr[B]): Repr[A] = Repr(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(x1, in2)
          case f: Failure => f
        }
        case f: Failure => f
      })

      def sep[B](sep: Repr[B]): Repr[List[A]] = Repr(in => this(in) match {
        case Success(x1, in1) => {
          @tailrec
          def loop(in: Input, l: List[A]): (Input, List[A]) = sep(in) match {
            case Success(_, in2) => this(in2) match {
              case Success(x3, in3) => loop(in3, x3 :: l)
              case _: Failure => (in, l)
            }
            case _: Failure => (in, l)
          }
          val (in4, l) = loop(in1, Nil)
          Success(x1 :: l.reverse, in4)
        }
        case f: Failure => f
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

      def ~[B](that: => Rule[B]): Rule[A ### B] = Rule(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(x1 ### x2, in2)
          case f: Failure => f
        }
        case f: Failure => f
      })

      def ++(that: => Rule[Input])(implicit ev: A <:< Input): Rule[Input] = Rule(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(concatenate(x1, x2), in2)
          case f: Failure => f
        }
        case f: Failure => f
      })

      def followed[B](that: Rule[B]): Rule[A] = this.~[B](that) ^^ {
        case x ### _ => x
      }
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

      def ~[B](that: => NonStoppableRule[B]): Rule[A ### B] = Rule(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(x1 ### x2, in2)
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

      def ++(that: => NonStoppableRule[Input])(implicit ev: A <:< Input): Rule[Input] = Rule(in => this(in) match {
        case Success(x1, in1) => that(in1) match {
          case Success(x2, in2) => Success(concatenate(x1, x2), in2)
          case f: Failure => f
        }
        case f: Failure => f
      })

      def followed[B](that: NonStoppableRule[B]): Rule[A] = this.~[B](that) ^^ {
        case x ### _ => x
      }
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

    lazy val any: Rule[Input] = Rule(in =>
      if(pos(in) < wholeSize) Success(slice(pos(in), pos(in) + 1), slice(pos(in) + 1, wholeSize))
      else Failure(Error("unexpected eof", frames) :: Nil))
  }

  case class StringInputData(src: String) extends InputData[StringView, String] {
    def slice(from: Int, until: Int): StringView = StringView(src, from, until)

    def slice(from: Int): StringView = StringView(src, from, wholeSize)

    def slice(self: StringView, from: Int, until: Int): StringView = self.slice(from, until)

    def slice(self: StringView, from: Int): StringView = self.slice(from, self.size)

    def size(self: StringView)(implicit dummy0 : Dummy0): Int = self.size

    def size(self: String)(implicit dummy1 : Dummy1): Int = self.length

    def wholeSize: Int = src.length

    def pos(self: StringView): Int = self.from

    def until(self: StringView): Int = self.until

    def startsWith(self: StringView, x: StringView)(implicit dummy0 : Dummy0): Boolean = self.startsWith(x)

    def startsWith(self: StringView, x: String)(implicit dummy1 : Dummy1): Boolean = self.startsWith(x)
  }

  trait StringParser extends Parser[StringView, String] {
    val inputOps: StringInputData

    import inputOps._

    implicit def Rule(string: String): Rule[StringView] = inputRule(string)

    case class Categories(names: String*)

    import scala.util.matching.Regex

    private[this] def Rule(regex: Regex): Rule[StringView] = {
      Rule(in => regex.findPrefixOf(slice(pos(in), wholeSize).toString).map(x => Success(slice(pos(in), pos(in) + x.length), slice(pos(in) + x.length, wholeSize))).
        getOrElse(Failure(Error(s"regex pattern '$regex' expected but ${slice(pos(in), pos(in) + 10)}.. found ", frames) :: Nil)))
    }

    implicit def Rule(categories: Categories): Rule[StringView] = Rule(categories.names.mkString("[\\p{", "}\\p{", "}]").r)

    implicit def Rule(range: immutable.NumericRange.Inclusive[Char]): Rule[StringView] = Rule(in => {
        val char = slice(in, 0, 1)
        val successOption = for {
          c <- char.headOption
          ret = Success(char, slice(in, 1, wholeSize)) if range.contains(c)
        } yield(ret)
        successOption.getOrElse(Failure(Error(s"Unicode character in the range from ${range.start} to ${range.end} expected, but ${char.headOption.getOrElse("EOF")} found", frames) :: Nil))
      })
  }
}
