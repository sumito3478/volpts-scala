package volpts
package compiler

package object syntax {
  import parsing._
  import collection._
  import util._
  import ast._

  trait VolptsTokenizer extends StringParser {
    import inputOps._

    lazy val upper: Rule[StringView] = ('A' to 'Z') / "$" / "_" / Categories("Lu")

    lazy val lower: Rule[StringView] = ('a' to 'z') / Categories("Ll")

    lazy val letter: Rule[StringView] = ('A' to 'Z') / ('a' to 'z') / "$" / "_" / Categories("Lu", "Ll", "Lt", "Lo", "Nl")

    lazy val decimalDigit: Rule[StringView] = '0' to '9'

//    val opchar = ! letter & ! ("[" / "]" / ".") & (('\u0020' to '\u007f') / Categories("Sm"))

    lazy val opchar: Rule[StringView] = ('!' to '/') / (':' to '@') / ('[' to '`') / ('{' to '~') / Categories("Sm")

    lazy val op: Rule[StringView] = opchar.repeat1

    lazy val idrest: NonStoppableRule[StringView] = (letter / decimalDigit).repeat ++ (Rule("_") ++ op).opt

    lazy val varid = lower ++ idrest

    lazy val plainid = (upper ++ idrest) / varid / op

    lazy val id: Rule[StringView] = plainid / (Rule("`") ++ any.repeat ++ "`")

    lazy val decimalLiteral = "-".opt ~ decimalDigit.repeat1 ~ ("L" / "l").? ^^ {
      case sign ### x ### suffix => suffix match {
        case Some(_) => Int64Literal(java.lang.Long.parseLong(sign.toString + x.toString, 10))
        case None => Int32Literal(java.lang.Integer.parseInt(sign.toString + x.toString, 10))
      }
    }

    lazy val hexLiteral = "-".opt ~ "0x" ~ hexDigit.repeat1 ~ ("L" / "l").? ^^ {
      case sign ### _ ### x ### suffix => suffix match {
        case Some(_) => Int64Literal(java.lang.Long.parseLong(sign.toString + x.toString, 16))
        case None => Int32Literal(java.lang.Integer.parseInt(sign.toString + x.toString, 16))
      }
    }

    lazy val binaryLiteral = "-".opt ~ "0b" ~ binaryDigit.repeat1 ~ ("L" / "l").? ^^ {
      case sign ### _ ### x ### suffix => suffix match {
        case Some(_) => Int64Literal(java.lang.Long.parseLong(sign.toString + x.toString, 2))
        case None => Int32Literal(java.lang.Integer.parseInt(sign.toString + x.toString, 2))
      }
    }

    lazy val integerLiteral = binaryLiteral / hexLiteral / decimalLiteral

    lazy val hexDigit = ('0' to '9') / ('A' to 'F') / ('a' to 'f')

    lazy val binaryDigit = "0" / "1"

    lazy val exponentPart = ("E" / "e") ++ ("+" / "-").opt ++ decimalDigit.repeat1

    lazy val floatType: Rule[StringView] = "F" / "f" / "D" / "d"

    lazy val floatingPointLiteral = ("-".opt ++ decimalDigit.repeat1 ++ "." ++ decimalDigit.repeat ++ exponentPart.opt ++ floatType.opt) /
      ("-".opt ++ Rule(".") ++ decimalDigit.repeat1 ++ exponentPart.opt ++ floatType.opt) /
      ("-".opt ++ decimalDigit.repeat1 ++ exponentPart ++ floatType.opt) /
      ("-".opt ++ decimalDigit.repeat1 ++ exponentPart.opt ++ floatType) ^^ (x => DoubleLiteral(java.lang.Double.parseDouble(x.toString)))

    lazy val booleanLiteral = "true" / "false" ^^ (x => BooleanLiteral(java.lang.Boolean.parseBoolean(x.toString)))

    lazy val unicodeEscapeSequence: Rule[String] = "\\u" ~ hexDigit.repeat1 ~ ";" ^^ {
      case _ ### x ### _ => new String(Character.toChars(java.lang.Integer.parseInt(x.toString, 16)))
    }

    lazy val stringLiteralPart: Rule[String] = unicodeEscapeSequence / ((! "\"") & any.repeat1 ^^ (_.toString))

    lazy val stringLiteral: Rule[StringLiteral] = "\"" ~ stringLiteralPart.* ~ "\"" ^^ {
      case _ ### xs ### _ => StringLiteral(xs.mkString)
    }

    lazy val nl = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029" / "\u0085"

    lazy val singleLineComment = "//" ~ ((! nl) & any.repeat) ~ nl.? ^^ {
      case _ ### xs ### _ => xs.toString
    }

    lazy val multiLineComment = "/*" ~ ((!"*/") & any.repeat) ~ "*/" ^^ {
      case _ ### xs ### _ => xs.toString
    }

    lazy val comment = singleLineComment / multiLineComment

    lazy val space = " " / "\t" / "\u000B" / "\u000C" / comment

    lazy val whitespace = space / nl

    lazy val semi: Rule[Unit] = ";" / (nl ~ space.*).+ ~ space.* ^^ (_ => ())

    lazy val let_# = Rule("let")

    lazy val =# = Rule("=")

    lazy val rec_# = Rule("rec")

    lazy val comma_# = Rule(",")

    implicit class TokenizerRule[A](val self: Rule[A]) {
      def s = self followed space

      def ws = self followed whitespace
    }

    implicit class TokenizerNonStoppableRule[A](val self: NonStoppableRule[A]) {
      def s = self followed space

      def ws = self followed whitespace
    }
  }

  trait VolptsParser extends VolptsTokenizer {
    lazy val literal: Rule[Literal] = integerLiteral / floatingPointLiteral / booleanLiteral / stringLiteral

    lazy val expr: Rule[Expr] = letExpr

    lazy val letExpr: Rule[Let] = rule(RuleValWithName(let_#.ws ~ pat.ws ~ =#.ws ~ expr.s ~ semi ~ expr ^^ {
      case _ ### pat ### _ ### expr1 ### _ ### expr2 => {
        Let(Binding(pat, expr1), expr2)
      }
    }))

    lazy val letRecExpr = rule(let_#.ws ~ rec_#.ws ~ pat.ws ~ =#.ws ~ expr.s ~ semi ~ expr ^^ {
      case _ ### _ ### pat ### _ ### expr1 ### _ ### expr2  => {
        LetRec(Binding(pat, expr1), expr2)
      }
    })

    lazy val tupleExpr = rule(expr.sep(comma_#.ws))

    lazy val pat: Rule[Pat] = ???
  }
}
