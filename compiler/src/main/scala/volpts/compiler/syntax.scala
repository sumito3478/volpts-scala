package volpts
package compiler

package object syntax {
  import parsing._
  import collection._
  import util._

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

    lazy val decimalNumeral = decimalDigit.repeat1

    lazy val hexNumeral = "0x" ~ hexDigit.repeat1

    lazy val binaryNumeral = "0b" ~ binaryDigit.repeat1

    lazy val integerLiteral = (decimalNumeral / hexNumeral / binaryNumeral) ~ ("L" / "l").?

    lazy val hexDigit = ('0' to '9') / ('A' to 'F') / ('a' to 'f')

    lazy val binaryDigit = "0" / "1"

    lazy val exponentPart = ("E" / "e") ++ ("+" / "-").opt ++ decimalDigit.repeat1

    lazy val floatType: Rule[StringView] = "F" / "f" / "D" / "d"

    lazy val floatingPointLiteral = (decimalDigit.repeat1 ++ "." ++ decimalDigit.repeat ++ exponentPart.opt ++ floatType.opt) /
      (Rule(".") ++ decimalDigit.repeat1 ++ exponentPart.opt ++ floatType.opt) /
      (decimalDigit.repeat1 ++ exponentPart ++ floatType.opt) /
      (decimalDigit.repeat1 ++ exponentPart.opt ++ floatType)

    lazy val booleanLiteral = "true" / "false"

    lazy val unicodeEscapeSequence: Rule[String] = "\\u" ~ hexDigit.repeat1 ~ ";" ^^ {
      case _ *** x *** _ => new String(Character.toChars(java.lang.Integer.parseInt(x.toString, 16)))
    }

    lazy val stringLiteralPart: Rule[String] = unicodeEscapeSequence / ((! "\"") & any.repeat1 ^^ (_.toString))

    lazy val stringLiteral: Rule[String] = "\"" ~ stringLiteralPart.* ~ "\"" ^^ {
      case _ *** xs *** _ => xs.mkString
    }

    lazy val nl = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029" / "\u0085"

    lazy val singleLineComment = "//" ~ ((! nl) & any.repeat) ~ nl.? ^^ {
      case _ *** xs *** _ => xs.toString
    }

    lazy val multiLineComment = "/*" ~ ((!"*/") & any.repeat) ~ "*/" ^^ {
      case _ *** xs *** _ => xs.toString
    }

    lazy val comment = singleLineComment / multiLineComment

    lazy val space = " " / "\t" / "\u000B" / "\u000C" / comment

    lazy val whitespace = space / nl

    lazy val semi: Rule[Unit] = ";" / (nl ~ space.*).+ ~ space.* ^^ (_ => ())
  }
}