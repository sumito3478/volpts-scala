package volpts.compiler

import com.ibm.icu.lang._
import UCharacterEnums.ECharacterCategory._
import scala.annotation._

package object antlr {
  def generateVolptsDef = {
    val builder = new StringBuilder
    builder ++= "// Automatically generated code by volpts.compiler.antlr.generateVolptsDef - Do not edit.\n\n"
    builder ++= "lexer grammar VolptsDef;\n"
    val unicodeCategories = Map(
      "Ll" -> LOWERCASE_LETTER,
      "Lu" -> UPPERCASE_LETTER,
      "Lt" -> TITLECASE_LETTER,
      "Lo" -> OTHER_LETTER,
      "Nl" -> LETTER_NUMBER,
      "Sm" -> MATH_SYMBOL
    )
    for {
      (name, value) <- unicodeCategories.iterator
    } {
      builder ++= s"fragment UnicodeCategory$name :\n"
      val codePoints = for {
        c <- UCharacter.MIN_CODE_POINT to UCharacter.MAX_CODE_POINT if UCharacter.isDefined(c) && UCharacter.isBMP(c) && UCharacter.getType(c) == value
      } yield c
      def spanRange(xs: List[Int]) = {
        var last = xs.head
        val (range, rest) = xs span {
          x =>
            val b = x <= last + 1
            last = x
            b
        }
        (range.head to range.last, rest)
      }
      @tailrec
      def loop(xs: List[Int], acc: List[Range.Inclusive]): List[Range.Inclusive] = xs match {
        case Nil => List()
        case xs => spanRange(xs) match {
          case (range, Nil) => range :: acc
          case (range, rest) => loop(rest, range :: acc)
        }
      }
      def extractRange(xs: List[Int]) = loop(xs, List()).reverse
      val ranges = extractRange(codePoints.toList)
      val lines = for {
        range <- ranges
      } yield
        if (range.start == range.end) f"'\\u${range.start}%04x'"
        else f"'\\u${range.start}%04x' .. '\\u${range.end}%04x'"
      builder ++= lines.mkString(" ", "\n | ", "\n")
      builder ++= " ;\n"
      builder ++= "\n"
    }
    builder.result
  }
}
