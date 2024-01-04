package einc
package parsing

import core.*
import parserc.*
import parserc.Parsers.*
import Parser.{*, given}
import FunctorOps.*
import MonadOps.*
import ApplicativeOps.*
import AlternativeOps.*

object Parsers:
  import untpd.*
  val nameP: Parser[String] =
    val p =
      (alpha.withDesc("an alphabetic character").withDesc("start of an identifier") ^~ whileP(ch => ch.isLetterOrDigit || ch == '-').withDesc("the remaining of an identifier")).map: (x, xs) =>
        x.toString + xs
    p.withDesc("an identifier")

  val exprP: Parser[Expr] = nameP.map(Ident(_))

  val numberP: Parser[String] =
    digit.withDesc("a digit").some.map(_.mkString)

  val stringLitP: Parser[String] =
    val p = whileP(_ != '"')
    val delimiter = char('"').withDesc("`\"`")
    p.surroundedBy(delimiter, delimiter)

  /** Parser for notation rules */
  object notationRule:
    /** Parses `ident:prec` in the pattern */
    val identP: Parser[NotationPatternPart.Ident] =
      (nameP ^~ char(':').withDesc("`:`") ^~ numberP.withDesc("precedence")).map: (ident, _, prec) =>
        NotationPatternPart.Ident(ident, prec.toInt)

    /** Parses operators in the pattern */
    val operatorP: Parser[NotationPatternPart.Operator] =
      stringLitP.map(NotationPatternPart.Operator(_))

    /** Parses the pattern */
    val patternP: Parser[NotationPattern] =
      val p = identP.withDesc("a pattern identifier") <|> operatorP.withDesc("an operator")
      p.trailingSpaces.some.withDesc("a notation pattern").map(NotationPattern(_))

    /** Parses the part `notation:prec` */
    val keywordP: Parser[Int] =
      (string("notation").withDesc("`notation`") >> char(':').withDesc("`:`") >> numberP.withDesc("precedence")).map(_.toInt)

    val ruleP: Parser[NotationRule] =
      (keywordP.withDesc("the `notation` keyword").trailingSpaces
        ^~ patternP.withDesc("a notation pattern").trailingSpaces
        ^~ string("=>").withDesc("`=>`").trailingSpaces
        ^~ exprP.withDesc("rhs of the notation rule")).map: (prec, pattern, _, rhs) =>
          NotationRule(prec, pattern, rhs)

    val parser: Parser[NotationRule] = ruleP.withDesc("a notation rule")
