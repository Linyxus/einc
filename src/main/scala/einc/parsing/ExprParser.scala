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
import untpd.*

trait ExprParser:
  import ExprParser.*

  private var rules: List[ParseRule] = Nil

  def atom: Parser[Expr]

  def installRule(rule: ParseRule): Unit = rules = rule :: rules

  def installRules(rules: List[ParseRule]): Unit = rules.foreach(installRule)

  def leftRecursiveRest(min: Precedence, current: Precedence, head: Expr): Parser[Expr] =
    def fetchHeadPrecedence(head: RulePart): Precedence = head match
      case RulePart.Rec(precedence) => precedence
      case RulePart.Operator(text) => assert(false)
    val recRules =
      rules
        .filter(_.isLeftRecursive).filter(_.parts.length > 1)
        .filter(min <= _.precedence)
        .filter(rule => fetchHeadPrecedence(rule.parts.head) <= current)

    def buildParser(rule: ParseRule): Parser[(ParseRule, List[Expr])] =
      val parts = rule.parts.tail
      val p = whitespace.many >> compileParts(this, parts)
      p.map((rule, _))

    choices(recRules.map(buildParser)).flatMap: (rule, exprs) =>
      val newHead = rule.constructor(head :: exprs)
      val newPrec = rule.precedence
      leftRecursiveRest(min, newPrec, newHead) <|> newHead.inject

  def getParser(precedence: Precedence): Parser[Expr] =
    val prefixParsers = rules.filterNot(_.isLeftRecursive).filter(precedence <= _.precedence).map(_.compile(this))
    choices(prefixParsers)
      <|> atom.flatMap: e =>
        leftRecursiveRest(precedence, Precedence.MAX, e) <|> e.inject

object ExprParser:
  case class Precedence(value: Int):
    assert(value >= 0 && value <= 1024)

    def <=(other: Precedence): Boolean =
      value <= other.value

  object Precedence:
    def MAX: Precedence = Precedence(1024)

  enum RulePart:
    case Rec(precedence: Precedence)
    case Operator(text: String)

    def compile(rec: ExprParser): Parser[Option[Expr]] = this match
      case Rec(precedence) => rec.getParser(precedence).map(Some(_))
      case Operator(text) => string(text).withDesc(s"`$text`").map(_ => None)

  case class ParseRule(precedence: Precedence, parts: List[RulePart], constructor: List[Expr] => Expr):
    import RulePart.*
    def isLeftRecursive: Boolean = parts match
      case Rec(_) :: _ => true
      case _ => false

    def compile(recParser: ExprParser): Parser[Expr] =
      compileParts(recParser, parts).map(constructor)

  def compileParts(rec: ExprParser, parts: List[RulePart]): Parser[List[Expr]] =
      parts.map(_.compile(rec)).sepBy(whitespace.many).map(_.flatten)

