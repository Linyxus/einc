package einc.parserc

import MonadOps.*

case class SourcePos(pos: Int):
  def next: SourcePos = SourcePos(pos + 1)

case class ParseInput(source: String, current: SourcePos):
  def currentChar: Option[Char] =
    if current.pos < source.length then Some(source(current.pos))
    else None

  def forward: ParseInput = ParseInput(source, current.next)

  override def toString(): String =
    val (prefix, suffix) = source.splitAt(current.pos)
    s"SourcePos($prefix|$suffix)"

object SourcePos:
  def init: SourcePos = SourcePos(0)

case class ParseContext(descs: List[String]):
  def withDesc(desc: String): ParseContext = ParseContext(desc :: descs)

object ParseContext:
  def empty: ParseContext = ParseContext(Nil)

def ctx(using ctx: ParseContext): ParseContext = ctx

sealed trait ParseOutcome[+X]:
  def addAlts(alts: List[ParseError]): ParseOutcome[X] = this match
    case err @ ParseError(msg, pos, alts1) => err.derivedParseError(msg, pos, alts1 ++ alts)
    case res @ ParseOk(x, next, altErrors) => res.copy(altErrors = altErrors ++ alts)

case class ParseError(msg: String, pos: SourcePos, alts: List[ParseError])(using ctx: ParseContext) extends ParseOutcome[Nothing]:
  val descs: List[String] = ctx.descs

  override def toString(): String =
    s"ParseError($msg, $pos, $alts) under $descs"

  def derivedParseError(msg1: String, pos1: SourcePos, alts1: List[ParseError]): ParseError =
    ParseError(msg1, pos1, alts1)

case class ParseOk[X](x: X, next: ParseInput, altErrors: List[ParseError]) extends ParseOutcome[X]

trait Parser[+X]:
  def run(input: ParseInput)(using ctx: ParseContext): ParseOutcome[X]

  def withDesc(desc: String): Parser[X] = Parser: input =>
    run(input)(using ctx.withDesc(desc))

object Parser:
  def apply[X](f: ParseInput => ParseContext ?=> ParseOutcome[X]): Parser[X] =
    new Parser[X]:
      def run(input: ParseInput)(using ctx: ParseContext): ParseOutcome[X] = f(input)

  def fail[X](msg: String): Parser[X] = Parser: _ =>
    ParseError(msg, SourcePos(0), Nil)

  def flatten[X](ppa: Parser[Parser[X]]): Parser[X] = Parser: input =>
    ppa.run(input) match
      case e @ ParseError(ctx, pos, alts) => e
      case ParseOk(pa, input1, altErrors1) => pa.run(input1) match
        case e @ ParseError(ctx, pos, alts) => ParseError(ctx, pos, altErrors1 ++ alts)
        case ParseOk(x, next, altErrors2) => ParseOk(x, next, altErrors1 ++ altErrors2)

  given parserIsApplicative: Applicative[Parser] with
    def pure[X](x: X): Parser[X] = Parser: i =>
      ParseOk(x, i, Nil)

    extension [A](fa: Parser[A]) def map[B](op: A => B): Parser[B] = Parser: i =>
      fa.run(i) match
        case err @ ParseError(msg, pos, alts) => err
        case ParseOk(x, next, altErrors) => ParseOk(op(x), next, altErrors)

    extension [A, B](ff: Parser[A => B]) def <*>(fa: => Parser[A]): Parser[B] =
      ff.flatMap: f =>
        fa.map(f)

  given parserIsMonad: Monad[Parser] with
    def unit[X](x: X): Parser[X] = x.embed

    extension [A](fa: Parser[A])
      def flatMap[B](op: A => Parser[B]): Parser[B] = flatten(op <#> fa)


  given parserIsAlternative: Alternative[Parser] with
    def fail[X]: Parser[X] = Parser.fail("none of the alternative matches")

    extension [A](pa: Parser[A]) 
      def <|>[B](pb: => Parser[B]): Parser[A | B] = Parser: input =>
        pa.run(input) match
          case err @ ParseError(msg, pos, alts1) => pb.run(input).addAlts(err :: alts1)
          case res @ ParseOk(x, next, altErrors) => res
