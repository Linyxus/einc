package einc.parserc

import MonadOps.*

case class SourcePos(pos: Int)

case class ParseInput(source: String, current: SourcePos)

case class ParseContext(descs: List[String]):
  def withDesc(desc: String): ParseContext = ParseContext(desc :: descs)

def ctx(using ctx: ParseContext): ParseContext = ctx

sealed trait ParseOutcome[+X]
case class ParseError(msg: String, pos: SourcePos, alts: List[ParseError])(using ctx: ParseContext) extends ParseOutcome[Nothing]:
  val descs: List[String] = ctx.descs
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
      case ParseOk(x, next, altErrors) => ???

  given parserIsMonad: Monad[Parser] with
    def unit[X](x: X): Parser[X] = Parser: i =>
      ParseOk(x, i, Nil)

    extension [A](fa: Parser[A])
      def flatMap[B](op: A => Parser[B]): Parser[B] = ???

  given parserIsApplicative: Applicative[Parser] = parserIsMonad.asApplicative
