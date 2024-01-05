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

case class ParseContext(descs: List[String], indentLevels: List[Int]):
  import ParseContext.*

  val uniqId: Int =
    numCtx += 1
    numCtx

  def withDesc(desc: String): ParseContext = copy(descs = desc :: descs)

  def dropDesc: ParseContext = copy(descs = Nil)

  def withIndentLevel(newLevel: Int): ParseContext = copy(indentLevels = newLevel :: indentLevels)

  def indentLevel: Int = indentLevels.headOption.getOrElse(0)

object ParseContext:
  private var numCtx: Int = 0

  def empty: ParseContext = ParseContext(Nil, Nil)

def ctx(using ctx: ParseContext): ParseContext = ctx

sealed trait ParseOutcome[+X]:
  def addAlts(alts: List[ParseError]): ParseOutcome[X] = this match
    case err @ ParseError(msg, pos, alts1, from) => err.derivedParseError(msg, pos, alts1 ++ alts, from)
    case res @ ParseOk(x, next, altErrors) => res.copy(altErrors = altErrors ++ alts)

  def maybeSelectErrors: ParseOk[X] | List[ParseError] = this match
    case err @ ParseError(msg, pos, alts1, from) => err.selectErrors
    case res @ ParseOk(x, next, altErrors) => res

object ParseError:
  private var cnt: Int = 0

case class ParseError(msg: String, pos: SourcePos, alts: List[ParseError], parsedFrom: SourcePos)(using ctx: ParseContext) extends ParseOutcome[Nothing]:
  val uniqId: Int =
    ParseError.cnt += 1
    ParseError.cnt

  val descs: List[String] = ctx.descs

  override def toString(): String =
    s"ParseError($msg, $pos) under $descs with id $uniqId"

  //assert(uniqId != 12, this)

  def derivedParseError(msg1: String, pos1: SourcePos, alts1: List[ParseError], from1: SourcePos): ParseError =
    val result = ParseError(msg1, pos1, alts1, from1)
    //println(s"DERIVATION: $uniqId ---> ${result.uniqId}")
    result

  def selectErrors: List[ParseError] =
    var selected: List[ParseError] = this :: Nil
    var furthest: Int = pos.pos
    @annotation.tailrec def go(errors: List[ParseError]): Unit = errors match
      case Nil =>
      case error :: errors =>
        if error.pos.pos > furthest then
          selected = error :: Nil
          furthest = error.pos.pos
        else if error.pos.pos == furthest then
          selected = error :: selected
        go(errors)
    go(alts)
    selected

case class ParseOk[+X](x: X, next: ParseInput, altErrors: List[ParseError]) extends ParseOutcome[X]

trait Parser[+X]:
  import Parser.numParsers

  val uniqId: Int =
    numParsers += 1
    numParsers

  //assert(uniqId != 128)

  def run(input: ParseInput)(using ctx: ParseContext): ParseOutcome[X]

  def runParser(input: ParseInput)(using ctx: ParseContext): ParseOutcome[X] =
    run(input) match
      case err @ ParseError(msg, pos, alts, from) =>
        //if err.uniqId == 12 then println(s"!!! $err thrown by parser $uniqId")
        err
      case res @ ParseOk(x, next, altErrors) => res

  def withDesc(desc: String): Parser[X] = Parser: input =>
    runParser(input)(using ctx.withDesc(desc))

  def dropDesc: Parser[X] = Parser: input =>
    runParser(input)(using ctx.dropDesc)

object Parser:
  private var numParsers: Int = 0

  def apply[X](f: ParseInput => ParseContext ?=> ParseOutcome[X]): Parser[X] =
    new Parser[X]:
      def run(input: ParseInput)(using ctx: ParseContext): ParseOutcome[X] = f(input)

  def fail[X](msg: String): Parser[X] = Parser: i =>
    ParseError(msg, i.current, Nil, i.current)

  def flatten[X](ppa: Parser[Parser[X]]): Parser[X] = Parser: input =>
    ppa.runParser(input) match
      case e @ ParseError(ctx, pos, alts, from) => e
      case ParseOk(pa, input1, altErrors1) => pa.runParser(input1) match
        case e @ ParseError(ctx, pos, alts, from) =>
          //ParseError(ctx, pos, altErrors1 ++ alts, input.current)
          e.derivedParseError(ctx, pos, altErrors1 ++ alts, input.current)
        case ParseOk(x, next, altErrors2) => ParseOk(x, next, altErrors1 ++ altErrors2)

  given parserIsApplicative: Applicative[Parser] with
    def pure[X](x: X): Parser[X] = Parser: i =>
      ParseOk(x, i, Nil)

    extension [A](fa: Parser[A]) def map[B](op: A => B): Parser[B] = Parser: i =>
      fa.runParser(i) match
        case err @ ParseError(msg, pos, alts, from) => err
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
        pa.runParser(input) match
          case err @ ParseError(msg, pos, alts1, from) => pb.runParser(input).addAlts(err :: alts1)
          case res @ ParseOk(x, next, altErrors) => res
