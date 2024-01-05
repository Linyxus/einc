package einc.parserc

import Parser.{*, given}
import FunctorOps.*
import MonadOps.*
import AlternativeOps.*
import ApplicativeOps.*

object Parsers:
  /** Helper function to quickly run a parser on a string */
  extension (s: String) def parseWith[X](p: Parser[X]): ParseOutcome[X] =
    p.runParser(ParseInput(s, SourcePos.init))(using ParseContext.empty)

  /** A parser that consumes a character if it matches the predicate */
  def predicate(pred: Char => Boolean, errMsg: String | Null = null): Parser[Char] =
    //assert(errMsg ne null)
    Parser: input =>
      input.currentChar match
        case None => ParseError("unexpected end of input", input.current, Nil, input.current)
        case Some(ch) =>
          val msg = if errMsg eq null then s"character `$ch` does not match the predicate" else errMsg
          if pred(ch) then ParseOk(ch, input.forward, Nil)
          else ParseError(msg, input.current, Nil, input.current)

  /** Read the current position of parsing */
  def currentPos: Parser[SourcePos] = Parser: input =>
    ParseOk(input.current, input, Nil)

  def originalSource: Parser[String] = Parser: input =>
    ParseOk(input.source, input, Nil)

  def getCtx: Parser[ParseContext] = Parser: input =>
    ParseOk(ctx, input, Nil)

  /** A parser that asserts EOF */
  def eof: Parser[Unit] = Parser: input =>
    input.currentChar match
      case None => ParseOk((), input, Nil)
      case Some(ch) => ParseError(s"expected EOF, but found more input", input.current, Nil, input.current)

  /** A parser that consumes a specific character */
  def char(ch: Char): Parser[Unit] = predicate(_ == ch, errMsg = s"unexpected character when looking for `$ch`") #> ()

  /** A parser that consumes a specific string */
  def string(s: String): Parser[Unit] = Parser: input =>
    if input.source.substring(input.current.pos).startsWith(s) then
      ParseOk((), ParseInput(input.source, SourcePos(input.current.pos + s.length)), Nil)
    else
      ParseError(s"expected string `$s`", input.current, Nil, input.current)

  def lookaheadStr(text: String): Parser[Unit] = Parser: input =>
    if input.source.substring(input.current.pos).startsWith(text) then
      ParseOk((), input, Nil)
    else ParseError(s"expected string `$text`", input.current, Nil, input.current)

  def whileP(p: Char => Boolean, toAvoid: Char => Boolean = ch => false): Parser[String] = Parser: input =>
    var current = input
    var errMsg: Option[String] = None
    @annotation.tailrec def go(): Unit =
      current.currentChar match
        case Some(ch) if toAvoid(ch) =>
          errMsg = Some(s"unexpected character `$ch`")
        case Some(ch) if p(ch) =>
          current = current.forward
          go()
        case _ =>
    go()
    errMsg match
      case None =>
        val str = input.source.substring(input.current.pos, current.current.pos)
        ParseOk(str, current, Nil)
      case Some(msg) =>
        ParseError(msg, current.current, Nil, input.current)

  def untilP(p: Char => Boolean, toAvoid: Char => Boolean = ch => false): Parser[String] = whileP(!p(_), toAvoid)

  /** Alphabetic characters */
  def alpha: Parser[Char] = predicate(_.isLetter, errMsg = "unexpected character when looking for an alphabetic character")

  /** Numeric characters */
  def digit: Parser[Char] = predicate(_.isDigit, errMsg = "unexpected character when looking for a digit")

  /** Whitespace */
  def whitespace: Parser[Char] = predicate(_.isWhitespace)

  extension[X] (px: Parser[X])
    def sepBy(psep: Parser[Any]): Parser[List[X]] = sepBy1(psep) <|> Nil.embed

    def sepBy1(psep: Parser[Any]): Parser[List[X]] =
      def rest: Parser[X] = psep >> px
      def rests: Parser[List[X]] = rest.many
      px.flatMap: x =>
        rests.map: xs =>
          x :: xs

    def optional: Parser[Option[X]] = px.map(Some(_)) <|> None.embed

    def surroundedBy(l: Parser[Any], r: Parser[Any]): Parser[X] =
      l >> px << r

  extension [X](pxs: List[Parser[X]])
    def sepBy(psep: Parser[Any]): Parser[List[X]] = pxs match
      case Nil => Nil.inject
      case p0 :: pxs =>
        def go(now: Parser[X], rest: List[Parser[X]]): Parser[List[X]] = rest match
          case Nil => now.map(List(_))
          case next :: rest =>
            (now << psep ^~ go(next, rest)).map: (x, xs) =>
              x :: xs
        go(p0, pxs)
