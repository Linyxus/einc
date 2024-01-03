package einc.parserc

import Parser.{*, given}
import FunctorOps.*
import MonadOps.*
import AlternativeOps.*

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

    def trailingSpaces: Parser[X] = px << whitespace.many.dropDesc
