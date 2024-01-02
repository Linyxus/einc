package einc.parserc

import FunctorOps.*
import ApplicativeOps.*
import AlternativeOps.*
import MonadOps.*
import Parser.given

object Parsers:
  def eof: Parser[Unit] = Parser:
    case i @ InputState(str, pos) if pos.idx >= str.length =>
      ParseOutcome(i, Nil, ())
    case i =>
      ParseError(i.pos, "expecting EOF but got more input")

  def predicate(p: Char => Boolean, msg: String | Null = null): Parser[Char] = Parser:
    case i @ InputState(str, pos) if pos.idx >= str.length =>
      ParseError(pos, "unexpected EOF")
    case i @ InputState(str, pos) if p(str(pos.idx)) =>
      ParseOutcome(InputState(str, pos.next), Nil, str(pos.idx))
    case i =>
      ParseError(i.pos, if msg ne null then msg else "unmatched predicate")

  def char(ch: Char): Parser[Unit] = predicate(_ == ch) #> ()

  def letter: Parser[Char] = predicate(_.isLetter)

  def digit: Parser[Char] = predicate(_.isDigit)

  def letterOrDigit: Parser[Char] = predicate(_.isLetterOrDigit)

  def string(s: String): Parser[Unit] =
    if s.length == 0 then ().embed
    else (char(s(0)), string(s.substring(1))).mapWith((_, _) => ())

  extension [X](p: Parser[X])
    def optional: Parser[Option[X]] = p.map(Some(_)) <|> None.embed

    def sepBy(sep: Parser[Any]): Parser[List[X]] =
      sepBy1(sep) <|> Nil.inject

    def sepBy1(sep: Parser[Any]): Parser[List[X]] =
      def more: Parser[List[X]] = sep.zip(p).map(_._2).many
      p.zip(more).map: (x, xs) => 
        x :: xs
