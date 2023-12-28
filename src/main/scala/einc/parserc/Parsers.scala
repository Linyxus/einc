package einc.parserc

import FunctorOps.*
import ApplicativeOps.*
import AlternativeOps.*
import MonadOps.*
import Parser.given

object Parsers:
  def predicate(p: Char => Boolean, msg: String | Null = null): Parser[Char] = Parser:
    case i @ InputState(str, pos) if pos.idx >= str.length =>
      ParseError(pos, "unexpected EOF")
    case i @ InputState(str, pos) if p(str(pos.idx)) =>
      (InputState(str, pos.next), str(pos.idx))
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
    def optional: Parser[Option[X]] = p.recoverable.map(Some(_)) <|> None.embed

    def sepBy(sep: Parser[Any]): Parser[List[X]] =
      sepBy1(sep) <|> Nil.inject

    def sepBy1(sep: Parser[Any]): Parser[List[X]] =
      p.flatMap: x =>
        sep.zip(sepBy(sep)).optional.map:
          case None => x :: Nil
          case Some((_, xs)) => x :: xs


