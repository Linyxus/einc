package einc.parserc

import FunctorOps.*
import ApplicativeOps.*
import AlternativeOps.*
import MonadOps.*
import Parser.given

object Parsers:
  def isEof: Parser[Boolean] = Parser:
    case i @ InputState(str, pos) =>
      Right(i, pos.idx >= str.length)

  def peek: Parser[Char] = Parser:
    case i @ InputState(str, pos) =>
      if pos.idx >= str.length then Left(ParseError(pos, "unexpected EOF"))
      else Right((i, str(pos.idx)))

  def predicate(p: Char => Boolean, msg: String | Null = null): Parser[Char] = Parser:
    case i @ InputState(str, pos) if pos.idx >= str.length =>
      Left(ParseError(pos, "unexpected EOF"))
    case i @ InputState(str, pos) if p(str(pos.idx)) =>
      Right(InputState(str, pos.next), str(pos.idx))
    case i =>
      Left(ParseError(i.pos, if msg ne null then msg else "unmatched predicate"))

  def char(ch: Char): Parser[Unit] = predicate(_ == ch) #> ()

  def string(s: String): Parser[Unit] =
    if s.length == 0 then ().embed
    else (char(s(0)), string(s.substring(1))).mapWith((_, _) => ())

  def sepBy[A](p: Parser[A], sep: Parser[Any]): Parser[List[A]] = ???

  def sepBy1[A](p: Parser[A], sep: Parser[Any]): Parser[List[A]] = ???

