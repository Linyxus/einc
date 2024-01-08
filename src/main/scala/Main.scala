import einc.*
import parserc.*
import Parser.{*, given}
import Parsers.*
import FunctorOps.*
import ApplicativeOps.*
import MonadOps.*
import AlternativeOps.*
import einc.core.Span
import printing.*
import parsing.Parsers.*

def testParsing[X](source: String, p: Parser[X]): Unit =
  source.parseWith(p) match
    case err: ParseError =>
      println("Parse error")
      println(err.show(source))
    case ParseOk(x, next, altErrors) =>
      println(s"Parse Ok: $x, next = $next")
      
@main def main: Unit =
  println("Welcome to einc")

