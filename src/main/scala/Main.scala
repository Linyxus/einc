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

  testParsing("def x = 1", definition.defDefP << eof)
  testParsing("def x =\n  val y = 1\n  y", definition.defDefP << eof)
  testParsing("def add(x: Int, y: Int): Int = add(x, y)", definition.defDefP << eof)
  testParsing("def add(x: Int, y: Int) = add(x, y)", definition.defDefP << eof)
  testParsing("def add(x: Int)(y: Int) = add(x, y)", definition.defDefP << eof)
  testParsing("def add[A]{inst: Add[A]}(x: A, y: A) = add(x, y)", definition.defDefP << eof)

