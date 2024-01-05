package einc
package parsing

import parserc.*
import parserc.Parsers.*
import Parser.{*, given}
import FunctorOps.*
import ApplicativeOps.*
import MonadOps.*
import AlternativeOps.*
import printing.*

object Common:
  def assertSuccess(input: String, parser: Parser[Any]): Unit =
    val result = input.parseWith(parser)
    result match
      case err @ ParseError(_, _, _, _) =>
        val repr = err.show(input)
        assert(false, s"Parsing test should succeed but failed with:\n$repr\n")
      case ParseOk(x, next, altErrors) =>

  def assertFailure(input: String, parser: Parser[Any]): Unit =
    val result = input.parseWith(parser)
    result match
      case err @ ParseError(_, _, _, _) =>
      case ParseOk(x, next, altErrors) =>
        assert(false, s"Parsing test should fail but succeeded with result $x and remaining input $next")

