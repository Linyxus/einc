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
import parsing.Parsers.*

class DefinitionSuite extends munit.FunSuite:
  import Common.*

  test("definition parsing: def"):
    assertSuccess("def x = 1", definition.defDefP << eof)
    assertSuccess("def x =\n  val y = 1\n  y", definition.defDefP << eof)
    assertSuccess("def add(x: Int, y: Int): Int = add(x, y)", definition.defDefP << eof)
    assertSuccess("def add(x: Int, y: Int) = add(x, y)", definition.defDefP << eof)
    assertSuccess("def add(x: Int)(y: Int) = add(x, y)", definition.defDefP << eof)
    assertSuccess("def add[A]{inst: Add[A]}(x: A, y: A) = add(x, y)", definition.defDefP << eof)

