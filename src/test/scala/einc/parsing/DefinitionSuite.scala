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

  test("definition parsing: data constructor"):
    assertSuccess("unit : Unit", definition.constructorP << eof)
    assertSuccess("inl[A](x: A): Or[A, B]", definition.constructorP << eof)
    assertSuccess("inl[A]{inst: Add[A]}(x: A): Or[A, B]", definition.constructorP << eof)
    assertSuccess("Nil[A]: List[A]", definition.constructorP << eof)
    assertSuccess("Cons[A](head: A, tail: List[A]): List[A]", definition.constructorP << eof)

  test("definition parsing: data def"):
    val testCases = List(
    """
data Or : Type => Type => Type where
  inl[A, B](a: A): Or[A, B]
  inr[A, B](b: B): Or[A, B]
    """,
    """
data Tup : Type => Type => Type where
  tup[A, B](a: A, b: B): Tup[A, B]
    """,
    """
data List : Type => Type where
  nil[A]: List[A]
  cons[A](head: A, tail: List[A]): List[A]
    """,
    )
    testCases.foreach: testCase =>
      assertSuccess(testCase, initWS >> definition.dataDefP << ws << eof)

