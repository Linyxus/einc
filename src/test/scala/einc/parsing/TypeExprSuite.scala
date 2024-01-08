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

class TypeExprSuite extends munit.FunSuite:
  import Common.*

  test("type expr parsing: type reference"):
    assertSuccess("Int", typeExpr.parser << eof)
    assertSuccess("String", typeExpr.parser << eof)
    assertSuccess("X", typeExpr.parser << eof)

  test("type expr parsing: applied type"):
    assertSuccess("List[Int]", typeExpr.parser << eof)
    assertSuccess("Tuple[Int, Int]", typeExpr.parser << eof)
    assertSuccess("Map[String, Int]", typeExpr.parser << eof)
    assertSuccess("Map[String, Map[TypeRef, Symbol]]", typeExpr.parser << eof)

