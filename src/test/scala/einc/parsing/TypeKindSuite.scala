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

class TypeKindSuite extends munit.FunSuite:
  import Common.*

  test("type kind parsing: basics"):
    assertSuccess("Type", typeKind.parser << eof)
    assertSuccess("Type => Type", typeKind.parser << eof)
    assertSuccess("Type => Type => Type", typeKind.parser << eof)
    assertSuccess("Type => Type => (Type => Type)", typeKind.parser << eof)

  test("type kind parsing: higher-order"):
    assertSuccess("(Type => Type) => (Type => Type)", typeKind.parser << eof)
    assertSuccess("((Type => Type) => Type) => (Type => Type)", typeKind.parser << eof)
    assertSuccess("((Type => Type) => Type) => (Type => Type) => Type", typeKind.parser << eof)

