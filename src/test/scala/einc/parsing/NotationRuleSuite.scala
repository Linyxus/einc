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

class NotationRuleSuite extends munit.FunSuite:
  import Common.*
  test("notation rule parsing: basics"):
    assertFailure("notation:50 lhs \"+\" rhs => lhs", notationRule.parser << eof)
    assertSuccess("notation:50 lhs:50 \"+\" rhs:51 => lhs", notationRule.parser << eof)
    assertFailure("notation: lhs:50 \"+\" rhs:51 => lhs", notationRule.parser << eof)
    assertFailure("notation:50lhs:50 \"+\" rhs:51 => lhs", notationRule.parser << eof)
    assertSuccess("notation:10 \"if\" cond:10 \"then\" iftrue:10 \"else\" iffalse:10 => cond", notationRule.parser << eof)

