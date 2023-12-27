package einc.parserc

import Parsers.*

class ParserSuite extends munit.FunSuite:
  test("parser: predicate"):
    val p = predicate(_ == 'a')

