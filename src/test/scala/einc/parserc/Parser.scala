package einc.parserc

import Parser.{*, given}

class ParserSuite extends munit.FunSuite:
  test("parser: predicate"):
    ()
    // val p = predicate(_ == 'a')
    // assert("abc".parseWith(p).getResult == 'a')
    // assert("bc".parseWith(p).isError)
