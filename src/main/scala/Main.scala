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

def basicTest: Unit =
  val p1 = predicate(_ == 'a')
  println("abc".parseWith(p1))

  val p2 = p1.withDesc("`a`").some
  println("aaabc".parseWith(p2))

  val p3 = string("https")
  println("https://www.google.com".parseWith(p3))
  println("http://www.google.com".parseWith(p3))

  val p4 = string("http")
  val p5 = p3 <|> p4
  println("https://www.google.com".parseWith(p5))
  println("http://www.google.com".parseWith(p5))

def testStr[X](s: String, p: Parser[X]): Unit =
  s.parseWith(p) match
    case err: ParseError =>
      println(err.show(s))
    case res @ ParseOk(x, next, altErrors) =>
      println(s"OK: $res")

def urlTest: Unit =
  val pname = alpha.withDesc("alphabetic character").some.map(_.mkString).withDesc("a subdomain")
  val domain = pname.sepBy1(char('.').withDesc("`.`")).withDesc("a domain")
  // println("abc".parseWith(domain))
  // println("abc.bcd".parseWith(domain))
  // println("abc.bcd.".parseWith(domain))

  val httpP = string("http") #> "http"
  val httpsP = string("https") #> "https"
  val tcpP = string("tcp") #> "tcp"

  val protocol = ((httpsP.withDesc("`http`") <|> httpP.withDesc("`https`") <|> tcpP.withDesc("`tcp`")) << string("://").withDesc("`://`")).withDesc("protocol")
  // println("http://".parseWith(protocol))
  // println("https://".parseWith(protocol))
  // println("https:/".parseWith(protocol))

  val port = (string(":") >> digit.withDesc("a digit").some.map(_.mkString.toInt)).withDesc("a port")
  // println(":".parseWith(port))
  // println(":123123".parseWith(port))

  val url = (protocol ^~ domain ^~ port.optional).withDesc("url") << eof
  // testStr("https://www.google.com:123123", url)
  // testStr("http://www.google.com:123123", url)
  testStr("http://www.google.:123123", url)
  testStr("http://www.google.com:12abc", url)
  testStr("http://www.google.com.", url)
  testStr("tc://www.google.com", url)
  testStr("tcp://.google.com", url)

def listLiteralTest: Unit =
  val identP: Parser[String] =
    alpha.withDesc("alphabetic character").some.map(_.mkString).withDesc("an identifier").trailingSpaces
  // println("hello".parseWith(identP << eof))
  // println("hello  ".parseWith(identP << eof))
  // println("helloworld123  ".parseWith(identP << eof).maybeSelectErrors)

  val numberP: Parser[Int] =
    digit.withDesc("digit character").some.map(_.mkString.toInt).withDesc("a number").trailingSpaces
  // println("123123  ".parseWith(numberP << eof).maybeSelectErrors)
  // println("123123u  ".parseWith(numberP << eof).maybeSelectErrors)

  val comma: Parser[Unit] =
    char(',').withDesc("`,`").trailingSpaces

  val leftParen: Parser[Unit] =
    char('[').withDesc("`[`").trailingSpaces

  val rightParen: Parser[Unit] =
    char(']').withDesc("`]`").trailingSpaces

  val elemP: Parser[String | Int] =
    (identP <|> numberP)
  val listP: Parser[List[String | Int]] =
    (leftParen >> elemP.sepBy(comma) << rightParen).withDesc("a list literal").withDesc("an expression").withDesc("the program")

  //testStr("[1, 2, 3]", listP)
  testStr("[1, 2, ]", listP << eof)
  testStr("[1, 2, a123]", listP << eof)
  testStr("[1, 2,,]", listP << eof)
  testStr("[1, 2]]", listP << eof)
  testStr("[123,123123,abc", listP << eof)

def printerTest: Unit =
  import printing.*
  val source = """
val x = 1
val y = 2
def add = x + y
"""
  val printer = new SpanPrinter(source)
  println(printer.sourceLines)
  println(printer.show(Span(1, 20), "test", List("test")))

@main def main: Unit =
  println("... Parsing tests ...")
  // println("--- Basic ---")
  // basicTest

  // println("--- URL ---")
  // urlTest

  println("--- List literal ---")
  listLiteralTest

  // println("... Printer tests ...")
  // println("--- Basic ---")
  // printerTest
