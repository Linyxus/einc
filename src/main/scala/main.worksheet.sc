import einc.parserc.*

import FunctorOps.*
import ApplicativeOps.*
import AlternativeOps.*
import MonadOps.*
import Parser.{given, *}
import Parsers.*

val p1 = predicate(_ == 'a')
val r1 = p1.run(InputState("abc", SourcePos(0)))
r1

val p2 = string("http")
val r2 = p2.run(InputState("http://baidu.com", SourcePos(0)))
r2

val r3 = p2.run(InputState("http", SourcePos(0)))
r3

val pname = letter.recoverable.some
"abcd".parseWith(pname)
"abcd123".parseWith(pname)

val pdomain = pname.sepBy1(char('.'))
"abc".parseWith(pdomain)
"www.baidu.com".parseWith(pdomain)

