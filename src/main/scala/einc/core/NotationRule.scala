package einc.core

enum NotationPatternPart:
  case Ident(name: String, precedence: Int)
  case Operator(text: String)

case class NotationPattern(parts: List[NotationPatternPart])

case class NotationRule(precedence: Int, pattern: NotationPattern, rhs: untpd.Expr)

