package einc.core.untpd

sealed trait Expr

case class Ident(name: String) extends Expr
case class Apply(fun: Expr, args: List[Expr]) extends Expr

