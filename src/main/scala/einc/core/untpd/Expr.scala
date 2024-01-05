package einc.core
package untpd

sealed trait Expr extends Positioned

case class Ident(name: String) extends Expr
case class Apply(fun: Expr, args: List[Expr]) extends Expr

