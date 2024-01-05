package einc.core
package untpd

sealed trait TypeExpr extends Positioned

object TypeExpr:
  case class Ident(name: String) extends TypeExpr

