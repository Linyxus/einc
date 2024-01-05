package einc.core
package untpd

sealed trait Definition extends Positioned

/** Definitions that can be local */
sealed trait LocalDef extends Definition

case class ValDef(name: String, body: Expr) extends LocalDef

