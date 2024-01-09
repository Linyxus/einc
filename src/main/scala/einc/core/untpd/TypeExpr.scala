package einc.core
package untpd

sealed trait TypeExpr extends Positioned

object TypeExpr:
  case class TypeRef(name: String) extends TypeExpr
  case class AppliedType(tycon: TypeExpr, args: List[TypeExpr]) extends TypeExpr
  case class FunctionType(argType: List[TypeExpr], resType: TypeExpr) extends TypeExpr

