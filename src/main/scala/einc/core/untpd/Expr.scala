package einc.core
package untpd

sealed trait Expr extends Positioned

object Expr:
  case class IntLit(value: Int) extends Expr
  case class StringLit(value: String) extends Expr
  case class Ident(name: String) extends Expr
  case class Apply(fun: Expr, args: List[Expr]) extends Expr

  case class LambdaParam(name: String, typ: Option[TypeExpr]) extends Positioned
  case class Lambda(params: List[LambdaParam], body: Expr) extends Expr

  case class Block(localDefs: List[LocalDef], expr: Expr) extends Expr

