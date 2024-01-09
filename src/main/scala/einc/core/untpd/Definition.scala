package einc.core
package untpd

sealed trait Definition extends Positioned

/** Definitions that can be local */
sealed trait LocalDef extends Definition

object Definition:
  case class ValDef(name: String, body: Expr) extends LocalDef

  sealed trait DefParam extends Positioned:
    val name: String
  case class DefTypeParam(name: String) extends DefParam
  case class DefSynthesisParam(name: String, tpe: TypeExpr) extends DefParam
  case class DefTermParam(name: String, tpe: TypeExpr) extends DefParam

  sealed trait DefParamList:
    val params: List[DefParam]
  case class TypeParamList(params: List[DefTypeParam]) extends DefParamList
  case class SynthesisParamList(params: List[DefSynthesisParam]) extends DefParamList
  case class TermParamList(params: List[DefTermParam]) extends DefParamList

  case class DefDef(name: String, paramss: List[DefParamList], resType: Option[TypeExpr], body: Expr) extends LocalDef

  case class ConstructorDef(name: String, components: List[DefParamList], tpe: TypeExpr) extends Positioned

  case class DataDef(name: String, kind: tpd.TypeKind, constructors: List[ConstructorDef]) extends LocalDef

