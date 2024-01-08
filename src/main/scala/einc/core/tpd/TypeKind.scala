package einc.core.tpd

enum TypeKind:
  case Star
  case Arrow(arg: TypeKind, result: TypeKind)

