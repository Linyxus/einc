package einc.core
package tpd

enum TypeKind extends Positioned:
  case Star
  case Arrow(arg: TypeKind, result: TypeKind)

