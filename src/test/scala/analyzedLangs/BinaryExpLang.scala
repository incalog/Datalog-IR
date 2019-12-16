package analyzedLangs

import org.inca.core.Typp.{DataTypeDeclaration, Typ}

object BinaryExpLang {
  abstract class Exp

  abstract class BinaryOp(lhs: Exp, rhs: Exp) extends Exp with Typ

  case class PlusExp(lhs: Exp, rhs: Exp) extends BinaryOp(lhs, rhs)
  case class MinusExp(lhs: Exp, rhs: Exp) extends BinaryOp(lhs, rhs)
  case class VariableDeclaration(name: String, initializer: Option[Exp], typ: Typ)
  case class LinkDeclaration(name: String)

  case class PrimitiveDataTypeDeclaration(name: String) extends DataTypeDeclaration(name)
}
