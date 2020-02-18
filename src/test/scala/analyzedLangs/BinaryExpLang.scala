package analyzedLangs

import org.inca.lang.core.Typ.DataTypeDeclaration
import org.inca.meta.MetaElements.MetaElement

object BinaryExpLang {
  abstract class Exp

  abstract class BinaryOp(lhs: Exp, rhs: Exp) extends Exp with MetaElement

  case class PlusExp(lhs: Exp, rhs: Exp) extends BinaryOp(lhs, rhs)
  case class MinusExp(lhs: Exp, rhs: Exp) extends BinaryOp(lhs, rhs)
  case class VariableDeclaration(name: String, initializer: Option[Exp], typ: MetaElement)
  case class LinkDeclaration(name: String)

  case class PrimitiveDataTypeDeclaration(name: String) extends DataTypeDeclaration(name)
}
