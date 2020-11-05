package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

/** Enumerates the values of the given type */
case class Enum(ty: Type) extends Expression {
  override def freeVars: Map[Name, Option[Type]] = Map()
  override def prettyprint(implicit indent: String): String = s"enum(${ty.prettyprint})"
}

/**
 * Extension adding enum expressions to @see Parser.
 */
trait EnumFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = Enum +: super.desugarables

  override protected[frontend] def atomicExp[_: P]: P[Expression] =
    P("enum" ~ "(" ~ typeAnno ~ ")").mapWithLoc(Enum.apply) |
      super.atomicExp

  override protected[frontend] def keywords: Set[String] = super.keywords + "enum"

  override def typecheckInternal(exp: Expression, anno: Option[Type]): Type = exp match {
    case Enum(ty) =>
      ty match {
        case TNothing => warn(s"$ty contains no values, enumeration will fail", ty)
        case TScala(_) => error(s"Cannot enumerate Scala values of type $ty", ty)
        case _ => // fine
      }
      TEnumeration(ty)
    case _ => super.typecheckInternal(exp, anno)
  }
}

object Enum extends Desugarable {

  override def trans(): DesugarTrans = new DesugarTrans {
    val enumStatements: ListBuffer[Statement] = ListBuffer()

    override def desugarExp(exp: Expression)(implicit gensym: Gensym): Expression = exp match {
      case Enum(ty) =>
        val sym = Name(gensym.fresh(s"enum_${ty.javastring}"))
        enumStatements += Values(sym, ty)
        changed(Var(sym).typed(ty))
      case _ => super.desugarExp(exp)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      if (enumStatements.isEmpty)
        desugared
      else {
        val prepend = enumStatements.toSeq
        enumStatements.clear()
        prepend ++ desugared
      }
    }
  }
}
