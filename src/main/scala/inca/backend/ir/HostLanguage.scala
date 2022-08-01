package inca.backend.ir

import inca.util.Scala
import truechange.JavaLitType

trait HostLanguage {
  type Literal
  type BaseType
  type Definition
  type Function
  type Aggregator
}

object ScalaHost extends HostLanguage {
  import scala.meta.quasiquotes._

  sealed trait Literal {
    def typ: DatalogScala.Type
  }
  case class IntLiteral(v: Int) extends Literal {
    override def typ: DatalogScala.Type = TScalaInt
  }
  case class LongLiteral(v: Long) extends Literal {
    override def typ: DatalogScala.Type = TScalaLong
  }
  case class DoubleLiteral(v: Double) extends Literal {
    override def typ: DatalogScala.Type = TScalaDouble
  }
  case class StringLiteral(v: String) extends Literal {
    override def typ: DatalogScala.Type = TScalaString
  }
  case class BooleanLiteral(v: Boolean) extends Literal {
    override def typ: DatalogScala.Type = TScalaBoolean
  }
  def True: DatalogScala.Constant = DatalogScala.Constant(BooleanLiteral(true))
  def False: DatalogScala.Constant = DatalogScala.Constant(BooleanLiteral(false))

  type BaseType = Scala[meta.Type]
  type Definition = Scala[meta.Stat]
  type Function = Scala[meta.Term.Function]
  type Aggregator = Scala[meta.Term]

  def typeAsScala(ty: DatalogScala.Type): meta.Type = ty match {
    case DatalogScala.TAny => t"Any"
    case DatalogScala.TData(name) => meta.Type.Name(name)
    case DatalogScala.TLiteral(litType) => litType match {
      case JavaLitType(cl) =>  Scala.mkQualTypename(cl.getCanonicalName)
      case _ => throw new UnsupportedOperationException
    }
    case DatalogScala.TScala(ty) => ty.tree
    case _: DatalogScala.TLinked => t"truechange.URI"
  }

  object TScalaBoolean extends DatalogScala.TScala(Scala(t"Boolean"))
  object TScalaInt extends DatalogScala.TScala(Scala(t"Int"))
  object TScalaLong extends DatalogScala.TScala(Scala(t"Long"))
  object TScalaDouble extends DatalogScala.TScala(Scala(t"Double"))
  object TScalaString extends DatalogScala.TScala(Scala(t"String"))

  def literalFromScalaMeta(t: meta.Lit): Option[Literal] = t match {
    case meta.Lit.Int(i) => Some(IntLiteral(i))
    case meta.Lit.Long(l) => Some(LongLiteral(l))
    case d: meta.Lit.Double => Some(DoubleLiteral(d.value.asInstanceOf[Double]))
    case meta.Lit.Boolean(b) => Some(BooleanLiteral(b))
    case meta.Lit.String(s) => Some(StringLiteral(s))
    case _ => None
  }
}