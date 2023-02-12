package inca.frontend.datalog.syntax

import inca.compiler.SourceLocation
import inca.frontend.util.Resolvable
import inca.util.Scala
import truechange.{JavaLitType, LitType}

import scala.meta.quasiquotes._

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def asScala: meta.Type
  override def toString: String = prettyprint
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def asScala: meta.Type = t"Any"
}
case object TNothing extends Type {
  override def prettyprint: String = "Nothing"
  override def asScala: meta.Type = t"Nothing"
}

case class TData(name: Name) extends Type with Resolvable[TData.Target] {
  override def prettyprint: String = name.name
  override def asScala: meta.Type = t"truechange.URI"
}
object TData {
  trait Target
}

case class TLiteral(litType: LitType) extends Type {
  override def prettyprint: String = litType match {
    case JavaLitType(cl) if cl == classOf[java.lang.Integer] => "Int"
    case JavaLitType(cl) => cl.getSimpleName
    case _ => throw new UnsupportedOperationException
  }

  override def asScala: meta.Type = litType match {
    case JavaLitType(cl) =>  Scala.mkQualTypename(cl.getCanonicalName)
    case _ => throw new UnsupportedOperationException
  }
}
object TLiteral {
  val Bool: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Boolean]))
  val Int: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Integer]))
  val Long: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Long]))
  val Double: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Double]))
  val String: TLiteral = TLiteral(JavaLitType(classOf[java.lang.String]))
}

case class TScala(ty: Scala[meta.Type]) extends Type {
  override def prettyprint: String = s"`${ty.syntax}`"
  override def asScala: meta.Type = ty.tree
}
object TScala {
  def apply(typeString: String): TScala = {
    import meta.parsers._
    new TScala(Scala(typeString.parse[meta.Type].get))
  }
}
object TScalaBoolean extends TScala(Scala(t"Boolean"))
object TScalaInt extends TScala(Scala(t"Int"))
object TScalaLong extends TScala(Scala(t"Long"))
object TScalaDouble extends TScala(Scala(t"Double"))
object TScalaString extends TScala(Scala(t"String"))
object TScalaAny extends TScala(Scala(t"Any"))
