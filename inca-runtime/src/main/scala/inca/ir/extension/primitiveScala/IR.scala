package inca.ir.extension.primitiveScala

import inca.Scala
import inca.ir.*


case class TScala(ty: Scala.Type) extends Type
object TScala:
  def int = TScala(Scala.TypeName("Int"))
  def double = TScala(Scala.TypeName("Double"))
  def bool = TScala(Scala.TypeName("Boolean"))

case class Constant[T](value: Scala.Literal[T], ty: TScala) extends Term
case class Application(out: Term, ty: TScala, fun: Scala.Lam, args: Seq[Term]) extends Atom:
  override def vars: Seq[Var] = out.vars ++ args.flatMap(_.vars)


trait IR extends BaseIR:
  override val name: String = "PrimitiveScala"
  override def language: Language = super.language + new IR {}
  // We can not lower this IR any further
  override def requires: Language = Language(new IR {})

object IR extends IR { }