package inca.foreign.scala.ir.primitive

import inca.foreign.scala.syntax.Scala
import inca.ir.*
import inca.ir.extension.foreign.{ForeignLanguage, ForeignTerm, ForeignType}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import inca.ir.extension.string.TString

object ScalaInca extends ForeignLanguage:
  type Code = Scala.Term

  def compileType(ty: Type): ScalaType = ty match
    case sty@ScalaType(_) => sty
    case TString => ScalaType.string
    case TInt => ScalaType.int
    case TDouble => ScalaType.double
    case TBoolean => ScalaType.bool
    case _ => throw IllegalStateException(s"No scala conversion for Type $ty")

case class ScalaType(ty: Scala.Type) extends ForeignType
object ScalaType:
  def string: ScalaType = ScalaType(Scala.TypeName("String"))
  def int: ScalaType = ScalaType(Scala.TypeName("Int"))
  def double: ScalaType = ScalaType(Scala.TypeName("Double"))
  def bool: ScalaType = ScalaType(Scala.TypeName("Boolean"))

case class ScalaTerm(code: Scala.Term, ty: ScalaType, args: Seq[Term]) extends ForeignTerm(args):
  override val lang: ScalaInca.type = ScalaInca
  override def vars: Seq[Var] = args.flatMap(_.vars)

  def inTypes: Seq[ScalaType] = args.map { a =>
    val tty = a.typ match
      case Some(TermType(ty, _)) => ty
      case _ => throw IllegalStateException(s"Untyped argument $a")
    ScalaInca.compileType(tty)
  }

  def outTypes: Seq[ScalaType] = Seq(ty)

  override def toString: String = s"""($code)(${args.mkString(", ")})"""


trait IR extends BaseIR:
  override val name: String = "PrimitiveScala"
  override def language: Language = super.language + new IR {}
  // We can not lower this IR any further
  override def requires: Language = Language(new IR {})
object IR extends IR { }