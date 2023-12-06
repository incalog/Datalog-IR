package inca.ir.extension.typeparam

import inca.ir.{BaseIR, Language, ModuleEntry, Name, Ref, RefByName, Type}

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "TypeParam"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class TypeVar(name: Name) extends Type:
  override def toString: String = name.name

case class ParametricModuleEntry(typeParams: Seq[Name], entry: ModuleEntry) extends ModuleEntry:
  override def toString: String = s"with[${typeParams.mkString(", ")}] $entry"
  def withExtendedName(suffix: String): ParametricModuleEntry =
    ParametricModuleEntry(typeParams, entry.withExtendedName(suffix))
  override val name: Name = entry.name

case class TypeApplication[Target](name: Name, args: Seq[Type]) extends Ref[Target]:
  override def toString: String = s"$name[${args.mkString(", ")}]"
object TypeApplication:
  def make[Target](name: Name, args: Seq[Type]): Ref[Target] =
    if (args.isEmpty)
      RefByName(name)
    else
      TypeApplication(name, args)