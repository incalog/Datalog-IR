package inca.ir.extension.typeparam

import inca.ir.{BaseIR, Language, ModuleEntry, Name, Ref, Type}

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "TypeParam"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class TypeVar(name: Name) extends Type

case class ParametricModuleEntry(typeParams: Seq[Name], entry: ModuleEntry) extends ModuleEntry:
  override val name: Name = entry.name

case class TypeApplication[Target](name: Name, args: Seq[Type]) extends Ref[Target]
