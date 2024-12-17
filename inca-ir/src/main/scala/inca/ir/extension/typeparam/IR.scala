package inca.ir.extension.typeparam

import inca.ir.{BaseIR, Language, ModuleEntry, Name, Ref, RefByName, Type}

object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "TypeParam"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class TypeVar(name: Name) extends Type

case class ParametricModuleEntry(typeParams: Seq[Name], entry: ModuleEntry) extends ModuleEntry:
  def withName(name: String): ParametricModuleEntry =
    ParametricModuleEntry(typeParams, entry.withName(name))
  override val name: Name = entry.name

object ParametricModuleEntry:
  def make(tyParams: Seq[Name], entry: ModuleEntry): ModuleEntry =
    if (tyParams.isEmpty)
      entry
    else
      ParametricModuleEntry(tyParams, entry)

case class TypeApplication[Target <: ModuleEntry](name: Name, args: Seq[Type]) extends Ref[Target]:
  override def unqualifiedName: Name = name
  override def path: Seq[Name] = Seq()

object TypeApplication:
  def make[Target <: ModuleEntry](name: Name, args: Seq[Type]): Ref[Target] =
    if (args.isEmpty)
      RefByName(name)
    else
      TypeApplication(name, args)