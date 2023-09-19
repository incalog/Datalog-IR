package inca.frontend.objectoriented.transformations
import inca.frontend.objectoriented.core.{ClassDef, MethodDef}
import inca.frontend.objectoriented.core._


class Monomorphize(val mod: Module) extends ModuleLowering {
  override def module: Module = mod

  override def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): Seq[MethodDef] = super.transMethodInternal(methodDef, classDef)

  override def transClassInternal(classDef: ClassDef): Seq[ClassDef] = super.transClassInternal(classDef)

  def transformModule(module: Module) : Module = ???
}
