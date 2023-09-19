package inca.frontend.objectoriented.transformations
import inca.frontend.objectoriented.core
import inca.frontend.objectoriented.core.{ClassDef, MethodDef}

class Monomorph extends ModuleLowering {
  override def module: core.Module = ???

  // override def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): Seq[MethodDef] = super.transMethodInternal(methodDef, classDef)

  override def transClassInternal(classDef: ClassDef): Seq[ClassDef] = super.transClassInternal(classDef)
}
