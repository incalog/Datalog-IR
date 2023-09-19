package inca.frontend.objectoriented.transformations
import inca.frontend.objectoriented.core.{ClassDef, MethodDef}
import inca.frontend.objectoriented.core._

import inca.util.{Gensym, Scala, TupleOps}
import truechange.SortType


object Monomorphize {
  def transformModule(module: Module): Module = {
    new Monomorphize(module).transModule()
  }
}
class Monomorphize(val mod: Module) extends ModuleLowering {
  private val gensym: Gensym = new Gensym(Iterable.empty)

  var auxClassDefs: Set[ClassDef] = Set()
  var defnClassDefs: Map[Type, ClassDef] = Map()
  override def module: Module = mod

  override def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): Seq[MethodDef] = {
    val MethodDef(annos, vis, name, genericTypeParams, params, outType, body) = methodDef

    val newParams = transParams(params)
    val newBody = transStatements(body)
    val newOutTypes = ???

    Seq(methodDef)
  }

  override def transClassInternal(classDef: ClassDef): Seq[ClassDef] = super.transClassInternal(classDef)

  override private[transformations] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    // make sure the defun class name is unique
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(classes.map(_.name.raw))
    val transClasses = classes.flatMap(transClass)
    Module(name, imports, transClasses ++ auxClassDefs ++ defnClassDefs.values)
  }


}
