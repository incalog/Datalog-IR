package inca.frontend.objectoriented.lowering

import inca.frontend.objectoriented.core.{Annotation, ClassContent, ClassDef, ClassRef, Module, MonotoneAnnotation, Name, TClass, TScalaAny, TScalaString, TUnit, Visibility}
import inca.frontend.objectoriented.lowering.InsertMonotones.transformModule

object InsertMonotones {
  def transformModule(module: Module): Module =
    new InsertMonotones(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}

/**
 * This class adds missing definitions, such as an empty constructor definition or an implicit return statement to the
 * module. This should be executed before we attempt to typecheck a module.
 */
class InsertMonotones(val module: Module) extends ModuleLowering {
  override private[lowering] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    val transClasses = classes.map(transClass)

    val monoMapName = Name("MonoMap")
    val monotones = Seq(
      ClassDef(
        // hardcore the map types for now
        Seq(),
        None,
        monoMapName,
        Seq(), // No parent class for now
        Seq()
      )
    )

    Module(name, imports, monotones ++ transClasses)
  }
}