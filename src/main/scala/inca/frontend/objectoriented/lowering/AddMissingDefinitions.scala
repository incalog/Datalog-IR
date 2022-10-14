package inca.frontend.objectoriented.lowering

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import inca.util.Gensym


object AddMissingDefinitions {
  def transformModule(module: Module): Module =
    new AddMissingDefinitions(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}

/**
 * This class adds missing definitions, such as an empty constructor definition to the module. This should be executed
 * before we attempt to typecheck a module.
 */
class AddMissingDefinitions(val module: Module) extends ModuleLowering {

  override def transClassInternal(classDef: ClassDef): ClassDef = {
    val ClassDef(annos, vis, name, parents, content) = classDef
    val addedContent = generateMissingConstructor(classDef)
    super.transClassInternal(ClassDef(annos, vis, name, parents, content ++ addedContent))
  }

  private def generateMissingConstructor(classDef: ClassDef): Option[ConstructorDef] = {
    if (classDef.constructors.isEmpty)
      Some(ConstructorDef(Seq(), None, Seq(), Seq()))
    else
      None
  }
}
