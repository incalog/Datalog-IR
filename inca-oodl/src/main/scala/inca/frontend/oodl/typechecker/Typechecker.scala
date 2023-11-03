package inca.frontend.oodl.typechecker

import inca.frontend.oodl.syntax.{Module, Private}
import inca.ir.typing.Resolvable
import inca.ir.util.SourceLocation

class Typechecker extends TypeContext with TypeIO:
  def typecheck(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(typecheck)
  }

  def typecheck(module: Module): Unit = scopedTypeContext {
    val moduleNames = module.name +: module.imports.map(_.name)
    val classNames = module.classes.map(_.name)

    for (name <- moduleNames.toSet.intersect(classNames.toSet))
      error(s"Module $name is shadowed by class $name", name)

    for (imp <- module.imports; importedModule <- lookupModule(imp.name)) {
      resolveTarget(imp)(importedModule)

      for (clazz <- importedModule.classes if !clazz.vis.contains(Private))
        bindClass(clazz, importedModule)
    }

    // detect inheritance cycles
    /*val inheritanceGraph = InheritanceGraph(module.classes)
    inheritanceGraph.cycles.foreach { c =>
      error(s"Cycle in inheritance hierarchy: ${c.map(_.name.raw).mkString(" <- ")}", c: _*)
    }*/
  }

  def resolveTarget[T](term: Resolvable[T] with SourceLocation)(computeTarget: => T): T = {
    val newTarget = computeTarget
    term.resolved(newTarget, force = true)
    newTarget
  }
