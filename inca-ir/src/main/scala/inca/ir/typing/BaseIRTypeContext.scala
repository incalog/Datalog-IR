package inca.ir.typing

import inca.ir.{Module, ModuleEntry, Name, Relation, Type, Var}

trait BaseIRTypeContext extends TypeIO:
  var modules: Map[Name, Module] = Map()
  var entries: Map[Name, ModuleEntry] = Map()
  var vars: Map[Name, (Var.Target, Type)] = Map()

  def scopedTypeContext[T](f: => T): T = {
    val modulesSaved = modules
    val entriesSaved = entries
    val varsSaved = vars
    val t = f
    vars = varsSaved
    entries = entriesSaved
    modules = modulesSaved
    t
  }

  def bindModule(module: Module): Unit = {
    val name = module.name
    modules.get(name).foreach { bound =>
      error(s"Found multiple modules with same name $name", name, bound.name)
    }
    modules += (name -> module)
  }

  def bindModuleEntry(entry: ModuleEntry): Unit = {
    val name = entry.name
    entries.get(name).foreach { bound =>
      error(s"Found multiple entries with same name $name", name, bound.name)
    }
    entries += (name -> entry)
  }

  def bindVar(name: Name, decl: Var.Target, ty: Type): Unit = {
    vars.get(name).foreach { case (previousDecl, _) =>
      error(s"Variable $name shadows previously defined variable $previousDecl", name, previousDecl)
    }
    vars += (name -> (decl, ty))
  }

  def lookupModuleEntry(name: Name): Option[ModuleEntry] = entries.get(name)

  def lookupVar(name: Name): Option[(Var.Target, Type)] = vars.get(name)
