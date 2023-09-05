package inca.ir.typing

import inca.ir.{Module, ModuleEntry, Name, Param, Relation, Term, TermType, Type, Var}

trait BaseIRTypeContext extends TypeIO:
  var modules: Map[Name, Module] = Map()
  var entries: Map[Name, ModuleEntry] = Map()

  case class VarInfo(target: Var.Target, ty: Type, positive: Boolean):
    def termType: TermType = TermType(ty, positive)

  var vars: Map[Name, VarInfo] = Map()

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

  def registerVar(name: Name, decl: Var.Target, ty: Type): Unit = {
    vars.get(name).foreach { case VarInfo(previousDecl, _, _) =>
      error(s"Variable $name shadows previously defined variable $previousDecl", name, previousDecl)
    }
    vars += name -> VarInfo(decl, ty, false)
  }

  def bindVar(name: Name): Unit = vars.get(name) match
    case None => error(s"Cannot bind unknown variable $name, which should have been registered before", name)
    case Some(info) => vars += name -> info.copy(positive = true)

  def lookupModuleEntry(name: Name): Option[ModuleEntry] = entries.get(name)

  def lookupVar(name: Name): Option[VarInfo] = vars.get(name)

  def isBoundVar(name: Name): Boolean = vars.get(name) match
    case None => false
    case Some(VarInfo(_, _, positive)) => positive

  inline def isFreeVar(name: Name): Boolean = !isBoundVar(name)

  inline def isFreeVar(term: Term): Boolean = term match
    case Var(name) => !isBoundVar(name)
    case _ => false

  def isParam(name: Name): Boolean = vars.get(name).exists(_.target.isInstanceOf[Param])
