package inca.ir.typing

import inca.ir.{Module, ModuleEntry, Name, Param, Term, Type, Var}

trait BaseIRTypeContext extends TypeIO:
  var modules: Map[Name, Module] = Map()
  var entries: Map[Name, ModuleEntry] = Map()

  case class VarInfo(target: Var.Target, ty: Type, mode: VarMode)

  var vars: Map[Name, VarInfo] = Map()

  protected def startContextTransaction(): Transaction = new Transaction(vars)
  class Transaction(oldVars: Map[Name, VarInfo]):
    private var committed: Boolean = false
    def commit(): Unit =
      if (committed)
        throw IllegalStateException(s"Transaction already committed")
      committed = true
    def abort(): Unit =
      if (committed)
        throw IllegalStateException(s"Transaction already committed")
      vars = oldVars
      committed = true


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

  def scopedVariables[T](vs: Set[Name])(f: => T): T = {
    val varsSaved = vs.map(v => v -> vars.get(v))
    val t = f
    varsSaved.foreach {
      case (v, None) => vars -= v
      case (v, Some(info)) => vars += v -> info
    }
    t
  }

  def bindModule(module: Module): Unit = {
    val name = module.name
    modules.get(name).foreach { bound =>
      error(s"Found multiple modules with same name $name", name, bound.name)
    }
    modules += (name -> module)
  }

  def registerModuleEntry(entry: ModuleEntry): Unit = {
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
    vars += name -> VarInfo(decl, ty, VarMode.Unbound)
  }

  def bindVar(name: Name): Unit = vars.get(name) match
    case None => error(s"Cannot bind unknown variable $name, which should have been registered before", name)
    case Some(info) => vars += name -> info.copy(mode = VarMode.Bound)

  def lookupModuleEntry(name: Name): Option[ModuleEntry] = entries.get(name)

  def lookupVar(name: Name): Option[VarInfo] = vars.get(name)

  def isBoundVar(name: Name): Boolean = vars.get(name) match
    case Some(VarInfo(_, _, VarMode.Bound)) => true
    case _ => false

  def isParam(name: Name): Boolean = vars.get(name).exists(_.target.isInstanceOf[Param])
