package inca.ir.typing

import inca.ir.{Import, Module, ModuleEntry, Name, Param, Provide, Ref, Require, Term, Type, Var}

import scala.reflect.ClassTag

trait BaseIRTypeContext extends TypeIO:
  var modules: Map[Name, Module] = Map()

  // (module, imported module) -> module alias name
  var moduleImports: Map[(Module, Name), Name] = Map()
  var provides: Map[(Module, Name), Provide[_]] = Map()
  var requires: Map[(Module, Name), Require] = Map()

  // (module, entry name) -> entry
  var entries: Map[Name, ModuleEntry] = Map()

  case class VarInfo(target: Var.Target, ty: Type, mode: VarMode)

  var vars: Map[Name, VarInfo] = Map()

  enum Dependency:
    case Postive
    case Negative

  object Dependency:
    def apply(neg: Boolean): Dependency = if (neg) Negative else Postive

  protected val dependencyGraph: DependencyGraph = new DependencyGraph

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
    val moduleAliasesSaved = moduleImports
    val entriesSaved = entries
    val varsSaved = vars
    val t = f
    vars = varsSaved
    entries = entriesSaved
    moduleImports = moduleAliasesSaved
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

  def bindModuleImport(imp: Import)(implicit module: Module): Unit = moduleImports.get((module, imp.as)) match
    case Some(_) => error(s"Found multiple aliases with the same name ${imp.as}", imp)
    case _ =>
      val moduleRef = imp.module
      lookupModule(moduleRef.name) match
        case Some(mod) => imp.module.resolved(mod)
        case _ => error(s"Could not resolve module ${moduleRef.name}", imp)
      moduleImports += ((module, imp.as) -> moduleRef.name)

  def registerProvide(entry: Provide[_])(implicit module: Module): Unit = {
    val name = entry.name
    provides.get((module, name)).foreach { bound =>
      error(s"Found multiple provides with same name $name", name, bound.name)
    }
    provides += ((module, name) -> entry)
  }

  def registerRequire(entry: Require)(implicit module: Module): Unit = {
    val name = entry.name
    requires.get((module, name)).foreach { bound =>
      error(s"Found multiple requires with same name $name", name, bound.name)
    }
    requires += ((module, name) -> entry)
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

  def lookupModuleByAlias(alias: Name)(implicit module: Module): Option[Module] =
    moduleImports.get((module, alias)) match
      case Some(name) => modules.get(name)
      case _ => None

  def lookupRequire[R <: Require](name: Name, module: Module)(implicit tag: ClassTag[R]): Option[R] =
    requires.get((module, name)) match
      case Some(req) if !tag.runtimeClass.isInstance(req) => None // not the kind of requirement we expected
      case Some(req) => Some(req.asInstanceOf[R])
      case _ => None

  def lookupProvide[P <: Provide[_]](name: Name, module: Module)(implicit tag: ClassTag[P]): Option[P] =
    provides.get((module, name)) match
      case Some(prov) if !tag.runtimeClass.isInstance(prov) => None // not the kind of requirement we expected
      case Some(prov) => Some(prov.asInstanceOf[P])
      case _ => None

  def lookupModule(name: Name): Option[Module] = modules.get(name)

  def lookupModuleEntry(name: Name): Option[ModuleEntry] = entries.get(name)

  def lookupVar(ref: Ref[Var.Target]): Option[VarInfo] =
    vars.get(ref.name) match
      case None => None
      case Some(info) =>
        ref.resolved(info.target)
        Some(info)

  def isBoundVar(name: Name): Boolean = vars.get(name) match
    case Some(VarInfo(_, _, VarMode.Bound)) => true
    case _ => false

  def isParam(name: Name): Boolean = vars.get(name).exists(_.target.isInstanceOf[Param])

  def addDependency(from: ModuleEntry, to: ModuleEntry, info: DependencyInfo): Unit =
    dependencyGraph.addEdge(from, to, info)

  def getDependencyGraph: DependencyGraph = dependencyGraph
