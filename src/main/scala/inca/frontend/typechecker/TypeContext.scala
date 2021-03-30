package inca.frontend.typechecker

import inca.frontend.core._

import scala.collection.immutable.MultiDict

trait TypeContext extends TypeIO {
  private var vars: Map[Name, (Var.Target, Type)] = Map()
  private var funs: MultiDict[Name, (Module, (Var.Target, TFun))] = MultiDict()
  private var dataDefs: Map[Name, DataDef] = Map()
  private var modules: Map[Name, Module] = Map()

  def scopedTypeContext[T](f: => T): T = {
    val varsSaved = vars
    val funsSaved = funs
    val modulesSaved = modules
    val t = f
    vars = varsSaved
    funs = funsSaved
    modules = modulesSaved
    t
  }

  def bindVar(name: Name, decl: Var.Target, ty: Type): Unit = {
    vars.get(name) foreach { case (previousDecl, _) =>
      error(s"Variable $name shadows previously defined variable $previousDecl", name, previousDecl)
    }
    vars += (name -> (decl, ty))
  }

  def lookupVar(name: Name): Option[(Var.Target,Type)] =
    vars.get(name) match {
      case Some(entry) => Some(entry)
      case None =>
        funs.get(name) match {
          case set if set.size == 1 =>
            Some(set.head._2)
          case set if set.size >= 2 =>
            val modules = set.toSeq.map(_._1)
            val modulesStr = modules.map(_.name).mkString(", ")
            error(s"Ambiguous call to $name, found definitions in $modulesStr", (name +: modules): _*)
            None
          case _ =>
            error(s"Unbound variable $name", name)
            None
        }
    }

  def isFreeVar(name: Name): Boolean =
    !vars.contains(name)

  def getBindings: Map[Name, Type] =
    vars.view.mapValues(_._2).toMap


  def bindFun(fun: FunctionDef, module: Module): Unit = {
    funs += fun.name -> (module, (fun, fun.funType))
  }

  def lookupCalled(name: Name): Option[(Var.Target, TFun)] =
    funs.get(name) match {
      case set if set.size == 1 =>
        Some(set.head._2)
      case set if set.size >= 2 =>
        val modules = set.toSeq.map(_._1)
        val modulesStr = modules.map(_.name).mkString(", ")
        error(s"Ambiguous call to $name, found definitions in $modulesStr", (name +: modules): _*)
        None
      case set if set.isEmpty => vars.get(name) match {
        case Some((trg, ty: TFun)) =>
          Some((trg, ty))
        case Some((_, ty)) =>
          error(s"Variable $name has type $ty, but required function type")
          None
        case None =>
          error(s"Unbound name $name", name)
          None
      }

    }

  def bindData(data: DataDef, module: Module): Unit = {
    dataDefs += data.name -> data
    data.constrs.foreach(c => funs += c.name -> (module, (c, c.constructorType(data))))
  }

  def isData(name: Name): Boolean =
    dataDefs.contains(name)

  def lookupData(name: Name): Option[DataDef] =
    dataDefs.get(name) match {
      case Some(data) => Some(data)
      case None =>
        error(s"Unbound data type $name", name)
        None
    }

  def bindModule(module: Module): Unit = {
    val name = module.name
    modules.get(name) foreach { bound =>
      error(s"Found multiple modules with same name $name", name, bound.name)
    }
    modules += (name -> module)
  }

  def lookupModule(name: Name): Option[Module] =
    modules.get(name) match {
      case Some(module) => Some(module)
      case None =>
        error(s"Unknown module $name", name)
        None
    }
}